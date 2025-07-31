# Advanced Customer Choice and Revenue Analysis
# ================================================

# Load and validate required packages
required_packages <- c("tidyverse", "zoo", "mlogit", "forecast", "lubridate", 
                       "ggplot2", "corrplot", "gridExtra", "broom", "knitr")

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% installed.packages()[, "Package"]]
  if(length(missing) > 0) {
    cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, dependencies = TRUE, quiet = TRUE)
  }
}

install_if_missing(required_packages)

# Load libraries with error handling
suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(mlogit)
  library(forecast)
  library(lubridate)
  library(ggplot2)
  library(corrplot)
  library(gridExtra)
  library(broom)
  library(knitr)
})

set.seed(42)

# Configuration parameters
CONFIG <- list(
  n_customers = 50,
  n_quarters = 8,  # Extended to 2 years
  products = c("Premium", "Standard", "Basic"),
  regions = c("North", "South", "East", "West", "Central"),
  forecast_horizon = 6
)

# ================================================
# 1. ENHANCED DATA GENERATION
# ================================================

generate_synthetic_data <- function(config) {
  
  cat("Generating synthetic customer choice data...\n")
  
  # Create customer base with realistic demographics
  customers_df <- tibble(
    customer_id = paste0("C", str_pad(1:config$n_customers, 3, pad = "0")),
    age = pmax(18, pmin(80, round(rnorm(config$n_customers, 35, 12)))),
    region = sample(config$regions, config$n_customers, replace = TRUE,
                    prob = c(0.25, 0.2, 0.2, 0.2, 0.15)),
    income_segment = sample(c("Low", "Medium", "High"), config$n_customers, 
                            replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    loyalty_score = runif(config$n_customers, 0, 1)
  )
  
  # Generate quarters
  quarters <- paste0(rep(2023:2024, each = 4), " Q", rep(1:4, 2))[1:config$n_quarters]
  
  # Create expanded dataset
  df_base <- expand_grid(
    customer_id = customers_df$customer_id,
    quarter = quarters,
    product = config$products
  ) %>%
    # Join customer demographics
    left_join(customers_df, by = "customer_id")
  
  # Add product attributes and calculate utilities
  df <- df_base %>%
    mutate(
      quarter_num = as.numeric(str_extract(quarter, "(?<=Q)\\d")),
      
      base_price = case_when(
        product == "Premium" ~ runif(n(), 180, 220),
        product == "Standard" ~ runif(n(), 100, 140),
        product == "Basic" ~ runif(n(), 50, 90),
        TRUE ~ 100
      ),
      
      seasonal_adj = case_when(
        quarter_num == 4 ~ 1.2,
        quarter_num == 1 ~ 0.9,
        TRUE ~ 1.0
      ),
      
      marketing_spend = runif(n(), 0, 1000) * seasonal_adj,
      marketing_score = pmin(1, marketing_spend / 500),
      price = base_price * seasonal_adj * runif(n(), 0.9, 1.1),
      
      sales_base = case_when(
        income_segment == "High" & product == "Premium" ~ runif(n(), 80, 120),
        income_segment == "High" & product == "Standard" ~ runif(n(), 60, 100),
        income_segment == "High" & product == "Basic" ~ runif(n(), 20, 40),
        income_segment == "Medium" & product == "Premium" ~ runif(n(), 40, 80),
        income_segment == "Medium" & product == "Standard" ~ runif(n(), 70, 110),
        income_segment == "Medium" & product == "Basic" ~ runif(n(), 50, 90),
        income_segment == "Low" & product == "Premium" ~ runif(n(), 10, 30),
        income_segment == "Low" & product == "Standard" ~ runif(n(), 40, 80),
        income_segment == "Low" & product == "Basic" ~ runif(n(), 60, 100),
        TRUE ~ 50
      ),
      
      sales = pmax(0, sales_base * (1 + marketing_score * 0.3) * seasonal_adj),
      revenue = sales * price,
      
      product_preference = case_when(
        income_segment == "High" & product == "Premium" ~ 2,
        income_segment == "Medium" & product == "Standard" ~ 1.5,
        income_segment == "Low" & product == "Basic" ~ 1,
        TRUE ~ 0
      ),
      
      utility = -0.01 * price +
        0.02 * marketing_score * 100 +
        0.01 * loyalty_score * 100 +
        product_preference +
        rnorm(n(), 0, 0.5),
      
      choice = 0
    ) %>%
    
    # Assign choices based on utility maximization with tie-break
    group_by(customer_id, quarter) %>%
    mutate(
      choice = ifelse(utility == max(utility, na.rm = TRUE), 1, 0)
    ) %>%
    mutate(
      choice = ifelse(sum(choice) > 1,
                      as.numeric(row_number(desc(utility)) == 1),
                      choice)
    ) %>%
    ungroup() %>%
    
    # Convert to appropriate types
    mutate(
      quarter = as.yearqtr(quarter, format = "%Y Q%q"),
      customer_id = as.factor(customer_id),
      product = factor(product, levels = c("Basic", "Standard", "Premium"), ordered = TRUE),
      region = as.factor(region),
      income_segment = factor(income_segment, levels = c("Low", "Medium", "High"), ordered = TRUE)
    )
  
  return(df)
}

# ================================================
# 2. DATA VALIDATION AND QUALITY CHECKS  
# ================================================

validate_data <- function(df) {
  
  cat("Performing data validation...\n")
  
  # Check for duplicates
  dup_check <- df %>%
    group_by(customer_id, quarter, product) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)
  
  if (nrow(dup_check) > 0) {
    stop("Duplicates detected in (customer_id, quarter, product) combinations!")
  }
  
  # Validate choice occasions
  choice_validation <- df %>%
    group_by(customer_id, quarter) %>%
    summarise(
      n_products = n_distinct(product),
      n_chosen = sum(choice, na.rm = TRUE),
      total_utility = sum(utility, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (any(choice_validation$n_products < 2)) {
    stop("Some choice occasions have fewer than 2 product alternatives.")
  }
  
  # Fix choice occasions that don't have exactly 1 choice
  problem_occasions <- choice_validation %>% 
    filter(n_chosen != 1)
  
  if (nrow(problem_occasions) > 0) {
    cat("Fixing", nrow(problem_occasions), "choice occasions with incorrect choice counts...\n")
    
    problem_ids <- problem_occasions %>%
      select(customer_id, quarter)
    
    df <- df %>%
      group_by(customer_id, quarter) %>%
      mutate(
        choice = case_when(
          paste(customer_id, quarter) %in% paste(problem_ids$customer_id, problem_ids$quarter) ~ 
            ifelse(utility == max(utility, na.rm = TRUE), 1, 0),
          TRUE ~ choice
        )
      ) %>%
      ungroup()
  }
  
  cat("✓ Data validation completed successfully\n")
  return(df)
}

# ================================================
# 3. EXPLORATORY DATA ANALYSIS
# ================================================

perform_eda <- function(df) {
  
  cat("Performing exploratory data analysis...\n")
  
  # Summary statistics by product
  product_summary <- df %>%
    group_by(product) %>%
    summarise(
      avg_price = mean(price, na.rm = TRUE),
      avg_sales = mean(sales, na.rm = TRUE), 
      avg_revenue = mean(revenue, na.rm = TRUE),
      choice_share = mean(choice, na.rm = TRUE),
      .groups = "drop"
    )


