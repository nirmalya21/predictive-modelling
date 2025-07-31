# Load necessary libraries
library(tidyverse)
library(mlogit)
library(lubridate)
library(zoo)
library(forecast)
library(readr)

# -----------------------------
# 1. Load and Prepare Data
# -----------------------------
# Read CSV (Make sure the CSV is named correctly)
data <- read_csv("product_choice_time_series.csv")

# Convert 'quarter' to year-quarter format
data$quarter <- as.yearqtr(data$quarter, format = "%Y Q%q")

# Add observation ID for mlogit
data <- data %>%
  mutate(obs_id = row_number(),
         customer_id = as.factor(customer_id),
         product = as.factor(product))

# -----------------------------
# 2. Prepare Data for mlogit
# -----------------------------
mlogit_data <- mlogit.data(data,
                           choice = "choice",
                           shape = "long",
                           alt.var = "product",
                           id.var = "customer_id",
                           chid.var = "obs_id")

# Fit Multinomial Logit Model
mlogit_model <- mlogit(choice ~ sales + revenue + marketing_score | age + region,
                       data = mlogit_data)

# Print summary
cat("\n--- Multinomial Logit Model Summary ---\n")
print(summary(mlogit_model))

# -----------------------------
# 3. Time Series Forecasting
# -----------------------------
# Aggregate total revenue by quarter
ts_data <- data %>%
  group_by(quarter) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  arrange(quarter)

# Create time series object
revenue_ts <- ts(ts_data$total_revenue, 
                 start = c(year(min(ts_data$quarter)), quarter(min(ts_data$quarter))), 
                 frequency = 4)

# Fit ARIMA model
arima_model <- auto.arima(revenue_ts)

# Print ARIMA model summary
cat("\n--- ARIMA Model Summary ---\n")
print(summary(arima_model))

# Forecast next 4 quarters
forecast_result <- forecast(arima_model, h = 4)

# Plot forecast
autoplot(forecast_result) + 
  ggtitle("Forecasted Revenue for Next 4 Quarters") +
  xlab("Quarter") + ylab("Revenue")

