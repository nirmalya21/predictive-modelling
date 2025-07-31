# Required packages
library(tidyverse)
library(forecast)
library(mlogit)
library(lubridate)

# Load dataset
data <- read.csv("product_choice_time_series.csv")

# Convert quarter to date
data$quarter <- as.yearqtr(data$quarter)

# ----------- TIME SERIES FORECASTING PART -------------

# Forecast revenue per product
forecast_list <- list()

for (prod in unique(data$product)) {
  ts_data <- data %>%
    filter(product == prod) %>%
    group_by(quarter) %>%
    summarise(revenue = sum(revenue)) %>%
    arrange(quarter)
  
  ts_rev <- ts(ts_data$revenue, frequency = 4)
  fit <- auto.arima(ts_rev)
  fc <- forecast(fit, h = 2)  # forecast next 2 quarters
  forecast_list[[prod]] <- data.frame(
    quarter = seq(max(ts_data$quarter) + 0.25, by = 0.25, length.out = 2),
    product = prod,
    revenue_forecast = fc$mean
  )
}

revenue_forecasts <- bind_rows(forecast_list)

# Merge back into dataset (only use known quarters for prediction)
data <- data %>%
  left_join(revenue_forecasts, by = c("quarter", "product")) %>%
  mutate(
    revenue_final = ifelse(is.na(revenue_forecast), revenue, revenue_forecast)
  )

# ----------- MULTINOMIAL LOGIT MODEL ------------------

# Prepare data for mlogit
mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  chid.var = "customer_id",
  alt.var = "product"
)

# Fit model using forecasted revenue and sales + demographics
model <- mlogit(
  choice ~ revenue_final + sales + age + marketing_score | 0,
  data = mlogit_data
)

summary(model)
