# === Load Required Libraries ===
library(tidyverse)     # includes dplyr, ggplot2, etc.
library(mlogit)        # multinomial logistic regression
library(lubridate)     # time/date handling
library(forecast)      # ARIMA modeling
library(tsibble)       # time series data frame
library(readr)
libray(ggplot)
library(zoo)

# === Load the dataset ===
df <- read_csv("product_choice_time_series.csv")

# === Data Cleaning and Preprocessing ===
df <- df %>%
  mutate(
    customer_id = as.factor(customer_id),
    product = as.factor(product),
    quarter = as.yearqtr(quarter),
    choice = as.logical(choice)  # ensure it's binary TRUE/FALSE
  )

# === Prepare Data for mlogit ===
mlogit_data <- mlogit.data(df, choice = "choice", shape = "long",
                           chid.var = "customer_id", alt.var = "product",
                           id.var = "customer_id")

# === Fit Multinomial Logit Model ===
mlogit_model <- mlogit(choice ~ sales + revenue + age + marketing_score | 0, data = mlogit_data)
summary(mlogit_model)

# === Time Series Forecasting with ARIMA (on total revenue by quarter) ===

# Aggregate revenue by quarter
ts_data <- df %>%
  group_by(quarter) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  arrange(quarter)

# Create time series object
revenue_ts <- ts(ts_data$total_revenue, start = c(2022, 1), frequency = 4)

# Fit ARIMA model
arima_model <- auto.arima(revenue_ts)
summary(arima_model)

# Forecast next 4 quarters
forecast_result <- forecast(arima_model, h = 4)
plot(forecast_result, main = "Revenue Forecast for Next 4 Quarters")

# Plot forecast
autoplot(forecast_result) + 
  ggtitle("Forecasted Revenue for Next 4 Quarters") +
  xlab("Quarter") + ylab("Revenue")

