# predictive-modelling
## Project Overview

The project simulates customer product choices across different income segments, regions, and quarters with three product tiers (Basic, Standard, Premium). It uses a multinomial logit model (`mlogit`) to analyze choice behavior, evaluates model performance, and forecasts revenue using advanced time series models like ARIMA and ETS.

---

## Features

- **Synthetic Data Generation**: Creates realistic customer demographics, quarterly product pricing, marketing spend, and purchase choices.
- **Data Validation**: Checks for duplicates, ensures one choice per customer per quarter, and fixes inconsistencies.
- **Exploratory Data Analysis (EDA)**: Summarizes product performance, income segment choice rates, and visualizes revenue trends, market share, and price sensitivity.
- **Advanced Choice Modeling**: Fits multinomial logit models with product attributes and customer characteristics, including models with product-specific coefficients.
- **Model Evaluation**: Computes confusion matrix, accuracy, precision, recall, and F1 score for choice predictions.
- **Revenue Forecasting**: Applies ARIMA, ETS, STL, and neural network models for quarterly revenue forecasting with model comparison based on AIC.

---

## Installation

Run the following R code to install required packages if missing:

```r
required_packages <- c("tidyverse", "zoo", "mlogit", "forecast", "lubridate", 
                       "ggplot2", "corrplot", "gridExtra", "broom", "knitr")

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% installed.packages()[,"Package"]]
  if(length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(required_packages)
