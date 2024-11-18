# global.R
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(elmer)
library(ggplot2)
library(dplyr)
library(scales)

# Generate dummy data for banking products
set.seed(123)
dates <- seq(as.Date('2023-01-01'), as.Date('2024-01-01'), by='days')
n_transactions <- 1000  # Increased sample size

# Define banking products
products <- c(
  'Easy Saver Account',
  'Premium Savings Plus',
  'Student Savings Account',
  'Senior Citizen Savings',
  'Fixed Term Deposit'
)

# Product characteristics
product_characteristics <- tibble(
  product_name = products,
  interest_rate = c(2.5, 3.5, 2.0, 3.8, 4.2),  # Annual interest rates
  min_balance = c(0, 5000, 0, 1000, 10000),    # Minimum balance requirements
  monthly_fee = c(0, 5, 0, 0, 0),              # Monthly maintenance fees
  withdrawal_limit = c(Inf, 4, 6, 3, 0)        # Monthly withdrawal limits
)

# Generate transaction data
dummy_data <- tibble(
  date = sample(dates, n_transactions, replace = TRUE) %>% sort(),
  product = sample(products, n_transactions, replace = TRUE),
  transaction_type = sample(
    c('deposit', 'withdrawal', 'interest', 'fee'), 
    n_transactions,
    prob = c(0.45, 0.35, 0.15, 0.05),
    replace = TRUE
  ),
  amount = case_when(
    transaction_type == 'deposit' ~ runif(n_transactions, 100, 10000),
    transaction_type == 'withdrawal' ~ -runif(n_transactions, 50, 2000),
    transaction_type == 'interest' ~ runif(n_transactions, 1, 100),
    transaction_type == 'fee' ~ -runif(n_transactions, 0, 10)
  )
) %>%
  group_by(product) %>%
  mutate(
    balance = cumsum(amount) + 
      sample(5000:50000, 1),  # Random starting balance for each product
    customer_count = sample(100:1000, 1),  # Random number of customers per product
    avg_customer_age = sample(18:65, 1),   # Average customer age for the product
    customer_satisfaction = sample(70:95, 1)/100  # Customer satisfaction score
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), round, 2))

# Initialize Claude chat object
chat <- chat_claude(
  model = "claude-3-sonnet-20240229",
  system_prompt = "You are a banking product analyst. Help analyze savings product performance data and provide insights. Focus on key metrics like balance growth, customer acquisition, interest rates, and product profitability. Format currency with proper $ signs and commas.",
  echo = FALSE
)

# Product metrics summary
product_metrics <- dummy_data %>%
  group_by(product) %>%
  summarise(
    total_balance = last(balance),
    total_customers = last(customer_count),
    avg_transaction = mean(abs(amount)),
    transaction_volume = n(),
    net_inflow = sum(amount[transaction_type == "deposit"]) + 
      sum(amount[transaction_type == "withdrawal"]),
    interest_paid = -sum(amount[transaction_type == "interest"]),
    fees_collected = -sum(amount[transaction_type == "fee"]),
    avg_cust_age = last(avg_customer_age),
    cust_satisfaction = last(customer_satisfaction)
  ) %>%
  left_join(product_characteristics, by = c("product" = "product_name"))