# 1. Packages -----------------------------------
source("header.R")
library(tidyquant)


options(scipen = 999)

etf <- get_data_vnstock("043_E1VFVN30_dai_han/01_data/raw/E1VFVN30.csv") %>% 
  filter(date >= "2015-01-01" & date <= "2021-12-31")

etf <- etf %>% 
  group_by(year = year(date), month = month(date)) %>% 
  slice(1) %>% 
  ungroup()

# Set investment amoiunt
investment_per_month <- 1000000

# Calculate units purchased each month
etf_monthly <- etf_monthly %>%
  mutate(units_purchased = investment_per_month / price)

# Calculate total units purchased
total_units <- sum(etf_monthly$units_purchased)

# Get the final price in December 2021
final_price <- tail(filter(etf_monthly, year == 2021 & month == 12), 1)$price

# Calculate final value of investment
final_value <- total_units * final_price

# Calculate total investment
total_investment <- nrow(etf_monthly) * investment_per_month

# Calculate returns
returns <- (final_value - total_investment) / total_investment * 100


# Calculate annualized returns
number_of_years <- time_length(difftime(as.Date("2021-12-01"), as.Date("2015-01-01")), "years")


annualized_returns <- (final_value / total_investment)^(1/number_of_years) - 1