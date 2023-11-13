# 1. Packages -----------------------------------
source("header.R")
library(tidyquant)


options(scipen = 999)

etf <- get_data_vnstock("043_E1VFVN30_dai_han/01_data/raw/E1VFVN30.csv") %>% 
  filter(date >= "2015-01-01" & date <= "2021-12-31")

# Convert daily prices to monthly.
etf <- etf %>% 
  group_by(year = year(date), month = month(date)) %>% 
  slice(1) %>% 
  ungroup()

# Set investment amoiunt
investment_per_month <- 1000000


# BASIC ##############################

# MONTHLY DATA
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


###########################################################

# Rolling returns
# Function to calculate rolling returns (MONTHLY data)


# Giả định rằng mỗi khoản đầu tư bắt đầu tại đầu mỗi tháng
#' @param rolling_period_years period để tính rolling returns. Cụ thể là 1 năm


calculate_rolling_returns <- function(etf, investment_per_month, rolling_period_years) {
  
  # Calculate units purchased each month
  etf <- etf %>%
    mutate(units_purchased = investment_per_month / price)
  
  # Create a tibble to store rolling returns
  rolling_returns <- tibble(
    StartDate = as.Date(character()),
    EndDate = as.Date(character()),
    AnnualizedReturns = numeric()
  )
  
  # Calculate rolling returns
  for (i in 1:(nrow(etf) - 12 * rolling_period_years)) {
    start_date <- etf$date[i]
    end_date <- etf$date[i + 12 * rolling_period_years]
    
    # Calculate total units purchased
    total_units <- sum(etf$units_purchased[i:(i + 12 * rolling_period_years - 1)])
    
    # Get the final price
    final_price <- etf$price[i + 12 * rolling_period_years]
    
    # Calculate final value of investment
    final_value <- total_units * final_price
    
    # Calculate total investment
    total_investment <- 12 * rolling_period_years * investment_per_month
    
    # Calculate annualized returns
    annualized_returns <- (final_value / total_investment)^(1/rolling_period_years) - 1
    
    # Add to rolling_returns tibble
    rolling_returns <- add_row(rolling_returns, StartDate = start_date, EndDate = end_date, AnnualizedReturns = annualized_returns)
  }
  
  return(rolling_returns)
}


##################################################################

# DAILY DATA
# Calculate rolling returns
# Version 3.0

# Tính toán dựa trên ngày giao dịch trong tháng

etf <- get_data_vnstock("043_E1VFVN30_dai_han/01_data/raw/E1VFVN30.csv") %>% 
  filter(date >= "2015-01-01" & date <= "2021-12-31")

investment_per_month <- 1000000


# Function to calculate rolling returns

calculate_rolling_returns <- function(etf, investment_per_month, trading_day_of_month, rolling_period_days) {
  
  # Xác định số ngày giao dịch trong tháng
  etf <- etf %>%
    group_by(year = year(date), month = month(date)) %>%
    mutate(trading_day = row_number()) %>% 
    ungroup()
  
  # Tính số chứng chỉ quỹ mua tại ngày bất kì trong tháng
  #' @param trading_day_of_month là ngày muốn bắt đầu. Ví dụ tại ngày 1 hoặc 15,
  # là ngày giao dịch thứ nhất và thứ 15 trong tháng.

  etf <- etf %>%
    mutate(units_purchased = if_else(trading_day == trading_day_of_month, investment_per_month / price, 0),
           
           #' @param investment_date Để hiển thị ngày thực sự dùng tiền để đầu tư
           investment_date = if_else(trading_day == trading_day_of_month, date, as.Date(NA)))
  
  # Tạo tibble để chứa giá trị rolling returns
  rolling_returns <- tibble(
    StartDate = as.Date(character()),
    EndDate = as.Date(character()),
    AnnualizedReturn = numeric()
  )
  
  # Tính rolling returns
  for (i in 1:(nrow(etf) - rolling_period_days)) {
    if (!is.na(etf$investment_date[i])) {
      start_date <- etf$investment_date[i]
      end_date <- etf$date[i + rolling_period_days - 1]
      
      # Tính số lượng chứng chỉ quỹ mua được
      total_units <- sum(etf$units_purchased[i:(i + rolling_period_days - 1)])
      
      # Lấy giá cuối cùng
      final_price <- etf$price[i + rolling_period_days - 1]
      
      # Tính giá trị cuối cùng của khoản đầu tư
      final_value <- total_units * final_price
      
      # Tính tổng số tiền đã dùng để đầu tư
      total_investment <- investment_per_month * sum(!is.na(etf$investment_date[i:(i + rolling_period_days - 1)]))
      
      # Annualized return
      # 250 trading days in a year
      annualized_return <- (final_value / total_investment)^(250/rolling_period_days) - 1  
      
      # Nhập dữ liệu vào rolling returns tibble
      rolling_returns <- add_row(rolling_returns, StartDate = start_date, EndDate = end_date, AnnualizedReturn = annualized_return)
    }
  }
  
  return(rolling_returns)
}

# Xác định số ngày đầu tư
# Ví dụ: ngày giao dịch thứ nhất, thứ hai, thứ ba... của tháng.
trading_days_of_month <- c(1, 15)  

# Tạo list để lưu kết quả
rolling_returns_1y_list <- list()
rolling_returns_2y_list <- list()

# Loop
#' @param trading_day tương tự như i trong for (i in ...)

# Ví dụ lựa chọn ngày 1 và ngày 15 để đầu tư. Thì tính từ ngày giao dịch đầu tiên và thứ 15 trong tháng.
#' @param trading_day ở đây là ngày 1 hoặc 15. 
#' Thay thế trading_day_of_month của function calculate_rolling_returns

for (trading_day in trading_days_of_month) {
  
  # Calculate 1-year (250 trading days) rolling returns
  rolling_returns_1y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 1000000, trading_day, 250)
  
  # Calculate 2-years (500 trading days) rolling returns
  rolling_returns_2y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 1000000, trading_day, 500)
}

rolling_returns_1y_list$day1
rolling_returns_1y_list$day15


##################################################################

# DAILY DATA

# ROLLING RETURNS SIMPLE VERSION
etf <- get_data_vnstock("043_E1VFVN30_dai_han/01_data/raw/E1VFVN30.csv") %>% 
  filter(date >= "2015-01-01" & date <= "2021-12-31")


# cash flow
investment_per_month <- 1000000

# Tìm ngày giao dịch đầu tiên của tháng
etf <- etf %>%
  mutate(month = month(date),
         year = year(date),
         first_trading_day = !duplicated(paste(year, month)))


# Tính số lượng chứng chỉ quỹ
etf <- etf %>%
  mutate(units_purchased = if_else(first_trading_day, investment_per_month / price, 0))

# Tổng số lượng chứng chỉ quỹ và tổng tiền đầu tư
etf <- etf %>%
  mutate(total_units = cumsum(units_purchased),
         total_investment = cumsum(if_else(first_trading_day, investment_per_month, 0)))

# Rolling returns
etf <- etf %>%
  mutate(rolling_return_1y = ifelse(row_number() > 250, (lag(total_units, 250) * price / lag(total_investment, 250)) - 1, NA),
         rolling_return_2y = ifelse(row_number() > 500, (lag(total_units, 500) * price / lag(total_investment, 500)) - 1, NA))

