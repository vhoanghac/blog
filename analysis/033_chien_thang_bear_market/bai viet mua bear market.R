# 1. Packages #### ####
source("header.R")  #Tidyverse, data.table, lubridate, and ggtext
library(tidyquant)

options(scipen = 999)

# 2. Import data #### ####
data <- get_data_investing("033_scenarios/01_data/VN 30 Historical Data.csv", format = "%m/%d/%Y") %>% 
  filter(date >= "2014-01-01")


# 3. Yearly returns #### ####
data %>% 
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "returns")

# 4. Daily returns #### ####
# Tinh daily return roi tinh annualized return se chinh xac hon so voi viec tinh ann return dua tren yearly returns
# Vi dau tu theo phuong phap DCA cho nen sung dung return.annualized() se phu hop hon.

# 4.1 2014
data_2014_daily <- data %>% 
  filter(date < "2018-01-01") %>% 
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "returns")


# 4.1.1 Annualized from daily return: 15%
data_2014_daily %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized)


# 4.1.2 Max drawdown: 21.9%
data_2014_daily %>% 
  tq_performance(Ra              = returns,
                 Rb              = NULL,
                 performance_fun = maxDrawdown)

# 4.2 2018
data_2018_daily <- data %>% 
  filter(date >= "2018-01-01") %>% 
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "returns")

# 4.2.1 Annualized from daily return 11.6%
data_2018_daily %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized)


# 4.2.2 Max drawdown: 48.1%
data_2018_daily %>% 
  tq_performance(Ra              = returns,
                 Rb              = NULL,
                 performance_fun = maxDrawdown)


# 4. Function #### #### ####

dca <- function(data, 
                scenario = "a", 
                money = 5000000){
  
  # Filter data based on scenario:
  if(scenario == "a"){
    
    data_tbl <- data %>% 
      filter(date >= "2014-01-01", date < "2018-01-01")
    
    color = "#2196F3"
      
  } else {
    
    data_tbl <- data %>% 
      filter(date >= "2018-01-01")
    
    color = "#F44336"
    
  }
  
  # Calculate DCA values
  df <- data_tbl %>% 
    
    # Find the first day of each month
    group_by(month = month(date),
             year  = year(date)) %>%
    
    # Money to invest each month
    mutate(cash = if_else(date == min(date), money, 0)) %>% 
    ungroup() %>% 
    
    # Calculation
    mutate(shares              = cash/price, 
           total_shares        = cumsum(shares),
           total_cash_invested = cumsum(cash),
           total_value         = total_shares * price,
           profit_number       = total_value - total_cash_invested,
           profit_pct          = (total_value / total_cash_invested) - 1) %>% 
    
    # Labels for charts:
    mutate(across(c(total_cash_invested, total_value, profit_number), 
                  ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1),
                  .names = "{col}_txt"),
           
           profit_pct_txt = scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01))
  
  
  # Plot
  
  df %>% 
    ggplot(aes(x = date, 
               y = total_value)) +
    
    geom_line(color = color) +
    
    theme_tq() +  theme2 +
    
    scale_y_continuous(labels = scales::number) +
    
    labs(x = "",
         y = "Tổng giá trị tài sản",
         title = str_glue("<span style = 'color:{color};'>Thành quả đầu tư vào phương án {toupper(scenario)}</span>"),
         subtitle = str_glue("Đầu tư <span style = 'color:{color};'>{scales::number(money, big.mark = '.', decimal.mark = ',')}</span>mỗi tháng từ năm {first(year(df$date))} <br>
                             Sau 4 năm đã đầu tư tổng cộng <span style = 'color:{color};'>{last(df$total_cash_invested_txt)}</span>đồng <br>
                             Tổng giá trị tài sản tại thời điểm năm thứ 4 là <span style = 'color:{color};'>{last(df$total_value_txt)}</span>đồng<br>
                             Lợi nhuận là <span style = 'color:{color};'>{last(df$profit_number_txt)}</span>tương đương <span style = 'color:{color};'>{last(df$profit_pct_txt)}</span>"))
  
  
}


# 5. ACTION !!!

dca(data, "a", 5000000)

dca(data, "b", 5000000)