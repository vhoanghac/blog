# 1. Import packages ----
library(tidyverse)
library(lubridate) # Date, time
library(tidyquant)
library(plotly)

# Load header import gia co phieu
source("header.R")

# Import data co phieu:
raw_data_tbl <- get_data_co_phieu()

# Import data VFMVFB:
vfmvfb_tbl <- read_rds("01_data/VFMVFB_06-2013_01-2021.rds") %>% 
  mutate(symbol = "VFMVFB")

# 2. Function
plan_dau_tu <- function(data = raw_data_tbl,
                        stock = "E1VFVN30",
                        from = "2015-01-01",
                        money = 1500000,
                        interactive = FALSE){
  
  # Last day observation:
  last_day <- data %>% 
    filter(symbol == stock & date >= from) %>% 
    last() %>% 
    mutate(price = price * 1000)
  
  # Prepare data:
  data_tbl <- data %>% 
    filter(symbol == stock & date >= from) %>% 
    
    mutate(year = year(date),
           month = month(date),
           price = price * 1000) %>% 
    
    group_by(year, month) %>% 
    filter(date == first(date)) %>% 
    ungroup() %>% 
    
    # Calculate asset:
    mutate(free_money = money,
           buy_ccq = free_money / price,
           ccq = cumsum(buy_ccq),
           asset = price * ccq)
  
  # Add last day observatrion:
  full_data_tbl <- data_tbl %>% 
    add_row(date = last_day$date,
            price = last_day$price,
            year = year(date),
            month = month(date),
            free_money = 0,
            buy_ccq = 0,
            ccq = last(data_tbl$ccq),
            asset = ccq * price) %>% 
    
    mutate(asset_label = scales::number(asset, suffix = " VND", big.mark = "\\."),
           sum_money = cumsum(free_money),
           sum_money_label = scales::number(sum_money, suffix = " VND", big.mark = "\\.")) %>% 
    
    mutate(label_text = str_glue("Thoi gian: {date}
                                 Tong tai san: {asset_label}
                                 Tien dau tu: {sum_money_label}
                                 So luong ccq: {round(ccq)}"))
  
  # VISUALIZATION:
  plot <- full_data_tbl %>% 
    ggplot(aes(x = date,
               y = asset,
               text = label_text)) +
    
    geom_line(aes(group = 1), color = "firebrick4") +
    geom_col(aes(y = sum_money)) +
    
    scale_y_continuous(label = scales::number_format(suffix = " VND", big.mark = "\\."),
                       breaks = scales::pretty_breaks(n = 7)) +
    
    scale_x_date(breaks = scales::pretty_breaks(6)) + theme2 +
    
    labs(x = "",
         y = "Tài sản",
         title = str_glue("Tổng tài sản \u0111ầu t\u01B0 vào {symbol} từ {from}"))

  # INTERACTIVE:
  if (interactive){
    
    plot <- plot + 
      labs(x = "",
           y = "")
    
    return(ggplotly(plot, tooltip = "text"))
    
  } else {
    
   return(plot)
    
  }
  
  
}

# 3. ANALYSIS: ----

# E1VFVN30 :
plan_dau_tu(stock = "E1VFVN30",
            from = "2015-01-01",
            money = 1500000,
            interactive = TRUE)

# VNINDEX:
plan_dau_tu(stock = "^VNINDEX",
            from = "2007-03-01",
            money = 10000000,
            interactive = FALSE)

# VFMVFB:
plan_dau_tu(data = vfmvfb_tbl,
            stock = "VFMVFB",
            from = "2015-01-01",
            interactive = FALSE)