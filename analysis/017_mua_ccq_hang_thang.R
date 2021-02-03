# 1.  Libraries: ----

library(tidyverse)
library(lubridate) # Date, time
library(tidyquant)
library(plotly)


# Load header:
source("header.R")

# Import ETF 
etf_tbl <- get_data_co_phieu() %>% 
  filter(symbol == "E1VFVN30")


vfmvfb_tbl <- read_rds("01_data/VFMVFB_06-2013_01-2021.rds")


# 2. BUY MONTHLY ETF: ----

# Data:
etf_buy_monthly <- etf_tbl %>% 
  filter(date >= "2015-01-01") %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>% 
  filter(date == first(date)) %>% 
  ungroup()


last_day_etf_price <- last(etf_tbl) %>% 
  select(-symbol) %>% 
  mutate(price = 19390)


# Calculate asset:
result_buy_monthly_2015 <- etf_buy_monthly %>% 
  select(-symbol) %>% 
  mutate(price = price * 1000) %>% 
  
  mutate(free_money = 1500000) %>% 
  mutate(buy_ccq = free_money / price) %>% 
  mutate(ccq = cumsum(buy_ccq)) %>% 
  mutate(asset = price * ccq) 
  

result_buy_monthly_2015_total <- result_buy_monthly_2015 %>%
  
  add_row(date = last_day_etf_price$date,
        price = last_day_etf_price$price,
        year = year(date),
        month = month(date),
        free_money = 0,
        buy_ccq = 0,
        ccq = last(result_buy_monthly_2015$ccq),
        asset = ccq * price)


# PLOT:

p <- result_buy_monthly_2015_total %>% 
  
  mutate(asset_label = scales::number(asset, suffix = " VND", big.mark = "\\."),
         ccq_label = scales::number(ccq, big.mark = "\\."),
         
         sum_money = cumsum(free_money),
         sum_money_label = scales::number(sum_money, suffix = " VND", big.mark = "\\.")) %>%
  
  mutate(label_text = str_glue("Thoi gian: {date}
                               Tong tai san: {asset_label}
                               Tien dau tu: {sum_money_label} 
                               So luong ccq: {ccq_label}")) %>% 
  
  ggplot(aes(x = date,
             y = asset,
             text = label_text)) +
  
  geom_line(aes(group = 1), color = "firebrick4") + 
  
  geom_col(aes(y = sum_money)) +
  
  scale_y_continuous(label = scales::number_format(suffix = " VND", big.mark = "\\."),
                     breaks = scales::pretty_breaks(n = 7)) +
  
  scale_x_date(breaks = scales::pretty_breaks(6)) + theme2 +
  
  theme(legend.position = "bottom") +
  
  labs(x = "",
       y = "",
       title = "Tổng tài sản \u0111ầu t\u01B0 từ n\u0103m 2015") 

p %>% ggplotly(tooltip = "text")


####

# Function:

ve_bieu_do <- function(thoigian = "2005-01-01"){
  
  etf_buy_monthly <- etf_tbl %>% 
    filter(date >= thoigian) %>% 
    mutate(year = year(date),
           month = month(date)) %>% 
    group_by(year, month) %>% 
    filter(date == first(date)) %>% 
    ungroup()
  
  result_buy_monthly <- etf_buy_monthly %>% 
    select(-symbol) %>% 
    mutate(price = price * 1000) %>% 
    
    mutate(free_money = 1500000) %>% 
    mutate(buy_ccq = free_money / price) %>% 
    mutate(ccq = cumsum(buy_ccq)) %>% 
    mutate(asset = price * ccq) 
  
  result_buy_monthly_total <- result_buy_monthly %>%
      add_row(date = last_day_etf_price$date,
            price = last_day_etf_price$price,
            year = year(date),
            month = month(date),
            free_money = 0,
            buy_ccq = 0,
            ccq = last(result_buy_monthly$ccq),
            asset = ccq * price)
  
  
  plot <- result_buy_monthly_total %>% 
    mutate(asset_label = scales::number(asset, suffix = " VND", big.mark = "\\."),
           ccq_label = scales::number(ccq, big.mark = "\\."),
           
           sum_money = cumsum(free_money),
           sum_money_label = scales::number(sum_money, suffix = " VND", big.mark = "\\.")) %>%
    
    mutate(label_text = str_glue("Thoi gian: {date}
                               Tong tai san: {asset_label}
                               Tien dau tu: {sum_money_label} 
                               So luong ccq: {ccq_label}")) %>% 
    
    ggplot(aes(x = date,
               y = asset,
               text = label_text)) +
    
    geom_line(aes(group = 1), color = "firebrick4") + 
    
    geom_col(aes(y = sum_money)) +
    
    scale_y_continuous(label = scales::number_format(suffix = " VND", big.mark = "\\."),
                       breaks = scales::pretty_breaks(n = 7)) +
    
    scale_x_date(breaks = scales::pretty_breaks(6)) + theme2 +
    
    theme(legend.position = "bottom") +
    
    labs(x = "",
         y = "") +
    
    ggtitle(str_glue("Tổng tài sản \u0111ầu t\u01B0 từ {thoigian}"))
  
  full_plot <- plot %>% ggplotly(tooltip = "text")
  
  
  # Save file html:
  # htmltools::save_html(full_plot, str_glue("plot_{thoigian}.html"))
  
  return(full_plot)
}


## Ve bieu do ngay cu the:

ve_bieu_do("2020-01-01")


########################################
# 3. Buy monthly VFMVFB: ----
########################################

vfm_buy_monthly <- vfmvfb_tbl %>%
  filter(date >= "2015-01-01") %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>% 
  filter(date == first(date)) %>% 
  ungroup()

result_vfm_buy_monthly <- vfm_buy_monthly %>% 
  mutate(free_money = 1500000) %>% 
  mutate(buy_ccq = free_money / price) %>% 
  mutate(ccq = cumsum(buy_ccq)) %>% 
  mutate(asset = price * ccq) 


result_vfm_buy_monthly_total <- result_vfm_buy_monthly %>% 
  add_row(date = last(vfmvfb_tbl)$date,
          price = last(vfmvfb_tbl)$price,
          year = year(date),
          month = month(date),
          free_money = 0,
          buy_ccq = 0,
          ccq = last(result_vfm_buy_monthly$ccq),
          asset = ccq * price)

# PLOT:

plot_vfm <- result_vfm_buy_monthly_total %>% 
  mutate(asset_label = scales::number(asset, suffix = " VND", big.mark = "\\."),
         ccq_label = scales::number(ccq, big.mark = "\\."),
         
         sum_money = cumsum(free_money),
         sum_money_label = scales::number(sum_money, suffix = " VND", big.mark = "\\.")) %>% 
  
  mutate(label_text = str_glue("Thoi gian: {date}
                               Tong tai san: {asset_label}
                               Tien dau tu: {sum_money_label} 
                               So luong ccq: {ccq_label}")) %>% 
  
  ggplot(aes(x = date,
             y = asset,
             text = label_text)) +
  
  geom_line(aes(group = 1), color = "firebrick4") + 
  
  geom_col(aes(y = sum_money), width = 3) +
  
  scale_y_continuous(label = scales::number_format(suffix = " VND", big.mark = "\\."),
                     breaks = scales::pretty_breaks(n = 6)) +
  
  scale_x_date(breaks = scales::pretty_breaks(6)) + theme2 +
  
  theme(legend.position = "bottom") +
  
  labs(x = "",
       y = "") +
  
  ggtitle(str_glue("Tổng tài sản \u0111ầu t\u01B0 từ 2015"))
  

full_plot_vfm <- plot_vfm %>% ggplotly(tooltip = "text")

htmltools::save_html(full_plot_vfm, "full_plot_vfm.html")


########################################
# 4. Buy monthly VNINDEX: ----
########################################

vnindex_tbl <- get_data_co_phieu() %>% 
  filter(symbol == "^VNINDEX")


vnindex_buy_monthly <- vnindex_tbl %>% 
  filter(date >= "2007-03-01") %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(year, month) %>% 
  filter(date == first(date)) %>% 
  ungroup()

result_vnindex_buy_monthly <- vnindex_buy_monthly %>% 
  mutate(free_money = 10000000) %>% 
  mutate(buy_ccq = free_money / price) %>% 
  mutate(ccq = cumsum(buy_ccq)) %>% 
  mutate(asset = price * ccq) 

result_vnindex_buy_monthly_total <- result_vnindex_buy_monthly %>% 
  add_row(date = last(vnindex_tbl)$date,
          price = last(vnindex_tbl)$price,
          year = year(date),
          month = month(date),
          free_money = 0,
          buy_ccq = 0,
          ccq = last(result_vnindex_buy_monthly$ccq),
          asset = ccq * price)

# PLOT:
plot_vnindex <- result_vnindex_buy_monthly_total %>% 
  mutate(asset_label = scales::number(asset, suffix = " VND", big.mark = "\\."),
         ccq_label = scales::number(ccq, big.mark = "\\."),
         
         sum_money = cumsum(free_money),
         sum_money_label = scales::number(sum_money, suffix = " VND", big.mark = "\\.")) %>% 
  
  mutate(label_text = str_glue("Thoi gian: {date}
                               Tong tai san: {asset_label}
                               Tien dau tu: {sum_money_label} 
                               So luong ccq: {ccq_label}")) %>% 
  
  ggplot(aes(x = date,
             y = asset,
             text = label_text)) +
  
  geom_line(aes(group = 1), color = "firebrick4") + 
  
  geom_col(aes(y = sum_money)) +
  
  scale_y_continuous(label = scales::number_format(suffix = " VND", big.mark = "\\."),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(12)) + theme_tq() + theme2 +
  
  labs(x = "",
       y = "") +
  
  ggtitle(str_glue("Tổng tài sản \u0111ầu t\u01B0 vào VNINDEX từ 03/2007"))


# PLOTLY:

full_plot_vnindex <- plot_vnindex %>% 
  ggplotly(tooltip = "text")

htmltools::save_html(full_plot_vnindex, "full_plot_vnindex.html")



# #### TODO ####
# 
# 
# # 3. Buy RSI
# 
# # XTS: Stoch RSI XTS:
# etf_xts <- etf_tbl %>% 
#   pivot_wider(names_from = "symbol", values_from = "price") %>% 
#   rename(close = E1VFVN30) %>% 
#   timetk::tk_xts(date_var = date)
# 
# etf_srsi_xts <- round(stochRSI(etf_xts),3)
# 
# # Convert xts -> tibble
# etf_stoch_rsi_tbl <- etf_srsi_xts %>% 
#   timetk::tk_tbl(preserve_index = TRUE, rename_index = "date")
# 
# # Tim ngay co RSI = 0
# etf_buy_date <- etf_stoch_rsi_tbl %>% 
#   mutate(year = year(date),
#          month = month(date),
#          day = day(date)) %>%
#   group_by(year, month) %>% 
#   filter(stochRSI == 0) %>% 
#   ungroup() %>% 
#   group_by(year, month) %>% 
#   filter(day == min(day)) %>% 
#   ungroup() %>% 
#   mutate(days = as.numeric(date - lag(date))) %>% 
#   replace(is.na(.), 100) # Replace NA = 100
# 
# 
# # Filter ngay duoi 30
# etf_buy_srsi_date <- etf_buy_date %>% 
#   
#   # Filter cac thang cach nhau it nhat 30 ngay
#   filter(days > 30) %>% 
#   select(date) %>% 
#   mutate(is_buy = 1)
# 
# 
# # Function:
# 
# cumsum_stop_at_0 <- function(x)
# {
#   cs = cumsum(x)
#   cs - cummax((x == 0) * cs)
# }
# 
# 
# result_buy_rsi_2015 <- etf_tbl %>% 
#   select(-symbol) %>% 
#   left_join(etf_buy_srsi_date, by = "date") %>% 
#   replace(is.na(.), 0) %>% 
#   
#   mutate(price = price * 1000) %>% 
#   mutate(year = year(date),
#          month = month(date)) %>% 
#   group_by(year, month) %>% 
#   filter(date == first(date) | is_buy == TRUE) %>% 
#   ungroup() %>% 
#   
#   mutate(money = case_when(is_buy == 0 ~ 1500000, TRUE ~ 0)) %>% 
#   
#   mutate(money_2 = money %>% cumsum_stop_at_0()) %>% 
#   
#   mutate(free_money = case_when(money_2 == 0 ~ lag(money_2), TRUE ~ 0)) %>% 
#   
#   mutate(free_money = ifelse(date == "2020-02-03" & free_money == 3000000, 4500000, free_money)) %>% 
#   
#   mutate(buy_ccq = free_money / price) %>% 
#   mutate(ccq = cumsum(buy_ccq)) %>% 
#   mutate(asset = price * ccq)
# 
# 
# summarise_buy_rsi_2015 <- result_buy_rsi_2015 %>%  
#   summarise(total_money_spent = sum(free_money),
#             total_ccq = last(ccq),
#             avg_price = total_money_spent / total_ccq,
#             total_asset = last(asset),
#             return = total_asset / total_money_spent - 1)