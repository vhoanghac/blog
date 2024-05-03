# 1. Packages 
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

###########################################
# 2. Import data
dcds <- read_csv("051_hieu_ve_drawdown/01_data/01_raw/mf_DCDS.csv") %>% 
  filter(date >= "2013-10-08")

etf <- get_data_vnstock("051_hieu_ve_drawdown/01_data/01_raw/E1VFVN30.csv")
  
###########################################
# 3. Function

# 3.1 Calculate returns
calculate_returns <- function(data){
  
  data %>% 
    tq_mutate(select     = price,
              mutate_fun = periodReturn,
              period     = "daily",
              col_rename = "returns")
}


# 3.2 Calculate growth (lump sum)

calculate_lump_sum <- function(data, lumpsum) {
  data %>%
    mutate(growth        = cumprod(1 + returns) * lumpsum,
           max_growth    = cummax(growth),
           under_ath     = if_else(growth < max_growth, TRUE, FALSE),
           start_new_ath = if_else(growth == max_growth & stats::lag(under_ath, default = TRUE), TRUE, FALSE)) %>%
    
    fill(start_new_ath, .direction = "down") %>%
    
    mutate(fill_area = if_else(under_ath & !is.na(start_new_ath) & !start_new_ath, max_growth, NA_real_))
}


# 3.3 Calculate growth (DCA method)
calculate_dca <- function(data, cash){
  
  data %>% 
    mutate(month             = month(date),
           year              = year(date),
           first_trading_day = !duplicated(paste(year, month))) %>% 
    
    # Tính số chứng chỉ quỹ mua được
    mutate(units_purchased = if_else(first_trading_day, cash / price, 0)) %>% 
    
    # Tính tổng số lượng chứng chỉ quỹ và tổng tiền đầu tư
    mutate(total_units = cumsum(units_purchased),
           total_cash  = cumsum(if_else(first_trading_day, cash, 0))) %>% 
    
    mutate(total_value = total_units * price,
           profit      = total_value - total_cash,
           profit_pct  = total_value / total_cash - 1) %>% 
    
    select(-month, -year)
  
}

# 3.4 Analyze DCA method
analyze_dca <- function(data, cash) {
  
  data %>%
    
    calculate_dca(cash) %>%
    
    select(date, price, profit, profit_pct) %>%
    
    mutate(max_profit    = cummax(profit),
           under_ath     = if_else(profit < max_profit, TRUE, FALSE),
           start_new_ath = if_else(profit == max_profit & stats::lag(under_ath, default = TRUE), TRUE, FALSE)) %>% 
    
    fill(start_new_ath, .direction = "down") %>% 
    
    mutate(fill_area = if_else(under_ath & !is.na(start_new_ath) & !start_new_ath, max_profit, NA_real_))
}



###########################################
# 4. Phân tích DCDS mua một lần

# 4.1 Calculate returns
dcds_ret <- dcds %>% 
  calculate_returns()

# 4.2 Lumpsum plan
dcds_lumpsum <- dcds_ret %>% 
  calculate_lump_sum(100) 


# 4.3 Plot
# Đếm số ngày dưới đỉnh.
# Có 90% số ngày danh mục hoạt động ở dưới đỉnh
dcds_lumpsum %>% 
  summary()


dcds_lumpsum %>% 
  ggplot(aes(x = date)) +  
  geom_line(aes(y = growth)) +
  
  geom_ribbon(aes(ymin = growth, 
                  ymax = fill_area), fill = "red", alpha = 0.2) +
  
  geom_line(aes(y = max_growth), linetype = "dashed") + theme_tq() + theme +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x = "",
       y = "Giá trị tài sản (triệu đồng)",
       title = "Giá trị tài sản và các đợt sụt giảm của DCDS (Lumpsum)",
       subtitle = "Hình thức: Đầu tư một lần vào ngày đầu tiên.",
       caption = "Quãng thời gian: 08/10/2023 đến 19/04/2024
       Nguồn dữ liệu: tổng hợp từ website DragonCapital và Fmarket")


# 4.4 Plot (growth) BACKUP

# 4.5 Plot

# dcds_lumpsum %>% 
#   ggplot(aes(x = date)) +  
#   geom_line(aes(y = growth - 1)) +
#   
#   geom_ribbon(aes(ymin = growth - 1, ymax = fill_area - 1), fill = "red", alpha = 0.2) +
#   
#   geom_line(aes(y = max_growth - 1), linetype = "dashed") + theme_tq() + theme +
#   
#   # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#   #                   breaks = scales::pretty_breaks(n = 10)) +
#   
#   scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
#   
#   labs(x = "",
#        y = "Tăng trưởng",
#        title = "Tăng trưởng và các đợt sụt giảm của DCDS (Lumpsum)",
#        subtitle = "Hình thức: Đầu tư một lần vào ngày đầu tiên.",
#        caption = "Quãng thời gian: 08/10/2023 đến 19/04/2024
#        Nguồn dữ liệu: tổng hợp từ website DragonCapital và Fmarket")


###########################################
# 5.Phân tích DCDS theo phương pháp DCA

# Tính lợi nhuận danh mục
dcds_dca <- dcds_ret %>% 
  analyze_dca(3000000)


# Đếm số ngày dưới đỉnh.
# Có 90% số ngày danh mục hoạt động ở dưới đỉnh
dcds_dca %>% 
  summary()

# Plot lợi nhuận
dcds_dca %>% 
  ggplot(aes(x = date)) +
  
  geom_line(aes(y = profit)) +
  
  geom_ribbon(aes(ymin = profit, 
                  ymax = fill_area), fill = "red", alpha = 0.2) +
  
  geom_line(aes(y = max_profit), linetype = "dashed") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(scale = 0.000001),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1),
  #                    breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x        = "",
       y        = "Lợi nhuận (triệu đồng)",
       title    = "Lợi nhuận khi đầu tư DCDS (DCA)",
       subtitle = "Hình thức: DCA 3.000.000 liên tục vào đầu mỗi tháng",
       caption  = "Quãng thời gian: 08/10/2023 đến 19/04/2024
       Nguồn dữ liệu: tổng hợp từ website DragonCapital và Fmarket")


# # Plot % tăng trưởng
# dcds_dca <- dcds_ret %>% 
#   calculate_dca() %>% 
#   select(date, price, profit_pct) %>% 
#   
#   mutate(max_profit = cummax(profit_pct)) %>% 
#   
#   mutate(under_ath = if_else(profit_pct < max_profit, TRUE, FALSE)) %>% 
#   
#   mutate(start_new_ath = if_else(profit_pct == max_profit & stats::lag(under_ath, default = TRUE), TRUE, FALSE)) %>% 
#   
#   fill(start_new_ath, .direction = "down") %>% 
#   
#   mutate(fill_area = if_else(under_ath & !is.na(start_new_ath) & !start_new_ath, max_profit, NA_real_))
# 
# 
# dcds_dca %>% 
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = profit_pct)) +
#   
#   geom_ribbon(aes(ymin = profit_pct, ymax = fill_area), fill = "red", alpha = 0.2) +
#   
#   geom_line(aes(y = max_profit), linetype = "dashed") + theme_tq() + theme +
# 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1),
#                      breaks = scales::pretty_breaks(n = 10)) +
#   
#   scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
#   
#   labs(x = "",
#        y = "Tăng trưởng",
#        title = "Tăng trưởng và các đợt sụt giảm của DCDS (DCA)",
#        subtitle = "Hình thức: DCA 3.000.000 liên tục vào đầu mỗi tháng",
#        caption = "Quãng thời gian: 08/10/2023 đến 19/04/2024
#        Nguồn dữ liệu: tổng hợp từ website DragonCapital và Fmarket")



##########################################
# 6. Phân tích ETF mua một lần

# 6.1 ETF returns
etf_ret <- etf %>% 
  calculate_returns()

# 6.2 Lumpsum plan
etf_lumpsum <- etf_ret %>% 
  calculate_lump_sum(100)

# 6.3 Plot
# Đếm số ngày dưới đỉnh.
# Có 95% số ngày danh mục hoạt động ở dưới đỉnh
etf_lumpsum %>% 
  summary()

etf_lumpsum %>% 
  ggplot(aes(x = date)) +  
  
  geom_line(aes(y = growth)) +
  
  geom_ribbon(aes(ymin = growth, 
                  ymax = fill_area), fill = "red", alpha = 0.2) +
  
  geom_line(aes(y = max_growth), linetype = "dashed") + theme_tq() + theme +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x        = "",
       y        = "Giá trị tài sản",
       title    = "Giá trị tài sản và các đợt sụt giảm của ETF VN30 (Lumpsum)",
       subtitle = "Hình thức: Đầu tư một lần vào ngày đầu tiên.",
       caption  = "Quãng thời gian: 06/10/2014 đến 19/04/2024
       Nguồn dữ liệu: tổng hợp bằng vnstock")


##########################################
# 7. Phân tích ETF theo phương pháp DCA

# Tính lợi nhuận danh mục theo phương pháp DCA
etf_dca <- etf_ret %>% 
  analyze_dca(3000000)

# Đếm số ngày dưới đỉnh.
# Có 93% số ngày danh mục hoạt động ở dưới đỉnh
etf_dca %>% 
  summary()

# Plot số tiền
etf_dca %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = profit)) +
  
  geom_ribbon(aes(ymin = profit, ymax = fill_area), fill = "red", alpha = 0.2) +
  
  geom_line(aes(y = max_profit), linetype = "dashed") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(scale = 0.000001),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  labs(x = "",
       y = "Lợi nhuận (triệu đồng)",
       title = "Lợi nhuận khi đầu tư ETF VN30 (DCA)",
       subtitle = "Hình thức: DCA 3.000.000 liên tục vào đầu mỗi tháng",
       caption = "Quãng thời gian: 06/10/2014 đến 19/04/2024
       Nguồn dữ liệu: tổng hợp bằng vnstock")
