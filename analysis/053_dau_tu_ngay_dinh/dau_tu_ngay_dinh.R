# 1. Packages #### ####
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

# 2. Import data #### ####
etf_vn30 <- get_data_tradingview("053_dau_tu_ngay_dinh/01_data/01_raw/HOSE_DLY_E1VFVN30, 1D.csv")
etf_midcap <- get_data_tradingview("053_dau_tu_ngay_dinh/01_data/01_raw/HOSE_DLY_VNMIDCAPTRI, 1D.csv")
etf_vnd <- get_data_tradingview("053_dau_tu_ngay_dinh/01_data/01_raw/HOSE_DLY_FUEVFVND, 1D.csv")
mf_dcds <- read_csv("053_dau_tu_ngay_dinh/01_data/01_raw/mf_DCDS.csv")
mf_vesaf <- read_csv("053_dau_tu_ngay_dinh/01_data/01_raw/mf_VESAF.csv") %>% 
  distinct(date, .keep_all = TRUE)

# 3. Calculation Function #### ####
# 3.1 Calculate returns
calculate_returns <- function(data){
  
  data %>% 
    tq_mutate(select     = price,
              mutate_fun = periodReturn,
              period     = "daily",
              col_rename = "returns")
}


# 3.2 Calculate growth (DCA method)
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
    
    select(-month, -year) %>% 
    
    mutate(label_txt = if_else(date == max(date),
                               paste("Tăng trưởng: ", scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))
  
}

# 3.3 Analyze data
analyze_data <- function(data, start_date, cash) {
  data %>%
    filter(date >= start_date) %>%
    calculate_returns() %>%
    calculate_dca(cash)
    
}

# 4. Variables #### ####

data_list <- list(
  etf_vn30 = etf_vn30,
  etf_midcap = etf_midcap,
  etf_vnd = etf_vnd,
  mf_dcds = mf_dcds,
  mf_vesaf = mf_vesaf)


top1 <- "2018-01-01"
top2 <- "2022-01-01"

cash <- 3000000

year <- "2018"

# 4.1 Charts
common_layers <- list(
  geom_line(aes(y = total_value, color = "Giá trị danh mục")),
  geom_area(aes(y = total_cash, fill = "Tiền đầu tư"), alpha = 0.2),
  theme_tq(), 
  theme2,
  scale_y_continuous(labels = scales::number_format(scale = 0.000001),
                     breaks = scales::pretty_breaks(n = 10)), 
  scale_x_date(breaks = scales::pretty_breaks(n = 6)),
  scale_color_manual(values = "firebrick4"),
  scale_fill_manual(values = "gray2"))



# 5. Analyze đỉnh 2018
# top1 = 2018-01-01
# cash = 3.000.000

# Analyze
results <- lapply(data_list, FUN = analyze_data, start_date = top1, cash = cash)

# Đặt tên:
names(results) <- paste0(names(results), "_", "2018")

# Xuất kết quả
list2env(results, envir = .GlobalEnv)


# 5.1 Plot ETF VN30
etf_vn30_2018 %>% head.tail()

etf_vn30_2018 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2019-03-01"), 
           y      = 300000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 25.6%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2019-04-01"), 
           y      = 250000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 3.5%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư ETF VN30 bằng phương pháp DCA từ năm 2018",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2018 đến 14/08/2024.
       Nguồn: TradingView")

# 5.2 Plot ETF MIDCAP
etf_midcap_2018 %>% head.tail()

etf_midcap_2018 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2019-03-01"), 
           y      = 390000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 58.8%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2019-04-20"), 
           y      = 320000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 7.24%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư ETF MIDCAP bằng phương pháp DCA từ năm 2018",
       caption = "Sử dụng dữ liệu chỉ số VNMIDCAP-TRI để thay thế cho ETF FUEDCMID
       Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2018 đến 14/08/2024.
       Nguồn: TradingView")

# 5.3 Plot MF DCDS 2018  
mf_dcds_2018 %>% head.tail()

mf_dcds_2018 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2019-03-01"), 
           y      = 390000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 55.4%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2019-04-20"), 
           y      = 320000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 6.88%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư DCDS bằng phương pháp DCA từ năm 2018",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2018 đến 14/08/2024.
       Nguồn: Fmarket và website của quỹ")



# 5.4 Plot MF VESAF 2018  
mf_vesaf_2018 %>% head.tail()

mf_vesaf_2018 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2019-03-01"), 
           y      = 440000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 80.9%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2019-04-01"), 
           y      = 370000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 9.4%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư VESAF bằng phương pháp DCA từ năm 2018",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2018 đến 14/08/2024.
       Nguồn: Fmarket")




# 6. Analyze đỉnh 2022

# top2 = 2022-01-01
# cash = 3.000.000

# Analyze
results2 <- lapply(data_list, FUN = analyze_data, start_date = top2, cash = cash)

# Đặt tên:
names(results2) <- paste0(names(results2), "_", "2022")

# Xuất kết quả
list2env(results2, envir = .GlobalEnv)

# 6.1 Plot ETF VN30
etf_vn30_2022 %>% head.tail()

etf_vn30_2022 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2022-06-01"), 
           y      = 90000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 7.7%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2022-06-15"), 
           y      = 75000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 2.9%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư ETF VN30 bằng phương pháp DCA từ năm 2022",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2022 đến 14/08/2024.
       Nguồn: TradingView")


# 6.2 Plot ETF MIDCAP 2022
etf_midcap_2022 %>% head.tail()

etf_midcap_2022 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2022-06-01"), 
           y      = 110000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 13.8%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2022-06-20"), 
           y      = 95000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 5.09%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư ETF MIDCAP bằng phương pháp DCA từ năm 2022",
       caption = "Sử dụng dữ liệu chỉ số VNMIDCAP-TRI để thay thế cho ETF FUEDCMID
       Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2022 đến 14/08/2024.
       Nguồn: TradingView")


# 6.3 Plot ETF VND 2022
etf_vnd_2022 %>% head.tail()

etf_vnd_2022 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2022-06-01"), 
           y      = 110000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 21%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2022-06-20"), 
           y      = 95000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 7.6%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư ETF VN DIAMOND bằng phương pháp DCA từ năm 2022",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2022 đến 14/08/2024.
       Nguồn: TradingView")

# 5.3 Plot MF DCDS 2022
mf_dcds_2022 %>% head.tail()

mf_dcds_2022 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2022-06-01"), 
           y      = 110000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 22%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2022-06-15"), 
           y      = 95000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 7.9%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư DCDS bằng phương pháp DCA từ năm 2022",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2022 đến 14/08/2024.
       Nguồn: Fmarket và website của quỹ")

# 5.4 Plot MF VESAF 2022
mf_vesaf_2022 %>% head.tail()

mf_vesaf_2022 %>% 
  ggplot(aes(x = date)) +
  
  common_layers +
  
  annotate("text", 
           x      = as.Date("2022-06-01"), 
           y      = 90000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 21.6%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2022-06-15"), 
           y      = 70000000, 
           size   = 9,
           family = "stix",
           label  = "Tăng trưởng: 7.8%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư VESAF bằng phương pháp DCA từ năm 2022",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2022 đến 14/08/2024.
       Nguồn: TradingView")
