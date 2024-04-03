# 1. Packages #######
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

# 2. Import #######

vn30_raw <- get_data_vnstock("049_Le_Anh_Tuan_DC_wisdom/01_data/01_raw/E1VFVN30.csv")

midcap_raw <- get_data_tradingview("049_Le_Anh_Tuan_DC_wisdom/01_data/01_raw/VNMIDCAPTRI.csv")

xau_raw <- get_data_tradingview("049_Le_Anh_Tuan_DC_wisdom/01_data/01_raw/GOLD_USDVND_1.20565.csv")

dcds_raw <- read_csv("049_Le_Anh_Tuan_DC_wisdom/01_data/01_raw/mf_DCDS.csv")



# 3. XAU vs DCDS
xau_dcds_tbl <- xau_raw %>% 
  filter(date >= "2014-04-01") %>% 
  full_join(dcds_raw, by = "date", suffix = c("_xau", "_dcds")) %>% 
  arrange(date) %>% 
  na.locf() %>% 
  select(date, xau = price_xau, dcds = price_dcds) %>% 
  pivot_longer(names_to = "portfolio", !date) %>% 
  mutate(portfolio = as_factor(portfolio))


# 3.1 Tính tăng trưởng
xau_dcds_ret <- xau_dcds_tbl %>% 
  group_by(portfolio) %>% 
  tq_transmute(select     = value,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1) %>% 
  mutate(label_txt = if_else(date == max(date),
                             paste(toupper(portfolio), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>%
  ungroup()


# 3.1.1 Annualized Returns
xau_dcds_ret %>%
  group_by(portfolio) %>%
  tq_performance(Ra = returns, performance_fun = Return.annualized)


# 3.2 Plot Tăng trưởng
xau_dcds_ret %>% 
  ggplot(aes(x = date , y = growth, color = portfolio)) +
  geom_line() + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 700,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 5)) +
  
  scale_color_manual(values = c("#f94144", "#018574")) + 
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của DCDS và vàng từ năm 2018",
       caption = "Nguồn dữ liệu DCDS: Tổng hợp từ website của DC và Fmarket.vn
       Nguồn dữ liệu XAU: TradingView")


# 3.3 Rolling
xau_dcds_roll <- xau_dcds_ret %>%
  group_by(portfolio) %>%
  tk_augment_slidify(.value  = returns,
                     .period = 1250,
                     .f      = Return.annualized,
                     scale   = 250,
                     .names  = "roll_ret",
                     .align  = "right") %>% 
  ungroup()


# 3.3.1 Plot Rolling
xau_dcds_roll %>% 
  pivot_wider(names_from = "portfolio", values_from = "roll_ret", id_cols = date) %>%
  mutate(diff = dcds - xau) %>% 
  mutate(diff_pos = case_when(diff > 0 ~ diff,
                              TRUE ~ NA_real_)) %>% 
  
  mutate(diff_neg = case_when(diff <= 0 ~diff, 
                              TRUE ~ NA_real_)) %>%

  ggplot(aes(x = date)) +
  geom_point(aes(y = diff_pos), color = "#018574", size = 0.8) +
  geom_point(aes(y = diff_neg), color = "#f94144", size = 0.8) + theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 8)) + 
  
  labs(x = "",
       y = "Chênh lệch",
       title = "Chênh lệch TSSL hằng năm giữa DCDS và vàng",
       subtitle = "Chu kỳ đầu tư 5 năm",
       
       caption = "Phương pháp tính: Tính kết quả đạt được sau 5 năm đầu tư vào DCDS và vàng tại từng thời điểm khác nhau,
       Sau đó tính tỷ suất sinh lợi trung bình mỗi năm,
       Tính chênh lệch và biểu diễn trên biểu đồ.
       
       Màu xanh: DCDS hoạt động hiệu quả hơn vàng.
       Màu đỏ: DCDS không hoạt động hiệu quả bằng vàng.
       
       Nguồn dữ liệu DCDS: Tổng hợp từ website của DC và Fmarket.vn
       Nguồn dữ liệu XAU: TradingView")



# 4. ETF VN30 #################################################################

xau_vn30_tbl <- xau_raw %>% 
  filter(date >= "2014-12-31") %>% 
  full_join(vn30_raw, by = "date", suffix = c("_xau", "_vn30")) %>% 
  arrange(date) %>% 
  na.locf() %>% 
  filter(date >= "2015-01-01") %>% 
  select(date, xau = price_xau, vn30 = price_vn30) %>% 
  pivot_longer(names_to = "portfolio", !date) %>% 
  mutate(portfolio = as_factor(portfolio))


# 4.1 Tính tăng trưởng
xau_vn30_ret <- xau_vn30_tbl %>% 
  # filter(date >= "2018-01-01") %>% 
  group_by(portfolio) %>% 
  tq_transmute(select = value,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1) %>% 
  mutate(label_txt = if_else(date == max(date),
                             paste(toupper(portfolio), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>%
  ungroup()




# 4.2 Plot
xau_vn30_ret %>% 
  ggplot(aes(x = date , y = growth, color = portfolio)) +
  geom_line() + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 500,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_color_manual(values = c("#f94144", "#018574")) + 
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của ETF E1VFVN30 và vàng từ năm 2018",
       caption = "Nguồn dữ liệu ETF: Tổng hợp bằng vnstock
       Nguồn dữ liệu XAU: TradingView")


# 5.3 Rolling

xau_vn30_roll <- xau_vn30_ret %>%
  group_by(portfolio) %>%
  tk_augment_slidify(.value  = returns,
                     .period = 1250,
                     .f      = Return.annualized,
                     scale   = 250,
                     .names  = "roll_ret",
                     .align  = "right") %>% 
  ungroup()


# 3.3.1 Plot Rolling
xau_vn30_roll %>% 
  pivot_wider(names_from = "portfolio", values_from = "roll_ret", id_cols = date) %>%
  mutate(diff = vn30 - xau) %>% 
  mutate(diff_pos = case_when(diff > 0 ~ diff,
                              TRUE ~ NA_real_)) %>% 
  
  mutate(diff_neg = case_when(diff <= 0 ~diff, 
                              TRUE ~ NA_real_)) %>%
  
  ggplot(aes(x = date)) +
  geom_point(aes(y = diff_pos), color = "#018574", size = 0.8) +
  geom_point(aes(y = diff_neg), color = "#f94144", size = 0.8) + theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 8)) + 
  
  labs(x = "",
       y = "Chênh lệch",
       title = "Chênh lệch TSSL hằng năm giữa ETF VN30 và vàng",
       subtitle = "Chu kỳ đầu tư 5 năm",
       
       caption = "Phương pháp tính: Tính kết quả đạt được sau 5 năm đầu tư vào ETF VN30 và vàng tại từng thời điểm khác nhau,
       Sau đó tính tỷ suất sinh lợi trung bình mỗi năm,
       Tính chênh lệch và biểu diễn trên biểu đồ.
       
       Màu xanh: ETF VN30 hoạt động hiệu quả hơn vàng.
       Màu đỏ: ETF VN30 không hoạt động hiệu quả bằng vàng.
       
       ETF VN30 là ETF E1VFVN30.
       Nguồn dữ liệu ETF: Tổng hợp bằng vnstock
       Nguồn dữ liệu XAU: TradingView")



#####################################

# 5. ETF MIDCAP


xau_midcap_tbl <- xau_raw %>% 
  filter(date >= "2016-04-06") %>% 
  full_join(midcap_raw, by = "date", suffix = c("_xau", "_midcap")) %>% 
  arrange(date) %>% 
  na.locf() %>% 
  select(date, xau = price_xau, midcap = price_midcap) %>% 
  pivot_longer(names_to = "portfolio", !date) %>% 
  mutate(portfolio = as_factor(portfolio))


# 4.1 Tính tăng trưởng
xau_midcap_ret <- xau_midcap_tbl %>% 
  # filter(date >= "2018-01-01") %>% 
  group_by(portfolio) %>% 
  tq_transmute(select = value,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1) %>% 
  mutate(label_txt = if_else(date == max(date),
                             paste(toupper(portfolio), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>%
  ungroup()



# 4.2 Plot
xau_midcap_ret %>% 
  ggplot(aes(x = date , y = growth, color = portfolio)) +
  geom_line() + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 700,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_color_manual(values = c("#f94144", "#018574")) + 
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của ETF MIDCAP và vàng từ năm 2016",
       caption = "ETF MIDCAP trong bài là chỉ số VNMIDCAP-TRI
       Nguồn dữ liệu ETF và XAU: TradingView")


# 4.3 So sánh với DCDS

midcap_dcds_tbl <- midcap_raw %>% 
  full_join(dcds, by = "date", suffix = c("_midcap", "_dcds")) %>% 
  arrange(date) %>% 
  na.locf() %>% 
  select(date, midcap = price_midcap, dcds = price_dcds) %>% 
  pivot_longer(names_to = "portfolio", !date) %>% 
  mutate(portfolio = as_factor(portfolio))


midcap_dcds_ret <- midcap_dcds_tbl %>% 
  group_by(portfolio) %>% 
  tq_transmute(select = value,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1) %>% 
  mutate(label_txt = if_else(date == max(date),
                             paste(toupper(portfolio), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>%
  ungroup()


midcap_dcds_ret %>% 
  ggplot(aes(x = date , y = growth, color = portfolio)) +
  geom_line() + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 750,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_color_manual(values = c("#f94144", "#018574")) + 
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của ETF MIDCAP và DCDS từ năm 2016",
       caption = "ETF MIDCAP trong bài là chỉ số VNMIDCAP-TRI
       Nguồn dữ liệu ETF: TradingView
       Nguồn dữ liệu DCDS: Tổng hợp từ website của DC và Fmarket.vn")

# 4.4 Rolling

xau_midcap_roll <- xau_midcap_ret %>%
  group_by(portfolio) %>%
  tk_augment_slidify(.value  = returns,
                     .period = 1250,
                     .f      = Return.annualized,
                     scale   = 250,
                     .names  = "roll_ret",
                     .align  = "right") %>% 
  ungroup()


# 3.3.1 Plot Rolling
xau_midcap_roll %>% 
  pivot_wider(names_from = "portfolio", values_from = "roll_ret", id_cols = date) %>%
  mutate(diff = midcap - xau) %>% 
  mutate(diff_pos = case_when(diff > 0 ~ diff,
                              TRUE ~ NA_real_)) %>% 
  
  mutate(diff_neg = case_when(diff <= 0 ~diff, 
                              TRUE ~ NA_real_)) %>%
  
  ggplot(aes(x = date)) +
  geom_point(aes(y = diff_pos), color = "#018574", size = 0.8) +
  geom_point(aes(y = diff_neg), color = "#f94144", size = 0.8) + theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 8)) + 
  
  labs(x = "",
       y = "Chênh lệch",
       title = "Chênh lệch TSSL hằng năm giữa ETF MIDCAP và vàng",
       subtitle = "Chu kỳ đầu tư 5 năm",
       
       caption = "Phương pháp tính: Tính kết quả đạt được sau 5 năm đầu tư vào ETF MIDCAP và vàng tại từng thời điểm khác nhau,
       Sau đó tính tỷ suất sinh lợi trung bình mỗi năm,
       Tính chênh lệch và biểu diễn trên biểu đồ.
       
       Màu xanh: ETF MIDCAP hoạt động hiệu quả hơn vàng.
       Màu đỏ: ETF MIDCAP không hoạt động hiệu quả bằng vàng.
       
       ETF MIDCAP là chỉ số VNMIDCAP-TRI
       Nguồn dữ liệu: TradingView")
