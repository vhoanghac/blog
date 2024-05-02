# 1. Packages 
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

########################

# 2. Import data & TIDY 

etf_raw <- get_data_vnstock("050_all_weather_portfolio_2023/01_data/01_raw/E1VFVN30.csv")

tcbf_raw <- read_csv("050_all_weather_portfolio_2023/01_data/01_raw/TCBF.csv")

midcap_raw <- get_data_tradingview("050_all_weather_portfolio_2023/01_data/01_raw/VNMIDCAPTRI.csv")

xau_raw <- get_data_tradingview("050_all_weather_portfolio_2023/01_data/01_raw/XAU.csv")

cpi_raw <- read_csv("050_all_weather_portfolio_2023/01_data/01_raw/cpi_mom.csv")

xau_raw %>% head.tail()

########################

# 3. TIDY 

cpi_tbl <- cpi_raw %>% 
  # Đổi format ngày từ Jan-2009 sang 2009-01-01
  mutate(date = parse_date(date, format = "%b-%Y"))


########################

# 4. Calculate Monthly returns

# Tạo data list
data_list <- list(etf_ret = etf_raw, tcbf_ret = tcbf_raw, midcap_ret = midcap_raw, xau_ret = xau_raw)

# Tính monthly returns từ list
ret_list <- lapply(data_list, function(df) {
  df %>% 
    tq_transmute(select = price,
                 mutate_fun = periodReturn,
                 period = "monthly",
                 col_rename = "returns") %>% 
    
    # Thay đổi giá trị ngày
    # Ví dụ 2014-10-31 sẽ đổi thành 2014-01-01, là ngày đầu tiên trong tháng để dễ dàng phân tích
    # Lý do là các tập dữ liệu có thể có nhiều ngày cuối tháng khác nhau
    # Điều này không làm ảnh hưởng đến kết quả
    mutate(date = floor_date(date, "month"))
})

# Xuất kết quả
list2env(ret_list, envir = .GlobalEnv)


########################

# Tập dữ liệu để phân tích.
etf_ret
tcbf_ret
midcap_ret
xau_ret
cpi_tbl

# Tỷ trọng các loại tài sản
# ETF là cổ phiếu trong trường hợp này
# 55% trái phiếu. 30% cổ phiếu. 15% vàng
# 60% trái phiếu. 40% cổ phiếu.
# 100% cổ phiếu 

weights <- c(0.55, 0.3, 0.15,
             0, 1, 0,
             0, 0, 1,
             0, 0.5, 0.5)

symbols <- c("TCBF", "ETF", "XAU")

weights_tbl <- tibble(symbols) %>% 
  tq_repeat_df(4) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio)


########################


# 4. Phân tích ETF E1VFVN30


# Dữ liệu CPI YOY.
cpivn30_tbl <- cpi_tbl %>% 
  filter(date >= "2015-09-01")

# Gộp dữ liệu của : ETF VN30, TCBF và XAU để tính toán
etfvn30_tbl <- tcbf_ret %>% 
  left_join(etf_ret, by = "date", suffix = c("_tcbf", "_etf")) %>% 
  left_join(xau_ret, by = "date") %>% 
  rename(TCBF = returns_tcbf,
         ETF = returns_etf,
         XAU = returns) %>% 
  
  # Tạo thêm 1 row trước đó với giá trị bằng 0. 
  # Mục đích là để biểu đồ cumulative returns bắt đầu tại cùng một điểm, là giá trị số 1 trên biểu đồ.
  add_row(date = as.Date("2015-08-01", format = "%Y-%m-%d"), TCBF = 0, ETF = 0, XAU = 0, .before = 1) %>%
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date)

# Tính toán danh mục đầu tư dựa theo tỷ trọng đã đề ra
etfvn30_ret <- etfvn30_tbl %>%
  tq_repeat_df(n = 4) %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_tbl,
               rebalance_on = "quarters",
               col_rename = "p_ret")


# Điều chỉnh lạm phát và tính Cumulative returns
etfvn30_inf_ret <- etfvn30_ret %>% 
  left_join(cpivn30_tbl, by = "date") %>%
  
  # Replace NA bằng 0
  # Do add thêm 1 row ở trước đó
  mutate(cpi = replace_na(cpi, 0),
         p_inf_ret = ((1 + p_ret)/(1 + cpi)) - 1,
         growth = cumprod(1 + p_inf_ret)) %>% 
  
  mutate(portfolio = as_factor(portfolio)) %>% 
  
  mutate(name = case_when(portfolio == 1 ~ "All Weather",
                          portfolio == 2 ~ "ETF VN30",
                          portfolio == 3 ~ "XAU",
                          TRUE ~ "ETF & XAU")) %>% 
  
  
  mutate(label_txt = if_else(date == max(date),
                             paste(name, ":", scales::percent(growth - 1, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% ungroup()

# Tính lợi nhuận bình quân năm
etfvn30_inf_ret %>% 
  group_by(portfolio) %>% 
  tq_performance(Ra = p_inf_ret,
                 performance_fun = Return.annualized,
                 scale = 12)


# 4.1 Plot cumulative returns
etfvn30_inf_ret %>% 
  ggplot(aes(x = date, y = growth - 1, color = portfolio)) + 
  
  geom_line(linewidth = 0.9) + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 800,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +

  scale_color_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                     labels = c("All Weather", "ETF VN30", "XAU", "ETF & XAU")) +
  labs(x = "",
       y = "Lợi nhuận tích lũy",
       title = "Thành quả đầu tư của các danh mục",
       subtitle = "Tỷ suất sinh lợi hằng năm (đã điều chỉnh lạm phát)<br>
       <span style = 'color:#f94144;'>All Weather:</span> 5,4%. <span style = 'color:#018574;'>ETF VN30:</span> 5,8%. <span style = 'color:orange;'>XAU:</span> 5,1%. <span style = 'color:#0078D7;'>ETF & XAU:</span> 6,4%.",
       caption = "ETF VN30 là E1VFVN30
       Các danh mục được tái cân bằng mỗi quý
       Dữ liệu ETF VN30: tổng hợp bằng vnstock
       Dữ liệu giá vàng: TradingView
       Dữ liệu lạm phát: vietstock")


# 4.2 Plot annual returns
etfvn30_annual_ret <- etfvn30_inf_ret %>% 
  arrange(portfolio, date) %>% 
  group_by(portfolio, year = year(date)) %>% 
  summarise(annual_inf_ret = prod(1 + p_inf_ret) - 1) %>% 
  ungroup() %>% 
  mutate(label_txt = scales::percent(annual_inf_ret, accuracy = 0.1)) %>% 
  mutate(year = as_factor(year))


etfvn30_annual_ret %>% 
  ggplot(aes(x = year, y = annual_inf_ret, fill = portfolio)) +
  
  geom_bar(position = "dodge", stat = "identity") + 
  
  scale_y_continuous(labels = scales::percent) + theme_tq() + theme2 + 
  
  facet_wrap(~year, ncol = 3, scales = "free") +
  
  geom_text(aes(label = label_txt), position = position_dodge(width = 0.9), stat = "identity", size = 4, vjust = -0.15) +
  
  
  scale_fill_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                    labels = c("All Weather", "ETF VN30", "XAU", "ETF & XAU")) +
  
  labs(x = "",
       y = "Tăng trưởng (%)",
       title = "Tăng trưởng mỗi năm của các danh mục",
       caption = "ETF VN30 là E1VFVN30
       Các danh mục được tái cân bằng mỗi quý
       Dữ liệu ETF VN30: tổng hợp bằng vnstock
       Dữ liệu giá vàng: TradingView
       Dữ liệu lạm phát: vietstock") +
  
  theme(axis.text.x = element_blank()) # Hide X-axis labels

# 4.3 Rolling returns
etfvn30_inf_ret %>%
  group_by(portfolio) %>%
  tk_augment_slidify(.value  = p_inf_ret,
                     .period = 12*5,
                     .f      = Return.annualized,
                     scale   = 12,
                     .names  = "roll_ret",
                     .align  = "right") %>%
  ungroup() %>%
  
  ggplot(aes(x = date, y = roll_ret, color = portfolio)) +
  geom_line(linewidth = 0.9) + theme_tq() + theme2 +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_color_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                     labels = c("All Weather", "ETF VN30", "XAU", "ETF & XAU")) +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi",
       title = "Tỷ suất sinh lợi trung bình với chu kỳ đầu tư 5 năm",
       caption = "ETF VN30 là E1VFVN30
       Các danh mục được tái cân bằng mỗi quý
       Dữ liệu ETF VN30: tổng hợp bằng vnstock
       Dữ liệu giá vàng: TradingView
       Dữ liệu lạm phát: vietstock")


# 4.4 PLot DRAWDOWNS

etfvn30_inf_ret %>% 
  group_by(portfolio) %>% 
  mutate(max_growth = cummax(growth),
         drawdown = - (1 - growth / max_growth)) %>%
  ungroup() %>% 
  ggplot(aes(x = date, y = drawdown, color = portfolio)) +
  geom_line(linewidth = 0.9) + theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_color_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                     labels = c("All Weather", "ETF VN30", "XAU", "ETF & XAU")) +
  
  labs(x = "",
       y = "Mức sụt giảm từ đỉnh (%)",
       title = "Mức độ sụt giảm từ đỉnh của các danh mục",
       caption = "ETF VN30 là E1VFVN30
       Các danh mục được tái cân bằng mỗi quý
       Dữ liệu ETF VN30: tổng hợp bằng vnstock
       Dữ liệu giá vàng: TradingView
       Dữ liệu lạm phát: vietstock")



########################

# 5. Phân tích ETF MIDCAP

mid_weights <- c(0.3, 0.55, 0.15,
                 1, 0, 0,
                 0, 0, 1,
                 0.5, 0, 0.5)

mid_symbols <- c("MIDCAP", "TCBF", "XAU")

mid_weights_tbl <- tibble(mid_symbols) %>% 
  tq_repeat_df(4) %>% 
  bind_cols(tibble(mid_weights)) %>% 
  group_by(portfolio)


# Dữ liệu CPI YOY.
cpimid_tbl <- cpi_tbl %>% 
  filter(date >= "2016-04-01")

# Gộp dữ liệu của : ETF VN30, TCBF và XAU để tính toán
etfmid_tbl <- midcap_ret %>% 
  left_join(tcbf_ret, by = "date", suffix = c("_midcap", "_tcbf")) %>% 
  left_join(xau_ret, by = "date") %>% 
  rename(MIDCAP = returns_midcap,
         TCBF = returns_tcbf,
         XAU = returns) %>% 
  
  # Tạo thêm 1 row trước đó với giá trị bằng 0. 
  # Mục đích là để biểu đồ cumulative returns bắt đầu tại cùng một điểm, là giá trị số 1 trên biểu đồ.
  add_row(date = as.Date("2016-03-01", format = "%Y-%m-%d"), MIDCAP = 0, TCBF = 0, XAU = 0, .before = 1) %>%
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date)

# Tính toán danh mục đầu tư dựa theo tỷ trọng đã đề ra
etfmid_ret <- etfmid_tbl %>%
  tq_repeat_df(n = 4) %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = mid_weights_tbl,
               rebalance_on = "quarters",
               col_rename = "p_ret")


# Điều chỉnh lạm phát và tính Cumulative returns
etfmid_inf_ret <- etfmid_ret %>% 
  left_join(cpimid_tbl, by = "date") %>%
  
  # Replace NA bằng 0
  # Do add thêm 1 row ở trước đó
  mutate(cpi = replace_na(cpi, 0),
         p_inf_ret = ((1 + p_ret)/(1 + cpi)) - 1,
         growth = cumprod(1 + p_inf_ret)) %>% 
  
  mutate(portfolio = as_factor(portfolio)) %>%
  
  mutate(name = case_when(portfolio == 1 ~ "All Weather",
                          portfolio == 2 ~ "MIDCAP",
                          portfolio == 3 ~ "XAU",
                          TRUE ~ "MIDCAP & XAU")) %>% 
  
  
  mutate(label_txt = if_else(date == max(date),
                             paste(name, ":", scales::percent(growth - 1, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% ungroup()


# Tính tỷ suất sinh lợi hằng năm
etfmid_inf_ret %>% 
  group_by(portfolio) %>% 
  tq_performance(Ra = p_inf_ret,
                 performance_fun = Return.annualized,
                 scale = 12)

# 5.1 Plot cumulative returns
etfmid_inf_ret %>% 
  ggplot(aes(x = date, y = growth - 1, color = portfolio)) + 
  geom_line(linewidth = 0.9) + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 800,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +

  scale_color_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                     labels = c("All Weather", "MIDCAP", "XAU", "ETF & XAU"))  +
  
  labs(x = "",
       y = "Lợi nhuận tích lũy",
       title = "Thành quả đầu tư của các danh mục",
       subtitle = "Tỷ suất sinh lợi hằng năm (đã điều chỉnh lạm phát)<br>
       <span style = 'color:#f94144;'>All Weather:</span> 7,2%. <span style = 'color:#018574;'>MIDCAP:</span> 10,5%. <span style = 'color:orange;'>XAU:</span> 4,8%. <span style = 'color:#0078D7;'>ETF & XAU:</span> 8,9%.",
       caption = "MIDCAP là chỉ số MIDCAP-TRI
       Các danh mục được tái cân bằng mỗi quý
       Dữ liệu MIDCAP và vàng: TradingView
       Dữ liệu lạm phát: vietstock")





# 5.2 Plot annual returns
etfmid_annual_ret <- etfmid_inf_ret %>% 
  arrange(portfolio, date) %>% 
  group_by(portfolio, year = year(date)) %>% 
  summarise(annual_inf_ret = prod(1 + p_inf_ret) - 1) %>% 
  ungroup() %>% 
  mutate(label_txt = scales::percent(annual_inf_ret, accuracy = 0.1)) %>% 
  mutate(year = as_factor(year))


etfmid_annual_ret %>% 
  ggplot(aes(x = year, y = annual_inf_ret, fill = portfolio)) +
  
  geom_bar(position = "dodge", stat = "identity") + 
  
  scale_y_continuous(labels = scales::percent) + theme_tq() + theme2 + 
  
  facet_wrap(~year, ncol = 4, scales = "free") +
  
  geom_text(aes(label = label_txt), position = position_dodge(width = 0.9), stat = "identity", size = 4, vjust = -0.15) +
  
  
  scale_fill_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                    labels = c("All Weather", "MIDCAP", "XAU", "MIDCAP & XAU")) +
  
  labs(x = "",
       y = "Tăng trưởng (%)",
       title = "Tăng trưởng mỗi năm của các danh mục",
       caption = "MIDCAP là chỉ số MIDCAP-TRI
        Các danh mục được tái cân bằng mỗi quý
       Dữ liệu MIDCAP và vàng: TradingView
       Dữ liệu lạm phát: vietstock") +
  
  theme(axis.text.x = element_blank()) # Hide X-axis labels

# 5.3 Rolling returns
etfmid_inf_ret %>%
  group_by(portfolio) %>%
  tk_augment_slidify(.value  = p_inf_ret,
                     .period = 12*5,
                     .f      = Return.annualized,
                     scale   = 12,
                     .names  = "roll_ret",
                     .align  = "right") %>%
  ungroup() %>%
  
  ggplot(aes(x = date, y = roll_ret, color = portfolio)) +
  geom_line(linewidth = 0.9) + theme_tq() + theme2 +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  scale_color_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                     labels = c("All Weather", "MIDCAP", "XAU", "ETF & XAU")) +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi",
       title = "Tỷ suất sinh lợi trung bình với chu kỳ đầu tư 5 năm",
       caption = "MIDCAP là chỉ số MIDCAP-TRI
        Các danh mục được tái cân bằng mỗi quý
       Dữ liệu MIDCAP và vàng: TradingView
       Dữ liệu lạm phát: vietstock")



# 5.4 PLot DRAWDOWNS

etfmid_inf_ret %>% 
  group_by(portfolio) %>% 
  mutate(max_growth = cummax(growth),
         drawdown = - (1 - growth / max_growth)) %>%
  ungroup() %>% 
  ggplot(aes(x = date, y = drawdown, color = portfolio)) +
  geom_line(linewidth = 0.9) + theme_tq() + theme2 +
  scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_manual(values = c("#f94144", "#018574", "orange",  "#0078D7"),
                     labels = c("All Weather", "MIDCAP", "XAU", "MIDCAP & XAU")) +
  
  labs(x = "",
       y = "Mức sụt giảm từ đỉnh (%)",
       title = "Mức độ sụt giảm từ đỉnh của các danh mục",
       caption = "MIDCAP là chỉ số MIDCAP-TRI
        Các danh mục được tái cân bằng mỗi quý
       Dữ liệu MIDCAP và vàng: TradingView
       Dữ liệu lạm phát: vietstock")




