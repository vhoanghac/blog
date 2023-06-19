# 1. Packages ---------------------------
source("header.R")
library(tidyquant)

# 2. Import data ---------------------------
xauusd <- get_data_investing("039_vang_va_ETF/01_data/raw/XAU_USD Historical Data.csv", format = "%m/%d/%Y")
usdvnd <- get_data_investing("039_vang_va_ETF/01_data/raw/USD_VND Historical Data.csv", format = "%m/%d/%Y")
etf    <- get_data_vnstock("039_vang_va_ETF/01_data/raw/E1VFVN30.csv")

# 3. Settings cho plot ---------------------------
plot_settings <- list(
  theme_tq(),
  theme2,
  scale_color_manual(values = c("#FF8C00", "#0078D7"),
                     labels = c("XAU", "E1VFVN30")),
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 8)),
  scale_x_date(breaks = scales::pretty_breaks(n = 10))
  )

# 4. Quy đổi XAU sang giá vàng Việt Nam ---------------------------

# 4.1 Tính giá vàng theo VND
# bằng cách lấy: giá vàng USD * tỷ giá USD/VND * 1.2057.
xauvnd <- xauusd %>% 
  full_join(usdvnd, by = "date", suffix = c("_xau", "_usdvnd")) %>%
  arrange(date) %>% 
  na.locf() %>%
  mutate(price = price_xau * price_usdvnd * 1.2057) %>% 
  select(date, price)

# Plot
xauvnd %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","),
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(28000000, 60000000)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 15)) +
  
  labs(x       = "",
       y       = "",
       title   = "Giá XAU sau khi quy đổi sang VND",
       caption = "Quy đổi bằng cách lấy giá XAU nhân với tỷ giá USD/VND và nhân với 1,2057
       Nguồn dữ liệu: investing.com")

# 4.2 Gộp XAU và ETF, đồng thời bổ sung thêm dữ liệu bị thiếu
data_tbl <- xauvnd %>%
  full_join(etf, by = "date", suffix = c("_xau","_etf")) %>% 
  arrange(date) %>% 
  na.locf() %>% 
  select(date, xau = price_xau, etf = price_etf) %>% 
  pivot_longer(names_to = "portfolio", !date) %>% 
  mutate(portfolio = as_factor(portfolio))


# 5. Phân tích toàn bộ quá trình ---------------------------
# 5.1 Tính returns
data_ret <- data_tbl %>% 
  group_by(portfolio) %>% 
  tq_mutate(select     = value,
            mutate_fun = periodReturn,
            period     = "daily",
            col_rename = "returns")

# 5.2 Tính tăng trưởng và vẽ biểu đồ
data_ret %>% 
  mutate(growth = cumprod(1 + returns) - 1) %>% 
  
  ggplot(aes(x = date, y = growth, col = portfolio)) +
  
  geom_line() + plot_settings + 

  labs(x       = "",
       y       = "Tăng trưởng",
       title   = str_glue("Tăng trưởng của XAU và E1VFVN30 từ năm 2014"),
       caption = "Nguồn dữ liệu: XAU (investing.com), E1VFVN30 (tổng hợp bằng vnstock)")


# 6. Tính tăng trưởng các giai đoạn khác nhau ---------------------------

# 6.1 Function:
growth_plot <- function(start_date, end_date){
  
  data_filtered <- data_ret %>% 
    filter(date >= start_date & date < end_date) %>% 
    mutate(growth = cumprod(1 + returns) - 1)
  
  # Plot
  data_filtered %>% 
    ggplot(aes(x = date, y = growth, col = portfolio)) +
    geom_line() + plot_settings +
    
    labs(x       = "",
         y       = "Tăng trưởng",
         title   = str_glue("Tăng trưởng của XAU và E1VFVN30 từ năm {year(start_date)}"),
         caption = "Nguồn dữ liệu: XAU và tỷ giá USD/VND (investing.com), E1VFVN30 (tổng hợp bằng vnstock)")

}

# 6.2 Giai đoạn XAU và ETF ngược chiều nhau
# Giai đoạn 2020 - 2021. Vàng và ETF di chuyển ngược chiều nhau
growth_plot("2020-01-01", "2022-01-01")

# Giai đoạn năm 2022 và nửa đầu năm 2023: Bất ổn do lạm phát leo thang và lãi suất tăng
growth_plot("2022-01-01", "2024-01-01")

# 6.3 Giai đoạn XAU và ETF cùng chiều nhau
growth_plot("2018-01-01", "2019-01-01")

# Giai đoạn 2018 - 2019.
# Khi thị trường điều chỉnh vào đầu năm 2018 thì giá vàng cũng giảm theo.
# Tới cuối năm 2018 thì giá vàng bắt đầu quay đầu tăng trở lại trong khi ETF thì giảm và đi ngang
growth_plot("2018-01-01", "2020-01-01")


# 7. Tính Rolling Correlation ---------------------------
# Đầu tiên là chuyển dữ liệu sang tháng 
# và lựa chọn chu kỳ 6 tháng để loại bỏ bớt biến động đột ngột
roll_corr <- data_tbl %>% 
  group_by(portfolio) %>% 
  
  tq_transmute(select     = value,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "returns") %>% 
  
  pivot_wider(names_from  = "portfolio",
              values_from = "returns") %>% 
  
  tq_mutate_xy(x          = xau,
               y          = etf,
               mutate_fun = runCor,
               n          = 6,
               col_rename = "corr")

# PLOT ---------------------------
roll_corr %>% 
  ggplot(aes(x = date, y = corr)) + theme_tq() + theme +
  
  geom_line(color = "firebrick4") + 

  scale_x_date(breaks = scales::pretty_breaks(n = 10)) + 
  
  geom_hline(yintercept = 0, linetype = 2) +
  
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
  
  labs(x       = "",
       y       = "",
       title   = "Hệ số tương quan giữa XAU và ETF E1VFVN30",
       caption = "Biến động của hệ số tương quan qua thời gian (6 tháng) giữa giá XAU và E1VFVN30.")