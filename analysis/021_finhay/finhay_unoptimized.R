# 1. Packages
library(tidyverse) # Tidy
library(readxl) # Import excel

library(lubridate) # Date, time
library(timetk) # Time series

library(see)
library(tidyquant) # Theme, Performance analytics
library(ggsci) # Theme
source("header.R") # Theme


# Import du lieu + TIDY:

get_data_co_phieu(ticker = "^VNINDEX") # VNINDEX

vfmvf1 <- read_csv("023_finhay/data/VFMVF1.csv") # VFMVF1

path <- "023_finhay/data/finhay.xlsx" # Import Finhay:


# TIDY
finhay_tidied_tbl <- path %>% 
  excel_sheets() %>% 
  set_names() %>%
  map_df(~ read_excel(path =  path, sheet = .), .id = "Sheet") %>% 
  mutate(date = ymd(date)) %>% 
  select(-price) %>% 
  rename(symbol = Sheet,
         price = value) %>% 
  group_by(symbol) %>% 
  inner_join(`data_^VNINDEX`, by = "date", suffix = c("", ".y")) %>% 
  ungroup() %>% 
  select(-symbol.y, -price.y)

vnindex_tidied_tbl <- finhay_tidied_tbl %>% 
  filter(symbol == "Rua") %>% 
  inner_join(`data_^VNINDEX`, by = "date", suffix = c(".x", "")) %>% 
  select(symbol, date, price) %>% 
  mutate(symbol = 'VNINDEX')
  
vfmvf1_tidied_tbl <- vnindex_tidied_tbl %>% 
  left_join(vfmvf1, by = "date", suffix = c(".x", "")) %>% 
  mutate(price = case_when(date == "2017-01-13" ~ 28817.91,
         TRUE ~ na.locf(price, na.rm = FALSE)),
         
         symbol = "VFMVF1") %>% 
  
  filter(!duplicated(.)) %>% 
  select(symbol, date, price)

  
# CALCULATE RETURNS:

# FUNCTION:

calculate_returns <- function(data, from = "2017-01-13", p = "daily"){
  
  data %>% 
    group_by(symbol) %>% 
    filter(date >= from) %>% 
    
    # Log Returns:
    tq_transmute(select = price, 
                 mutate_fun = periodReturn, 
                 period = p,
                 type = "log",
                 col_rename = "returns") %>% 
    
    # Growth of 10,000,000 VND:
    mutate(growth = 10000000 * exp(cumsum(returns))) %>% 
    
    ungroup() %>% 
    left_join(data, by = c("symbol", "date")) %>% 
    mutate(symbol = symbol %>% as_factor())
    
}


# Daily returns:
finhay_daily_tbl <- calculate_returns(finhay_tidied_tbl, p = "daily")

vnindex_daily_tbl <- calculate_returns(vnindex_tidied_tbl, p = "daily")

vfmvf1_daily_tbl <- calculate_returns(vfmvf1_tidied_tbl, p = "daily")

# Monthly returns:
finhay_monthly_tbl <- calculate_returns(finhay_tidied_tbl, p = "monthly")

vnindex_monthly_tbl <- calculate_returns(vnindex_tidied_tbl, p = "monthly")

vfmvf1_monthly_tbl <- calculate_returns(vfmvf1_tidied_tbl, p = "monthly")

# Plot 5 Quy:
finhay_monthly_tbl %>% 
  ggplot(aes(x = date,
             y = price,
             color = symbol)) +
  
  geom_line(alpha = 0.8, size = 1.2) +
  
  # facet_wrap(~symbol, ncol = 1) + 
  
  theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::number_format(big.mark = "\\."),
                     breaks = scales::pretty_breaks(n = 6)) +
  
  scale_color_npg() +
  labs(x = "",
       y = "Tài sản",
       color = "Danh mục",
       title = "Giá trị của các danh mục khi \u0111ầu t\u01B0 từ 2017-01-13 \u0111ến 2021-03-05",
       caption = "Dữ liệu theo tháng\nGiả \u0111ịnh \u0111ầu t\u01B0 10.000.000 \u0111ồng\nNguồn: Finhay")



# VIOLIN PLOT
finhay_monthly_tbl %>% 
  ggplot(aes(x = symbol, y = returns, fill = symbol)) +
  geom_violin() +
  geom_jitter(color = "black",
              size = 1,
              alpha = 0.8,
              position = position_jitter(0.2)) + 
  

  geom_hline(yintercept = 0,
             color = "black",
             linetype = "dashed") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = scales::pretty_breaks(n = 6)) +
  
  theme_tq() + theme +
  scale_fill_npg() +
  
  labs(x = "",
       y = "Lợi nhuận",
       title = "Biểu \u0111ồ phân phối biến \u0111ộng lợi nhuận theo tháng")


# DISTRIBUTION PLOT:

qtl <- finhay_daily_tbl %>% 
  group_by(symbol) %>% 
  summarise(q1 = quantile(returns, c(0.025)),
            q9 = quantile(returns, c(0.975)))

finhay_daily_tbl %>% 
  ggplot(aes(x = returns)) +
  geom_histogram(bins = 40) +
  geom_rug(color = "grey") +
  
  scale_x_continuous(labels = scales::percent_format()) +
  
  geom_vline(xintercept = 0, lty = 2, color = "black") +
  geom_vline(data = qtl, aes(xintercept = q1), color = "firebrick4", lty = 2) +
  geom_vline(data = qtl, aes(xintercept = q9), color = "firebrick4", lty = 2) +
  
  facet_wrap(~symbol, ncol = 2, scales = "free") + theme_tq() + theme2 + 
  
  labs(x = "Log Returns",
       y = "",
       title = "Biểu \u0111ồ phân phối biến \u0111ộng lợi nhuận theo ngày",
       subtitle = "Distribution of daily log-returns",
       caption = "Dữ liệu tính toán theo ngày\nKhoảng nằm giữa hai \u0111ường màu \u0111ỏ chứa 95% số quan sát")


# DRAWDOWN:

finhay_daily_tbl %>% 
  bind_rows(vfmvf1_daily_tbl) %>% 
  select(date, symbol, returns) %>% 
  spread(symbol, returns) %>% 
  tk_xts(date_var = date, select = c("Rua", "CoTrang", "Voi", "Saola", "Trau", "VFMVF1")) %>% 
  chart.Drawdown(plot.engine = "ggplot2") + theme_tq() + theme2 + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  
  labs(x = "",
       y = "Mức sụt giảm",
       color = "Danh mục",
       title = "Mức sụt giảm vốn từ \u0111ỉnh của các danh mục",
       caption = "Dữ liệu tính toán theo ngày") +
  scale_color_npg() 

vfmvf1_daily_tbl %>% filter(date >= "2018-01-01" & date < "2019-01-01") %>% View()


# returns above Rf

finhay_monthly_tbl %>% 
  group_by(symbol) %>% 
  mutate(sharpe = mean(returns - rf_m) / sd(returns - rf_m)) %>% 
  
  mutate(returns_below_rf = ifelse(returns < rf_m, returns, as.numeric(NA))) %>% 
  
  mutate(returns_above_rf = ifelse(returns > rf_m, returns, as.numeric(NA))) %>% 
  mutate_if(is.numeric, funs(round(., 4))) %>% 
  
  ggplot(aes(x = date)) +
  
  geom_point(aes(y = returns_below_rf),
             color = "red") +
  
  geom_point(aes(y = returns_above_rf),
             color = "green") +
  
  geom_hline(yintercept = rf_m,
             color = "firebrick4",
             linetype = "dashed") +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks()) +
  
  facet_wrap(~symbol, scales = "free", ncol = 2) + theme_tq() + theme + 
  
  labs(x = "",
       y = "Lợi nhuận",
       title = "Lợi nhuận theo tháng của các danh mục so với lãi suất giả \u0111ịnh 6,5%",
       caption = "Dữ liệu tính toán theo tháng\nCác \u0111iểm bên d\u01b0ới \u0111\u01b0ờng gạch nối màu \u0111ỏ là thời \u0111iểm\n lợi nhuận thấp h\u01a1n lãi suất ngân hàng trả trong tháng")



#DAILY RETURNS:

finhay_daily_tbl %>% 
  bind_rows(vfmvf1_daily_tbl) %>% 
  group_by(symbol) %>% 
  mutate(sharpe = mean(returns - rf_d) / sd(returns - rf_d)) %>% 
  
  mutate(returns_below_rf = ifelse(returns < rf_d, returns, as.numeric(NA))) %>% 
  
  mutate(returns_above_rf = ifelse(returns > rf_d, returns, as.numeric(NA))) %>% 
  mutate_if(is.numeric, funs(round(., 4))) %>% 
  
  ggplot(aes(x = date)) +
  
  geom_point(aes(y = returns_below_rf),
             color = "red",
             size = 0.5) +
  
  geom_point(aes(y = returns_above_rf),
             color = "green",
             size = 0.5) +
  
  geom_hline(yintercept = rf_d,
             color = "black",
             linetype = "dashed") +
  
  facet_wrap(~symbol, scales = "free", ncol = 2) + theme_tq() + theme + 
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 6)) +
  
  labs(x = "",
       y = "Lợi nhuận",
       title = "Lợi nhuận theo ngày của các danh mục so với lãi suất giả \u0111ịnh 6,5%",
       caption = "Dữ liệu tính toán theo ngày\nCác \u0111iểm bên d\u01b0ới \u0111\u01b0ờng gạch nối màu \u0111ỏ là thời \u0111iểm\n lợi nhuận thấp h\u01a1n lãi suất ngân hàng trả trong ngày")



# SHARPE

rf_d <- 0.065 / 252
rf_m <- 0.065 / 12

finhay_daily_tbl %>% 
  group_by(symbol) %>% 
  summarise(std = sd(returns),
            sharpe = mean(returns - rf_d) / sd(returns - rf_d))

finhay_daily_tbl %>% 
  group_by(symbol) %>% 
  tq_performance(Ra = returns,
                 performance_fun = SharpeRatio,
                 Rf = rf_d,
                 FUN = "StdDev")

finhay_daily_tbl %>% 
  group_by(symbol) %>% 
  tq_performance(Ra = returns, 
                 Rf = rf_d,
                 performance_fun = table.AnnualizedReturns) %>% t()



# VISUALIZE SHARPE / STANDARD DEVIATION
finhay_monthly_tbl %>% 
  group_by(symbol) %>% 
  # filter(date >= "2018-01-01" & date < "2019-01-01") %>%
  summarise(stdev = sd(returns),
            sharpe = mean(returns - rf_m) / sd(returns - rf_m)) %>%
  
  mutate(sharpe_pct = scales::number(sharpe, accuracy = 0.001)) %>% 
  
  ggplot(aes(x = stdev,
             y = sharpe,
             color = symbol)) + theme_tq() + theme +
  
  geom_point(size = 2) + 
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = expand_scale(mult = c(0.1, .2))) + 
  
  geom_text(aes(y = sharpe + 0.01, label = str_glue("{symbol}\n{sharpe_pct}"))) +
  
  scale_color_npg() +
  
  labs(x = "Độ lệch chuẩn",
       y = "Tỉ số Sharpe",
       title = "Tỉ số Sharpe và Độ lệch chuẩn của các danh mục",
       caption = "Dữ liệu tính toán theo tháng từ 2017-01-13 \u0111ến 2021-03-05")



### ROLLING SHARPE RATIO:

rolling_sharpe <- function(x){
  
  SharpeRatio(x,
              Rf = rf_m,
              FUN = "StdDev")
}

finhay_monthly_rollsharpe <- finhay_monthly_tbl %>% 
  bind_rows(vfmvf1_monthly_tbl) %>% 
  group_by(symbol) %>% 
  tq_mutate(select = returns,
            mutate_fun = rollapply,
            width = 6,
            align = "right",
            FUN = rolling_sharpe,
            col_rename = "sharpe") %>% 
  na.omit() 

finhay_monthly_rollsharpe %>% view()
  group_by(symbol) %>% 
 
  ggplot(aes(x = date,
             y = sharpe,
             color = symbol)) +
  
  geom_line(size = 1.2) + theme_tq() + theme2 + scale_color_npg() +
  
  labs(x = "",
       y = "Tỉ số Sharpe",
       title = "Biến \u0111ộng tỉ số Sharpe qua thời gian",
       color = "Danh mục",
       caption = "Dữ liệu tính toán theo tháng. Chu kỳ 6 tháng.\nRf = 6,5%/n\u0103m")

  
finhay_monthly_tbl %>% 
  group_by(symbol) %>% 
  filter(date < "2018-01-01") %>% 
  tq_performance(Ra = returns,
                 Rf = rf_m,
                 performance_fun = SharpeRatio,
                 scale = 12,
                 geometric = TRUE) %>% t()



## new chart MEAN RETURNS / STDEV

finhay_monthly_tbl %>% 
  group_by(symbol) %>% 
  summarise(mean_returns = mean(returns),
            stdev = sd(returns)) %>%
  
  mutate(returns_label = scales::percent(mean_returns, accuracy = 0.01)) %>% 
  
  ggplot(aes(x = stdev, y = mean_returns, color = symbol)) +
  
  geom_point() + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + 
  
  geom_text(aes(y = mean_returns + 0.0002, label = str_glue("{symbol}\n{returns_label}"))) + theme_tq() + theme +
  
  labs(x = "Độ lệch chuẩn",
       y = "Lợi nhuận trung bình (tháng)",
       title = "Lợi nhuận / rủi ro của các danh mục",
       caption = "Dữ liệu tính toán theo tháng từ 2017-01-13 \u0111ến 2021-03-05")


# TABLES:
finhay_monthly_tbl %>% 
  bind_rows(vfmvf1_monthly_tbl) %>% 
  group_by(symbol) %>% 
  tq_performance(Ra = returns,
                 performance_fun = table.Drawdowns) %>% t()

finhay_monthly_tbl %>% 
  bind_rows(vfmvf1_monthly_tbl) %>% 
  group_by(symbol) %>% 
  tq_performance(Ra = returns,
                 performance_fun = table.DownsideRisk) %>% t()

finhay_monthly_tbl %>% 
  bind_rows(vfmvf1_monthly_tbl) %>% 
  inner_join(vnindex_monthly_tbl, by = "date") %>% 
  select(symbol.x, date, returns.x, returns.y) %>% 
  group_by(symbol.x) %>% 
  tq_performance(Ra = returns.x,
                 Rf = rf_m,
                 performance_fun = SharpeRatio) %>% t()

