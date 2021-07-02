# 1. Packages #### #### #### #### 

library(tidyverse)
library(tidyquant)

source("header.R")

# 2. Import Data: #### #### #### #### 

etf <- get_data_co_phieu(ticker = "E1VFVN30") %>% 
  spread(symbol, price) %>% 
  mutate(E1VFVN30 = E1VFVN30 * 1000) %>% 
  filter(date <= "2021-06-14" & date >= "2015-09-09" )

tcbf <- read_csv("026_rebalance/data/TCBF.csv")

# Gop data ETF va TCBF:
data <- etf %>% 
  left_join(tcbf, by = "date") %>%
  fill(TCBF) %>% 
  
  gather(symbol, price, -date) %>% 
  
  group_by(symbol) %>% 
  
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "returns") %>% 
  ungroup() %>% 
  pivot_wider(id_cols     = date,
              names_from  = symbol,
              values_from = "returns")

# Convert sang xts:
data_xts <- data %>% 
  timetk::tk_xts(date_var = date) 

# 3. PORTFOLIO ANALYSIS #### #### #### #### 

weights <- c(0.4, 0.6)

# 3.1 BUY AND HOLD:

port_bh <- Return.portfolio(R = data_xts, weights = weights, verbose = TRUE, wealth.index = TRUE)

port_bh_cumr <- port_bh$wealthindex %>% 
  timetk::tk_tbl(rename_index = "date")

port_bh_r <- port_bh$wealthindex %>% 
  timetk::tk_tbl(rename_index = "date") %>% 
  tq_transmute(select = portfolio.wealthindex, mutate_fun = periodReturn, period = "daily", col_rename = "P_BH")

# 3.2. Danh muc P_PP:

# Configs:
chenhlech <- 0.1  # Chenh lech ty trong thi bat dau tai can bang
result <- tibble()  # Luu tru ket qua
i0 <- 1 
n_rebalance <- 1 # Dem so lan mua
pf_value <- 1 # Gia tri danh muc

# Analysis:
for (i in 1:nrow(data)) {
  r <- data[i0:i,]
  
  j <- 0
  
  r_i <- r %>% 
    mutate_if(is.numeric, list(v = ~ pf_value * weights[j <<- j + 1] * cumprod(1 + .))) %>%
    mutate(pf = rowSums(select(., contains("_v")))) %>% 
    mutate_at(vars(ends_with("_v")), list(w = ~ ./pf))
  
  touch_upper_band <- any(r_i[nrow(r_i),] %>% select(ends_with("_w")) %>% unlist() > weights + chenhlech)
  touch_lower_band <- any(r_i[nrow(r_i),] %>% select(ends_with("_w")) %>% unlist() < weights - chenhlech)
  
  if (touch_upper_band | touch_lower_band | i == nrow(data)) {
    i0 <- i + 1
    result <- bind_rows(result, r_i %>% mutate(n_rebalance = n_rebalance))
    pf_value <- r_i[[nrow(r_i), "pf"]]
    n_rebalance <- n_rebalance + 1
  }
}

# 4. PLOT:

# Plot etf:
etf %>% 
  ggplot(aes(x = date, 
             y = E1VFVN30)) +
  
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  labs(x       = "",
       y       = "",
       title   = "Giá chứng chỉ quỹ ETF E1VFVN30",
       caption = "Nguồn: cophieu68.
       Từ 2015-09-09 \u0111ến 2021-06-14")

# Plot Portfolio PP returns
result %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = pf - 1), 
            color = "firebrick4") + theme_tq() + theme + 
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::percent_format()) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  labs(x     = "",
       y     = "Lợi nhuận tích l\u0169y",
       title = "Thành quả danh mục P_PP")

# Plot weight
result %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = E1VFVN30_v_w), color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::percent_format()) +
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  labs(x = "",
       y = "",
       title = "Tỷ trọng của TCBF trong danh mục P_PP")


# So sanh danh muc Buy and hold:
# Cumulative returns
result %>% 
  ggplot(aes(x = date)) + 
  
  geom_line(aes(y = pf - 1, color = "y")) +
  
  geom_line(data = port_bh_cumr, aes(y     = portfolio.wealthindex - 1, 
                                     color = "x"))    +
  
  theme_tq() + theme2 + theme(legend.text = element_text(size = 14)) +
  
  scale_color_manual(values = c("#3C5488B2", "#DC0000B2"),
                     labels = c("Danh mục P_BH", "Danh mục P_PP")) + 
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                     labels = scales::percent_format()) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  labs(x       = "",
       y       = "Lợi nhuận tích l\u0169y",
       title   = "Thành quả hai danh mục P_BH và P_PP",
       color   = "",
       caption = "P_BH: Buy and hold.
       P_PP: Tái cân bằng khi chênh lệch >10% so với tỷ trọng \u0111ề ra")


# Drawdown chart:
drawdown_xts <- result %>% 
  tq_transmute(select     = pf, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               col_rename = "P_PP") %>% 
  
  bind_cols(port_bh_r %>% select(P_BH)) %>% 
  bind_cols(result %>% select(E1VFVN30)) %>% 
  timetk::tk_xts(date_var = date)


chart.Drawdown(drawdown_xts, geometric = TRUE, plot.engine = "ggplot2") + theme_tq() + theme2 + 
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                     labels = scales::percent_format(accuracy = 1L)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +

  scale_color_manual(values = c("#DC0000B2","#3C5488B2", "#00A087B2"),
                     labels = c("Danh mục P_PP", "Danh mục P_BH", "ETF E1VFVN30")) +
  
  labs(color = "",
       title = "Mức \u0111ộ sụt giảm vốn từ \u0111ỉnh")