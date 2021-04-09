# 1. Packages
library(tidyverse)
library(tidyquant)
library(timetk)
library(ggsci)
library(cowplot)

source("header.R") 
source("024_bitcoin_port/tidy_data.R")

# 2. BITCOIN #### #### #### #### 

# Gia bitcoin:
btc_vnd_tbl %>% 
  ggplot(aes( x = date, y = BITCOIN)) +
  geom_line(color = "#E64B35FF") +
  scale_y_log10(labels = scales::number_format(big.mark = "\\.")) +
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) + theme_tq() + theme2 +
  labs(x = "",
       y = "VND",
       title = "Giá Bitcoin theo VND (log scale)",
       caption = str_glue("{first(btc_vnd_tbl$date)} \u0111ến {last(btc_vnd_tbl$date)}
                          Nguồn giá Bitcoin USD: FRED
                          Nguồn tỷ giá USD/VND: investing.com"))

# Returns TABLE:
# Quarters:
btc_vnd_tbl %>% 
  tq_transmute(select = BITCOIN, 
               mutate_fun = periodReturn,
               period = "quarterly",
               col_rename = "returns") %>% 
  mutate(labels = returns %>% scales::percent())

# Years
btc_vnd_tbl %>% 
  tq_transmute(select = BITCOIN, 
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "returns") %>% 
  mutate(labels = returns %>% scales::percent())


# Volatility (Rolling Standard deviation)
tidied_full_data_r <- tidied_full_data %>% 
  gather(symbol, price, -date) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "returns") %>% 
  ungroup()


tidied_full_data_rsd <- tidied_full_data_r %>% 
  group_by(symbol) %>% 
  tq_mutate(mutate_fun = rollapply,
            width = 90,
            FUN = sd.annualized,
            col_rename = "rolling_sd") %>% 
  ungroup()


############### Plot:
tidied_full_data_rsd %>% 
  ggplot(aes(x = date, y = rolling_sd, color = symbol)) +
  geom_line(size = 1) + theme_tq() + theme2 +
  scale_color_npg() +
  scale_y_continuous(labels = scales::percent_format(suffix = "", accuracy = 1L),
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = "",
       y = "Phần tr\u0103m",
       color = "",
       title = "Biến \u0111ộng giá của Bitcoin và các loại chứng khoán khác",
       subtitle = str_glue("{first(tidied_full_data_rsd$date)} \u0111ến {last(tidied_full_data_rsd$date)}"),
       caption = "Annualized 90-day rolling stdev
       Daily log returns")



# ROLLING CORELATION:
btc_returns <- tidied_full_data_r %>% 
  filter(symbol == "BITCOIN") %>% 
  spread(symbol, returns)


tidied_full_data_r %>% 
  filter(!symbol == "BITCOIN") %>% 
  group_by(symbol) %>% 
  left_join(btc_returns, by = "date") %>% 
  tq_transmute_xy(x = returns,
                  y = BITCOIN,
                  mutate_fun = runCor,
                  n = 90,
                  col_rename = "rolling_corr_90days") %>% 
  ungroup() %>% 
  
  ggplot(aes(x = date, y = rolling_corr_90days)) +
  
  geom_rect(ymin = -0.25, 
            ymax = 0.25,
            xmin = -Inf, 
            xmax = Inf, 
            fill = "#CFD8DC", alpha = 0.2) +
  
  geom_line(color = "#E64B35FF") +
  
  facet_wrap(~symbol, scales = "free") + theme_tq() + theme +
  
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
  
  labs(x = "",
       y = "",
       title = "Hệ số t\u01b0\u01a1ng quan giữa Bitcoin với các cổ phiếu khác",
       subtitle = str_glue("{first(tidied_full_data_r$date)} \u0111ến {last(tidied_full_data_r$date)}"),
       caption = "90-day rolling correlation
                  Daily log returns")




# 3. DANH MUC DAU TU #### #### #### #### #### #### #### #### 

tidied_data_r <- tidied_data %>%  
  gather(symbol, price, -date) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "returns") %>% 
  ungroup()


# Portfolios:
port_multi <- tidied_data_r %>% 
  group_by(symbol) %>% 
  tq_repeat_df(n = 4)

weights <- c(   0, 0.40 , 0.60,
             0.01, 0.396, 0.594,
             0.02, 0.392, 0.588,
             0.03, 0.388, 0.582)

symbol <- c("BITCOIN", "E1VFVN30", "TCBF")

weights_table <- tibble(symbol) %>% 
  tq_repeat_df(n = 4) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio)


# Portfolio returns:
port_returns_multi <- port_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_table,
               rebalance_on = "quarters",
               col_rename = "p.returns") %>% 
  mutate(growth = exp(cumsum(p.returns)) - 1) %>% 
  ungroup() %>% 
  mutate(portfolio = portfolio %>% as_factor())


# Plot:
port_returns_multi %>%  
  ggplot(aes(x = date, y = growth, color = portfolio)) +
  geom_line(size = 1.1) + theme_tq() + theme2 +  
  
  theme(legend.direction = "vertical",
        legend.text = element_text(face = "bold", size = 13)) + 
  
  scale_color_npg(labels = c("Danh mục 60/40",
                             "1% Bitcoin", 
                             "2% Bitcoin",
                             "3% Bitcoin")) + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     breaks = scales::pretty_breaks()) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  geom_hline(yintercept = 0, linetype = 2, color = "grey") + 
  
  labs(x = "",
       y = "Phần tr\u0103m",
       color = "",
       title = "Lợi nhuận tích l\u0169y của các danh mục",
       subtitle = str_glue("{first(port_returns_multi$date)} \u0111ến {last(port_returns_multi$date)}"),
       caption = "Các danh mục \u0111\u01b0ợc tái cân bằng tỷ trọng mỗi quý
                  Log returns. 20 là lời 20%")


# TABLES: Annualized Returns
port_returns_multi %>% 
  group_by(portfolio) %>% 
  summarise(annualized_r = exp(mean(p.returns) * 252 ) - 1,
            annualized_stdev = sqrt(252) * sd(p.returns),
            annualized_sharpe = annualized_r / annualized_stdev)

# Risk contribution of 2% Bitcoin:
returns_tbl <- tidied_data %>%  
  gather(symbol, price, -date) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "quarterly",
               type = "log",
               col_rename = "returns") %>% 
  ungroup()  %>% 
  spread(symbol, returns)

# Import functions:
source("025_bitcoin_port/risk_contribution_function.R")

risk_per <- returns_tbl %>% 
  select(-date) %>% 
  component_matrix(., w)

risk_per %>% 
  mutate(weights = w) %>% 
  gather(type, percent, -symbol) %>% 
  mutate(label = scales::percent(percent)) %>% 
  group_by(type) %>% 
  
  ggplot(aes(x = symbol,
             y = percent,
             fill = type)) +
  
  geom_col(position = "dodge") + theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  
  scale_fill_discrete(name = "", labels = c("% Đóng góp", "Tỷ trọng")) +
  
  geom_text(aes(label = label), 
            position = position_dodge(width = 0.9), 
            stat = "identity", size = 4, vjust = - 0.5) +
  
  labs(x = "",
       y = "Phần tr\u0103m",
       title = "Phần tr\u0103m \u0111óng góp vào biến \u0111ộng của danh mục 2% Bitcoin",
       subtitle = str_glue("{first(port_returns_multi$date)} \u0111ến {last(port_returns_multi$date)}"),
       caption = "Tính toán dựa vào biến \u0111ộng giá tài sản trong danh mục 
       và có tái cân bằng tỷ trọng mỗi quý.
       Log returns.")

# # Weights
# 
# tidied_data_r_xts <- tidied_data_r %>% 
#   spread(symbol, returns) %>% 
#   tk_xts(date_var = date, silent = TRUE)
# 
# 
# port_2b_xts <- Return.portfolio(R = tidied_data_r_xts,
#                  weights = w,
#                  rebalance_on = "quarters",
#                  verbose = TRUE)
# 
# port_2b_xts$EOP.Weight %>% 
#   tk_tbl(rename_index = "date") %>% 
#   gather(symbol, weight, -date) %>% 
#   
#   ggplot(aes(x = date, y = weight, fill = symbol)) +
#   
#   geom_area() + theme_tq() + theme2 + scale_fill_npg()


### Rolling cumulative Returns:
# Tinh base portfolio 0% Bitcoin
# Tinh tq_mutate cum returns.
# Lay cum_returns cac portfolio tru cho cum_returns base portfolio

rolling_cum_returns <- port_returns_multi %>% 
  filter(portfolio %in% c(1, 3)) %>% 
  group_by(portfolio) %>% 
  mutate(roll_cumreturns = rollapply(p.returns,
                                     FUN = function(x) exp(sum(x)) - 1,
                                     width = 1095,
                                     align = "right",
                                     fill = NA)) %>% 
  ungroup() %>% 
  select(date, portfolio, roll_cumreturns) %>% 
  spread(portfolio, roll_cumreturns) %>% 
  rename(normal = `1`,
         bitcoin = `3`) %>% 
  
  mutate(green = ifelse(bitcoin > normal, bitcoin, as.numeric(NA)),
         red = ifelse(bitcoin < normal, bitcoin, as.numeric(NA)))

p1 <- rolling_cum_returns %>%   
    ggplot(aes(x = date)) +
  
    geom_ribbon(aes(x = date,
                  ymax = normal,
                  ymin = red, 
                  fill = "red")) +

    geom_ribbon(aes(x = date, 
                    ymax = green, 
                    ymin = normal,
                    fill = "green")) +
  
  geom_line(aes(y = normal), show.legend = FALSE) +
  
  scale_fill_manual(values = c("#00CC6A", "#E74856"),
                     labels = c("Tốt",
                                "Xấu")) +
  
  theme_tq() + theme2 + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  labs(x = "",
       y = "Phần tr\u0103m",
       fill = "",
       title = "Đóng góp của 2% Bitcoin vào lợi nhuận tích l\u0169y của danh mục 60/40",
       subtitle = str_glue("Chu kỳ nắm giữ 1 n\u0103m từ {first(port_returns_multi$date)} \u0111ến {last(port_returns_multi$date)}
                           1 year rolling cumulative returns"))


plot_grid(p1, p2, p3, ncol = 1)
  
  
rolling_cum_returns %>% 
  select(date, normal, bitcoin) %>% 
  # summary()
  summarise(pos = sum(bitcoin > 0, na.rm = TRUE),
            neg = sum(bitcoin <= 0, na.rm = TRUE)) %>% 
  mutate(win_rate = (pos / (pos+neg)),
         loss_rate = 1 - win_rate)
  

# BITCOIN WEIGHTS 

port_multi10 <- tidied_data_r %>% 
  group_by(symbol) %>% 
  tq_repeat_df(n = 11)

weights10 <- c(   0,  0.40 , 0.60,
                0.01, 0.396, 0.594,
                0.02, 0.392, 0.588,
                0.03, 0.388, 0.582,
                0.04, 0.384, 0.576,
                0.05, 0.38 , 0.57 ,
                0.06, 0.376, 0.564,
                0.07, 0.372, 0.558,
                0.08, 0.368, 0.552,
                0.09, 0.364, 0.546,
                0.1 , 0.36 , 0.54)

symbol <- c("BITCOIN", "E1VFVN30", "TCBF")

weights_table10 <- tibble(symbol) %>% 
  tq_repeat_df(n = 11) %>% 
  bind_cols(tibble(weights10)) %>% 
  group_by(portfolio)


port_returns_multi10 <- port_multi10 %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_table10,
               rebalance_on = "quarters",
               col_rename = "p.returns") %>% 
  ungroup()









# TABLE: Cumulative returns:
cum_returns_10 <- port_returns_multi10 %>% 
  mutate(roll_cumreturns = rollapply(p.returns,
                                     FUN = function(x) exp(sum(x)) - 1,
                                     width = 1095,
                                     align = "right",
                                     fill = NA)) %>% 
  ungroup() %>% 
  select(date, portfolio, roll_cumreturns)



# PLOT: 
label <- c("0%", "1%", "2%",
          "3%", "4%", "5%",
          "6%", "7%", "8%",
          "9%", "10%")

cum_returns_10 %>%
  group_by(portfolio) %>% 
  mutate(average = mean(roll_cumreturns, na.rm = TRUE)) %>%
  mutate(portfolio = portfolio %>% as_factor()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = portfolio)) +
  
  geom_point(aes(y = roll_cumreturns), alpha = 0.05) +
  
  geom_line(aes(x = as.numeric(portfolio), y = average, color = "Trung bình"),
            size = 1.2, show.legend = TRUE) + theme_tq() + theme2 +
  
  scale_x_discrete(labels = label) + 
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0, 1.2)) +

  labs(x = "Tỷ trọng Bitcoin trong danh mục",
       y = "Lợi nhuận tích l\u0169y",
       color = "",
       title = "Lợi nhuận tích l\u0169y t\u01b0\u01a1ng ứng với tỷ trọng Bitcoin",
       subtitle = "3 years rolling cumulative returns",
       caption = "Thời gian nắm giữ 3 n\u0103m
       Danh mục tái cân bằng tỷ trọng mỗi quý")





# PLOT: Rolling stdev:

stdev_10_tq <- port_returns_multi10 %>% 
  tq_mutate(mutate_fun = rollapply,
            width = 1095,
            FUN = sd.annualized,
            col_rename = "rolling_sd") %>% 
  ungroup()


###


stdev_10_tq %>% 
  group_by(portfolio) %>% 
  mutate(average = mean(rolling_sd, na.rm = TRUE)) %>% 
  mutate(portfolio = portfolio %>% as_factor()) %>% 
  ungroup() %>% 
  ggplot(aes(x = portfolio)) +
  
  geom_point(aes(y = rolling_sd), alpha = 0.05) +
  
  geom_line(aes(x = as.numeric(portfolio), y = average, color = "Trung bình"),
            size = 1.2, show.legend = TRUE) + theme_tq() + theme2 +
  
  scale_x_discrete(labels = label) + 
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  labs(x = "Tỷ trọng Bitcoin trong danh mục",
       y = "Độ lệch chuẩn",
       color = "",
       title = "Độ lệch chuẩn t\u01b0\u01a1ng ứng với tỷ trọng Bitcoin",
       subtitle = "3 years rolling standard deviation",
       caption = "Thời gian nắm giữ 3 n\u0103m
       Danh mục tái cân bằng tỷ trọng mỗi quý")


# PLOT: Sharpe:
port_returns_multi %>% 
  group_by(portfolio) %>% 
  summarise(annualized_r = exp(mean(p.returns) * 252 ) - 1,
            annualized_stdev = sqrt(252) * sd(p.returns),
            annualized_sharpe = annualized_r / annualized_stdev)





sharpe_10 <- stdev_10_tq %>% 
  group_by(portfolio) %>% 
  mutate(annualized_returns = rollapply(p.returns,
                                     FUN = function(x) exp(mean(x) * 252) - 1,
                                     width = 1095,
                                     align = "right",
                                     fill = NA)) %>% 
  mutate(sharpe = annualized_returns / rolling_sd) %>% 
  
  mutate(average = mean(sharpe, na.rm = TRUE)) %>% 
  
  mutate(portfolio = portfolio %>% as_factor()) %>% 
  ungroup()
  

sharpe_10 %>% 
  ggplot(aes(x = portfolio)) +
  
  geom_point(aes(y = sharpe), alpha = 0.05) +
  
  geom_line(aes(x = as.numeric(portfolio), y = average, color = "Trung bình"),
            size = 1.2, show.legend = TRUE) + theme_tq() + theme2 +
  
  scale_x_discrete(labels = label) + 
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, 2)) +
  
  labs(x = "Tỷ trọng Bitcoin trong danh mục",
       y = "Tỉ số Sharpe",
       color = "",
       title = "Tỉ số Sharpe t\u01b0\u01a1ng ứng với tỷ trọng Bitcoin",
       subtitle = "3 years rolling sharpe ratio",
       caption = "Thời gian nắm giữ 3 n\u0103m
       Danh mục tái cân bằng tỷ trọng mỗi quý
       Rf = 0%")


# DRAWDOWN

max_dd_10 <- port_returns_multi10 %>% 
  tq_mutate(mutate_fun = rollapply,
            width = 1095,
            FUN = maxDrawdown,
            col_rename = "max_dd")


max_dd_10 %>% 
  mutate(average = mean(max_dd, na.rm = TRUE)) %>% 
  mutate(portfolio = portfolio %>% as_factor()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = portfolio)) +
  
  geom_point(aes(y = max_dd), alpha = 0.05) +
  
  geom_line(aes(x = as.numeric(portfolio), y = average, color = "Trung bình"),
            size = 1.2, show.legend = TRUE) + theme_tq() + theme2 +
  
  scale_x_discrete(labels = label) + 
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  labs(x = "Tỷ trọng Bitcoin trong danh mục",
       y = "Mức \u0111ộ sụt giảm vốn",
       color = "",
       title = "Mức \u0111ộ sụt giảm vốn t\u01b0\u01a1ng ứng với tỷ trọng Bitcoin",
       subtitle = "3 years rolling maximum drawdown",
       caption = "Thời gian nắm giữ 3 n\u0103m
       Danh mục tái cân bằng tỷ trọng mỗi quý")
  


# 5 . TABLES #### #### #### ####
# Cumulative Returns
cum_returns_10 %>% 
  spread(portfolio, roll_cumreturns) %>% 
  summary() %>% t()

# Standard dev:
stdev_10_tq %>% 
  select(date, portfolio, rolling_sd) %>% 
  spread(portfolio, rolling_sd) %>% 
  summary() %>% t()

# Sharpe:
sharpe_10 %>% 
  select(date, portfolio, sharpe) %>% 
  spread(portfolio, sharpe) %>% 
  summary() %>% t()
  
# Max drawdown:
max_dd_10 %>% 
  select(date, portfolio, max_dd) %>% 
  spread(portfolio, max_dd) %>% 
  summary() %>% t()
09