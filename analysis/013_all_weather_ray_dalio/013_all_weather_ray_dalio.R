# Header ----
source(file.path(paste0(getwd(),"/header.R")))

# Import libraries ----
library(tidyverse)
library(readxl)
library(lubridate) #date
library(tidyquant)

# Functions:
import <- function(url){
  read_csv(url,
           col_types = cols(date = col_date(format = "%Y-%m-%d")))
}


# Import data ----
etf_tbl <- import(paste0(data_dir, "/ETF_E1VFVN30.csv"))
tcbf_tbl <- import(paste0(data_dir, "/TCBF.csv"))
xau_tbl <- import(paste0(data_dir, "/giavang.csv"))


cpi_tbl <- read_csv(paste0(data_dir, "/cpi.csv"),
                    col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                     ret_cpi = col_number()))

# TIDY AND CALCUALTE RETURNS ----

etf_returns <- etf_tbl %>% 
  filter(date >= "2015-09-09") %>% 
  tq_transmute(mutate_fun = periodReturn,
               period     = "monthly",
               type = "log",
               col_rename = "etf")

tcbf_returns <- tcbf_tbl %>% 
  tq_transmute(mutate_fun = periodReturn,
               period     = "monthly",
               type = "log",
               col_rename = "tcbf") %>% 
  bind_cols(etf_returns %>% select(date)) %>% 
  select(date...3, tcbf) %>% 
  rename(date = date...3)

xau_returns <- xau_tbl %>% 
  select(-row) %>% 
  filter(date >= "2015-09-09") %>% 
  tq_transmute(mutate_fun = periodReturn,
               period     = "monthly",
               type = "log",
               col_rename = "xau") %>% 
  bind_cols(etf_returns %>% select(date)) %>% 
  select(date...3, xau) %>% 
  rename(date = date...3)

cpi_returns <- cpi_tbl %>% 
  mutate(ret_cpi = ret_cpi / 10000) %>% 
  filter(date >= "2015-09-09") %>% 
  bind_cols(etf_returns %>% select(date)) %>% 
  select(date...3, ret_cpi) %>% 
  rename(date = date...3,
         cpi = ret_cpi)
  
raw <- etf_returns %>% 
  left_join(tcbf_returns, by = "date") %>% 
  left_join(xau_returns,  by = "date") %>% 
  left_join(cpi_returns,  by = "date") %>% 
  mutate(etf  = etf - cpi,
         tcbf = tcbf - cpi,
         xau  = xau - cpi) %>% 
  select(-cpi) %>% 
  add_row(date = as.Date("2015-08-31", format = "%Y-%m-%d"), etf = 0, tcbf = 0, xau = 0, .before = 1)

# Convert to XTS:
raw_xts <- raw %>% 
  timetk::tk_xts(date_var = date)

# PORTFOLIO RETURNS ----

# Weights:
w_all_weather <- c(0.3, 0.55, 0.15)
w_4060        <- c(0.4, 0.6, 0)
w_etf         <- c(1, 0, 0)

# Portfolio returns:
p_all_weather_rebalance <- Return.portfolio(raw_xts, 
                                            weights = w_all_weather,
                                            rebalance_on = "quarters")

p_4060_rebalance <- Return.portfolio(raw_xts,
                                     weights = w_4060,
                                     rebalance_on = "quarters")

p_etf <- Return.portfolio(raw_xts, weights = w_etf)


# All portfolio rebalance:
all_portfolio_rebalance <- merge(p_all_weather_rebalance, p_4060_rebalance, p_etf)
colnames(all_portfolio_rebalance) <- c("All_Weather", "P_40.60", "P_ETF")


# CHART ----

# Cum returns
chart.CumReturns(all_portfolio_rebalance, wealth.index = TRUE,
                 plot.engine = "ggplot2")+
  theme_tq() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các danh mục", subtitle ="Tái cân bằng mỗi quý", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


# 2020
chart.CumReturns(all_portfolio_rebalance["2019-12/2020"], wealth.index = FALSE,
                 geometric = TRUE,
                 plot.engine = "ggplot2")+
  theme_tq() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các danh mục từ \u0111ầu n\u0103m 2020", subtitle ="Tái cân bằng mỗi quý", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(-0.4, 0.11)) 

# Drawdowns
chart.Drawdown(all_portfolio_rebalance, geometric = TRUE, plot.engine = "ggplot2") +
  theme_tq() + theme2 + 
  labs(title = "Tỷ lệ sụt giảm của các danh mục", y = "", color = "Danh mục") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent, limits = c(-0.58, 0))


################################## TIDYQuant ###################################

# Function: ----

calculate_portfolio_returns <- function(x, weights){
  x %>% 
    tq_portfolio(assets_col = assets,
                 returns_col = returns,
                 weights = weights,
                 col_rename = "Ra", rebalance_on = "quarters")
}


# Calculate portfolio monthly returns Tidyquant: ----
stock_returns_monthly <- raw %>% 
  gather(assets, returns, -date)


portfolio_all_weather <- calculate_portfolio_returns(stock_returns_monthly, w_all_weather)
portfolio_4060        <- calculate_portfolio_returns(stock_returns_monthly, w_4060)
portfolio_etf         <- calculate_portfolio_returns(stock_returns_monthly, w_etf)


# PORTFOLIO ANNUALL RETURNS: ----

portfolio_yearly_returns <- portfolio_all_weather %>% 
  
  # Merge:
  left_join(portfolio_4060, by = "date") %>% 
  left_join(portfolio_etf, by = "date") %>% 
  rename(All_Weather = Ra.x,
         P_40.60 = Ra.y,
         P_ETF = Ra) %>% 
  
  # Wide to long format:
  gather(asset, returns, -date) %>% 
  
  mutate(date = year(date)) %>% 
  group_by(date, asset) %>% 
  summarise(returns = sum(returns)) %>% 
  ungroup() %>% 
  mutate(returns_chr = scales::percent(returns, accuracy = 0.1L))
  

# ANNUAL RETURNS CHART:  
portfolio_yearly_returns %>% 
  ggplot(aes(x = date, y = returns, fill = asset)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = returns_chr), position = position_dodge(width = 0.9), stat = "identity", size = 3.2, vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2015:2020)) +
  theme_classic() + theme2 +
  labs(title = "Tỷ suất sinh lợi thực mỗi n\u0103m của các danh mục", 
       subtitle ="\u0110ã \u0111iều chỉnh lạm phát", 
       caption = "Các danh mục \u0111ược \u0111ầu t\u01B0 vào tháng 09/2015",
       y = "Lợi nhuận", x = "",
       fill = "Danh mục") +
  theme_tq() + theme2