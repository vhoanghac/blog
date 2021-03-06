# 1. Packages

library(tidyverse)
library(lubridate)
library(tidyquant)
library(RColorBrewer)

source("header.R") # Import co phieu

# 2. Import data:

VFMVF1_raw <- read_rds("019_Juchi/data/VFMVF1.rds")

VFMVFB_raw <- read_rds("019_Juchi/data/VFMVFB.rds")

raw_data_tbl <- get_data_co_phieu(all = TRUE)


# 3 Tidy daily.

VFMVF1_daily_tbl <- VFMVF1_raw

DIAMOND_daily_tbl <- raw_data_tbl %>% 
  filter(symbol == "FUEVFVND") %>% 
  mutate(price = price * 1000)

E1VFVN30_daily_tbl <- raw_data_tbl %>% 
  filter(symbol == "E1VFVN30") %>% 
  mutate(price = price * 1000)

FINLEAD_daily_tbl <- raw_data_tbl %>% 
  filter(symbol == "FUESSVFL") %>% 
  mutate(price = price * 1000)

FUESSV50_daily_tbl <- raw_data_tbl %>% 
  filter(symbol == "FUESSV50") %>% 
  mutate(price = price * 1000)


# 4. FUNCTION Phan tich QUY:
phan_tich_01 <- function(time  = "2014-10-06",
                         n_quy = 2) {

VFMVF1 <- VFMVF1_daily_tbl %>% 
  filter(date >= time) %>% 
  pivot_wider(names_from = symbol,
              values_from = price,
              values_fn = mean)

E1VFVN30 <- E1VFVN30_daily_tbl %>% 
  filter(date >= time) %>% 
  pivot_wider(names_from = symbol,
              values_from = price)

FUESSV50 <- FUESSV50_daily_tbl %>% 
  filter(date >= time) %>% 
  pivot_wider(names_from = symbol,
              values_from = price)

data_2_quy <- VFMVF1 %>% 
  left_join(E1VFVN30, by= "date") %>% 
  na.locf() %>%
  gather(symbol, price, -date)

data_3_quy <- VFMVF1 %>% 
  left_join(E1VFVN30, by = "date") %>% 
  na.locf() %>%
  left_join(FUESSV50, by = "date") %>% 
  # mutate(FUESSV50 = na.locf(FUESSV50, na.rm = FALSE)) %>%
  # 
  mutate(FUESSV50 = case_when(is.na(FUESSV50) & date < "2017-10-24" ~ NA_real_,
                          TRUE ~ na.locf(FUESSV50, na.rm = FALSE))) %>%
  
  gather(symbol, price, -date) 


data_2_quy_daily <- data_2_quy %>% 
  group_by(symbol) %>% 
  
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  na.omit() %>% 
  mutate(cum_returns = cumprod(1 + returns)) %>%
  ungroup()


data_3_quy_daily <- data_3_quy %>% 
  group_by(symbol) %>% 
  
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  na.omit() %>% 
  mutate(cum_returns = cumprod(1 + returns)) %>%
  ungroup()


assign("phan_tich_01_2_quy_price", data_2_quy, envir = .GlobalEnv)
assign("phan_tich_01_3_quy_price", data_3_quy, envir = .GlobalEnv)
assign("phan_tich_01_2_quy_returns", data_2_quy_daily, envir = .GlobalEnv)
assign("phan_tich_01_3_quy_returns", data_3_quy_daily, envir = .GlobalEnv)

# PLOT:

if (n_quy == 2){
  
  plot_2_quy <- data_2_quy_daily %>% 
    ggplot(aes(x = date,
               y = cum_returns,
               color = symbol)) +
    geom_line() + theme_tq() + theme2 + 
    
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
    
    geom_smooth(method = "loess", span = 0.2, se = FALSE ) +
    
    labs(x = "",
         y = "Lợi nhuận",
         title = str_glue("Lợi nhuận \u0111ầu t\u01B0 từ {time}"),
         color = "Quỹ")
  
  return(plot_2_quy)
  
} else {
    
  plot_3_quy <- data_3_quy_daily %>% 
    ggplot(aes(x = date,
               y = cum_returns,
               color = symbol)) +
    geom_line() + theme_tq() + theme2 + 
    
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
    
    geom_smooth(method = "loess", span = 0.2, se = FALSE ) +
    
    labs(x = "",
         y = "Lợi nhuận",
         title = str_glue("Lợi nhuận \u0111ầu t\u01B0 từ {time}"),
         color = "Quỹ")
  
  return(plot_3_quy)
  }

}

##############################################
# 5. PHAN TICH 2 QUY VFMVF1 VA E1VFVN30: #####
##############################################

phan_tich_01("2018-01-01")

# 5.1 Phan tich TSSL moi nam:
phan_tich_01_2_quy_price %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly") %>% 
  
  mutate(year = date %>% lubridate::year()) %>% 
  
  mutate(returns_label = scales::percent(yearly.returns)) %>% 
  
  ggplot(aes(x = year,
             y = yearly.returns,
             fill = symbol)) +
  
  geom_bar(position = "dodge", stat = "identity") +
  
  geom_text(aes(label = returns_label), position = position_dodge(width = 0.9), stat = "identity", size = 3.2, vjust = -0.25) +
  
  scale_y_continuous(labels = scales::percent) +
  
  facet_wrap(~symbol) + theme_tq() + theme2 +
  
  labs(x = "",
       y = "TSSL (%)",
       title = str_glue("Tỷ suất sinh lợi mỗi n\u0103m của các quỹ khi \u0111ầu t\u01B0 từ {first(phan_tich_01_3_quy_price$date)}"),
       fill = "Quỹ")


# 5.2 DRAWDOWN CHART:
phan_tich_01_2_quy_returns %>% 
  select(-cum_returns) %>% 
  spread(symbol, returns) %>% 
  timetk::tk_xts(select = c("E1VFVN30", "VFMVF1"), date_var = date) %>% 
  chart.Drawdown(plot.engine = "ggplot2") + theme_tq() + theme2 +
  labs(title = str_glue("Mức sụt giảm từ \u0111ỉnh của các quỹ từ {first(phan_tich_01_3_quy_price$date)}"),
       color = "Quỹ") +
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
  
  

#6. PHAN TICH 3 QUY

phan_tich_01("2017-10-24", 3)

#6.1 TSSL MOI NAM:
phan_tich_01_3_quy_price %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly") %>% 
  
  mutate(year = date %>% lubridate::year()) %>% 
  
  mutate(returns_label = scales::percent(yearly.returns)) %>% 
  
  ggplot(aes(x = year,
             y = yearly.returns,
             fill = symbol)) +
  
  geom_bar(position = "dodge", stat = "identity") +
  
  geom_text(aes(label = returns_label), position = position_dodge(width = 0.9), stat = "identity", size = 3.2, vjust = -0.25) +
  
  scale_y_continuous(labels = scales::percent) +
  
  facet_wrap(~symbol) + theme_tq() + theme2 +
  
  labs(x = "",
       y = "TSSL (%)",
       title = str_glue("Tỷ suất sinh lợi mỗi n\u0103m của các quỹ khi \u0111ầu t\u01B0 từ {first(phan_tich_01_3_quy_price$date)}"),
       fill = "Quỹ")


# TABLE:
phan_tich_01_2_quy_returns %>% 
  group_by(symbol) %>% 
  tq_performance(Ra = returns,
                 performance_fun = table.DownsideRisk) %>% 
  t()


##################################################
# PHAN TICH 02:  
##################################################

VFMVF1_daily <- VFMVF1_daily_tbl %>% 
  pivot_wider(names_from = symbol,
              values_from = price,
              values_fn = mean)
  
DIAMOND_daily <- DIAMOND_daily_tbl %>% 
  pivot_wider(names_from = symbol,
              values_from = price)

FINLEAD_daily <- FINLEAD_daily_tbl %>% 
  pivot_wider(names_from = symbol,
              values_from = price)

FUESSV50_daily <- FUESSV50_daily_tbl %>% 
  pivot_wider(names_from = symbol,
              values_from = price)


data_4_quy_tbl <- VFMVF1_daily %>% 
  filter(date >= "2020-06-24") %>% 
  left_join(DIAMOND_daily, by = "date") %>% 
  left_join(FINLEAD_daily, by = "date") %>% 
  left_join(FUESSV50_daily, by = "date") %>% 
  na.locf() %>% 
  gather(symbol, price, -date)

data_4_quy_returns <- data_4_quy_tbl %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  ungroup()

data_4_quy_returns %>% 
  spread(symbol, returns) %>% 
  select(-date) %>% 
  cor() %>% 
  corrplot::corrplot(method = "number",
                     order = "hclust", 
                     
                     number.digits = 2,
                     col = colorRampPalette(c("blue", "cornflowerblue", "white", "forestgreen", "green", "green2","seagreen2", "tomato", "red"))(100))


###############################################
############ PORTOFLIO'S return ###############
###############################################

weight_1 = c(1, 0, 0, 0)

weights <- c(1, 0, 0, 0,
             0, 1, 0, 0,
             0, 0, 1, 0,
             0, 0, 0, 1)

etf = c("VFMVF1", "FUEVFVND", "FUESSVFL", "FUESSV50")

weights_table <- tibble(etf) %>% 
  tq_repeat_df(n = 4) %>% 
  bind_cols(tibble(weights)) %>% 
  group_by(portfolio)


data_4_quy_returns_multi <- data_4_quy_returns %>% 
  tq_repeat_df(n = 4)

data_4_quy_returns_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_table,
               col_rename = "growth",
               wealth.index = TRUE) %>% 
  
  ggplot(aes(x = date,
             y = growth,
             color = factor(portfolio))) +
  geom_line() + 
  scale_color_discrete(name = "Portfolio",
                       labels = etf) + theme_tq() + theme2 +
  
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  
  labs(x = "",
       y = "Lợi nhuận",
       title = "Lợi nhuận \u0111ầu t\u01B0 vào 4 quỹ từ tháng 06/2020")


data_4_quy_returns_multi %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_table,
               col_rename = "growth",
               wealth.index = TRUE) %>% 
  mutate(growth = growth * 1000) %>% 
  tq_mutate(select = growth,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "returns") %>% 
  
  tq_performance(Ra = returns,
                 performance_fun = table.Variability) %>% t()



### PORTFOLIO PLAN #######
weights_portfolio <- c(0.625, 0.375, 0, 0,
                       0.625, 0, 0.375, 0)

portfolio_label = c("50_VFMVF1 & 30_DIA",
        "50_VFMVF1 & 30_FIN")

weights_portfolio_table <- tibble(etf) %>% 
  tq_repeat_df(n = 2) %>% 
  bind_cols(tibble(weights_portfolio)) %>% 
  group_by(portfolio)

data_4_quy_returns_portfolio <- data_4_quy_returns %>% 
  tq_repeat_df(n = 2)

data_4_quy_returns_portfolio %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_portfolio_table,
               col_rename = "growth",
               wealth.index = TRUE) %>% 
  mutate(growth = growth * 1000) %>% 
  
  ggplot(aes(x = date,
             y = growth,
             color = factor(portfolio))) +
  geom_line() + 
  scale_color_discrete(name = "Portfolio",
                       labels = portfolio_label) + theme_tq() + theme2


# Table:
data_4_quy_returns_portfolio %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_portfolio_table,
               col_rename = "growth",
               wealth.index = TRUE) %>% 
  mutate(growth = growth * 1000) %>% 
  
  mutate(portfolio = case_when(portfolio == 1 ~ "Diamond",
                               portfolio == 2 ~ "Finlead")) %>% 
  tq_mutate(select = growth,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "returns") %>% 
  
  tq_performance(Ra = returns,
                 performance_fun = table.Variability) %>% t()