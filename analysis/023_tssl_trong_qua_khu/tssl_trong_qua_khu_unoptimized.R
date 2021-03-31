# 1. Packages
library(tidyverse) 
library(lubridate) #Date, time
library(tidyquant) #Theme, calculate returns
library(ggrepel) #Label
library(readxl) # Export, import
library(ggsci) # Theme
library(timetk)

source("header.R")
source("023_chasing_performance/tidy_data.R")


# 

funds_tbl <- full_data_cleaned

# Funds returns:

funds_r_tbl <- funds_tbl %>% 
  gather(symbol, price, -date) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "returns") %>% 
  ungroup()


# Last 365 days (Nam truoc)
funds_one_year_tbl <- funds_tbl %>% 
  slice(tail(row_number(), 368))


funds_one_year_tbl %>% 
  select(-VNINDEX) %>% 
  gather(symbol, price, -date) %>% 
  mutate(symbol = symbol %>% as_factor()) %>% 
  group_by(symbol) %>% 
  
  tq_transmute(select = price, 
               mutate_fun = periodReturn,
               period = "daily", 
               type = "log",
               col_rename = "returns") %>% 
  
  mutate(growth = exp(cumsum(returns))) %>% 
  
  mutate(label = if_else(date == max(date), as.character(symbol), NA_character_ )) %>% ungroup() %>% 
  
  ggplot(aes(x = date, y = growth, color = symbol)) +
  geom_line(size = 1.2) +
  coord_cartesian(xlim = c(min(funds_one_year_tbl$date), max(funds_one_year_tbl$date) + 50)) +
  geom_label_repel(aes(label = label),
                   fontface = "bold",
                   segment.color = NA,
                   nudge_x = 20, 
                   na.rm = TRUE, 
                   label.size = 0.8) +
  
  theme_tq() + theme + scale_color_npg() +
  
  geom_hline(yintercept = 1, linetype = 2) +
  
  labs(x = "Thời gian (ngày)",
       y = "T\u0103ng tr\u01b0ởng",
       title = str_glue("Thành quả \u0111ầu t\u01b0 sau một n\u0103m từ {first(funds_one_year_tbl$date)}"),
       caption = "Quãng thời gian từ 21/03/2020 \u0111ến 21/03/2021\nLog returns. 1.2 là lãi 20%")
  


### Momentum:

# Function:
portfolio_returns <- function(data,
                              name = "best"){
  
  n_stock <- data %>% 
    distinct(symbol) %>% count()
  
  w <- rep(as.numeric(1 / n_stock), n_stock)
  
  portfolio <- data %>% 
    tq_portfolio(assets_col = symbol,
                 returns_col = returns,
                 weights = w,
                 rebalance_on = "months",
                 geometric = FALSE,
                 col_rename = "returns") %>% 
    
    mutate(growth = exp(cumsum(returns)))
    
    names(portfolio) <- c("date", "returns", name)
    
    return(portfolio)
}



# Data Funds:
good_funds_one_year_tbl <- funds_r_tbl %>% 
  filter(symbol %in% c("VFMVF4", "VFMVF1", "SSISCA"))


bad_funds_one_year_tbl <- funds_r_tbl %>% 
  filter(symbol %in% c("MBVF", "TCEF", "BVFED"))



# Portfolio returns + Rebalance monthly:
good_funds_one_year_growth_monthly <- good_funds_one_year_tbl %>% 
  portfolio_returns(name = "Good")


bad_funds_one_year_growth_monthly <- bad_funds_one_year_tbl %>% 
  portfolio_returns(name = "Bad")

index_funds_growth_monthly <- funds_r_tbl %>% 
  portfolio_returns(name = "Index")

vnindex_growth_monthlty <- funds_r_tbl %>% 
  filter(symbol == "VNINDEX") %>% 
  portfolio_returns(name = "VNINDEX")



# PLOT:
good_funds_one_year_growth_monthly %>% 
  left_join(bad_funds_one_year_growth_monthly, by = "date") %>% 
  left_join(index_funds_growth_monthly , by = "date") %>% 
  left_join(vnindex_growth_monthlty, by = "date") %>% 
  select(date, Good, Bad, Index, VNINDEX) %>% 
  gather(symbol, price, -date) %>% 
  mutate(symbol = symbol %>% as_factor()) %>% 
  
  ggplot(aes(x = date, y = price, color = symbol)) + 
  geom_line(size = 1.2) +  theme_tq() + theme2 + 
  
  theme(legend.direction = "vertical",
        legend.text = element_text(face = "bold", size = 13)) +
  
  scale_color_manual(values = c("#00A087FF", "#E64B35FF", "#8491B4B2", "#744DA9"),
                     labels = c("Good (1 n\u0103m, tái cân bằng mỗi tháng)",
                                "Bad (1 n\u0103m, tái cân bằng mỗi tháng)",
                                "Index quỹ mở",
                                "VNINDEX")) +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "",
       y = "T\u0103ng tr\u01b0ởng",
       title = "Bonus 1: Thành quả hoạt \u0111ộng của các danh mục (1 n\u0103m)",
       color = "",
       caption = "Quãng thời gian từ 09/09/2015 \u0111ến 21/03/2021
       Sắp xếp danh mục dựa theo TSSL 1 n\u0103m gần nhất của các quỹ
       Log returns. 1.2 là lãi 20%")
  




###########################

# Last 2 years
funds_five_year_tbl <- funds_tbl %>% 
  slice(tail(row_number(), 1836))

funds_two_year_tbl %>% 
  gather(symbol, price, -date) %>% 
  mutate(symbol = symbol %>% as_factor()) %>% 
  group_by(symbol) %>% 
  
  tq_transmute(select = price, 
               mutate_fun = periodReturn,
               period = "daily", 
               type = "log",
               col_rename = "returns") %>% 
  
  mutate(growth = exp(cumsum(returns))) %>% 
  
  mutate(label = if_else(date == max(date), as.character(symbol), NA_character_ )) %>% ungroup() %>% 
  
  ggplot(aes(x = date, y = growth, color = symbol)) +
  geom_line(size = 1.2) +
  coord_cartesian(xlim = c(min(funds_five_year_tbl$date), max(funds_five_year_tbl$date) + 50)) +
  geom_label_repel(aes(label = label),
                   fontface = "bold",
                   segment.color = NA,
                   nudge_x = 20, 
                   na.rm = TRUE, 
                   label.size = 0.8) +
  
  theme_tq() + theme + scale_color_npg() + 
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  
  geom_hline(yintercept = 1, linetype = 2) +
  
  labs(x = "Thời gian (ngày)",
       y = "T\u0103ng tr\u01b0ởng",
       title = str_glue("Thành quả \u0111ầu t\u01b0 sau ba n\u0103m từ {first(funds_three_year_tbl$date)}"),
       caption = "Quãng thời gian từ 21/03/2018 \u0111ến 21/03/2021\nLog returns. 1.2 là lãi 20%")


## 

good_funds_three_years_tbl <- funds_r_tbl %>% 
  filter(symbol %in% c("MBVF", "VFMVF1", "VCBFBCF"))

bad_funds_three_years_tbl <- funds_r_tbl %>% 
  filter(symbol %in% c("BVFED", "TCEF", "VFMVF4"))

good_funds_three_years_growth <- good_funds_three_years_tbl  %>% 
  portfolio_returns(name = "Good")

bad_funds_three_years_growth <- bad_funds_three_years_tbl %>% 
  portfolio_returns(name = "Bad")


good_funds_three_years_growth %>% 
  left_join(bad_funds_three_years_growth, by = "date") %>% 
  left_join(index_funds_growth_monthly, by = "date") %>% 
  left_join(vnindex_growth_monthlty, by = "date") %>% 
  select(date, Good, Bad, Index, VNINDEX) %>% 
  gather(symbol, price, -date) %>% 
  mutate(symbol = symbol %>% as_factor()) %>% 
  ggplot(aes(x = date, y = price, color = symbol)) + 
  geom_line(size = 1) + theme_tq() + theme2 +
  
  theme(legend.direction = "vertical",
        legend.text = element_text(face = "bold", size = 13)) +
  
  scale_color_manual(values = c("#00A087FF", "#E64B35FF", "#8491B4B2", "#744DA9"),
                     labels = c("Good (3 n\u0103m, tái cân bằng mỗi tháng)",
                                "Bad (3 n\u0103m, tái cân bằng mỗi tháng)",
                                "Index quỹ mở",
                                "VNINDEX")) +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "",
       y = "T\u0103ng tr\u01b0ởng",
       title = "Bonus 3: Thành quả hoạt \u0111ộng của các danh mục (3 n\u0103m)",
       color = "",
       caption = "Quãng thời gian từ 09/09/2015 \u0111ến 21/03/2021
       Sắp xếp danh mục dựa theo TSSL 3 n\u0103m gần nhất của các quỹ
       Log returns. 1.2 là lãi 20%")
