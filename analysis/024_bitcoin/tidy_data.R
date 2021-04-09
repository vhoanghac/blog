# 1. Packages

library(tidyverse)
library(tidyquant)

source("header.R") 


# 2. Import data #### #### #### #### #### 

# Ty gia USD VND
usd_vnd <- get_data_investing("025_bitcoin_port/data/USD_VND.csv")

# BITCOIN:
btc_usd_tbl <- tq_get("CBBTCUSD", 
       get  = "economic.data",
       from = "2015-09-09",
       to = "2021-03-31") %>% 
  spread(symbol, price) %>% 
  na.locf()

btc_vnd_tbl <- btc_usd_tbl %>% 
  full_join(usd_vnd, by = "date") %>%
  fill(price) %>% 
  mutate(CBBTCUSD = CBBTCUSD * price) %>% 
  select(date, BITCOIN = CBBTCUSD)

# Data co phieu:
full_data <- get_data_co_phieu(all = TRUE)

ticker <- c("E1VFVN30", "VNM", "VIC", "HPG", "VCB")

stock_tbl <- full_data %>% 
  filter(symbol %in% ticker) %>% 
  group_by(symbol) %>% 
  filter(date >= "2015-09-09" & date <= "2021-03-31") %>% 
  mutate(price = price * 1000) %>% 
  ungroup() %>% 
  spread(symbol, price)

# VN30: 
vn30_tbl <- stock_tbl %>% 
  select(date, E1VFVN30)

# TCBF:
tcbf_tbl <- read_csv("025_bitcoin_port/data/TCBF.csv")


# Merge data #### #### #### #### #### 
tidied_data <- btc_vnd_tbl %>% 
  full_join(vn30_tbl, by = "date") %>% 
  full_join(tcbf_tbl, by = "date") %>% 
  fill(c(E1VFVN30, TCBF))

tidied_full_data <- btc_vnd_tbl %>% 
  full_join(stock_tbl, by = "date") %>% 
  full_join(tcbf_tbl, by = "date") %>% 
  fill(c(E1VFVN30:TCBF))
