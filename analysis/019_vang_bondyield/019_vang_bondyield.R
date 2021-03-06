# 1. Import Libraries ----
library(tidyverse)
library(tidyquant)
library(timetk)

# Requires:
source("header.R") # Theme

# Import

treasury_10_y <- tq_get("DGS10", get = "economic.data", from = "1980-01-01") %>%
  na.locf() %>% 
  spread(symbol, price)

tips_10_y <- tq_get("DFII10", get = "economic.data", from = "1980-01-01") %>%
  na.locf() %>% 
  spread(symbol, price)

sp_500 <- tq_get("SP500", get = "economic.data", from = "2000-01-01") %>% 
  na.locf() %>% 
  spread(symbol, price) %>% 
  slice(-n())

bitcoin <- tq_get("CBBTCUSD", get = "economic.data", from = "2000-01-01") %>% 
  na.locf() %>% 
  spread(symbol, price) %>% 
  rename(BITCOIN = CBBTCUSD) %>% 
  slice(1:(n()-2))

xau_tbl <- get_data_investing("01_data/XAU_USD_2000_2020.csv") %>% rename(XAU = price)

dxy_tbl <- get_data_investing("01_data/DXY_2000_2020.csv") %>% rename(DXY = price)


get_data_co_phieu()
vnindex <- data_tbl %>% 
  filter(symbol == "^VNINDEX") %>% 
  spread(symbol, price) %>% 
  rename(VNINDEX = `^VNINDEX`) %>% 
  filter(date >= "2010-01-01" & date <= "2021-02-25")


# Plot:

treasury_10_y %>% 
  left_join(tips_10_y, by = "date") %>% 
  gather(symbol, price, -date) %>% 
  filter(date >= "1980-01-01" & symbol == "DGS10" ) %>% 
  
  ggplot(aes(x = date, y = price)) +
  
  geom_line(color = "firebrick4") +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 7)) + theme_tq() + theme +
  
  geom_hline(yintercept =  0) +
  
  labs(x = "",
       y = "Phần tr\u0103m",
       title = "Lợi tức trái phiếu chính phủ Mỹ kỳ hạn 10 n\u0103m. 1980 - 2021",
       caption = "Thời \u0111iểm từ tháng 01/1980 \u0111ến cuối tháng 02/2021\nNguồn: FRED")


# Vang:

xau_tbl %>% 
  left_join(tips_10_y, by = "date") %>% 
  rename(real_yield = DFII10) %>% 
  gather(symbol, price, -date) %>% 
  mutate(year = year(date)) %>% 

  filter(date >= "2020-01-01") %>% 
  plot_time_series(date, price,
                   .facet_ncol = 2, .color_var = symbol, .facet_vars = c(symbol, year),
                   .facet_scales = "free",
                   
                   .smooth = FALSE,
                   
                   .plotly_slider = FALSE,
                   .title = "Biến \u0111ộng giá vàng và lợi tức TIPS kỳ hạn 10 n\u0103m. 2020 - 2021",
                   
                   .legend_show = FALSE,
                   .interactive = FALSE)
  

# SP 500:

sp_500 %>% 
  left_join(tips_10_y, by = "date") %>% 
  rename(real_yield = DFII10) %>% 
  gather(symbol, price, -date) %>% 
  mutate(year = year(date)) %>% 
  
  filter(date >= "2020-01-01") %>% 
  plot_time_series(date, price,
                   .facet_ncol = 2, .color_var = symbol, .facet_vars = c(symbol, year),
                   .facet_scales = "free",
                   
                   .smooth = FALSE,
                   
                   .plotly_slider = FALSE,
                   .title = "Biến \u0111ộng SP500 và lợi tức TIPS kỳ hạn 10 n\u0103m. 2020 - 2021",
                   
                   .legend_show = FALSE,
                   .interactive = FALSE)

# Bitcoin:
bitcoin %>% 
  left_join(tips_10_y, by = "date") %>% 
  na.locf() %>% 
  rename(real_yield = DFII10) %>% 
  gather(symbol, price, -date) %>% 
  mutate(year = year(date)) %>% 
  
  filter(date >= "2020-01-01") %>% 
  plot_time_series(date, price,
                   .facet_ncol = 2, .color_var = symbol, .facet_vars = c(symbol, year),
                   .facet_scales = "free",
                   
                   .smooth = FALSE,
                   
                   .plotly_slider = FALSE,
                   .title = "Biến \u0111ộng giá BITCOIN và lợi tức TIPS kỳ hạn 10 n\u0103m. 2020 - 2021",
                   
                   .legend_show = FALSE,
                   .interactive = FALSE)

# VNINDEX

vnindex %>% 
  left_join(tips_10_y, by = "date") %>% 
  na.locf() %>% 
  rename(real_yield = DFII10) %>% 
  gather(symbol, price, -date) %>% 
  mutate(year = year(date)) %>% 
  
  filter(date >= "2020-01-01") %>% 
  plot_time_series(date, price,
                   .facet_ncol = 2, .color_var = symbol, .facet_vars = c(symbol, year),
                   .facet_scales = "free",
                   
                   .smooth = FALSE,
                   
                   .plotly_slider = FALSE,
                   .title = "Biến \u0111ộng VNINDEX và lợi tức TIPS kỳ hạn 10 n\u0103m. 2020 - 2021",
                   
                   .legend_show = FALSE,
                   .interactive = FALSE)
  

