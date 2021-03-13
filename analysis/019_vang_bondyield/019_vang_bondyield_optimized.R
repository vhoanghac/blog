# Import Packages
library(tidyverse)
library(tidyquant)
library(timetk)

# Requires:
source("header.R") # Theme

# Import du lieu tu FRED:
tick = c("DGS10", "DFII10", "SP500", "CBBTCUSD")

for (i in tick){
  data <- tq_get(i, 
                 get  = "economic.data",
                 from = "2000-01-01") %>% 
    spread(symbol, price) %>% 
    na.locf()
  
  assign(i, data, envir = .GlobalEnv)
}

# Import Gia vang, DXY tu investing.com:
xau_tbl <- get_data_investing("01_data/XAU_USD_2000_2020.csv") %>% rename(XAU = price)

dxy_tbl <- get_data_investing("01_data/DXY_2000_2020.csv") %>% rename(DXY = price)

# Import VNINDEX:
get_data_co_phieu(all = TRUE)

vnindex <- data_tbl %>% 
  filter(symbol == "^VNINDEX") %>% 
  spread(symbol, price) %>% 
  rename(VNINDEX = `^VNINDEX`) %>% 
  filter(date >= "2010-01-01")


# TIDY: (Cap nhat yield moi nhat tai ngay 12/03/2021)
treasury_10_y <- DGS10 %>%
  add_row(date = as.Date("2021-03-12"),
          DGS10 = 1.635)

tips_10_y <- DFII10 %>% 
  add_row(date = as.Date("2021-03-12"),
          DFII10 = -0.643)

# Plot:
treasury_10_y %>% 
  left_join(tips_10_y, by = "date") %>% 
  gather(symbol, price, -date) %>% 
  
  # Filter date tu 1980:
  filter(date >= "1980-01-01" & symbol == "DGS10" ) %>% 
  
  ggplot(aes(x = date, y = price)) +
  geom_line(color = "firebrick4") +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 7)) + theme_tq() + theme +
  geom_hline(yintercept =  0) +
  
  labs(x = "",
       y = "Phần tr\u0103m",
       title = "Lợi tức trái phiếu chính phủ Mỹ kỳ hạn 10 n\u0103m. 1980 - 2021",
       caption = "Thời \u0111iểm từ tháng 01/1980 \u0111ến cuối tháng 02/2021\nNguồn: FRED")



# FUNCTION PLOT DU LIEU:
plot_du_lieu <- function(x, y, from = "2017-01-01", to = as.character(lubridate::today()), n_col = 5){
  
  data <- x %>% 
    left_join(y, by = "date") %>% 
    na.locf() %>% 
    rename_all(function(h) case_when(h == "DFII10" ~ "real_yield",
                                     h == "DGS10" ~ "Bond Yield (10Y)",
                                     h == "CBBTCUSD" ~ "Bitcoin",
                                     TRUE ~ as.character(h))) %>% 
    
    gather(symbol, price, -date) %>% 
    mutate(year = year(date))
  
  name <- data %>% distinct(symbol) %>% pull()
  
  plot <- data %>%     
    filter(date >= from & date <= to) %>% 
    
    plot_time_series(date, price,
                     .facet_ncol = n_col,
                     .color_var  = symbol,
                     .facet_vars = c(symbol, year),
                     .facet_scales = "free",
                     .smooth = FALSE,
                     .plotly_slider = FALSE,
                     .legend_show = FALSE,
                     .interactive = FALSE,
                     .title = str_glue("Biến \u0111ộng {name[1]} và {name[2]} giai \u0111oạn {year(time)} - {data$year %>% last()}"))
  return(plot)
  
}

# Vang:
plot_du_lieu(x     = xau_tbl, 
             y     = tips_10_y, 
             from  = "2020-01-01", 
             n_col = 2)


# SP 500 tu 2018 -> last observation in 2021.
plot_du_lieu(x     = SP500, 
             y     = treasury_10_y,
             from  = "2018-01-01", 
             n_col = 2)

# Bitcoin tu 2018 -> 2019
plot_du_lieu(x     = CBBTCUSD,
             y     = tips_10_y,
             from  = "2018-01-01", 
             to    = "2019-12-31",
             n_col = 2)

# VNINDEX tu 2017 -> 2021. 5 cot.
plot_du_lieu(x     = vnindex,
             y     = tips_10_y,
             from  = "2017-01-01",
             n_col = 5)
