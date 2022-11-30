# Packages ####
require(tidyverse)
require(data.table)
require(lubridate)
require(ggtext) #element_markdown() - Improved text rendering for ggplot2

require(showtext) #Custom font
font_add_google("STIX Two Text", "stix")
showtext_auto()

# 1. Themes ####
theme <- theme(
  text = element_text(family = "stix", size = 15),
  
  title = element_text(color = "#8b0000", size = 20),
  
  plot.title = element_text(hjust = 0.5),
  
  axis.text.y = element_text(face = "bold"),
  
  strip.text = element_text(face = "bold", size = 14),
  
  plot.subtitle = element_text(hjust = 0.5, size = 16),
  
  legend.position = "none", # No Legends
  
  plot.caption = element_text(size = 14, color = "black", face = "italic"))  



#Element mark down ggtext:   # <span style = 'color:#8b0000;'>text</span>
theme2 <- theme(
  text = element_text(family = "stix", size = 15),
  
  title = element_text(color = "#8b0000", size = 21),
  
  plot.title = element_markdown(hjust = 0.5, margin=margin(0,0,10,0)),
  
  axis.text.y = element_text(face = "bold"),
  
  strip.text = element_text(face = "bold", size = 14),
  
  plot.subtitle = element_markdown(size = 15, color = "#4C4A48", lineheight= 1.2),
  
  plot.caption = element_text(size = 14, color = "black", face = "italic"),
  
  legend.key.size = unit(1, 'cm'),
  legend.text = element_text(size = 16))




# 2. Function lay gia co phieu ####
# Lay tu cophieu68: https://www.cophieu68.vn/export/metastock_all.php
# Update: 06/2022 - phat hien data cophieu68 bi sai sot qua nhieu nen ngung su dung
# 
# get_data_co_phieu <- function(ticker = "VNM", all = FALSE){
#   
#   raw_data <- fread("01_data/amibroker_all_data.txt", 
#                     select = c('<DTYYYYMMDD>', '<Ticker>', '<Close>'))
#   
#   setnames(raw_data, c("date", "symbol", "price"))
#   
#   # Convert number to date
#   raw_data[, date := lubridate::ymd(date)]
# 
#   
#   # IF ELSE:
#   # Lua chon tat ca data hoac co phieu bat ky
#   
#   # Full stock data:
#   if(all == TRUE){
#     
#     data_tbl <- raw_data %>% 
#       as_tibble()
#     
#     assign("data_tbl", data_tbl, envir = .GlobalEnv)  
#     
#   # Be specific:
#   } else {
#     
#     data_symbol <- raw_data[ticker, on = "symbol"] %>% 
#       as_tibble()
#     
#     assign(str_glue("data_{ticker}"), data_symbol, envir = .GlobalEnv)
#     
#   }
#   
# }

# 3. Function import file investing.com ####

get_data_investing <- function(data_dir, format = "%b %d, %Y"){
  read_csv(data_dir,
           col_types = cols(Date = col_date(format = format))) %>% 
    select(Date, Price) %>% 
    arrange(Date) %>% 
    janitor::clean_names(.)
}


# 4. Head Tail data ####

head.tail <- function(data){
  print(head(data, 5))
  print(tail(data, 5))
}