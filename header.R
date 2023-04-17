# Packages ####
require(tidyverse) # Tidyverse 2.0 da bao gom lubridate
require(data.table)
require(ggtext) #element_markdown() - Improved text rendering for ggplot2

require(showtext) #Custom font
font_add_google("STIX Two Text", "stix")
showtext_auto()

# 1. Themes ####
theme <- theme(
  text = element_text(family = "stix", size = 15),
  
  title = element_text(color = "#8b0000", size = 21),
  
  plot.title = element_text(hjust = 0.5, margin=margin(0,0,10,0)),
  
  axis.text.y = element_text(face = "bold"),
  
  strip.text = element_text(face = "bold", size = 14),
  
  plot.subtitle = element_markdown(size = 16, color = "#4C4A48", lineheight= 1.2),
  
  plot.caption = element_text(size = 14, color = "black", face = "italic"),
  
  legend.position = "none") # No Legends



#Element mark down ggtext:   # <span style = 'color:#8b0000;'>text</span>
theme2 <- theme(
  text = element_text(family = "stix", size = 15),
  
  title = element_text(color = "#8b0000", size = 21),
  
  plot.title = element_markdown(hjust = 0.5, margin=margin(0,0,10,0)),
  
  axis.text.y = element_text(face = "bold"),
  
  strip.text = element_text(face = "bold", size = 14),
  
  plot.subtitle = element_markdown(size = 16, color = "#4C4A48", lineheight= 1.2),
  
  plot.caption = element_text(size = 14, color = "black", face = "italic"),
  
  legend.key.size = unit(1, 'cm'),
  legend.text = element_text(size = 16),
  legend.title = element_blank())


# 2. Head Tail data ####

head.tail <- function(data){
  print(head(data, 5))
  print(tail(data, 5))
}


# 3. Function import prices from investing.com ####

get_data_investing <- function(data_dir, format = "%b %d, %Y"){
  read_csv(data_dir,
           col_types = cols(Date = col_date(format = format))) %>% 
    select(Date, Price) %>% 
    arrange(Date) %>% 
    janitor::clean_names(.)
}


# 4. Function import prices from tradingview.com ####

get_data_tradingview <- function(data_dir){
  
  read_csv(data_dir) %>% 
    select(time, contains("close")) %>% 
    mutate(date = as.Date(time)) %>%
    janitor::clean_names(.) %>% 
    rename_with(~str_replace(., "close", "price")) %>% 
    select(date, contains("price"))
  
}

# 5. Function import prices from vnstock
# Url: https://github.com/thinh-vu/vnstock
get_data_vnstock <- function(data_dir){
  
  read_csv(data_dir) %>% 
    select(date  = TradingDate,
           price = Close)
  
}
  
