#
require(tidyverse)
require(data.table)
require(lubridate)

# Themes:
theme <- theme(
  text = element_text(family = "serif", size = 15),
  
  title = element_text(color = "#8b0000", size = 17),
  
  plot.title = element_text(hjust = 0.5),
  
  axis.text.y = element_text(face = "bold"),
  
  strip.text = element_text(face = "bold", size = 13),
  
  plot.subtitle = element_text(hjust = 0.5, size = 16),
  legend.position = "none",
  plot.caption = element_text(size = 14, color = "black", face = "italic"))  # No Legends


theme2 <- theme(
  text = element_text(family = "serif", size = 15),
  
  title = element_text(color = "#8b0000", size = 17),
  
  plot.title = element_text(hjust = 0.5),
  
  axis.text.y = element_text(face = "bold"),
  
  strip.text = element_text(face = "bold", size = 13),
  
  
  plot.subtitle = element_text(hjust = 0.5, size = 16),
  plot.caption = element_text(size = 14, color = "black", face = "italic"))


# Function lay gia co phieu

get_data_co_phieu <- function(ticker = "VNM", all = FALSE){
  
  raw_data <- fread("01_data/amibroker_all_data.txt", 
                    select = c('<DTYYYYMMDD>', '<Ticker>', '<Close>'))
  
  setnames(raw_data, c("date", "symbol", "price"))
  
  # Convert number to date
  raw_data[, date := lubridate::ymd(date)]

  
  # IF ELSE:
  
  # Full stock data:
  if(all == TRUE){
    
    data_tbl <- raw_data %>% 
      as_tibble()
    
    assign("data_tbl", data_tbl, envir = .GlobalEnv)  
    
  # Be specific:
  } else {
    
    data_symbol <- raw_data[ticker, on = "symbol"] %>% 
      as_tibble()
    
    assign(str_glue("data_{ticker}"), data_symbol, envir = .GlobalEnv)
    
  }
  
}

# Function import file investing.com

get_data_investing <- function(data_dir){
  read_csv(data_dir,
           col_types = cols(Date = col_date(format = "%b %d, %Y"))) %>% 
    select(Date, Price) %>% 
    arrange(Date) %>% 
    janitor::clean_names(.)
}

# Head Tail data

head.tail <- function(data){
  print(head(data, 5))
  print(tail(data, 5))
}
