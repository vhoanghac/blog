# 1. Packages #### ####
source("header.R") #Tidyverse, data.table, lubridate
library(tidytable)
library(tidyquant)
library(timetk) # Summarise time

# 2. Import data #### ####

# Get the CSVs' paths:
paths <- fs::dir_ls("030_etf_mfunds_2022/01_data/tidied")

# Get names of the CSVs (to replace the names later)
datanames <-  gsub("\\.csv$","", list.files(path = "030_etf_mfunds_2022/01_data/tidied",
                                            pattern = "\\.csv$")) %>% 
  tolower()

# Import the CSVs into one list:
list <- paths %>%
  map(function(path) {
    fread(path)
  })

# Add rows to some equity mutual funds that have recorded data in the first week of the year
# BVPF. First observation @ 2017-01-06 
list$`030_etf_mfunds_2022/01_data/tidied/BVPF.csv` <- list$`030_etf_mfunds_2022/01_data/tidied/BVPF.csv` %>% 
  add_row(date = as_date("2016-12-31"), price = 10000)
  
# DFVNCAF. First observation @ 2017-01-07
list$`030_etf_mfunds_2022/01_data/tidied/DFVNCAF.csv` <- list$`030_etf_mfunds_2022/01_data/tidied/DFVNCAF.csv` %>% 
  add_row(date = as_date("2018-12-31"), price = 10000)

# VNDAF. First observation @ 2018-01-12
list$`030_etf_mfunds_2022/01_data/tidied/VNDAF.csv` <- list$`030_etf_mfunds_2022/01_data/tidied/VNDAF.csv` %>% 
  add_row(date = as_date("2017-12-31"), price = 10000)

# Convert to annual data:
list_converted <- list %>% 
  set_names(datanames) %>% 
  
  lapply(function(x) {
    
    x %>%
      
      # Last price of each year
      summarise_by_time(.date_var = date,
                        .by       = "year",
                        price     = last(price),
                        .type     = "floor") %>%
      
      # Calculate annual returns. 
      # Leading = FALSE: Skip if it doesn't have enough data for 1 full year.
      tq_transmute(select     = price,
                   mutate_fun = periodReturn,
                   period     = "yearly",
                   col_rename = "returns",
                   leading    = FALSE)
    
  })


# Add other equity mutual funds
# VEF: https://www.dragoncapital.com/vef/
vef <- tibble(date = as_date(list_converted$dcds$date[12:18]),
              vef = as.double(c(0.1325, 0.1936, 0.4135, -0.0343, -0.0009, 0.1608, 0.5408)))


# 3. Analysis #### ####
# Only show the annual returns of E1VFVN30 vs. other equity mutual funds. 
returns_tbl <- list_converted %>% 
  enframe.() %>% 
  unnest.(value) %>% 
  rename(symbol = name) %>% 
  pivot_wider(names_from = symbol, values_from = returns) %>% 
  filter.(date >= "2015-01-01" & date < "2022-01-01") %>% 
  
  # Add other equity mutual funds
  left_join.(vef, by = "date")

# Analysis
result_tbl <- returns_tbl %>%
  select.(-e1vfvn30) %>%
  
  # 1: If equity mutual funds beat the VN30 ETF
  # 0: If equity mutual funds failed to beat the VN30 ETF
  mutate_if(is.numeric, ~case_when.(. > returns_tbl$e1vfvn30 ~ 1,
                                       is.na(.) ~ NA_real_,
                                       TRUE ~ 0)) %>%
  select.(-date) %>%
  mutate.(
    
    # Total number of equity mutual funds that have better returns than ETF VN30
    n_win = rowSums(., na.rm = TRUE),
    
    # Total number of equity mutual funds outperformed by the ETF VN30
    n_loss = rowSums(. == 0, na.rm = TRUE),
    
    total_funds = n_win + n_loss,
    pct_etf_win = n_loss / total_funds,
    date = returns_tbl$date
  ) %>%
  select.(date, n_win, n_loss, total_funds, pct_etf_win) %>%
  mutate.(date = year(date)) %>% 
  mutate.(pct_label = scales::percent(pct_etf_win, accuracy = 1))

# Plot:
result_tbl %>% 
  ggplot(aes(x = date, y = pct_etf_win)) +
  geom_col() + theme_tq() + theme +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  geom_text(aes(label = pct_label), vjust = -0.5) +
  
  labs(x = "",
       y = "",
       title = "Tỷ lệ các quỹ cổ phiếu bị ETF VN30 đánh bại",
       caption = "Trong bài viết ETF VN30 là E1VFVN30")