# 1. Packages 
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

# 2. Import data

bitcoin_raw <- read_csv("055_bitcoin_tet_nguyen_dan/01_data/01_raw/btcusd.csv")


# 3. Tidy

bitcoin_price <- bitcoin_raw %>% 
  mutate(date = as_date(date))

# 4. Table New Year Holiday

newyear_tbl <- tibble(year = 2014:2024, 
                  date = as_date(c("2014-01-31",
                                   "2015-02-19",  
                                   "2016-02-08",
                                   "2017-01-28",
                                   "2018-02-16",
                                   "2019-02-05",
                                   "2020-01-25",
                                   "2021-02-12",
                                   "2022-02-01",
                                   "2023-01-22",
                                   "2024-02-10")))


# 5. Function to calculate cumulative returns

calculate_cumulative_returns <- function(data, start_date, end_date, before = TRUE) {
  
  period_data <- data %>% 
    
    dplyr::filter(date >= start_date & date <= end_date) %>% 
    
    tq_mutate(select = price,
              mutate_fun = periodReturn,
              period = "daily",
              col_rename = "returns") %>% 
    
    mutate(growth = cumprod(1 + returns) - 1) %>% 
    
    mutate(days_from_start = if (before) {
      as.integer(date - end_date)
    } else {  
      as.integer(date - start_date)
    })
    
  return(period_data)
}


# 6. Analysis 30 days before Tet Holiday

# 6.1 list

performance_30d_before <- vector("list", length = nrow(newyear_tbl))

# 6.2 Calculate

for (i in 1:nrow(newyear_tbl)) {
  
  newyear_day <- newyear_tbl$date[i]
  
  start_date <- newyear_day - days(30)
  
  # Calculate cumulative returns
  year_data <- calculate_cumulative_returns(bitcoin_price, start_date, newyear_day)
  
  # Data labels
  year_data <- year_data %>% 
    mutate(year = newyear_tbl$year[i]) %>% 
    group_by(year) %>% 
    mutate(label_txt = if_else(date == max(date),
                               paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
    
    ungroup()
  
  # Store data in the list
  performance_30d_before[[i]] <- year_data
  
}


# 6.2 Combine results
before_30_tbl <- bind_rows(performance_30d_before)


# 6.3 Plot: 

before_30_tbl %>% 
  
  ggplot(aes(x = days_from_start, 
             y = growth, 
             color = factor(year))) +
  
  geom_line() + theme_tq() + theme + 
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  
  
  labs(title="Tăng trưởng của BITCOIN 30 ngày trước Tết",
       x = "Số ngày đếm ngược",
       y = "Tăng trưởng") + 
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 20,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7)


# 7. 30 days after Tet ############

# 7.1 list

performance_30d_after <- vector("list", length = nrow(newyear_tbl))

# 7.2 Calculate

for (i in 1:nrow(newyear_tbl)) {
  
  newyear_day <- newyear_tbl$date[i]
  
  end_date <- newyear_day + days(30)
  
  # Calculate cumulative returns
  year_data <- calculate_cumulative_returns(bitcoin_price, newyear_day, end_date, before = FALSE)
  
  # Data labels
  year_data <- year_data %>% 
    mutate(year = newyear_tbl$year[i]) %>% 
    group_by(year) %>% 
    mutate(label_txt = if_else(date == max(date),
                               paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
    
    ungroup()
  
  # Store data in the list
  performance_30d_after[[i]] <- year_data
  
}


# 7.3 Combine results
after_30_tbl <- bind_rows(performance_30d_after)


# 7.4 Plot: 

after_30_tbl %>% 
  
  ggplot(aes(x = days_from_start, 
             y = growth, 
             color = factor(year))) +
  
  geom_line() + theme_tq() + theme + 
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  
  
  labs(title="Tăng trưởng của BITCOIN 15 ngày sau Tết",
       x = "Số ngày",
       y = "Tăng trưởng") + 
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 20,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7)
