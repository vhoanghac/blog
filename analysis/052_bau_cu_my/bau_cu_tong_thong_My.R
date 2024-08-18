# 1. Packages 
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

# 2. Import data

vnindex <- get_data_tradingview("052_bau_cu_my/01_data/01_raw/HOSE_DLY_VNINDEX, 1D.csv")


# 3. Function tìm Election Day
# Election Day: Vào ngày thứ ba đầu tiên của tháng.
get_election_day <- function(year) {
  if (year %% 4 == 0 && year >= 2000) {
    first_day_november <- as.Date(paste(year, "11", "01", sep = "-"))
    first_monday_november <- first_day_november + ((9 - lubridate::wday(first_day_november)) %% 7)
    first_tuesday_november <- first_monday_november + 1
    return(ymd(first_tuesday_november))
  } else {
    return(NA)
  }
}


# 4. Table: Election Day
year <- vnindex %>% 
  mutate(year = lubridate::year(date))

election_days <- sapply(unique(year$year), get_election_day)

election_tbl <- data.frame(
  year = unique(year$year),
  election_day = as.Date(election_days),
  end_of_year = as.Date(paste(unique(year$year), "12", "31", sep = "-"))
) %>% 
  filter(!is.na(election_day) & year < 2024)




# 5. Function to calculate cumulative returns

calculate_cumulative_returns <- function(data, start_date, end_date, before = TRUE) {
  
  period_data <- vnindex %>% 
    
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


# 5.1 list

performance_180d <- list()

# 5.2 Calculate

for (i in 1:nrow(election_tbl)) {
  
  election_day <- election_tbl$election_day[i]
  
  start_date <- election_day - days(180)
  
  year_data <- calculate_cumulative_returns(data, start_date, election_day)
  
  year_data <- year_data %>% 
    mutate(year = election_tbl$year[i]) %>% 
    group_by(year) %>% 
    mutate(label_txt = if_else(date == max(date),
                               paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
    
    ungroup()
  
  performance_180d[[i]] <- year_data
  
}


# 6. Analysis 180 days before Election day
before_180_tbl <- bind_rows(performance_180d)

# 7. Plot: 

before_180_tbl %>% 
  
  filter(year != 2000) %>% 
  
  ggplot(aes(x = days_from_start, 
             y = growth, 
             color = factor(year))) +
  
  geom_line() + theme_tq() + theme + 
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  
  
  labs(title="Tăng trưởng của VNINDEX 6 tháng trước bầu cử tổng thống Mỹ",
       x = "Số ngày đếm ngược",
       y = "Tăng trưởng",
       caption = "Ngày bầu cử: ngày thứ ba (Tuesday) sau ngày thứ hai (Monday) đầu tiên của tháng 11
       Nguồn dữ liệu: TradingView") + 
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 50,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7)



# Tăng trưởng trong Election Day ###########

performance_election_day <- list()

# 5.2 Calculate

for (i in 1:nrow(election_tbl)) {
  
  start_day <- election_tbl$election_day[i]
  
  end_day <- election_tbl$end_of_year[i]
  
  year_data <- calculate_cumulative_returns(data, start_day, end_day, before = FALSE)
  
  year_data <- year_data %>% 
    mutate(year = election_tbl$year[i]) %>% 
    group_by(year) %>% 
    mutate(label_txt = if_else(date == max(date),
                               paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
    ungroup()
  
  performance_election_day[[i]] <- year_data
  
}


# 6. Analysis 
election_day_tbl <- bind_rows(performance_election_day)

# 7. Plot: 

election_day_tbl %>% 
  
  ggplot(aes(x = days_from_start, 
             y = growth, 
             color = factor(year))) +
  
  geom_line() + theme_tq() + theme +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_vline(xintercept = 0, color = "red") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  
  labs(title="Tăng trưởng của VNINDEX từ ngày bầu cử tổng thống Mỹ đến hết năm",
       x = "Số ngày",
       y = "Tăng trưởng",
       caption = "Ngày bầu cử: ngày thứ ba (Tuesday) sau ngày thứ hai (Monday) đầu tiên của tháng 11
       Nguồn dữ liệu: TradingView")  +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 15,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7)





# Tăng trưởng 1 năm sau Election Day ###########

performance_1y <- list()

# Calculate

for (i in 1:nrow(election_tbl)) {
  
  start_day <- election_tbl$election_day[i]
  
  end_day <- election_tbl$election_day[i] + days(360)
  
  year_data <- calculate_cumulative_returns(data, start_day, end_day, before = FALSE)
  
  year_data <- year_data %>% 
    mutate(year = election_tbl$year[i]) %>% 
    group_by(year) %>% 
    mutate(label_txt = if_else(date == max(date),
                               paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
    ungroup()
  
  performance_1y[[i]] <- year_data
  
}


# 6. Analysis 
election_1y_tbl <- bind_rows(performance_1y)

election_1y_tbl %>% 
  filter(year == 2020) %>% head.tail()

# 7. Plot: 

election_1y_tbl %>% 
  
  ggplot(aes(x = days_from_start, 
             y = growth, 
             color = factor(year))) +
  
  geom_line() + theme_tq() + theme +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_vline(xintercept = 0, color = "red") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  
  labs(title="Tăng trưởng của VNINDEX 1 năm sau bầu cử tổng thống Mỹ",
       x = "Số ngày",
       y = "Tăng trưởng",
       caption = "Ngày bầu cử: ngày thứ ba (Tuesday) sau ngày thứ hai (Monday) đầu tiên của tháng 11
       Nguồn dữ liệu: TradingView")  +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 100,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7)
