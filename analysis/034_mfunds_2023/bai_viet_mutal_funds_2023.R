# 1. Packages #### ####
source("header.R") #Tidyverse, data.table, lubridate
library(tidyquant)
library(timetk) #summarise_by_time - Lựa chọn ngày đầu tiên trong tháng
library(tidytable)
library(ggrepel) #Labels cho chart

options(scipen = 999)

# 2. Import data #### ####
paths <- fs::dir_ls("034_mfunds_2023/01_data/raw/")

# Lấy tên file để sau này đặt tên cho dữ liệu
datanames <-  gsub("\\.csv$","", list.files(path = "034_mfunds_2023/01_data/raw/", pattern = "\\.csv$"))

# Import các CSV, gộp lại thành một list
list <- paths %>% 
  map(function(path){
    read_csv(path)
  })

# Lấy dữ liệu theo tháng và tính monthly returns
list_converted <- list %>% 
  
  # Đặt tên cho từng list riêng để sau này xuất ra dễ làm việc
  set_names(datanames) %>% 
  
  # Vì là list nên sử dụng lapply cho nhanh
  lapply(function(x){
    
    x %>%
      
      # First day of each month in the dataset
      summarise_by_time(.date_var = date,
                        .by       = "month",
                        price     = first(price),
                        .type     = "floor") %>% 
      
      # Calculate monthly returns
      tq_transmute(select     = price,
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "returns")
    
  }) 

# 3. Phân tích #### ####

# 3.1 Giai đoạn 2021

data_2021 <- list_converted %>% 
  
  # convert vectors thành data frames
  enframe() %>% 
  
  # Xuất dữ liệu từ list value ra thành date và price
  unnest(value) %>% 
  rename(symbol = name) %>% 
  group_by(symbol) %>% 
  
  # 2021-01-01 = 0 để tính đúng lợi nhuận tích lũy dựa theo cumprod()
  filter(date >= "2021-01-01" & date <= "2022-01-01") %>%
  mutate(returns = case_when(date == "2021-01-01" ~ 0,
                            TRUE ~ returns)) %>% 

  mutate(growth = cumprod(1 + returns) - 1,
         label_txt = if_else(date == max(date),
                             paste(symbol, ":", scales::percent(growth, big.mark =".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_)) %>% 
  ungroup()
  
# 3.2 Giai đoạn 2022

data_2022 <- list_converted %>% 
  enframe() %>% 
  unnest(value) %>% 
  rename(symbol = name) %>% 
  group_by(symbol) %>% 
  filter(date >= "2022-01-01" & date <= "2023-01-01") %>%
  mutate(returns = case_when(date == "2022-01-01" ~ 0,
                             TRUE ~ returns)) %>% 
  mutate(growth = cumprod(1 + returns) - 1,
         label_txt = if_else(date == max(date),
                             paste(symbol, ":", scales::percent(growth, big.mark =".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_)) %>% 
  ungroup()

# 4. PLOT #### ####

data_2022 %>% 
  ggplot(aes(x = date, y = growth, color = symbol)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5, color = "firebrick4") +
  
  theme_tq() + theme +
  
  coord_cartesian(xlim = c(min(data_2022$date), max(data_2022$date) + 200)) +
  
  geom_label_repel(aes(label = label_txt),
                  force_pull = 0,
                  nudge_x = 60,
                  direction ="y",
                  max.overlaps = 10,
                  segment.size = 0.3,
                  segment.linetype = 2,
                  hjust = 0,
                  size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của các quỹ mở cổ phiếu năm 2022",
       caption = "Nguồn dữ liệu: Fmarket.vn")
