# 1. Packages #######
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

# 2. Import data ######

# Giá vàng nhẫn
# Nguồn: https://bieudogiavang.vn/gia-vang-sjc
xau_raw <- read_csv("048_gia_vang_2024/01_data/raw/vang_nhan.csv")
vn30 <- get_data_vnstock("048_gia_vang_2024/01_data/raw/E1VFVN30.csv")
midcap <- get_data_vnstock("048_gia_vang_2024/01_data/raw/FUEDCMID.csv")



# 3. Tidy ######
# Giá vàng trong tập dữ liệu là giá dựa trên từng khung giờ của mỗi ngày.
# Tidy data bằng cách chỉ lấy giá đóng cửa

# VÀNG #
xau_tbl <- xau_raw %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  slice(n()) %>% 
  ungroup()


# Tidy: VN30 + MIDCAP. Lấp đầy các dữ liệu NA cho phù hợp với số ngày của tập dữ liệu vàng.
# Do thị trường chứng khoán đóng cửa thứ 7 và CN.

full_data_tidied <- xau_tbl %>% 
  full_join(vn30) %>% 
  full_join(midcap, by = "date") %>% 
  arrange(date) %>% 
  na.locf(na.rm = TRUE) %>% 
  rename("vn30" = "price.x",
         "midcap" = "price.y")


# Lọc ra các năm riêng biệt:
data_2024 <- full_data_tidied %>% 
  filter(date >= "2024-01-01")


data_2023 <- full_data_tidied


# 4. Analysis 2024 #######

growth_2024 <- data_2024 %>% 
  mutate(buy_price = case_when(date == min(date) ~ first(data_2024$sell_price), TRUE ~ buy_price)) %>% 
  select(date, xau = buy_price, vn30, midcap) %>% 
  pivot_longer(names_to = "symbol",
               values_to = "price", -date) %>% 
  group_by(symbol) %>% 
  tq_mutate(select = price,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1,
         label_txt = if_else(date == max(date),
                             paste(toupper(symbol), ":", scales::percent(growth, big.mark =".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_)) %>% 
  
  ungroup()




# Vàng:

# Plot giá vàng:
growth_2024 %>% 
  filter(symbol == "xau") %>% 
  ggplot(aes(x = date, y = price*1000)) +
  geom_line(linewidth = 0.9, 
            color = "orange") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  labs(x = "",
       y = "Giá",
       title = "Giá vàng nhẫn từ đầu năm 2024",
       caption = "Dữ liệu giá vàng: bieudogiavang.vn")


# Plot tăng trưởng:
growth_2024 %>% 
  filter(symbol == "xau") %>% 
  ggplot(aes(x = date, y = growth)) +
  geom_line(linewidth = 0.9, 
            color = "orange") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(x = "",
       y = "Tăng trưởng (%)",
       title = "Tăng trưởng giá vàng nhẫn từ đầu năm 2024",
       caption = "Dữ liệu giá vàng: bieudogiavang.vn")



# VN30:


growth_2024 %>% 
  filter(symbol == "vn30") %>% 
  ggplot(aes(x = date, y = growth)) +
  geom_line(linewidth = 0.9, 
            color = "orange") + theme_tq() + theme


# MIDCAP:


growth_2024 %>% 
  filter(symbol == "midcap") %>% 
  ggplot(aes(x = date, y = growth)) +
  geom_line() 



# 5. Analysis 2023 - 2024 ######


growth_2023 <- data_2023 %>% 
  mutate(buy_price = case_when(date == min(date) ~ first(data_2023$sell_price), TRUE ~ buy_price)) %>% 
  select(date, xau = buy_price, vn30, midcap) %>% 
  pivot_longer(names_to = "symbol",
               values_to = "price", -date) %>% 
  group_by(symbol) %>% 
  tq_mutate(select = price,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1,
         label_txt = if_else(date == max(date),
                             paste(toupper(symbol), ":", scales::percent(growth, big.mark =".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_)) %>% 
  
  ungroup()


# Vàng:

growth_2023 %>% 
  filter(symbol == "xau") %>% 
  ggplot(aes(x = date, y = growth)) +
  geom_line()



# VN30:

growth_2023 %>% 
  filter(symbol == "vn30") %>% 
  ggplot(aes(x = date, y = growth)) +
  geom_line()



# MIDCAP:

growth_2023 %>% 
  filter(symbol == "midcap") %>% 
  ggplot(aes(x = date, y = growth)) +
  geom_line()


# Combo 2024

growth_2024 %>% 
  ggplot(aes(x = date, y = growth, color = symbol)) +
  
  geom_line(linewidth = 0.9) + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_x_date(breaks = seq(as.Date("2024-01-01"), as.Date("2024-12-28"), by="1 month"), date_labels = "%b") +
  
  scale_color_manual(values = c("#1d3557", "#f94144", "orange")) +
  
  coord_cartesian(xlim = c(min(growth_2024$date), max(growth_2024$date) +10)) +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 60,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.3,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  labs(x = "",
       y = "Tăng trưởng (%)",
       title = "Tăng trưởng của ETF VN30, MIDCAP và vàng nhẫn từ đầu năm 2024",
       caption = "ETF VN30 là: E1VFVN30
       ETF MIDCAP là: FUEDCMID
       Dữ liệu giá vàng: bieudogiavang.vn
       Dữ liệu ETF: tổng hợp bằng vnstock")


# Combo 2023


growth_2023 %>% 
  ggplot(aes(x = date, y = growth, color = symbol)) +
  
  geom_line(linewidth = 0.9) + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_x_date(breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-28"), by="3 month"), date_labels = "%b-%y") +
  
  scale_color_manual(values = c("#1d3557", "#f94144", "orange")) +
  
  coord_cartesian(xlim = c(min(growth_2023$date), max(growth_2023$date) + 50)) +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 60,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.3,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  labs(x = "",
       y = "Tăng trưởng (%)",
       title = "Tăng trưởng của ETF VN30, MIDCAP và vàng nhẫn từ đầu năm 2023",
       caption = "ETF VN30 là: E1VFVN30
       ETF MIDCAP là: FUEDCMID
       Dữ liệu giá vàng: bieudogiavang.vn
       Dữ liệu ETF: tổng hợp bằng vnstock")
