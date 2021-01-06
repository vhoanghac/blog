# Import packages ----
library(readxl) # Import Excel files
library(fs)

library(tidyverse) # Tidy data
library(lubridate) # Date
library(purrr) 

library(PerformanceAnalytics) # Skewness

library(tidyquant) # Theme tq

# Header ----
source(file.path(paste0(getwd(),"/header.R")))

# Import data: ----

file_paths <- fs::dir_ls("IndexNganh/data/")

data_nested <- file_paths %>% 
  as_tibble() %>%
  mutate(data = value %>% map(read_csv)) %>% 
  
  # Change name in column value
  mutate(value = c("Bất Động Sản",
                   "Công Nghệ",
                   "Công Nghiệp",
                   "Chứng Khoán",
                   "Dầu Khí",
                   "Dịch Vụ Hạ Tầng",
                   "Du Lịch Giải Trí",
                   "Ngân Hàng",
                   "Sản Xuất Thực Phẩm",
                   "Vật Liệu C\u01a1 Bản",
                   "Y Tế"))

# PURR ----

# Creating Function:
calculate_return <- function(data){
  
  data %>% 
    select(-open, -high, -low, - vol) %>% 
    separate(date, c("year", "month"), sep = "-", remove = FALSE) %>% 
    mutate(daily_return = log(close) - log(lag(close))) %>% 
    na.omit() %>% 
    mutate(avg = mean(daily_return),
           sd  = sd(daily_return),
           skew = skewness(daily_return))
}

# PURR map()

# data_completed <- data_nested %>% 
#   mutate(data_completed = data %>% map(calculate_return))  


#Data 2020:  ----
data_2020_tbl <- data_nested %>% 
  unnest() %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(value) %>% 
  calculate_return() %>% 
  ungroup()

# Full chart indexes
data_2020_tbl %>% 
  ggplot(aes(x = date,
             y = close,
             color = value)) +
  
  geom_line(show.legend = FALSE) + 
  geom_smooth(method = "loess", span = 0.2, color ="grey30", se = FALSE, size = 1) +
  
  facet_wrap(~ value, scales = "free_y", ncol = 2) + theme_tq() + theme +
  
  theme(strip.text = element_text(face = "bold",
                                  size = 13)) +
  
  labs(x = "",
       y = "Chỉ số",
       title = "Chỉ số 11 ngành nghề n\u0103m 2020",
       caption = "Nguồn: FireAnt\nCấu trúc phân ngành 4 cấp theo chuẩn ICB") 
  
# Daily returns Chart:
data_2020_tbl %>% 
  group_by(value) %>% 
  mutate(col_neg = if_else(daily_return < 0, daily_return, NA_real_),
         col_pos = if_else(daily_return > 0, daily_return, NA_real_)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date)) +
  
  geom_col(aes(y = col_neg),
           fill = "tomato",
           color = "tomato") +
  
  geom_col(aes(y = col_pos),
           fill = "cornflowerblue",
           color = "cornflowerblue") +
  
  facet_wrap(~value, ncol = 2) + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  
  labs(x = "",
       y = "",
       title = "Biến \u0111ộng chỉ số ngành theo ngày của 11 ngành nghề n\u0103m 2020",
       caption = "Nguồn: FireAnt\nCấu trúc phân ngành 4 cấp theo chuẩn ICB")
  
# Min daily return in 2020
months <- data_2020_tbl %>% 
  mutate(months = month(date, label = TRUE, abbr = FALSE)) %>% 
  pull() %>% 
  levels() %>% 
  as.character()

data_2020_tbl %>% 
  group_by(value) %>% 
  filter(daily_return == min(daily_return)) %>% 
  mutate(month = as.numeric(month)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = month,
             y = daily_return,
             color = value)) +
  
  geom_point() + theme_tq() + theme2 +
  scale_x_continuous(breaks = 1:12,
                     labels = months) +
  
  expand_limits(y = 0) +

  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = scales::pretty_breaks(n = 8)) +
 
  labs(x = "",
       y = "Mức sụt giảm",
       title = "Mức \u0111ộ sụt giảm lớn nhất trong ngày của các ngành n\u0103m 2020",
       color = "Ngành")

# Skew chart :
data_2020_tbl %>% 
  mutate(value2 = fct_reorder(value, skew) %>% fct_rev()) %>% 
  ggplot(aes(x = value2,
             y = skew,
             fill = value)) +
  
  geom_col(width = .3) +
  
  theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::number_format(scale = 1e-2, suffix = "%")) +
  
  theme(
    axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1),
    legend.position = "none"
  ) +
  
  labs(x = "",
       title = "Độ lệch của các ngành n\u0103m 2020")
  

# Standard deviation chart:
data_2020_tbl %>% 
  mutate(value2 = fct_reorder(value, sd)) %>% 
  ggplot(aes(x = value2,
             y = sd,
             fill = value)) +
  
  geom_col(width = .3) +  theme_bw() + theme +
  
  theme(
    axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1)
  ) +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::number_format(scale = 4e-4)) +
  
  labs(x = "",
       y = "Độ lệch chuẩn",
       title = "Độ lệch chuẩn n\u0103m 2020")
  
  
# Data 2008 - 2020: ----
data_2008_2020_tbl <- data_nested %>% 
  unnest() %>%
  mutate(year_num = year(date)) %>% 
  group_by(value, year_num) %>% 
  calculate_return() %>% 
  ungroup()


# SKEW chart: 
data_2008_2020_tbl %>% 
  group_by(year, value) %>% 
  
  ggplot(aes(x = value,
             y = skew,
             fill = value)) +
  
  geom_col(width = .3) +
  
  scale_y_continuous(labels = scales::number_format(scale = 1e-2, suffix = "%")) +
  
  facet_wrap(~year, scales = "free_y", ncol = 3) +
  
  theme_tq() + theme2 +
  
  theme(
    axis.text.x = element_blank()
  ) +
  
  labs(x = "",
       y = "skew",
       title = "Độ lệch các ngành qua từng giai \u0111oạn 2008 - 2020",
       fill = "Ngành")
  

# Min return by year
data_2008_2020_tbl %>% 
  group_by(year, value) %>% 
  filter(daily_return == min(daily_return)) %>% 
  
  mutate(month = as.numeric(month)) %>% 
  
  ggplot(aes(x = month,
             y = daily_return,
             color = value)) +
  
  geom_point() +
  
  scale_x_continuous(breaks = 1:12,
                     labels = months) +
  
  expand_limits(y = 0) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = scales::pretty_breaks(n = 4)) +
  
  facet_wrap(~year, ncol = 3, scales = "free_y") + theme_tq() + theme2 +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  
  labs(x = "",
       y = "Mức sụt giảm",
       title = "Mức \u0111ộ sụt giảm lớn nhất trong tháng của các ngành giai \u0111oạn 2008 - 2020",
       color = "Ngành")

# Max return by year
data_2008_2020_tbl %>% 
  group_by(year, value) %>% 
  filter(daily_return == max(daily_return)) %>% 
  
  mutate(month = as.numeric(month)) %>% 
  
  ggplot(aes(x = month,
             y = daily_return,
             color = value)) +
  
  geom_point() +
  
  scale_x_continuous(breaks = 1:12,
                     labels = months) +
  
  expand_limits(y = 0) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = scales::pretty_breaks(n = 4)) +
  
  facet_wrap(~year, ncol = 3, scales = "free_y") + theme_tq() + theme2 +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  
  labs(x = "",
       y = "Mức sụt giảm",
       title = "Mức \u0111ộ sụt giảm lớn nhất trong tháng của các ngành giai \u0111oạn 2008 - 2020",
       color = "Ngành")
