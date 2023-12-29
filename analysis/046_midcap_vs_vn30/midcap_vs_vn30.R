# 1. Packages #######
source("header.R")
library(tidyquant)
library(ggrepel)

# 2. Import data #######
vn30 <- get_data_vnstock("046_midcap_vs_vn30/01_data/01_raw/E1VFVN30.csv") %>% 
  filter(date >= "2023-01-01") %>% 
  distinct(date, .keep_all = TRUE)

mid <- get_data_vnstock("046_midcap_vs_vn30/01_data/01_raw/FUEDCMID.csv") %>% 
  filter(date >= "2023-01-01") %>% 
  distinct(date, .keep_all = TRUE)

vnd <- get_data_vnstock("046_midcap_vs_vn30/01_data/01_raw/FUEVFVND.csv") %>% 
  filter(date >= "2023-01-01") %>% 
  distinct(date, .keep_all = TRUE)


# 3. Tidy


table <- vn30 %>% 
  left_join(mid, by = "date") %>% 
  left_join(vnd, by = "date") %>% 
  rename(VN30 = price.x,
         MIDCAP = price.y,
         DIAMOND = price) %>% 
  pivot_longer(names_to = "symbol",
               values_to = "price", -date)

# 4. Analyse

growth_tbl <- table %>% 
  group_by(symbol) %>% 
  tq_mutate(select = price,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1,
         label_txt = if_else(date == max(date),
                             paste(symbol, ":", scales::percent(growth, big.mark =".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_)) %>% 
  
  ungroup()


# 5. PLOT

growth_tbl %>% 
  filter(!symbol == "DIAMOND") %>% 
  
  ggplot(aes(x = date, y = growth, color = symbol)) + 
  
  geom_line(linewidth = 0.9) + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_x_date(breaks = seq(as.Date("2023-01-01"), as.Date("2023-12-28"), by="1 month"), date_labels = "%b") +
  
  scale_color_manual(values = c("#f94144", "#1d3557")) +
  
  coord_cartesian(xlim = c(min(growth_tbl$date), max(growth_tbl$date) + 100)) +
  
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
       y = "Tăng trưởng",
       title = "Tăng trưởng của FUEDCMID và E1VFVN30 năm 2023",
       caption = "FUEDCMID là ETF mô phỏng chỉ số MIDCAP-TRI
       E1VFVN30 là ETF mô phỏng chỉ số VN30-TRI
       Nguồn: tổng hợp bằng vnstock")


# 6. BONUS: VND

growth_tbl %>% 
  filter(!symbol == "VN30") %>% 
  
  ggplot(aes(x = date, y = growth, color = symbol)) + 
  geom_line(linewidth = 0.9) + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_x_date(breaks = seq(as.Date("2023-01-01"), as.Date("2023-12-28"), by="1 month"), date_labels = "%b") +
  
  scale_color_manual(values = c("#1d3557", "#f94144")) +
  
  coord_cartesian(xlim = c(min(growth_tbl$date), max(growth_tbl$date) + 100)) +
  
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
       y = "Tăng trưởng",
       title = "Tăng trưởng của FUEDCMID và FUEVFVND năm 2023",
       caption = "FUEDCMID là ETF mô phỏng chỉ số MIDCAP-TRI
       FUEVFVND là ETF mô phỏng chỉ số VN DIAMOND
       Nguồn: tổng hợp bằng vnstock")
