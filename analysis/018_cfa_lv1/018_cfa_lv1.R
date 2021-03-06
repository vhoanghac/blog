# Import packages ----

library(tidyverse)
library(lubridate)
library(tidyquant)


source("header.R")

# Import data: ----

time_tbl <- read_csv("01_data/cfa_time.csv",
                     col_types = cols(reading = col_factor()))

mock_tbl <- read_csv("01_data/cfa_mock.csv",
         col_types = cols(mock    = col_factor(),
                          session = col_factor(),
                          type    = col_factor()))

# Plot tong thoi gian hoc tung mon:
time_tbl %>% 
  group_by(reading) %>% 
  summarise(study_hour = sum(duration)) %>% 
  mutate(study_hour    = hms::hms(seconds_to_period(study_hour))) %>% 
  
  # Tao label cho chart:
  mutate(study_hour_label = as.character(study_hour)) %>% 
  
  # Plot:
  ggplot(aes(x    = fct_reorder(reading, study_hour),
             y    = study_hour,
             fill = reading)) +
  
  geom_bar(position = "dodge",
           stat     = "identity")  +
  
  # Label:
  geom_text(aes(label = study_hour_label),
            position  = position_dodge(width = 0.9),
            stat      = "identity",
            hjust     = -0.1) +
  
  coord_flip() +
  
  # Tang limit de label hien thi day du tren chart:
  ylim(0, 1050000) +
  
  tidyquant::theme_tq() + theme +
  
  # Xoa text x axis
  theme(axis.text.x = element_blank()) +
  
  labs(x       = "",
       y       = "",
       title   = "Thời gian học dành cho từng môn",
       caption = "Thời gian học tính bằng giờ")


# Plot duration per week
ime_tbl %>% 
  mutate(duration = as.numeric(duration) / 3600) %>%
  
  group_by(date) %>% 
  
  summarise(study_hour_per_day = sum(duration)) %>% 
  
  tq_transmute(select     = study_hour_per_day,
               mutate_fun = apply.weekly,
               FUN        = sum,
               col_rename = "study_hour_per_week") %>% 
  
  ungroup() %>% 
  
  ggplot(aes(x = date,
             y = study_hour_per_week)) +
  
  geom_area(color = "firebrick4", fill = "red", alpha = 0.3) +
  
  # geom_smooth(method = "loess", span = 0.15, se = FALSE) +
   
  theme_tq() + theme +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0, 32)) +
  
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week",
               date_labels = "%m-%Y") +
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  labs(x = "",
       y = "Số giờ học",
       title = "Thời gian học mỗi tuần")

  
# Plot chart ngay nao hoc nhieu nhat:
time_tbl %>% 
  mutate(duration = as.numeric(duration) / 3600) %>% 
  
  mutate(w_day = WEEKDAY(date, label = TRUE, week_start = 1)) %>% 
  
  group_by(w_day) %>% 
  
  summarise(total_hours = sum(duration)) %>% 
  mutate(percent = total_hours / sum(total_hours)) %>% 
  
  mutate(percent_label = scales::percent(percent)) %>% 
  
  ggplot(aes(x = w_day,
             y = percent)) +
  
  geom_col(fill = " red", alpha = 0.7) +
  
  geom_label(aes(label = percent_label),
            vjust = -0.5,
            size  = 4,
            color = "firebrick4") +
  
  expand_limits(y = 0.19) + theme_tq() + theme +
  
  theme(axis.text.y = element_blank()) +
  
  labs(x = "",
       y = "",
       title = "Thứ ba là ngày học nhiều nhất",
       caption = "Tính số giờ học mỗi ngày trong tuần và chia cho tổng")
  
#############################################
# MOCK EXAM 
#############################################

mock_long_tbl <- mock_tbl %>% 
  rename(DERI = DERIVATIVES) %>% 
  pivot_longer(names_to = "reading", values_to = "mark", cols = 5:14) %>% 
  group_by(mock, type, reading) %>% 
  summarise(n = sum(mark)) %>% 
  ungroup() %>% 
  
  group_by(mock, reading) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() 


# Plot mock 1:
mock_long_tbl %>% 
  filter(mock == 1) %>% 

  mutate(pct_label = scales::percent(pct, accuracy = 1)) %>% 
  
  ggplot(aes(x = reading,
             y = pct,
             fill = fct_rev(type))) +
  
  geom_bar(stat = "identity") +
  
  scale_fill_manual(values = c("lightcoral", "seagreen4")) + 
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  geom_hline(aes(yintercept = 0.57), color = "black", linetype = "dashed") +
  
  # Hien thi text cho cac cau tra loi dung
  geom_text(aes(label = ifelse(type == "correct", pct_label, "")),
            color = "white",
            fill = "white",
            position = position_stack(vjust = 0.5)) + theme_tq() + theme +
  
  geom_text(aes(x = "CF",
                y = 0.6),
            label = "Mean = 57.1%") +
  
  # theme(axis.text.x = element_text(angle = 90)) +
  
  labs(x = "",
       y = "",
       title = "Số câu trả lời \u0111úng mock 1")


# Plot diem kiem tra cua cac bai mock exam::
mock_long_tbl %>% 
  
  mutate(pct_label = scales::percent(pct, accuracy = 1)) %>% 
  
  ggplot(aes(x    = reading,
             y    = pct,
             fill = fct_rev(type))) +
  
  geom_bar(stat = "identity") +
  
  facet_wrap(~mock, ncol = 2, scales = "free_x") +
  
  scale_fill_manual(values = c("lightcoral", "seagreen4")) + 
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  # Hien thi text cho cac cau tra loi dung
  geom_text(aes(label = ifelse(type == "correct", pct_label, "")),
            color = "white",
            fill  = "white",
            position = position_stack(vjust = 0.5)) + theme_tq() + theme +
  
  theme(axis.text.x = element_text(angle = 90))+
  
  labs(x = "",
       y = "",
       title = "Số câu trả lời \u0111úng mỗi bài kiểm tra thử")


# Violin plot:
mock_long_tbl %>% 
  filter(type == "correct") %>% 
  ggplot(aes(x = reading,
             y = pct,
             fill = reading)) +
  
  geom_violin() + theme_tq() + theme + 
  
  geom_jitter(position = position_jitter(0.2)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  labs(x = "",
       y = "",
       title = "Thống kê \u0111iểm kiểm tra thử theo từng môn")


# Sample = 6 x 12 
mock_tbl %>% 
  rename(DERI = DERIVATIVES) %>% 
  pivot_longer(names_to = "reading", values_to = "mark", cols = 5:14) %>% 
  group_by(mock, session, type, reading) %>% 
  summarise(n = sum(mark)) %>% 
  ungroup() %>% 
  
  group_by(mock, session, reading) %>% 
  mutate(pct = n / sum(n)) %>% 
  filter(type == "correct") %>%
  
  # Plot:
  ggplot(aes(x    = reading,
             y    = pct,
             fill = reading)) +
  
  geom_violin() + theme_tq() + theme + 
  
  geom_jitter(position = position_jitter(0.2)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 8),
                     limits = c(0, 1)) +
  
  labs(x = "",
       y = "",
       title = "Thống kê \u0111iểm kiểm tra thử theo từng môn")


# Summary diem min, max...
mock_long_tbl %>% 
  filter(type == "correct") %>% 
  select(-mock, -type, -n) %>% 
  pivot_wider(names_from  = reading,
              values_from = pct) %>% 
  unnest(cols = c(AI, CF, DERI, ECON, EQUITY, ETHICS, FI, FRA, PM, QM)) %>% 
  
  summary()


#############################################
# REAL EXAM
#############################################


real_score <- tibble(reading    = c("AI_DERI", "CF", "ECON", "EQUITY", "ETHICS", "FI", "FRA", "PM", "QM"),
                     study_hour = c(34, 54, 86, 47, 38, 69, 113, 58, 99),
                     score      = c(89, 73, 48, 80, 72, 73, 75, 68, 90))

real_score %>% 
  ggplot(aes(x = study_hour,
             y = score)) +
  
  geom_point(color = "firebrick4") +
  
  geom_label(aes(label = reading,
                 y = score + 2),
                 color = "firebrick4") + theme_tq() + theme +
  
  labs(x = "Số giờ học",
       y = "Điểm (ước lượng)",
       title = "Thống kê số \u0111iểm t\u01B0\u01A1ng ứng thời gian học của từng môn")


