# 1. Packages -----------------------------------
source("header.R")
library(tidyquant)
library(timetk)
library(ggdist)
library(ggforce)

options(scipen = 999)

###########################################################################
# 2. FUNCTIONS tính toán ##################################################
###########################################################################

# Function tính returns và rolling returns dựa theo chu kỳ đầu tư
#' @param period chu kỳ đầu tư muốn tính toán, ví dụ: 250 ngày, 500 ngày...
#' @param scale quy năm tỷ suất sinh lợi, ví dụ: 250 là một năm có 250 ngày giao dịch

calc_roll_returns <- function(data, period = 250, scale = 250){
  
  data %>% 
    tq_transmute(select     = price,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 col_rename = "returns") %>% 
    
    # Rolling returns
    tk_augment_slidify(.value  = returns,
                       .period = period,
                       .f      = Return.annualized,
                       scale   = scale,
                       .names  = "roll_ret",
                       .align  = "right")
}

# Function tính tỷ suất sinh lợi tương ứng với các symbols
# Lọc dữ liệu, chỉ lựa chọn min và max returns theo từng chu kỳ

roll_symbol <- function(symbol, data, periods, scale = 250) {
  
  roll_tbl <- tibble(symbol = symbol, period = periods)
  
  roll_tbl <- roll_tbl %>%
    mutate(
      
      min = sapply(periods, function(period) {
        data %>%
          calc_roll_returns(period, scale = scale) %>%
          pull(roll_ret) %>%
          min(na.rm = TRUE)
      }),
      
      max = sapply(periods, function(period) {
        data %>%
          calc_roll_returns(period, scale = scale) %>%
          pull(roll_ret) %>%
          max(na.rm = TRUE)
      }),
      
      min_txt = scales::percent(min),
      max_txt = scales::percent(max)
    )
  
  return(roll_tbl)
}


################
# ETF E1VFVN30 #
################

# Import dữ liệu:
etf <- get_data_vnstock("043_E1VFVN30_dai_han/01_data/raw/E1VFVN30.csv")

# Giả định các chu kỳ đầu tư:
# 1 năm = 250 ngày. 3 năm = 750 ngày...

periods <- c(250, 500, 750, 1000, 1250, 1500, 1750, 2000)

# Tính toán rolling returns
etf_roll_tbl <- roll_symbol(symbol = "ETF", etf, period = periods)

# PLOT
etf_roll_tbl %>% 
  ggplot(aes(x = as.factor(period))) +

  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#0078D7",
            color = "black") +

  geom_text(aes(y    = min, label = min_txt), 
            size     = 5, 
            vjust    = 2, 
            fontface = "bold", 
            color    = ifelse(etf_roll_tbl$min < 0, "red", "#018574")) +
  
  geom_text(aes(y = max, label = max_txt), 
            size     = 5, 
            vjust    = -0.5, 
            fontface = "bold", 
            color    = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1.25, 0.25), 
                     limits = c(-0.75, 1.25)) +
  
  # X-axis: đổi nhãn. 
  # Nếu không có dòng dưới đây thì sẽ bị lỗi khi vẽ biểu đồ:
  # Error: Discrete value supplied to continuous scale
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", "750" = "3 năm", "1000" = "4 năm",
                              "1250" = "5 năm", "1500" = "6 năm", "1750" = "7 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x        = "",
       y        = "Tỷ suất sinh lợi mỗi năm",
       title    = "Phạm vi biến động TSSL khi đầu tư vào ETF VN30",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014. <br> Tỷ suất sinh lợi nằm trong khoảng <span style = 'color:firebrick4;'>-40,8% đến 99,7% </span>khi nắm giữ trong vòng 1 năm. <br> Nhưng khoảng cách này sẽ được thu hẹp khi đầu tư trong một thời gian dài.<br> Nếu đầu tư 8 năm, tỷ suất sinh lợi hằng năm nằm trong khoảng <span style = 'color:firebrick4;'>5,5% đến 11,4% </span>",
       caption  = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: tổng hợp bằng vnstock")



### Plot Pos Neg Counts ################################################

# Import dữ liệu:
etf <- get_data_vnstock("042_niem_tin_dai_han/01_data/raw/E1VFVN30.csv")

# Giả định các chu kỳ đầu tư:
# 1 nam = 250 ngay. 3 nam = 750 ngay. 5 nam = 1250 ngay. 8 nam = 2000 ngay
periods_pos_neg <- c(250, 500, 750, 1000, 1250, 1500, 1750, 2000)

# Tính toán
etf_pos_neg <- etf %>% 
  calc_roll_returns(period = periods_pos_neg)

etf_pos_neg <- etf_pos_neg %>% 
  
  rename("250"  = roll_ret...1,
         "500"  = roll_ret...2,
         "750"  = roll_ret...3,
         "1000" = roll_ret...4,
         "1250" = roll_ret...5,
         "1500" = roll_ret...6,
         "1750" = roll_ret...7,
         "2000" = roll_ret...8) %>% 
  
  pivot_longer(cols      = -c(date, returns),
               names_to  = "period",
               values_to = "value") %>% 
  
  select(-date, -returns)


etf_pos_neg_tidied <- etf_pos_neg %>%
  
  group_by(period) %>%
  
  summarise(positive = sum(value > 0, na.rm = TRUE) / n(),
            negative = sum(value < 0, na.rm = TRUE) / n(),
            total = n()) %>% 
  
  mutate(period = as.numeric(period)) %>% 
  
  mutate(period_label = case_when(period == "250" ~ "1 năm",
                                  period == "500" ~ "2 năm",
                                  period == "750" ~ "3 năm",
                                  period == "1000" ~ "4 năm",
                                  period == "1250" ~ "5 năm",
                                  period == "1500" ~ "6 năm",
                                  period == "1750" ~ "7 năm",
                                  period == "2000" ~ "8 năm",
                                  TRUE ~ as.character(period))) %>%
  arrange(period) %>% 
  mutate(period_label = as_factor(period_label))


# PLOT Pos Neg Counts

etf_pos_neg_tidied %>% 
  ggplot() +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 2 * pi * (1 - negative)), size = 2, color = "#000000") +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1.02, start = 0 * pi * negative, end = 2 * pi * -negative), size = 3, color = "#dc372c") +
  geom_text(aes(x = 0, y = 0, label = scales::percent(negative)), fontface = "bold", size = 6, hjust = 0.5, color = "#dc372c") +
  facet_wrap(~ period_label, nrow = 2) + 
  coord_equal(clip = "off") + theme_void() +
  
  theme(text = element_text(family = "stix", size = 15),
        plot.title = element_text(size = 28, hjust = 0.5, face = "bold", color = "#000000", margin=margin(0,0,10,0)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, color = "#000000", hjust = 0.5, margin = margin(t = 25)),
        plot.subtitle = element_markdown(hjust = 0.5, margin=margin(0,0,20,0)),
        strip.text = element_text(size = 15, color = "#000000", hjust = 0.5, margin = margin(b = 5)),
        panel.spacing = unit(1.25, "lines"),
        legend.position = "none",
        plot.margin = unit(c(0.75, 0, 0.75, 0), "cm"),
        plot.background = element_rect(fill = "#FFFFFF", color = NA)) +
  
  labs(title    = "Thống kê số lần danh mục có TSSL âm",
       subtitle = "Tỷ lệ số lần mà TSSL bị âm tương ứng với mỗi chu kỳ đầu tư")



# Raincloud ################################################

etf_ret_tbl <- etf %>% 
  calc_roll_returns(period = periods)

etf_ret_tbl <- etf_ret_tbl %>% 
  
  rename("250"  = roll_ret...1,
         "500"  = roll_ret...2,
         "750"  = roll_ret...3,
         "1000" = roll_ret...4,
         "1250" = roll_ret...5,
         "1500" = roll_ret...6,
         "1750" = roll_ret...7,
         "2000" = roll_ret...8) %>% 
  
  pivot_longer(cols      = -c(date, returns),
               names_to  = "period",
               values_to = "value") %>% 
  
  mutate(period = as_factor(period))


# Full Rainclou Plot
etf_ret_tbl %>% 
  
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust        = 1,
               justification = -0.1,
               .width        = 0,
               point_colour  = NA) +
  
  geom_boxplot(width         = 0.1,
               # Ẩn outliers trên chart (không xóa)
               outlier.color = NA,
               alpha         = 0.5) +
  
  stat_dots(side          = "left",
            justification = 1.1,
            binwidth      = 0.006) +
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.5, 1.1, 0.1), 
                     limits = c(-0.5, 1.1)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) + 
  
  labs(x        = "",
       y        = "Tỷ suất sinh lợi mỗi năm",
       title    = "Biểu đồ phân phối tỷ suất sinh lợi khi đầu tư vào ETF VN30",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014.",
       caption  = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự.
       Nguồn dữ liệu: tổng hợp bằng vnstock")


# 250 day
etf_ret_tbl %>%
  filter(period == 1000) %>% 
  
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust        = 1,
               justification = -0.1,
               .width        = 0,
               point_colour  = NA) +
  
  geom_boxplot(width         = 0.1,
               # Ẩn outliers trên chart (không xóa)
               outlier.color = NA,
               alpha         = 0.5) +
  
  stat_dots(side          = "left",
            justification = 1.2,
            binwidth      = 0.002) +
  
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.1, 0.3, 0.1), 
                     limits = c(-0.1, 0.3)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", "750" = "3 năm", "1000" = "4 năm",
                              "1250" = "5 năm", "1500" = "6 năm", "1750" = "7 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "firebrick4") +
  
  labs(x        = "",
       y        = "Tỷ suất sinh lợi mỗi năm",
       title    = "Biểu đồ phân phối TSSL khi đầu tư ETF VN30 trong 4 năm",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014.",
       caption  = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Nguồn dữ liệu: tổng hợp bằng vnstock") +
  
  coord_flip()

# Bảng thống kê
etf_ret_tbl %>% 
  group_by(period) %>% 
  tq_performance(Ra              = value, 
                 performance_fun = table.Stats)