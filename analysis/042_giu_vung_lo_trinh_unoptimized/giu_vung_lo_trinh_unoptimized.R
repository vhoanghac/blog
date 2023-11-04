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

###########################################################################
# 3. Phân tích các ETF  ###################################################
###########################################################################

################
# ETF E1VFVN30 #
################

# Import dữ liệu:
etf <- get_data_vnstock("042_niem_tin_dai_han/01_data/raw/E1VFVN30.csv")


test <- etf %>%
  filter(date >= "2018-03-30")
  

test_end <- test$date[1:(1 + 500 - 1)]


etf %>% 
  filter(date %in% test_end) %>% 
  mutate(unit = 100) %>% 
  mutate(value = unit * price) %>% 
  summarise(total_investment = 1888000,
            last = last(value),
            total_return = (last / total_investment) - 1 ,
            returns = (last / total_investment)^(1 / 250) - 1)





# Giả định các chu kỳ đầu tư:
# 1 nam = 250 ngay. 3 nam = 750 ngay. 5 nam = 1250 ngay. 8 nam = 2000 ngay
periods <- c(250, 500, 750, 1250, 2000)

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

  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 2, fontface = "bold", color = ifelse(etf_roll_tbl$min < 0, "red", "#018574")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1.25, 0.25), 
                     limits = c(-0.75, 1.25)) +
  
  # X-axis: đổi nhãn. 
  # Nếu không có dòng dưới đây thì sẽ bị lỗi khi vẽ biểu đồ:
  # Error: Discrete value supplied to continuous scale
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào ETF VN30",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014. <br> Tỷ suất sinh lợi nằm trong khoảng <span style = 'color:firebrick4;'>-40,8% đến 99,7% </span>khi nắm giữ trong vòng 1 năm. <br> Nhưng khoảng cách này sẽ được thu hẹp khi đầu tư trong một thời gian dài.<br> Nếu đầu tư 8 năm, tỷ suất sinh lợi hằng năm nằm trong khoảng <span style = 'color:firebrick4;'>5,5% đến 11,4% </span>",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
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
  rename("250" = roll_ret...1,
         "500" = roll_ret...2,
         "750" = roll_ret...3,
         "1000" = roll_ret...4,
         "1250" = roll_ret...5,
         "1500" = roll_ret...6,
         "1750" = roll_ret...7,
         "2000" = roll_ret...8) %>% 
  pivot_longer(cols = -c(date, returns),
               names_to = "period",
               values_to = "value") %>% 
  select(-date, -returns)


etf_pos_neg_tidied <- etf_pos_neg %>%
  group_by(period) %>%
  summarise(positive = sum(value > 0, na.rm = TRUE) / n(),
            negative = sum(value < 0, na.rm = TRUE) / n(),
            total = n()) %>% 
  mutate(period = as.numeric(period)) %>% 
  mutate(period_label = case_when(
    period == "250" ~ "1 năm",
    period == "500" ~ "2 năm",
    period == "750" ~ "3 năm",
    period == "1000" ~ "4 năm",
    period == "1250" ~ "5 năm",
    period == "1500" ~ "6 năm",
    period == "1750" ~ "7 năm",
    period == "2000" ~ "8 năm",
    TRUE ~ as.character(period)
  )) %>%
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
  
  labs(title = "Thống kê số lần danh mục có TSSL âm",
       subtitle = "Tỷ lệ số lần mà TSSL bị âm tương ứng với mỗi chu kỳ đầu tư")



# Raincloud ################################################


violin_tbl <- etf %>% 
  calc_roll_returns(period = periods)


violin_plot_tbl <- violin_tbl %>% 
  rename("250" = roll_ret...1,
         "500" = roll_ret...2,
         "750" = roll_ret...3,
         "1250" = roll_ret...4,
         "2000" = roll_ret...5) %>% 
  pivot_longer(cols = -c(date, returns),
               names_to = "period",
               values_to = "value") %>% 
  select(-date, -returns) %>% 
  mutate(period = as_factor(period))




violin_plot_tbl %>% 
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust = 1,
               justification = -0.1,
               .width = 0,
               point_colour = NA) +
  
  geom_boxplot(width = 0.1,
               
               # Xóa outliers
               outlier.color = NA,
               alpha = 0.5) +
  
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.006) +
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.5, 1.1, 0.1), 
                     limits = c(-0.5, 1.1)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) + 
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Biểu đồ phân phối tỷ suất sinh lợi khi đầu tư vào ETF VN30",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự.
       Nguồn dữ liệu: tổng hợp bằng vnstock")


# 250

violin_plot_tbl %>%
  filter(period == 250) %>% 
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust = 1,
               justification = -0.1,
               .width = 0,
               point_colour = NA) +
  
  geom_boxplot(width = 0.1,
               
               # Xóa outliers
               outlier.color = NA,
               alpha = 0.5) +
  
  stat_dots(side = "left",
            justification = 1.2,
            binwidth = 0.008) +
  
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.5, 1.1, 0.1), 
                     limits = c(-0.5, 1.1)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "firebrick4") +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Biểu đồ phân phối TSSL khi đầu tư ETF VN30 trong 1 năm",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Nguồn dữ liệu: tổng hợp bằng vnstock") +
  
  coord_flip()

# 500

violin_plot_tbl %>%
  filter(period == 2000) %>% 
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust = 1,
               justification = -0.1,
               .width = 0,
               point_colour = NA) +
  
  geom_boxplot(width = 0.1,
               
               # Xóa outliers
               outlier.color = NA,
               alpha = 0.5) +
  
  stat_dots(side = "left",
            justification = 1.2,
            binwidth = 0.002) +
  
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.05, 0.2, 0.05), 
                     limits = c(-0.05, 0.2)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "firebrick4") +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Biểu đồ phân phối TSSL khi đầu tư ETF VN30 trong 8 năm",
       subtitle = "ETF VN30 là ETF E1VFVN30. Thời điểm bắt đầu: Ngày 06/10/2014.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Nguồn dữ liệu: tổng hợp bằng vnstock") +
  
  coord_flip()




##################
# CÁC INDEX KHÁC #
##################

# Dữ liệu các index:
symbols_data <- list("MIDCAP" = "042_niem_tin_dai_han/01_data/raw/VNMIDCAPTRI.csv",
                     "VN30"   = "042_niem_tin_dai_han/01_data/raw/VN30TRI.csv",
                     "VN100"  = "042_niem_tin_dai_han/01_data/raw/VN100TRI.csv")


#' @param ind_periods giả định chu kỳ đầu tư
ind_periods <- c(250, 500, 750, 1250, 1750)

# Tính toán kết quả
result_list <- lapply(names(symbols_data), function(symbol) {
  
  # get_data_tradingview: import data
  data <- get_data_tradingview(symbols_data[[symbol]])
  
  roll_symbol(symbol, data, ind_periods)
})

# Tibble:
final_result <- bind_rows(result_list)

# PLOT
symbol_colors <- c("MIDCAP" = "#0078D7", "VN30" = "#E74856", "VN100" = "#009344")

dodge_width <- 0.85

final_result %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = period - 0.4,
                xmax = period + 0.4,
                ymin = min,
                ymax = max,
                fill = symbol),
            position = position_dodge(width = dodge_width)) +
  
  geom_text(aes(y = min, label = min_txt, color = symbol), size = 5, vjust = 1.25, fontface = "bold", position = position_dodge(width = dodge_width)) +
  geom_text(aes(y = max, label = max_txt, color = symbol), size = 5, vjust = -0.5, fontface = "bold", position = position_dodge(width = dodge_width)) +
  
  scale_fill_manual(values = symbol_colors) +
  scale_color_manual(values = symbol_colors) +
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1.5, 0.25), 
                     limits = c(-0.75, 1.5)) +
  
  # X-axis: đổi nhãn
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "1750" = "7 năm")) + theme_tq() + theme2 +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào các chỉ số",
       subtitle = "MIDCAP là chỉ số MIDCAP-TRI. VN100 là chỉ số VN100-TRI. VN30 là chỉ số VN30-TRI.<br> 
       Thời điểm bắt đầu: ngày 06/04/2016.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251, ngày 03 đến ngày 252...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: TradingView")



##################
# CHỈ SỐ FINLEAD #
##################


finlead <- get_data_tradingview("042_niem_tin_dai_han/01_data/raw/VNFIN.csv")

finlead_roll_tbl <- roll_symbol(symbol = "FINLEAD", finlead, period = ind_periods)


finlead_roll_tbl %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#0078D7",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 1.5, fontface = "bold", color = ifelse(finlead_roll_tbl$min < 0, "red", "#018574")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1.80, 0.25), 
                     limits = c(-0.75, 1.80)) +
  
  # X-axis: đổi nhãn. 
  # Nếu không có dòng dưới đây thì sẽ bị lỗi khi vẽ biểu đồ:
  # Error: Discrete value supplied to continuous scale
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", "750" = "3 năm", "1250" = "5 năm", "1750" = "7 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào chỉ số ngành tài chính VNFIN",
       subtitle = "Chỉ số ngành tài chính tương tự như ETF FINLEAD. Thời điểm bắt đầu: Ngày 06/10/2014.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251, ngày 03 đến ngày 252...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: TradingView")


# Biểu đồ phân phối

finlead_rain_tbl <- finlead %>% 
  calc_roll_returns(period = ind_periods, scale = 250)

finlead_rain_plot <- finlead_rain_tbl %>% 
  rename("250" = roll_ret...1,
         "500" = roll_ret...2,
         "750" = roll_ret...3,
         "1250" = roll_ret...4,
         "1750" = roll_ret...5) %>% 
  pivot_longer(cols = -c(date, returns),
               names_to = "period",
               values_to = "value") %>% 
  select(-date, -returns) %>% 
  mutate(period = as_factor(period))


# 
finlead_rain_plot %>% 
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust = 1,
               justification = -0.1,
               .width = 0,
               point_colour = NA) +
  
  geom_boxplot(width = 0.1,
               
               # Xóa outliers
               outlier.color = NA,
               alpha = 0.5) +
  
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.006) +
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.5, 1.1, 0.1), 
                     limits = c(-0.5, 1.1)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "1750" = "7 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) + 
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Biểu đồ phân phối tỷ suất sinh lợi khi đầu tư vào chỉ số VNFIN",
       subtitle = "Chỉ số VNFIN tương tự ETF VNFIN LEAD. Thời điểm bắt đầu: Ngày 06/04/2016.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự.
       Nguồn dữ liệu: TradingView")


# Periods
finlead_rain_plot %>% 
  filter(period == 1750) %>% 
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust = 1,
               justification = -0.1,
               .width = 0,
               point_colour = NA) +
  
  geom_boxplot(width = 0.1,
               
               # Xóa outliers
               outlier.color = NA,
               alpha = 0.5) +
  
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.002) +
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.05, 0.2, 0.05), 
                     limits = c(-0.05, 0.2)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "1750" = "7 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "firebrick4") +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Biểu đồ phân phối TSSL khi đầu tư vào chỉ số VNFIN trong 7 năm",
       subtitle = "Chỉ số VNFIN tương tự ETF VNFIN LEAD. Thời điểm bắt đầu: Ngày 06/04/2016.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự.
       Nguồn dữ liệu: TradingView") +
  
  coord_flip()

###########################################################################
# 4. Phân tích quỹ cổ phiếu DCDS ##########################################
###########################################################################

dcds <- read_csv("042_niem_tin_dai_han/01_data/raw/DCDS.csv") %>% 
  filter(date >= "2014-01-01")

periods <- c(250, 500, 750, 1250, 2000)


dcds %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(count = n())

dcds_roll_tbl <- roll_symbol(symbol = "DCDS", dcds, period = periods)

# PLOT
dcds_roll_tbl %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#038387",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 1.15, fontface = "bold", color = ifelse(dcds_roll_tbl$min < 0, "red", "#018574")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1.25, 0.25), 
                     limits = c(-0.75, 1.25)) +
  
  # X-axis: đổi nhãn
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào DCDS",
       subtitle = "Thời gian bắt đầu: 01/01/2014.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251, ngày 03 đến ngày 252...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: Fmarket")

# Rainplot

dcds_rain_tbl <- dcds %>% 
  calc_roll_returns(period = periods, scale = 250)

dcds_rain_plot <- dcds_rain_tbl %>% 
  rename("250" = roll_ret...1,
         "500" = roll_ret...2,
         "750" = roll_ret...3,
         "1250" = roll_ret...4,
         "1750" = roll_ret...5) %>% 
  pivot_longer(cols = -c(date, returns),
               names_to = "period",
               values_to = "value") %>% 
  select(-date, -returns) %>% 
  mutate(period = as_factor(period))


# Periods
dcds_rain_plot %>% 
  filter(period == 1750) %>% 
  ggplot(aes(x = period, y = value, fill = period)) +
  
  stat_halfeye(adjust = 1,
               justification = -0.1,
               .width = 0,
               point_colour = NA) +
  
  geom_boxplot(width = 0.1,
               
               # Xóa outliers
               outlier.color = NA,
               alpha = 0.5) +
  
  stat_dots(side = "left",
            justification = 1.1,
            binwidth = 0.0015) +
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.05, 0.2, 0.05), 
                     limits = c(-0.05, 0.2)) +
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "1750" = "7 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "firebrick4") +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Biểu đồ phân phối TSSL khi đầu tư DCDS trong 7 năm",
       subtitle = "Thời điểm bắt đầu: Ngày 01/01/2014.",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự.
       Nguồn dữ liệu: Fmarket") +
  
  coord_flip()


###########################################################################
# 4. Phân tích quỹ trái phiếu #############################################
###########################################################################

###############
# VCBF-FIF ####
###############

fif <- read_csv("042_niem_tin_dai_han/01_data/raw/VCBFIF.csv") %>% 
  distinct(date, .keep_all = TRUE)

# (62+56+77) / 3 = 65
fif %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(count = n())

fif_periods <- c(70, 140, 210, 280)

fif_roll_tbl <- roll_symbol(symbol = "FIF", fif, period = fif_periods, scale = 70)

# PLOT
fif_roll_tbl %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#107C10",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 1.5, fontface = "bold", color = ifelse(fif_roll_tbl$min < 0, "red", "#107C10")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#107C10") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.05, 0.15, 0.05), 
                     limits = c(-0.05, 0.15)) +
  
  # X-axis: đổi nhãn
  scale_x_discrete(labels = c("70" = "1 năm", "140" = "2 năm", 
                              "210" = "3 năm", "280" = "4 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào VCBF-FIF",
       subtitle = "Thời gian bắt đầu: 09/08/2019",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch (có dữ liệu) trong một năm là vào khoảng 65 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 65, tiếp đó là ngày 02 đến ngày 66...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: Fmarket")


###########
# TCBF ####
###########

# Sau đó "điền vào chỗ trống" bằng cách dùng phép nội suy tuyến tính Linear Interpolation

tcbf <- read_csv("042_niem_tin_dai_han/01_data/raw/TCBF.csv")

tcbf <- tcbf %>% 
  
  # Xóa dữ liệu của quãng thời gian bị bán tháo
  # 11/11/2022 : 16.426
  # 01/09/2023 : 17.086
  mutate(price_na = case_when(date < "2022-11-11"  ~ price,
                              date >= "2023-09-01" ~ price,
                              TRUE ~ NA_real_)) %>% 
  
  # Tạo dữ liệu mới bằng phương pháp nội suy tuyến tính
  mutate(price_li = ts_impute_vec(price_na, period = 1, lambda = NULL))


tcbf_periods <- c(365, 365*2, 365*3, 365*5, 365*8)

# Đã chỉnh sửa:
tcbf_roll_tbl <- roll_symbol(symbol = "TCBF", tcbf %>% select(date, price = price_li), period = tcbf_periods, scale = 365)

# Dữ liệu thực:
real_roll_tbl <- roll_symbol(symbol = "TCBF", tcbf %>% select(date, price), period = tcbf_periods, scale = 365)

# PLOT dữ liệu đã chỉnh sửa:
li_tcbf_roll_tbl %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#FFB900",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 1.5, fontface = "bold", color = ifelse(li_tcbf_roll_tbl$min < 0, "red", "#018574")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.05, 0.15, 0.05), 
                     limits = c(-0.05, 0.15)) +
  
  # X-axis: đổi nhãn
  scale_x_discrete(labels = c("365" = "1 năm", "730" = "2 năm", 
                              "1095" = "3 năm", "1825" = "5 năm", "2920" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào TCBF (dữ liệu chỉnh sửa)",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch (có dữ liệu) trong một năm là vào khoảng 365 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 365, tiếp đó là ngày 02 đến ngày 366, ngày 03 đến ngày 367...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Dữ liệu đã xóa giai đoạn bị nhà đầu tư bán tháo, từ 11/11/2022 đến 01/09/2023. Sau đó sử dụng phương pháp nội suy tuyến tính để thay thế dữ liệu.
       Nguồn dữ liệu: tổng hợp từ TCBS")

# PLOT dữ liệu thực tế:
real_tcbf_roll_tbl %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#FFB900",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 1.25, fontface = "bold", color = ifelse(real_tcbf_roll_tbl$min < 0, "red", "#018574")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.20, 0.2, 0.05), 
                     limits = c(-0.2, 0.2)) +
  
  # X-axis: đổi nhãn
  scale_x_discrete(labels = c("365" = "1 năm", "730" = "2 năm", 
                              "1095" = "3 năm", "1825" = "5 năm", "2920" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư vào TCBF (dữ liệu thực)",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch (có dữ liệu) trong một năm là vào khoảng 365 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 365, tiếp đó là ngày 02 đến ngày 366, ngày 03 đến ngày 367...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: tổng hợp từ TCBS")





###########################################################################
# 5. Phân tích danh mục 50/50 #############################################
###########################################################################

# Import dữ liệu
midcap <- get_data_tradingview("042_niem_tin_dai_han/01_data/raw/VNMIDCAPTRI.csv") %>% 
  mutate(symbol = "midcap")

vn30 <- get_data_tradingview("042_niem_tin_dai_han/01_data/raw/VN30TRI.csv") %>% 
  mutate(symbol = "vn30")

# Xác định tỷ trọng
weights <- c(0.5, 0.5)

# Phân tích danh mục
portfolio_data <- midcap %>% 
  bind_rows(vn30) %>% 
  group_by(symbol) %>% 
  tq_transmute(sekect     = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  
  ungroup() %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights,
               col_rename = "returns",
               reblance_on = "yearly") %>% 
  mutate(symbol = "PORTFOLIO")

# Tính rolling returns của danh mục
portfolio_roll_tbl <- tibble(symbol = "PORTFOLIO", period = ind_periods)

portfolio_roll_tbl <- portfolio_roll_tbl %>% 
  mutate(
    
    min = sapply(ind_periods, function(period){
      
      portfolio_data %>% 
        tk_augment_slidify(.value  = returns,
                           .period = period,
                           .f      = Return.annualized,
                           scale   = 250,
                           .names  = "roll_ret",
                           .align  = "right") %>% 
        
        pull(roll_ret) %>% 
        min(na.rm = TRUE)
      
      
    }),
    
    max = sapply(ind_periods, function(period) {
      portfolio_data %>% 
        tk_augment_slidify(.value  = returns,
                           .period = period,
                           .f      = Return.annualized,
                           scale   = 250,
                           .names  = "roll_ret",
                           .align  = "right") %>% 
        pull(roll_ret) %>% 
        max(na.rm = TRUE)
    }),
    
    min_txt = scales::percent(min),
    max_txt = scales::percent(max)
    
  )

# Gộp dữ liệu

portfolio_final_result <- final_result %>% 
  bind_rows(portfolio_roll_tbl) %>% 
  filter(symbol != "VN100")

# PLOT:
portfolio_colors <- c("MIDCAP" = "#0078D7", "VN30" = "#E74856", "PORTFOLIO" = "#009344")

dodge_width <- 0.85

portfolio_final_result %>% 
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = period - 0.4,
                xmax = period + 0.4,
                ymin = min,
                ymax = max,
                fill = symbol),
            position = position_dodge(width = dodge_width)) +
  
  geom_text(aes(y = min, label = min_txt, color = symbol), size = 5, vjust = 1.25, fontface = "bold", position = position_dodge(width = dodge_width)) +
  geom_text(aes(y = max, label = max_txt, color = symbol), size = 5, vjust = -0.5, fontface = "bold", position = position_dodge(width = dodge_width)) +
  
  scale_fill_manual(values = portfolio_colors) +
  scale_color_manual(values = portfolio_colors) +
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1.5, 0.25), 
                     limits = c(-0.75, 1.5)) +
  
  # X-axis: đổi nhãn
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "1750" = "7 năm")) + theme_tq() + theme2 +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  
  labs(x = "",
       y = "Tỷ suất sinh lợi mỗi năm",
       title = "Phạm vi biến động TSSL khi đầu tư danh mục 50/50 (MIDCAP/VN30)",
       caption = "Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251, ngày 03 đến ngày 252...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       PORTFOLIO là danh mục đầu tư 50% vào MIDCAP-TRI và 50% vào VN30-TRI. Tái cân bằng mỗi năm một lần.
       Nguồn dữ liệu: TradingView")


#### 




