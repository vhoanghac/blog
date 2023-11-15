# 1. Packages -----------------------------------
source("header.R")
library(tidyquant)
library(timetk)
library(ggdist) #Raincloud plot
library(ggforce)

options(scipen = 999)

# 2. Import data

etf <- get_data_vnstock("043_E1VFVN30_dai_han/01_data/raw/E1VFVN30.csv")

# 3. Analysis
calculate_rolling_returns <- function(etf, investment_per_month, trading_day_of_month, rolling_period_days) {
  
  # Xác định ngày giao dịch của tháng
  etf <- etf %>%
    group_by(year  = year(date), 
             month = month(date)) %>%
    
    mutate(trading_day = row_number()) %>%
    ungroup()
  
  # Tính toán số lượng chứng chỉ quỹ mua được dựa vào ngày giao dịch
  etf <- etf %>%
    mutate(units_purchased = if_else(trading_day == trading_day_of_month, investment_per_month / price, 0),
           investment_date = if_else(trading_day == trading_day_of_month, date, as.Date(NA)))
  
  rolling_returns <- tibble(StartDate        = as.Date(character()),
                            EndDate          = as.Date(character()),
                            AnnualizedReturn = numeric()
  )
  
  # Rolling returns
  for (i in 1:(nrow(etf) - rolling_period_days)) {
    if (!is.na(etf$investment_date[i])) {
      start_date <- etf$investment_date[i]
      end_date <- etf$date[i + rolling_period_days - 1]
      
      # Tính tổng số lượng chứng chỉ quỹ mua được
      total_units <- sum(etf$units_purchased[i:(i + rolling_period_days - 1)])
      
      # Giá cuối cùng
      final_price <- etf$price[i + rolling_period_days - 1]
      
      # Giá trị cuối cùng
      final_value <- total_units * final_price
      
      # Tổng tiền đầu tư
      total_investment <- investment_per_month * sum(!is.na(etf$investment_date[i:(i + rolling_period_days - 1)]))
      
      # Annualized return
      # 250 ngày giao dịch mỗi năm
      annualized_return <- (final_value / total_investment)^(250/rolling_period_days) - 1  
      
      rolling_returns <- add_row(rolling_returns, StartDate = start_date, EndDate = end_date, AnnualizedReturn = annualized_return)
    }
  }
  
  return(rolling_returns)
}


# Lựa chọn ngày thực hiện DCA:
# Ví dụ: 1 và 15 là ngày đầu giao dịch đầu tiên và thứ 15 của tháng.
trading_days_of_month <- c(1, 15) 

# Tạo một list để lưu kết quả
rolling_returns_1y_list <- list()
rolling_returns_2y_list <- list()
rolling_returns_3y_list <- list()
rolling_returns_4y_list <- list()
rolling_returns_5y_list <- list()
rolling_returns_6y_list <- list()
rolling_returns_7y_list <- list()
rolling_returns_8y_list <- list()

# Loop
for (trading_day in trading_days_of_month) {
  
  # Calculate 1-year (250 trading days) rolling returns
  #' @param 3000000 số tiền đầu tư mỗi tháng
  rolling_returns_1y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 250)
  
  rolling_returns_2y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 500)
  
  rolling_returns_3y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 750)
  
  rolling_returns_4y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 1000)
 
  rolling_returns_5y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 1250)

  rolling_returns_6y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 1500)

  rolling_returns_7y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 1750)

  rolling_returns_8y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(etf, 3000000, trading_day, 2000)
}

# List các rolling returns theo chu kỳ
rolling_returns_1y_list$day1
rolling_returns_2y_list$day1
rolling_returns_3y_list$day1
rolling_returns_4y_list$day1
rolling_returns_5y_list$day1
rolling_returns_6y_list$day1
rolling_returns_7y_list$day1
rolling_returns_8y_list$day1

# Rolling list tính min max
rolling_returns_list <- list(`250`  = rolling_returns_1y_list$day1,
                             `500`  = rolling_returns_2y_list$day1,
                             `750`  = rolling_returns_3y_list$day1,
                             `1000` = rolling_returns_4y_list$day1,
                             `1250` = rolling_returns_5y_list$day1,
                             `1500` = rolling_returns_6y_list$day1,
                             `1750` = rolling_returns_7y_list$day1,
                             `2000` = rolling_returns_8y_list$day1)

# Min/max returns
min_max <- tibble(period  = as.numeric(names(rolling_returns_list)),
                  min     = map_dbl(rolling_returns_list, ~ min(.x$AnnualizedReturn, na.rm = TRUE)),
                  max     = map_dbl(rolling_returns_list, ~ max(.x$AnnualizedReturn, na.rm = TRUE)),
                  min_txt = scales::percent(min),
                  max_txt = scales::percent(max))


# Plot Min max
min_max %>% 
  
  ggplot(aes(x = as.factor(period))) +
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#0078D7",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), 
            size     = 5, 
            vjust    = 2, 
            fontface = "bold", 
            color    = ifelse(min_max$min < 0, "red", "#018574")) +
  
  geom_text(aes(y = max, label = max_txt), 
            size     = 5, 
            vjust    = -0.5, 
            fontface = "bold", 
            color    = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1, 0.25), 
                     limits = c(-0.75, 1)) +
  
  # X-axis: đổi nhãn. 
  # Nếu không có dòng dưới đây thì sẽ bị lỗi khi vẽ biểu đồ:
  # Error: Discrete value supplied to continuous scale
  scale_x_discrete(labels = c("250"  = "1 năm", "500" = "2 năm", "750" = "3 năm", 
                              "1000" = "4 năm", "1250" = "5 năm", "1500" = "6 năm", "1750" = "7 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x        = "",
       y        = "Tỷ suất sinh lợi mỗi năm",
       title    = "Phạm vi biến động TSSL khi DCA mỗi tháng vào ETF VN30",
       subtitle = "ETF VN30 là ETF E1VFVN30. Dữ liệu bắt đầu tại ngày 06/10/2014. <br> Tỷ suất sinh lợi nằm trong khoảng <span style = 'color:firebrick4;'>-31,1% đến 54,5% </span>khi nắm giữ trong vòng 1 năm. <br> Nhưng khoảng cách này sẽ được thu hẹp khi đầu tư trong một thời gian dài.<br> Nếu đầu tư 8 năm, tỷ suất sinh lợi hằng năm nằm trong khoảng <span style = 'color:firebrick4;'>2,7% đến 4,6% </span>",
       caption  = "Giả định rằng nhà đầu tư DCA vào ngày giao dịch đầu tiên của mỗi tháng
       Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: tổng hợp bằng vnstock")


# Biểu đồ positive, negative
pos_neg_list <- list(`250`  = rolling_returns_1y_list$day1,
                     `500`  = rolling_returns_2y_list$day1,
                     `750`  = rolling_returns_3y_list$day1,
                     `1000` = rolling_returns_4y_list$day1,
                     `1250` = rolling_returns_5y_list$day1,
                     `1500` = rolling_returns_6y_list$day1,
                     `1750` = rolling_returns_7y_list$day1,
                     `2000` = rolling_returns_8y_list$day1)

# Step 1: Calculate Positive and Negative Counts
pos_neg_counts <- map_df(pos_neg_list, ~ tibble(positive = sum(.$AnnualizedReturn > 0, na.rm = TRUE),
                                                negative = sum(.$AnnualizedReturn < 0, na.rm = TRUE)), .id = "day") %>% 
  mutate(total = positive + negative) %>% 
  mutate(across(c(positive, negative), ~ ./total)) %>% 
  mutate(day_label = case_when(day == "250" ~ "1 năm",
                               day == "500" ~ "2 năm",
                               day == "750" ~ "3 năm",
                               day == "1000" ~ "4 năm",
                               day == "1250" ~ "5 năm",
                               day == "1500" ~ "6 năm",
                               day == "1750" ~ "7 năm",
                               day == "2000" ~ "8 năm",
                               TRUE ~ as.character(day))) %>% 
  mutate(day_label = as_factor(day_label))
  
  

## PLOT pos neg counts
pos_neg_counts %>% 
  ggplot() +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 2 * pi * (1 - negative)), size = 2, color = "#000000") +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1.02, start = 0 * pi * negative, end = 2 * pi * -negative), size = 3, color = "#dc372c") +
  geom_text(aes(x = 0, y = 0, label = scales::percent(negative)), fontface = "bold", size = 6, hjust = 0.5, color = "#dc372c") +
  facet_wrap(~ day_label, nrow = 2) + 
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


# Raincloud plot:
rolling_returns_5y_list$day1 %>% 
  
  ggplot(aes(y = AnnualizedReturn, fill = "x")) + 
    
  stat_halfeye(adjust        = 1,
               justification = -0.1,
               .width        = 0,
               point_colour  = NA) +
  
  geom_boxplot(width         = 0.1,
               # Ẩn outliers trên biểu đồ (không có xóa)
               outlier.color = NA,
               alpha         = 0.5) +
  
  stat_dots(side = "left",
            justification    = 1.2,
            binwidth         = 0.01) +
  
  
  scale_fill_tq() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-02, 0.3, 0.1), 
                     limits = c(-0.2, 0.3)) + 
  
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", 
                              "750" = "3 năm", "1250" = "5 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "firebrick4") + 
  
  labs(x        = "",
       y        = "Tỷ suất sinh lợi mỗi năm",
       title    = "Biểu đồ phân phối TSSL khi DCA vào ETF VN30 trong 5 năm",
       subtitle = "ETF VN30 là ETF E1VFVN30. Dữ liệu bắt đầu tài ngày: Ngày 06/10/2014.",
       caption  = "Giả định rằng nhà đầu tư DCA vào ngày giao dịch đầu tiên của mỗi tháng,
       Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Nguồn dữ liệu: tổng hợp bằng vnstock") +
  
  coord_flip()
  
# Bảng thống kê
stats_tbl <- list_rbind(rolling_returns_list, names_to = "period")

stats_tbl %>% 
  group_by(period) %>% 
  tq_performance(Ra              = AnnualizedReturn, 
                 performance_fun = table.Stats)