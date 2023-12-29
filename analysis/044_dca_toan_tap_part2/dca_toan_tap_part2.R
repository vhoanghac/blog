# 1. Packages -----------------------------------
source("header.R")
library(tidyquant)
library(timetk)
library(ggforce)
#library(tvm) # xirr

options(scipen = 999)

# 2. Import data
dcds <- read_csv("044_dca_toan_tap_part2/01_data/01_raw/DCDS.csv")


########################################################
# Function #############################################
########################################################

# Function tính returns theo giá daily
calculate_returns <- function(data){
  
  data %>% 
    tq_mutate(select     = price,
              mutate_fun = periodReturn,
              period     = "daily",
              col_rename = "returns") %>% 
    
    mutate(growth = cumprod(1 + returns) - 1)
}

# Function tính lợi nhuận theo phương pháp DCA
calculate_dca <- function(data){
  
  data %>% 
    mutate(month             = month(date),
           year              = year(date),
           first_trading_day = !duplicated(paste(year, month))) %>% 
    
    # # Cash flow XIRR
    # mutate(cash_flow = if_else(first_trading_day, -cash, 0)) %>% 
    
    # Tính số chứng chỉ quỹ mua được
    mutate(units_purchased = if_else(first_trading_day, cash / price, 0)) %>% 
    
    # Tính tổng số lượng chứng chỉ quỹ và tổng tiền đầu tư
    mutate(total_units = cumsum(units_purchased),
           total_cash  = cumsum(if_else(first_trading_day, cash, 0))) %>% 
    
    mutate(total_value = total_units * price,
           profit      = total_value / total_cash - 1) %>% 
    
    select(-month, -year)
  
}

# Tính rolling returns (cho mục 2.3 của bài viết)
calculate_rolling_returns <- function(etf, investment_per_month, trading_day_of_month, rolling_period_days) {
  
  # Xác định ngày giao dịch trong tháng
  etf <- etf %>%
    group_by(year  = year(date),
             month = month(date)) %>%
    mutate(trading_day = row_number()) %>%
    ungroup()
  
  # Tính số chứng chỉ quỹ mua được
  etf <- etf %>%
    mutate(units_purchased = if_else(trading_day == trading_day_of_month, investment_per_month / price, 0),
           investment_date = if_else(trading_day == trading_day_of_month, date, as.Date(NA)))
  
  rolling_returns <- tibble(StartDate        = as.Date(character()),
                            EndDate          = as.Date(character()),
                            AnnualizedReturn = numeric())
  
  # Rolling returns
  for (i in 1:(nrow(etf) - rolling_period_days)) {
    if (!is.na(etf$investment_date[i])) {
      
      start_date <- etf$investment_date[i]
      
      end_date <- etf$date[i + rolling_period_days - 1]
      
      total_units <- sum(etf$units_purchased[i:(i + rolling_period_days - 1)])
      
      final_price <- etf$price[i + rolling_period_days - 1]
      
      final_value <- total_units * final_price

      total_investment <- investment_per_month * sum(!is.na(etf$investment_date[i:(i + rolling_period_days - 1)]))
      
      annualized_return <- (final_value / total_investment)^(250/rolling_period_days) - 1  # 250 trading days in a year

      rolling_returns <- add_row(rolling_returns, StartDate = start_date, EndDate = end_date, AnnualizedReturn = annualized_return)
    }
  }
  
  return(rolling_returns)
}

# Biểu đồ giá:
dcds %>% 
  filter(date >= "2015-01-01" & date <= "2021-12-31") %>% 
  
  ggplot(aes(x = date, 
             y = price)) + 
  
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Giá chứng chỉ quỹ DCDS từ năm 2015 đến tháng 05/2022",
       caption = "Nguồn: Fmarket")


########################################################
# 3. Analysis ##########################################
########################################################

#########################################################
# 1: Đầu tư 3 năm, thị trường tăng: #####################
#########################################################

dcds_first <- dcds %>% 
  filter(date >= "2015-01-01" & date <= "2017-12-31")


# LUMP SUM: ########

ls_dcds_first <- dcds_first %>% 
  calculate_returns()

# Cumulative return
ls_dcds_first %>% head.tail()

# Annualized return
ls_dcds_first %>% 
  tq_performance(Ra = returns,
                 performance_fun = Return.annualized,
                 scale = 250)


# DCA: #############

cash <- 3000000

dca_dcds_first <- dcds_first %>% 
  calculate_dca()

# cumulative return
dca_dcds_first %>% head.tail()

# Annualized return
(1 +last(dca_dcds_first$profit))^(250/nrow(dcds_first)) - 1

# # XIRR
# xirr_dcds_first <- dca_dcds_first %>%
#   mutate(cash_flow = if_else(first_trading_day, -cash, 0)) %>% 
#   filter(cash_flow == -3000000) %>%
#   add_row(last(dca_dcds_first)) %>%
#   mutate(cash_flow = if_else(date == last(date), total_value, cash_flow))
# 
# 
# xirr_dcds_first %>% head.tail()
# 
# xirr(xirr_dcds_first$cash_flow, xirr_dcds_first$date, comp_freq = 1)


# PLOT DCA

dca_dcds_first %>% 
  ggplot(aes(x = date, y = profit)) + 
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks()) +
  
  annotate("text", 
           x      = as.Date("2016-01-01"), 
           y      = 0.5, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 55.3%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2016-01-20"), 
           y      = 0.4, 
           size   = 9,
           family = "stix",
           label  = "Tỷ suất sinh lợi: 15.3%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Lợi nhuận khi đầu tư DCDS bằng phương pháp DCA",
       caption = "Giả định đầu tư vào ngày đầu tiên của mỗi tháng từ năm 2015 đến hết năm 2017.
       Nguồn: Fmarket")


# PLOT AVERAGE
dca_dcds_first %>% 
  mutate(avg_price = if_else(first_trading_day, total_cash / total_units, 0)) %>% 
  filter(avg_price > 0) %>% 
  
  ggplot(aes(x = date, 
             y = avg_price)) +
  
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  labs(x       = "",
       y       = "Giá trung bình",
       title   = "Giá trung bình ngày càng tăng theo thời gian",
       caption = "Giá trung bình của lô chứng chỉ quỹ được tính bằng công thức:
       Lấy tổng số tiền đã đầu tư chia cho tổng số lượng chứng chỉ quỹ đang nắm giữ
       Nguồn: Fmarket.vn")


#########################################################
# 2: Đầu tư 4 năm, thị trường tăng và giảm ##############
#########################################################

dcds_second <- dcds %>% 
  filter(date >= "2015-01-01" & date <= "2018-12-31") 

# Lump sum: #########
ls_dcds_second <- dcds_second %>% 
  calculate_returns()

# Cumulative return:
ls_dcds_second %>% head.tail()

# Annualized
ls_dcds_second %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized,
                 scale = 250)


# DCA ###############

dca_dcds_second <- dcds_second %>% 
  calculate_dca()

# cumulative return
dca_dcds_second %>% head.tail()

# Annualized return
(1 +last(dca_dcds_second$profit))^(250/nrow(dcds_second)) - 1

# PLOT:
dca_dcds_second %>% 
  
  ggplot(aes(x = date, 
             y = profit)) + 
  
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks()) +
  
  annotate("text", 
           x      = as.Date("2016-01-01"), 
           y      = 0.7, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 28.6%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2016-01-20"), 
           y      = 0.6, 
           size   = 9,
           family = "stix",
           label  = "Tỷ suất sinh lợi: 6.3%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Lợi nhuận khi đầu tư DCDS bằng phương pháp DCA đến hết 2018",
       caption = "Giả định đầu tư vào ngày đầu tiên của mỗi tháng từ năm 2015 đến hết năm 2018.
       Nguồn: Fmarket")




#########################################################
# 2.2: Đầu tư 7 năm, thị trường tăng và giảm ############
#########################################################

dcds_third <- dcds %>% 
  filter(date >= "2015-01-01" & date <= "2022-05-31") 

# Lump sum: ########
ls_dcds_third <- dcds_third %>% 
  calculate_returns()

# Cumulative return:
ls_dcds_third %>% head.tail()

# Annualized
ls_dcds_third %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized,
                 scale = 250)


# DCA ################

dca_dcds_third <- dcds_third %>% 
  calculate_dca()

# cumulative return
dca_dcds_third %>% last()

# Annualized return
(1 +last(dca_dcds_third$profit))^(250/nrow(dcds_third)) - 1

# PLOT:
dca_dcds_third %>% 
  
  ggplot(aes(x = date, 
             y = profit)) + 
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  annotate("text", 
           x      = as.Date("2017-01-01"), 
           y      = 1.3, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 83.8%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2017-01-20"), 
           y      = 1.1, 
           size   = 9,
           family = "stix",
           label  = "Tỷ suất sinh lợi: 8.4%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Lợi nhuận khi đầu tư DCDS bằng phương pháp DCA đến 05/2022",
       caption = "Giả định đầu tư vào ngày đầu tiên của mỗi tháng từ năm 2015 đến tháng 05/2022.
       Nguồn: Fmarket")


#########################################################
# 2.3: Đầu tư 7 năm, thị trường tăng và giảm ############
#########################################################

dcds_fourth <- dcds %>% 
  filter(date >= "2015-01-01" & date <= "2021-05-31")

# Lump sum: ########
ls_dcds_fourth <- dcds_fourth %>% 
  calculate_returns()

# Cumulative return:
ls_dcds_fourth %>% head.tail()

# Annualized
ls_dcds_fourth %>% 
  tq_performance(Ra = returns,
                 performance_fun = Return.annualized)

# DCA ################

# Rolling returns
dcds_roll <- dcds %>% 
  filter(date >= "2014-01-01" & date <= "2021-05-31")

# Lựa chọn ngày thực hiện DCA:
trading_days_of_month <- 1

# Tạo một list để lưu kết quả
rolling_returns_1y_list <- list()
rolling_returns_2y_list <- list()
rolling_returns_3y_list <- list()
rolling_returns_4y_list <- list()
rolling_returns_5y_list <- list()
rolling_returns_6y_list <- list()

# Loop
for (trading_day in trading_days_of_month) {
  
  #  1-year (250 ngày giao dịch) rolling returns
  #' @param 3000000 số tiền đầu tư mỗi tháng
  rolling_returns_1y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(dcds_roll, 3000000, trading_day, 250)
  
  rolling_returns_2y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(dcds_roll, 3000000, trading_day, 500)
  
  rolling_returns_3y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(dcds_roll, 3000000, trading_day, 750)
  
  rolling_returns_4y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(dcds_roll, 3000000, trading_day, 1000)
  
  rolling_returns_5y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(dcds_roll, 3000000, trading_day, 1250)
  
  rolling_returns_6y_list[[paste0("day", trading_day)]] <- calculate_rolling_returns(dcds_roll, 3000000, trading_day, 1500)
  
}

# Rolling list tính min max
rolling_returns_list <- list(`250`  = rolling_returns_1y_list$day1,
                             `500`  = rolling_returns_2y_list$day1,
                             `750`  = rolling_returns_3y_list$day1,
                             `1000` = rolling_returns_4y_list$day1,
                             `1250` = rolling_returns_5y_list$day1,
                             `1500` = rolling_returns_6y_list$day1)

# Calculate min and max returns

min_max <- tibble(period  = as.numeric(names(rolling_returns_list)),
                  min     = map_dbl(rolling_returns_list, ~ min(.x$AnnualizedReturn, na.rm = TRUE)),
                  max     = map_dbl(rolling_returns_list, ~ max(.x$AnnualizedReturn, na.rm = TRUE)),
                  min_txt = scales::percent(min),
                  max_txt = scales::percent(max))

# Plot Min max
min_max %>% 
  
  ggplot(aes(x = as.factor(period)))+
  
  geom_rect(aes(xmin = as.numeric(as.factor(period)) - 0.3,
                xmax = as.numeric(as.factor(period)) + 0.3,
                ymin = min,
                ymax = max),
            fill = "#0078D7",
            color = "black") +
  
  geom_text(aes(y = min, label = min_txt), size = 5, vjust = 2, fontface = "bold", color = ifelse(min_max$min < 0, "red", "#018574")) +
  geom_text(aes(y = max, label = max_txt), size = 5, vjust = -0.5, fontface = "bold", color = "#018574") + 
  
  # Y-axis: Hiển thị phần trăm
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-0.75, 1, 0.25), 
                     limits = c(-0.75, 1)) +
  
  # X-axis: đổi nhãn. 
  # Nếu không có dòng dưới đây thì sẽ bị lỗi khi vẽ biểu đồ:
  # Error: Discrete value supplied to continuous scale
  scale_x_discrete(labels = c("250" = "1 năm", "500" = "2 năm", "750" = "3 năm", 
                              "1000" = "4 năm", "1250" = "5 năm", "1500" = "6 năm", "1750" = "7 năm", "2000" = "8 năm")) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) + 
  
  theme_tq() + theme +
  
  labs(x        = "",
       y        = "Tỷ suất sinh lợi mỗi năm",
       title    = "Phạm vi biến động TSSL khi DCA mỗi tháng vào DCDS",
       subtitle = "Dữ liệu bắt đầu tại ngày 01/01/2014 đến 31/05/2021.",
       caption  = "Giả định rằng nhà đầu tư DCA vào ngày giao dịch đầu tiên của mỗi tháng
       Tỷ suất sinh lợi mỗi năm trong biểu đồ là rolling returns. Số ngày giao dịch trong một năm là vào khoảng 250 ngày.
       Với trường hợp 1 năm: được tính bằng cách giả định đầu tư tại ngày 01 đến ngày 250, tiếp đó là ngày 02 đến ngày 251...
       Các trường hợp khác cũng được tính toán tương tự. Cuối cùng lựa chọn giá trị thấp nhất và cao nhất để biểu diễn trên biểu đồ.
       Nguồn dữ liệu: Fmarket")



#############
# 2021

dca_dcds_fourth <- dcds_fourth %>% 
  calculate_dca()

# cumulative return
dca_dcds_fourth %>% last()

# Annualized return
(1 +last(dca_dcds_fourth$profit))^(250/nrow(dcds_fourth)) - 1

# PLOT:
dca_dcds_fourth %>% 
  ggplot(aes(x = date, y = profit)) + 
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  annotate("text", 
           x      = as.Date("2017-01-01"), 
           y      = 1.1, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 106%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2017-01-20"), 
           y      = 0.97, 
           size   = 9,
           family = "stix",
           label  = "Tỷ suất sinh lợi: 11.7%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Lợi nhuận khi đầu tư DCDS bằng phương pháp DCA đến hết 05/2021",
       caption = "Giả định đầu tư vào ngày đầu tiên của mỗi tháng từ năm 2015 đến 05/2021.
       Nguồn: Fmarket")



########################

# BONUS VESAF

# Import dữ liệu
vesaf <- read_csv("044_dca_toan_tap_part2/01_data/01_raw/VESAF.csv") %>% 
  filter(date >= "2020-11-17")

# Số lượng ngày giao dịch trong năm
vesaf %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(count = n())

# Tính tăng trưởng
ls_vesaf <- vesaf %>% 
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "returns")
  mutate(growth = cumprod(1 + returns) - 1)

# Cumulative returns
ls_vesaf %>% head.tail()
  
# Annualized
ls_vesaf %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized,
                 scale           = 12)


#### DCA VESAF
dca_vesaf <- vesaf %>% 
  calculate_dca()

dca_vesaf <- vesaf %>% 
  summarise_by_time(.date_var = date,
                    .by       = "month",
                    price     = first(price),
                    .type     = "floor") %>% 
  add_row(last(vesaf)) %>% 
  calculate_dca()

# Cumulative returns
dca_vesaf %>% head.tail()

# Annualized return
(1 +last(dca_vesaf$profit))^(12/37) - 1


###################################
# BONUS full quá trình DCDS 2014 - 2023

dcds_fifth <- dcds %>% 
  filter(date >= "2015-01-01")

# Lump sum: ########
ls_dcds_fifth <- dcds_fifth %>% 
  calculate_returns()

# Cumulative return:
ls_dcds_fifth %>% head.tail()

# Annualized
ls_dcds_fifth %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized)


#### DCA
dca_dcds_fifth <- dcds_fifth %>% 
  calculate_dca()

# cumulative return
dca_dcds_fifth %>% last()

# Annualized return
(1 +last(dca_dcds_fifth$profit))^(250/nrow(dcds_fifth)) - 1

# PLOT:

dca_dcds_fifth %>% 
  ggplot(aes(x = date, y = profit)) + 
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  annotate("text", 
           x      = as.Date("2017-01-01"), 
           y      = 1.1, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 68.5%",
           color  = "firebrick4") +
  
  annotate("text", 
           x      = as.Date("2017-01-20"), 
           y      = 0.97, 
           size   = 9,
           family = "stix",
           label  = "Tỷ suất sinh lợi: 6%/năm",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Lợi nhuận khi đầu tư DCDS bằng phương pháp DCA từ 2015 đến 2023",
       caption = "Giả định đầu tư vào ngày đầu tiên của mỗi tháng từ năm 2015 đến 10/11/2023.
       Nguồn: Fmarket")



# PLOT giá trung bình
dca_dcds_fifth %>% 
  mutate(avg_price = if_else(first_trading_day, total_cash / total_units, 0)) %>% 
  filter(avg_price > 0) %>% 
  
  ggplot(aes(x = date, y = avg_price)) +
  
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x       = "",
       y       = "Giá trung bình",
       title   = "Giá trung bình cho mỗi chứng chỉ quỹ DCDS",
       caption = "Giá trung bình của lô chứng chỉ quỹ được tính bằng công thức:
       Lấy tổng số tiền đã đầu tư chia cho tổng số lượng chứng chỉ quỹ đang nắm giữ
       Nguồn: Fmarket.vn")


# Bonus kiểm tra kết quả
bonus <- dcds %>% 
  filter(date >= "2022-05-01" & date <= "2023-06-01") %>% 
  calculate_dca()

bonus %>% head.tail()

# Tổng số ngày đầu tư: 385
dcds %>% 
  filter(date >= "2022-05-01") %>% 
  nrow()


# tổng số chứng chỉ quỹ: 739
# Giá hiện tại mỗi chứng chỉ quỹ: 62989
# Tính toán:
bonus_total_value <- last(bonus$total_units) * last(dcds$price)
total_cash <- last(bonus$total_cash)
bonus_profit <- bonus_total_value / total_cash - 1

# Annualized return

(1 + bonus_profit)^(250/385) - 1

# Lump-sum:
ls_bonus <- dcds %>% 
  filter(date >= "2022-05-01") %>% 
  calculate_returns()

ls_bonus %>% head.tail()

ls_bonus %>% 
  tq_performance(Ra              = returns,
                 performance_fun = Return.annualized,
                 scale           = 250)
