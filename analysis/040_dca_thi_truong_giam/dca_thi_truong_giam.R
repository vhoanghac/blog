# 1. Packages -----------------------
source("header.R")
library(tidyquant)
library(timetk)

# 2. Functions -----------------------

# Function tính returns
cal_ret <- function(data){
  
  data %>% 
    tq_transmute(select     = price,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 col_rename = "returns")
}


# Function tính drawdown table
table_dd <- function(data){
  
  data %>% 
    cal_ret() %>% 
    tk_xts() %>% 
    # Hiển thị 10 lần sụt giảm lớn nhất
    table.Drawdowns(top = 10) %>% 
    tk_tbl()
  
}

# Function plan DCA
cal_dca_plan <- function(data){
  
  data %>% 
    group_by(month, year) %>% 
    
    # Plan A: Mua liên tục mỗi tháng
    mutate(cash = ifelse(date == min(date), cash, 0)) %>%
    
    # Plan B: Mua liên tục mỗi tháng. Khi ETF giảm hơn 10%
    # thì x2 số tiền đầu tư mỗi tháng cho tới khi ETF trở lại đỉnh cũ.
    # Trong trường hợp này, để thỏa mãn điều kiện ifelse thì
    # phải đặt điều kiện đầu tiên là date == min(date) và period == 1 (tạm gọi X)
    # nếu đặt date == min(date) là điều kiện đầu tiên thì khi tính toán
    # công thức sẽ bỏ qua điều kiện (X).
    mutate(planb_cash = ifelse(date == min(date) & period == 1, cash * 2,
                               ifelse(date == min(date), cash, 0))) %>% 
    
    # Plan C: Mua liên tục mỗi tháng. Khi ETF giảm hơn 10% thì dừng
    # chỉ mua trở lại khi ETF chạm lại đỉnh cũ
    mutate(planc_cash = ifelse(date == min(date) & period == 1, 0,
                               ifelse(date == min(date), cash, 0))) %>% 
    
    
    ungroup() %>% 
    select(-period, -year, -month) %>% 
    
    # DCA Plan A: Mua liên tục
    # q = quantity. Số lượng chứng chỉ quỹ
    mutate(q = cash / price) %>% 
    mutate(total_q             = cumsum(q),
           total_cash_invested = cumsum(cash),
           total_value         = total_q * price,
           profit_pct          = (total_value / total_cash_invested) - 1,
           profit_number       = total_value - total_cash_invested,
           average             = total_cash_invested / total_q) %>% 
    
    # DCA Plan B: Giá giảm hơn 10% là gấp đôi số tiền đầu tư mỗi tháng
    mutate(planb_q = planb_cash / price) %>% 
    mutate(planb_total_q             = cumsum(planb_q),
           planb_total_cash_invested = cumsum(planb_cash),
           planb_total_value         = planb_total_q * price,
           planb_profit_pct          = (planb_total_value / planb_total_cash_invested) - 1,
           planb_profit_number       = planb_total_value - planb_total_cash_invested,
           planb_average             = planb_total_cash_invested / planb_total_q) %>% 
    
    # DCA Plan C: Giá giảm hơn 10% là dừng đầu tư. Chờ tới khi giá quay trở lại đỉnh cũ.
    mutate(planc_q = planc_cash / price) %>% 
    mutate(planc_total_q             = cumsum(planc_q),
           planc_total_cash_invested = cumsum(planc_cash),
           planc_total_value         = planc_total_q * price,
           planc_profit_pct          = (planc_total_value / planc_total_cash_invested) - 1,
           planc_profit_number       = planc_total_value - planc_total_cash_invested,
           planc_average             = planc_total_cash_invested / planc_total_q) %>% 
    
    
    # Tạo Label cho charts
    mutate(across(contains("total_cash"), ~scales::number(., big.mark = ".", decimal.mark = ","), .names = "{col}_txt")) %>% 
    mutate(across(contains("profit_pct"), ~scales::percent(., accuracy = 0.01, big.mark = ".", decimal.mark = ","), .names = "{col}_txt")) %>% 
    mutate(across(contains("profit_number"), ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1), .names = "{col}_txt")) %>% 
    mutate(across(contains("average"), ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1), .names = "{col}_txt"))
  
}

# 3. Settings cho plot  -----------------------

dca_plot <- function(data){
  
  title <- substitute(data) %>% 
    deparse() %>% 
    sub("_tbl.*", "",.) %>% 
    toupper()
  
  data %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = profit_pct, color = "A: Mua liên tục")) +
    geom_line(aes(y = planb_profit_pct, color = "B: x2 tiền ")) +
    geom_line(aes(y = planc_profit_pct, color = "C: Chờ đạt đỉnh")) +
    scale_colour_manual(values=c("#8b0000", "#107C10", "#0078D7")) +
    theme_tq() + theme2 +
    scale_y_continuous(labels = scales::percent, 
                       breaks = scales::pretty_breaks(n = 8)) +
    
    scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
                   
    labs(x        = "",
         y        = "Lợi nhuận",
         title    = str_glue("Lợi nhuận của 3 phương pháp khi đầu tư vào {title}"),
         subtitle = str_glue("
         Với <span style = 'color:#8b0000;'>phương pháp A</span> bạn đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(data$total_cash_invested_txt)}</span>, lợi nhuận là <span style = 'color:#8b0000;'>{last(data$profit_number_txt)}</span>, tương đương <span style = 'color:#8b0000;'>{last(data$profit_pct_txt)}</span> <br>
         Với <span style = 'color:#107C10;'>phương pháp B</span> bạn đã đầu tư tổng cộng <span style = 'color:#107C10;'>{last(data$planb_total_cash_invested_txt)}</span>, lợi nhuận là <span style = 'color:#107C10;'>{last(data$planb_profit_number_txt)}</span>, tương đương <span style = 'color:#107C10;'>{last(data$planb_profit_pct_txt)}</span> <br>
         Với <span style = 'color:#0078D7;'>phương pháp C</span> bạn đã đầu tư tổng cộng <span style = 'color:#0078D7;'>{last(data$planc_total_cash_invested_txt)}</span>, lợi nhuận là <span style = 'color:#0078D7;'>{last(data$planc_profit_number_txt)}</span>, tương đương <span style = 'color:#0078D7;'>{last(data$planc_profit_pct_txt)}</span>"))

}


##########################
###### PART 2 - ETF ######
##########################

# 1. Import data -----------------------
etf <- get_data_vnstock("040_dca_thi_truong_giam/01_data/raw/E1VFVN30.csv")

# 2. Tìm các đợt sụt giảm. -----------------------
# Chỉ lấy kết quả những đợt sụt giảm lớn hơn 10%.
etf_dd_tbl <- table_dd(etf) %>% 
  filter(Depth <= -0.1) %>% 
  select(From, To, Drawdown = Depth) %>% 
  arrange(From) %>% 
  mutate(down_period = c(1, 2, 3, 4 ,5, 6, 7))


# Dựa theo etf_dd_tbl, [From] là đỉnh, [To] là thời điểm giá ETF trở lại đỉnh cũ.
# Tìm thời điểm mà giá ETF thực sự giảm hơn 10% kể từ đỉnh.
# Đánh dấu các giai đoạn đó để thực hiện kế hoạch đầu tư
down_period_tbl <- etf %>% 
  cal_ret() %>% 
  mutate(down_period = case_when(date >= "2014-10-07" & date <= "2016-07-13" ~ 1,
                                 date >= "2017-07-06" & date <= "2017-10-12" ~ 2,
                                 date >= "2018-01-26" & date <= "2018-03-19" ~ 3,
                                 date >= "2018-04-10" & date <= "2021-01-11" ~ 4,
                                 date >= "2021-01-18" & date <= "2021-02-19" ~ 5,
                                 date >= "2021-07-05" & date <= "2021-11-26" ~ 6,
                                 date >= "2021-11-29" ~ 7,
                                 TRUE ~ 0)) %>% 
  
  # Tính drawdown
  mutate(growth    = cumprod(1 + returns) - 1,
         maxgrowth = cummax(growth + 1),
         drawdown  = (growth + 1) / maxgrowth - 1) %>% 
  select(date, down_period, drawdown) %>% 
  filter(down_period > 0) %>% 

  # Tìm thời điểm mà tỷ lệ sụt giảm vượt qua mốc 10%
  group_by(down_period) %>% 
  filter(drawdown <= -0.1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  left_join(etf_dd_tbl, by = "down_period") %>% 
  select(date, to = To, down_period)


# 3. THỰC HIỆN KẾ HOẠCH ĐẦU TƯ -----------------------
# period_tbl: tổng hợp data có đánh dấu thời kỳ mà giá ETF giảm hơn 10%
# so với đỉnh.
period_tbl <- etf %>% 
  mutate(period = case_when(date >= "2014-12-16" & date <= "2016-07-13" ~ 1,
                            date >= "2017-07-18" & date <= "2017-10-12" ~ 1,
                            date >= "2018-02-06" & date <= "2018-03-19" ~ 1,
                            date >= "2018-04-24" & date <= "2021-01-11" ~ 1,
                            date >= "2021-01-28" & date <= "2021-02-19" ~ 1,
                            date >= "2021-07-19" & date <= "2021-11-26" ~ 1,
                            date >= "2022-04-25" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(year  = lubridate::year(date),
         month = lubridate::month(date))


# Tính toán DCA của ba phương pháp
cash <- 2000000

etf_tbl <- period_tbl %>% 
  cal_dca_plan()

# 4. PLOT -----------------------
# Toàn bộ quá trình
dca_plot(etf_tbl)

# PLOT giá trung bình chứng chỉ quỹ
etf_tbl %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = average, color = "A: Mua liên tục")) +
  geom_line(aes(y = planb_average, color = "B: x2 tiền ")) +
  geom_line(aes(y = planc_average, color = "C: Chờ đạt đỉnh")) +
  scale_colour_manual(values=c("#8b0000", "#107C10", "#0078D7")) +
  theme_tq() + theme2 +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x = "",
       y = "",
       title = "Giá trung bình của ba phương pháp")


# Từ năm 2017
etf_tbl2017 <- period_tbl %>% 
  filter(date >= "2017-01-01" & date < "2022-01-01") %>% 
  cal_dca_plan()

dca_plot(etf_tbl2017)

###########################
###### PART 2 - DCDS ######
###########################

dcds <- read_csv("040_dca_thi_truong_giam/01_data/raw/DCDS.csv") %>% 
  filter(date >= "2013-10-08")

dcds_dd_tbl <- table_dd(dcds) %>% 
  filter(Depth <= -0.1) %>% 
  select(From, To, Drawdown = Depth) %>% 
  arrange(From) %>% 
  mutate(down_period = c(1, 2, 3, 4 ,5, 6, 7))

dcds_down_period_tbl <- dcds %>% 
  cal_ret() %>% 
  mutate(down_period = case_when(date >= "2014-03-26" & date <= "2014-08-27" ~ 1,
                                 date >= "2014-09-24" & date <= "2015-10-23" ~ 2,
                                 date >= "2015-11-17" & date <= "2016-04-11" ~ 3,
                                 date >= "2018-04-10" & date <= "2020-12-16" ~ 4,
                                 date >= "2021-01-18" & date <= "2021-02-18" ~ 5,
                                 date >= "2021-07-06" & date <= "2021-08-09" ~ 6,
                                 date >= "2021-11-28" ~ 7,
                                 TRUE ~ 0)) %>% 
  
  # Tính drawdown
  mutate(growth    = cumprod(1 + returns) - 1,
         maxgrowth = cummax(growth + 1),
         drawdown  = (growth + 1) / maxgrowth - 1) %>% 
  select(date, down_period, drawdown) %>% 
  filter(down_period > 0) %>% 
  
  # Lọc các thời điểm mà drawdown > 10%
  group_by(down_period) %>% 
  filter(drawdown <= -0.1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  left_join(dcds_dd_tbl, by = "down_period") %>% 
  select(date, to = To, down_period)

# Lên kế hoạch đầu tư
dcds_period_tbl <- dcds %>% 
  mutate(period = case_when(date >= "2014-05-14" & date <= "2014-08-27" ~ 1,
                            date >= "2014-12-09" & date <= "2015-10-23" ~ 1,
                            date >= "2016-01-18" & date <= "2016-04-11" ~ 1,
                            date >= "2018-04-26" & date <= "2020-12-16" ~ 1,
                            date >= "2021-01-28" & date <= "2021-02-18" ~ 1,
                            date >= "2021-07-19" & date <= "2021-08-09" ~ 1,
                            date >= "2022-04-19" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(year  = lubridate::year(date),
         month = lubridate::month(date))


# Tính toán DCA
dcds_tbl <- dcds_period_tbl  %>% 
  cal_dca_plan()

# PLOT
dca_plot(dcds_tbl)

# PLOT giá trung bình chứng chỉ quỹ
dcds_tbl %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = average, color = "A: Mua liên tục")) +
  geom_line(aes(y = planb_average, color = "B: x2 tiền ")) +
  geom_line(aes(y = planc_average, color = "C: Chờ đạt đỉnh")) +
  scale_colour_manual(values=c("#8b0000", "#107C10", "#0078D7")) +
  theme_tq() + theme2 +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","),
                     breaks = scales::pretty_breaks(n = 15)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x = "",
       y = "",
       title = "Giá trung bình khi đầu tư vào DCDS")

# Plot giá DCDS
dcds %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line()

dcds %>% 
  head.tail()

# DCDS 2017
dcds_tbl2017 <- dcds_period_tbl  %>% 
  filter(date >= "2017-01-01" & date < "2022-01-01") %>% 
  cal_dca_plan()

dca_plot(dcds_tbl2017)

############################
###### PART 3 - VESAF ######
############################

vesaf <- read_csv("040_dca_thi_truong_giam/01_data/raw/VESAF.csv")

# Table Drawdown
vesaf_dd_tbl <- table_dd(vesaf) %>% 
  filter(Depth <= -0.1) %>% 
  select(From, To, Drawdown = Depth) %>% 
  arrange(From) %>% 
  mutate(down_period = c(1, 2, 3))

vesaf_down_period_tbl <- vesaf %>% 
  cal_ret() %>% 
  mutate(down_period = case_when(date >= "2018-04-17" & date <= "2020-12-15" ~ 1,
                                 date >= "2021-01-26" & date <= "2021-02-23" ~ 2,
                                 date >= "2022-04-08" ~ 3,
                                 TRUE ~ 0)) %>% 
  
  # Tính drawdown
  mutate(growth    = cumprod(1 + returns) - 1,
         maxgrowth = cummax(growth + 1),
         drawdown  = (growth + 1) / maxgrowth - 1) %>% 
  select(date, down_period, drawdown) %>% 
  filter(down_period > 0) %>% 
  
  
  # Lọc các thời điểm mà drawdown > 10%
  group_by(down_period) %>% 
  filter(drawdown <= -0.1) %>% 
  slice(1) %>% 
  ungroup() %>% 
  left_join(vesaf_dd_tbl, by = "down_period") %>% 
  select(date, to = To, down_period)


# Lên kế hoạch đầu tư
vesaf_period_tbl <- vesaf %>% 
  mutate(period = case_when(date >= "2018-04-30" & date <= "2020-12-15" ~ 1,
                            date >= "2021-02-02" & date <= "2021-02-23" ~ 1,
                            date >= "2022-04-26" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(year  = lubridate::year(date),
         month = lubridate::month(date))


# Tính toán DCA
vesaf_tbl <- vesaf_period_tbl  %>% 
  filter(date >= "2017-01-01" & date < "2022-01-01") %>% 
  cal_dca_plan()

# PLOT
dca_plot(vesaf_tbl)
