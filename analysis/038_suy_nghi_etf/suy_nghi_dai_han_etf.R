# 1. Packages #### ####
source("header.R")
library(tidyquant)
library(ggrepel)

# 2. Import data #### ####
etf <- get_data_vnstock("038_suy_nghi_etf/01_data/raw/E1VFVN30.csv")

# 3. Function và các thiết lập khác #### #### 
# 3.1 Tính lợi nhuận đầu tư một lần
calculate_growth <- function(data, start_year, end_year){
  
  data %>% 
    
    filter(date >= paste0(start_year, "-01-01") & date < paste0(end_year, "-01-01")) %>% 
    tq_transmute(select     = price,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 col_rename = "returns") %>% 
    
    mutate(growth = cumprod(1 + returns) - 1) %>% 
    mutate(day    = row_number()) %>% 
    mutate(year   = as_factor(start_year))
  
}

# 3.2 Tính lợi nhuận DCA
calculate_dca_growth <- function(data, cash, start_year, end_year){
  
  cash <- cash
  
  data %>%
    filter(date >= paste0(start_year, "-01-01") & date < paste0(end_year, "-01-01")) %>% 
    mutate(month = month(date),
           year  =  as_factor(start_year)) %>% 
    group_by(month, year) %>% 
    mutate(cash = if_else(date == min(date), cash, 0)) %>% 
    ungroup() %>%
    mutate(q = cash / price) %>% 
    
    
    group_by(year) %>% 
    mutate(total_q             = cumsum(q),
           total_cash_invested = cumsum(cash),
           total_value         = total_q * price,
           profit_pct          = (total_value / total_cash_invested) - 1) %>% 
    mutate(day = row_number()) %>% 
    ungroup() %>% 
    select(day, year, profit_pct)
}

# 3.3 Plot

label_1y <- geom_label_repel(aes(label        = label_txt),
                             force_pull       = 0,
                             nudge_x          = 80,
                             direction        ="y",
                             max.overlaps     = 10,
                             segment.size     = 0.4,
                             segment.linetype = 2,
                             hjust            = 0,
                             size             = 7)

label_2y <- geom_label_repel(aes(label        = label_txt),
                             force_pull       = 0,
                             nudge_x          = 150,
                             direction        ="y",
                             max.overlaps     = 10,
                             segment.size     = 0.4,
                             segment.linetype = 2,
                             hjust            = 0,
                             size             = 7)

label_3y <- geom_label_repel(aes(label        = label_txt),
                             force_pull       = 0,
                             nudge_x          = 200,
                             direction        ="y",
                             max.overlaps     = 10,
                             segment.size     = 0.4,
                             segment.linetype = 2,
                             hjust            = 0,
                             size             = 7)

####################
### Đầu tư 1 lần ###
####################

# 4. Đầu tư 1 năm #### ####
# 4.1 Tính toán
etf_1y_ret <- etf %>% 
  filter(date >= "2015-01-01" & date < "2023-01-01") %>% 
  
  # Factor để vẽ biểu đồ có thể hiện thị riêng từng năm
  mutate(year = as.factor(lubridate::year(date))) %>% 
  
  group_by(year) %>% 
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "returns") %>% 
  
  mutate(growth = cumprod(1 + returns) - 1,
         day    = row_number()) %>% 
  
  # Label để hiển thị trên chart
  mutate(label_txt = if_else(date == max(date),
                             paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
  ungroup()


# 4.2 Plot
etf_1y_plot <- etf_1y_ret %>% 
  ggplot(aes(x = day, y = growth, color = year)) +
  geom_line() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  labs(x       = "Ngày",
       y       = "Lợi nhuận",
       title   = "Lợi nhuận sau một năm đầu tư vào E1VFVN30",
       caption = "Lựa chọn thời điểm đầu tư là vào đầu mỗi năm.
       Sau đó tính lợi nhuận của khoản đầu tư trong một năm (khoảng 250 ngày giao dịch).
       Nguồn dữ liệu: tổng hợp bằng vnstock")


etf_1y_plot +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), 
                     expand = expansion(add = 1)) +
  
  label_1y


# 4.3 Facet Wrap 
etf_1y_plot + 
  facet_wrap(~year, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 



# 5. Đầu tư 2 năm #### ####
# years là số năm bắt đầu (hay danh mục đầu tư)
years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)

# 5.1 Tính quãng thời gian đầu tư là hai năm kể từ lúc bắt đầu: .x + 2
etf_2y_ret <- map_dfr(years, ~ calculate_growth(etf, .x, .x + 2)) %>%
  select(day, year, growth) %>% 
  group_by(year) %>% 
  mutate(label_txt = if_else(day == max(day),
                             paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))

# 5.2 PLOT:
etf_2y_plot <- etf_2y_ret %>% 
  ggplot(aes(x = day, y = growth, col = year)) +
  geom_line() + theme_tq() + theme +

  labs(x       = "Ngày",
       y       = "Lợi nhuận",
       title   = "Lợi nhuận sau hai năm đầu tư vào E1VFVN30",
       caption = "Lựa chọn thời điểm đầu tư là vào đầu mỗi năm.
       Sau đó tính lợi nhuận của khoản đầu tư trong hai năm tiếp theo (khoảng 500 ngày giao dịch).
       Nguồn dữ liệu: tổng hợp bằng vnstock")


etf_2y_plot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), 
                     expand = expansion(add = 1)) +

  label_2y


# 5.3 Facet Wrap:
etf_2y_plot + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap(~year, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")


# 6. Đầu tư 3 năm #### ####
years_3y <- c(2015, 2016, 2017, 2018, 2019, 2020)

# 6.1 Tính toán
etf_3y_ret <- map_dfr(years_3y, ~ calculate_growth(etf, .x, .x + 3)) %>%
  select(day, year, growth) %>% 
  group_by(year) %>% 
  mutate(label_txt = if_else(day == max(day),
                             paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))

# 6.2 PLOT:
etf_3y_plot <- etf_3y_ret %>% 
  
  ggplot(aes(x = day, y = growth, col = year)) +
  geom_line() + theme_tq() + theme +
  
  labs(x       = "Ngày",
       y       = "Lợi nhuận",
       title   = "Lợi nhuận sau ba năm đầu tư vào E1VFVN30",
       caption = "Lựa chọn thời điểm đầu tư là vào đầu mỗi năm.
       Sau đó tính lợi nhuận của khoản đầu tư trong ba năm tiếp theo (khoảng 750 ngày giao dịch).
       Nguồn dữ liệu: tổng hợp bằng vnstock")


etf_3y_plot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), 
                     expand = expansion(add = 1)) +
  
  label_3y


# 6.3 Facet Wrap:
etf_3y_plot + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap(~year, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")


#############################
############ DCA ############
#############################

cash <- 3000000

# 7. DCA 1 năm #### ####

# 7.1 Tính toán
dca_1y_ret <- etf %>% 
  filter(date >= "2015-01-01" & date < "2023-01-01") %>% 
  mutate(month = month(date),
         year  =  as_factor(lubridate::year(date))) %>% 
  group_by(month, year) %>% 
  mutate(cash = if_else(date == min(date), cash, 0)) %>% 
  ungroup() %>%
  
  mutate(q = cash / price) %>% 
  
  group_by(year) %>% 
  mutate(total_q             = cumsum(q),
         total_cash_invested = cumsum(cash),
         total_value         = total_q * price,
         profit_pct          = (total_value / total_cash_invested) - 1) %>% 
  
  mutate(label_txt = if_else(date == max(date),
                             paste(year, ":", scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>% 
  mutate(day = row_number()) %>% 
  ungroup() %>% 
  select(day, year, profit_pct, label_txt)

# 7.2 PLOT
dca_1y_plot <- dca_1y_ret %>% 
  ggplot(aes(x = day, y = profit_pct, color = year)) +
  geom_line() + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  labs(x       = "Ngày",
       y       = "Lợi nhuận",
       title   = "Lợi nhuận sau một năm DCA vào E1VFVN30",
       caption = "Thời điểm bắt đầu đầu tư là vào đầu mỗi năm. 
       Thực hiện DCA vào ngày giao dịch đầu tiên của mỗi tháng.
       Tính lợi nhuận so với vốn gốc trong một năm, tương đương 250 ngày giao dịch.
       Nguồn dữ liệu: tổng hợp bằng vnstock.")


dca_1y_plot + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), 
                     expand = expansion(add = 1)) +
  label_1y

# 7.3 FACET WRAP
dca_1y_plot +
  facet_wrap(~year, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 


# 8. DCA 2 năm ### #### 
dca_years <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021)

# Tính quãng thời gian đầu tư là hai năm kể từ lúc bắt đầu: .x + 2
dca_2y_ret <- map_dfr(dca_years, ~ calculate_dca_growth(etf, 3000000, .x, .x + 2)) %>%
  group_by(year) %>% 
  mutate(label_txt = if_else(day == max(day),
                             paste(year, ":", scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))

# PLOT
dca_2y_plot <- dca_2y_ret %>% 
  ggplot(aes(x = day, y = profit_pct, col = year)) +
  geom_line() + theme_tq() + theme +
  
  labs(x       = "Ngày",
       y       = "Lợi nhuận",
       title   = "Lợi nhuận sau hai năm DCA vào E1VFVN30",
       caption = "Thời điểm bắt đầu đầu tư là vào đầu mỗi năm. 
       Thực hiện DCA vào ngày giao dịch đầu tiên của mỗi tháng.
       Tính lợi nhuận so với vốn gốc trong hai năm, tương đương 500 ngày giao dịch.
       Nguồn dữ liệu: tổng hợp bằng vnstock.")


dca_2y_plot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), 
                     expand = expansion(add = 1)) +
  
  label_2y


# FACET WRAP

dca_2y_plot +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap(~year, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")


# 9. DCA 3 năm #### ####

dca_years_3y <- c(2015, 2016, 2017, 2018, 2019, 2020)

dca_3y_ret <- map_dfr(dca_years_3y, ~ calculate_dca_growth(etf, 3000000, .x, .x + 3)) %>%
  group_by(year) %>% 
  mutate(label_txt = if_else(day == max(day),
                             paste(year, ":", scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))


dca_3y_plot <- dca_3y_ret %>% 
  ggplot(aes(x = day, y = profit_pct, col = year)) +
  geom_line() + theme_tq() + theme +
  
  labs(x = "Ngày",
       y = "Lợi nhuận",
       title = "Lợi nhuận sau ba năm DCA vào E1VFVN30",
       caption = "Thời điểm bắt đầu đầu tư là vào đầu mỗi năm. 
       Thực hiện DCA vào ngày giao dịch đầu tiên của mỗi tháng.
       Tính lợi nhuận so với vốn gốc trong ba năm, tương đương 750 ngày giao dịch.
       Nguồn dữ liệu: tổng hợp bằng vnstock.")


dca_3y_plot +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), 
                     expand = expansion(add = 1)) +
  
  label_3y


# FACET WRAP
dca_3y_plot +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  facet_wrap(~year, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")