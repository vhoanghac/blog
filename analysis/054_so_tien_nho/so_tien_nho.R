# 1. Packages #### ####
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)
library(cowplot)

# 2. Import data #### ####
mf_dcds <- read_csv("053_dau_tu_ngay_dinh/01_data/01_raw/mf_DCDS.csv")

# 3. Functions #### ####

# Step 1: Normal returns
calculate_returns <- function(data){
  
  data %>% 
    tq_mutate(select     = price,
              mutate_fun = periodReturn,
              period     = "daily",
              col_rename = "returns")
}

# Step 2: Normal DCA 
calculate_dca_stop <- function(data, cash, stop_date){
  
  data %>% 
    mutate(month             = month(date),
           year              = year(date),
           first_trading_day = !duplicated(paste(year, month)),
           within_dca_period = date <= stop_date) %>% 
    
    # Calculate units purchased only within DCA period
    mutate(units_purchased = if_else(first_trading_day & within_dca_period, cash / price, 0)) %>% 
    
    # Calculate total units and total cash invested
    mutate(total_units = cumsum(units_purchased),
           total_cash  = cumsum(if_else(first_trading_day & within_dca_period, cash, 0))) %>% 
    
    mutate(total_value = total_units * price,
           profit      = total_value - total_cash,
           profit_pct  = total_value / total_cash - 1) %>% 
    
    select(-month, -year, -within_dca_period) %>% 
    
    mutate(label_txt = if_else(date == max(date),
                               paste("Tăng trưởng: ", scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))
  
}

# Step 3: Calculate portfolio value after DCA period ends
calculate_dca_stop_value <- function(data) {
  data %>% 
    mutate(total_value = total_units * price,
           profit      = total_value - total_cash,
           profit_pct  = total_value / total_cash - 1) %>% 
    
    mutate(label_txt = if_else(date == max(date),
                               paste("Tăng trưởng: ", scales::percent(profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))
}

# Step 4: Analyze data with stop date
analyze_data <- function(data, start_date, stop_date, cash) {
  
  data_dca <- data %>%
    filter(date >= start_date) %>%
    calculate_returns() %>%
    calculate_dca_stop(cash, stop_date)
  
  data_dca_stop <- calculate_dca_stop_value(data_dca)
  
  return(data_dca_stop)
}

# 3.3 Tidy CPI Data
prepare_cpi_data <- function(data, start_date) {
  data %>%
    mutate(date = parse_date(date, format = "%b-%Y")) %>%
    arrange(date) %>%
    filter(date >= start_date) %>%
    mutate(cpi_factor = 1 + cpi, cum_inflation = cumprod(cpi_factor))
}


# 4 Variables #### ####
cash <- 3000000

# Normal plots
common_layers <- list(
  geom_line(aes(y = total_value, color = "Giá trị danh mục")),
  geom_area(aes(y = total_cash, fill = "Tiền đầu tư"), alpha = 0.2),
  theme_tq(), 
  theme2,
  scale_y_continuous(labels = scales::number_format(scale = 0.000001),
                     breaks = scales::pretty_breaks(n = 10)), 
  scale_x_date(breaks = scales::pretty_breaks(n = 6)),
  scale_color_manual(values = "firebrick4"),
  scale_fill_manual(values = "gray2"))

# Inflation plots
common_layers_inf <- list(
  geom_line(aes(y = real_total_value, color = "Giá trị danh mục")),
  geom_area(aes(y = real_cash, fill = "Tiền đầu tư"), alpha = 0.8),
  theme_tq(), 
  theme2,
  scale_y_continuous(labels = scales::number_format(scale = 0.000001),
                     breaks = scales::pretty_breaks(n = 10)), 
  scale_x_date(breaks = scales::pretty_breaks(n = 6)),
  scale_color_manual(values = "firebrick4"),
  scale_fill_manual(values = "gray"))


# I - Phân tích không lạm phát

# 5. Phân tích DCDS

# 5.1 DCDS 2014 - 2016
# CAGR 2014 - 2016: 6.68%
# CAGR 2017 - 2024: 14.33%

mf_dcds_2014 <- analyze_data(mf_dcds, start_date = "2014-01-01", stop_date = "2016-12-31", cash = cash)

# Plot 
mf_dcds_2014 %>% 
  ggplot(aes(x = date)) +
  
  common_layers + 
  
  annotate("text", 
           x      = as.Date("2016-01-01"), 
           y      = 350000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 237%",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2016-06-01"), 
           y      = 300000000, 
           size   = 9,
           family = "stix",
           label  = "Tiền đầu tư: 108.000.000 đ",
           color  = "firebrick4") + 

  annotate("text", 
           x      = as.Date("2016-05-01"), 
           y      = 250000000, 
           size   = 9,
           family = "stix",
           label  = "Lợi nhuận: 255.711.564 đ",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư DCDS từ 2014 đến 2016",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2014 đến hết năm 2016.
       Nguồn: Fmarket và website của quỹ")


# 5.2 DCDS 2017 - 2024
# CAGR = 6.96%
mf_dcds_2017 <- analyze_data(mf_dcds, start_date = "2017-01-01", stop_date = "2024-12-31", cash = cash)

mf_dcds_2017 %>% 
  ggplot(aes(x = date)) +
  
  common_layers + 
  
  annotate("text", 
           x      = as.Date("2019-01-01"), 
           y      = 450000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 66.9%",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2019-05-01"), 
           y      = 400000000, 
           size   = 9,
           family = "stix",
           label  = "Tiền đầu tư: 276.000.000 đ",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2019-04-01"), 
           y      = 350000000, 
           size   = 9,
           family = "stix",
           label  = "Lợi nhuận: 184.557.072 đ",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư DCDS từ năm 2017 đến nay",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2017 nay.
       Nguồn: Fmarket và website của quỹ")


# 5.3 Price Plot:

main_plot <- mf_dcds %>%
  filter(date >= "2014-01-01") %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(color = "firebrick4") +
  theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x       = "",
       y       = "Giá",
       title   = "Giá chứng chỉ quỹ mở DCDS 2014 - 2024",
       caption = "Nguồn: Fmarket và website của quỹ") 
  

# 5.4 DCDS 2014 - 2016

# 5.4.1 Zoomed-In Plot (2015-2016)
zoomed_plot <- mf_dcds %>%
  filter(date >= "2014-01-01" & date <= "2015-12-31") %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(color = "firebrick4") +
  theme_tq() + theme(
    axis.text = element_text(size = 8),
    axis.title = element_blank()
  ) + theme + 
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(breaks = scales::pretty_breaks(n = 4))+
  labs(title = "2014 - 2015")
  
  
# 5.4.2 2017 - 2024
mf_dcds %>%
  filter(date >= "2017-01-01") %>%
  ggplot(aes(x = date, y = price)) +
  geom_line(color = "firebrick4") +
  theme_tq() + theme(
    axis.text = element_text(size = 8),
    axis.title = element_blank()
  ) + theme + 
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 0.0001),
                     breaks = scales::pretty_breaks(n = 8))+
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "2017 - 2024")

# 5.4.3 Combine Plots
final_plot <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(zoomed_plot, x = 0.1, y = 0.5, width = 0.4, height = 0.4)


final_plot


# II - Phân tích lạm phát #### ####

# 1. Import data
cpi_raw <- read_csv("054_so_tien_nho/01_data/01_raw/cpi_mom.csv")

# 2. Tidy cpi data
cpi_2014 <- prepare_cpi_data(cpi_raw, "2014-01-01")
cpi_2017 <- prepare_cpi_data(cpi_raw, "2017-01-01")

# 3. Convert Daily Fund Data to Monthly 
mf_dcds_monthly <- mf_dcds %>%
  
  group_by(year  = year(date), 
           month = month(date)) %>%
  
  filter(date == min(date)) %>% 
  
  mutate(date = floor_date(date, "month")) %>% 
  ungroup() %>%
  select(-year, -month) %>% 
  filter(date < "2024-08-01")

# 4. Merge CPI Data with Fund Data 
mf_dcds_inf_2014_tbl <- mf_dcds_monthly %>%
  filter(date >= "2014-01-01") %>% 
  left_join(cpi_2014 %>% select(date, cpi_factor, cum_inflation), by = "date")


mf_dcds_inf_2017_tbl <- mf_dcds_monthly %>%
  filter(date >= "2017-01-01") %>% 
  left_join(cpi_2017 %>% select(date, cpi_factor, cum_inflation), by = "date")

# 5. Function: Calculate DCA (inflation)

calculate_dca_inflation <- function(data, cash, stop_date){
  
  data %>% 
    mutate(month             = month(date),
           year              = year(date),
           first_trading_day = !duplicated(paste(year, month)),
           within_dca_period = date <= stop_date) %>% 
    
    # Calculate units purchased only within DCA period
    mutate(units_purchased = if_else(first_trading_day & within_dca_period, cash / price, 0)) %>% 
    
    # Calculate total units and total cash invested
    mutate(total_units = cumsum(units_purchased),
           total_cash  = cumsum(if_else(first_trading_day & within_dca_period, cash, 0))) %>% 
    
    # Adjust total value for inflation
    mutate(total_value      = total_units * price,
           real_total_value = total_units * price / cum_inflation,
           real_cash        = total_cash / cum_inflation,
           real_profit      = real_total_value - real_cash,
           real_profit_pct  = real_total_value / real_cash - 1) %>% 
    
    select(-month, -year, -within_dca_period, -first_trading_day) %>% 
    
    mutate(label_txt = if_else(date == max(date),
                               paste("Tăng trưởng: ", scales::percent(real_profit_pct, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))
}

# 6. DCDS 2014 - 2016
# CAGR: 2014-2016: 7.27%
# CAGR: 2017-2024: 11.47%

mf_dcds_inf_2014 <- calculate_dca_inflation(mf_dcds_inf_2014_tbl, cash = 3000000, stop_date = as.Date("2016-12-31"))

# Plot 
mf_dcds_inf_2014 %>% 
  ggplot(aes(x = date)) +
  
  common_layers_inf + 
  
  annotate("text", 
           x      = as.Date("2016-01-01"), 
           y      = 330000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 245%",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2016-06-01"), 
           y      = 290000000, 
           size   = 9,
           family = "stix",
           label  = "Tiền đầu tư: 79.579.273 đ",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2016-06-01"), 
           y      = 250000000, 
           size   = 9,
           family = "stix",
           label  = "Lợi nhuận: 194.868.613 đ",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư DCDS từ 2014 đến 2016 (điều chỉnh lạm phát)",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2014 đến hết năm 2016.
       Nguồn: Fmarket và website của quỹ")


# 7. DCDS 2017-2024

mf_dcds_inf_2017 <- calculate_dca_inflation(mf_dcds_inf_2017_tbl, cash = 3000000, stop_date = as.Date("2024-12-31"))

# Plot
mf_dcds_inf_2017 %>% 
  ggplot(aes(x = date)) +
  
  common_layers_inf + 
  
  annotate("text", 
           x      = as.Date("2018-05-01"), 
           y      = 350000000, 
           size   = 9,
           family = "stix",
           label  = "Tổng lợi nhuận: 71%",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2018-10-01"), 
           y      = 310000000, 
           size   = 9,
           family = "stix",
           label  = "Tiền đầu tư: 218.152.289 đ",
           color  = "firebrick4") + 
  
  annotate("text", 
           x      = as.Date("2018-09-01"), 
           y      = 270000000, 
           size   = 9,
           family = "stix",
           label  = "Lợi nhuận: 156.309.524 đ",
           color  = "firebrick4") +
  
  labs(x       = "",
       y       = "Giá Trị (triệu đồng)",
       title   = "Đầu tư DCDS từ năm 2017 đến nay (điều chỉnh lạm phát)",
       caption = "Giả định đầu tư 3.000.000 VND vào ngày đầu tiên của mỗi tháng từ năm 2017.
       Nguồn: Fmarket và website của quỹ")