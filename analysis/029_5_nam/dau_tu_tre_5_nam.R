# 1. Packages ####
source("header.R") # tidyverse, function get_data_investing

library(tidyquant)
library(ggrepel) # Label
library(ggsci) # colors

options(scipen = 999)

# 2. Import & clean data ####
# Raw data tu investing.com

etf <- get_data_investing("029_5_nam/01_data/raw/E1VFVN30_raw.csv") %>%
  rename(E1VFVN30 = price)

tcbf <- read_csv("029_5_nam/01_data/tidied/TCBF.csv")

# Tidy data

data <- etf %>%
  filter(date >= first(tcbf$date)) %>%
  left_join(tcbf) %>%
  fill(TCBF) %>%
  
  mutate(year  = year(date),
         month = month(date)) %>%
  
  group_by(year, month) %>%
  filter(date == first(date)) %>%
  ungroup() %>%
  
  mutate(date = FLOOR_DATE(as_date(date), by = "month")) %>%
  select(-year, -month)

# 3. Calculate returns ####

# 3.1 Tinh annualized return cua TCBF = 7.5%/nam
 
# data %>%
#   select(date, TCBF) %>%
#   filter(date >= "2016-01-01" & date < "2022-01-01") %>%
#   tq_transmute(select     = TCBF,
#                mutate_fun = periodReturn,
#                period     = "yearly",
#                col_rename = "returns") %>%
#   tq_performance(Ra              = returns,
#                  performance_fun = Return.annualized)

# 3.2 Function ke hoach dau tu

ke_hoach_dau_tu <- function(money  = 3000000) {
  
  # Money = so tien dau tu moi thang
  # Period: thang. Vd: 12 la tre 12 thang
  
  period <- c(0, 12, 24, 36, 48, 60) 
  
  for(i in period) {
    
    result <- data %>%
      mutate(cash_tcbf = if_else(date < first(date) %m+% months(i), money, 0)) %>% 
      mutate(cash_etf  = if_else(date >= first(date) %m+% months(i), money, 0)) %>% 
      
      mutate(n_tcbf = cash_tcbf / TCBF,
             n_etf  = cash_etf / E1VFVN30) %>% 
      
      mutate(total_tcbf = cumsum(n_tcbf),
             total_etf  = cumsum(n_etf)) %>%
      
      mutate(value_tcbf  = total_tcbf * TCBF,
             value_etf   = total_etf  * E1VFVN30,
             total_asset = value_tcbf + value_etf)
    
    # Xuat ket qua cua cac period
    assign(str_glue("result_{i}"), result, envir = .GlobalEnv)
    
  }
 
  # Group data
  # Sau khi co ket qua cua cac period rieng le thi gop lai thanh mot tep data hoan chinh
  
  total_asset_tbl <- result_0 %>% 
    select(date, p0 = total_asset) %>% 
    left_join(result_12 %>% select(date, p12 = total_asset), by = "date") %>% 
    left_join(result_24 %>% select(date, p24 = total_asset), by = "date") %>% 
    left_join(result_36 %>% select(date, p36 = total_asset), by = "date") %>%
    left_join(result_48 %>% select(date, p48 = total_asset), by = "date") %>%
    left_join(result_60 %>% select(date, p60 = total_asset), by = "date")
  
  assign(str_glue("total_asset_tbl"), total_asset_tbl, envir = .GlobalEnv)
  
}

# 4. Execute ####

ke_hoach_dau_tu()

# 5. PLOT ####
# Color:
color <- pal_nejm(alpha = 1)(6)

# Plot:
total_asset_tbl %>% 
  pivot_longer(cols      = c(2:7), 
               names_to  = "portfolio", 
               values_to = "value") %>% 
  
  # Tao label cho chart
  mutate(value_text = if_else(date == max(date),
                         scales::number(value, big.mark = ".", decimal.mark = ","),
                         NA_character_)) %>% 
  # PLOT:
  ggplot(aes(x = date, y = value, color = portfolio)) +
  
  coord_cartesian(xlim = c(min(total_asset_tbl$date), max(total_asset_tbl$date) + 300)) +
  
  geom_label_repel(aes(label  = value_text),
                  fontface    = "bold",
                  nudge_x     = 300,
                  na.rm       = TRUE,
                  label.size  = 0.8,
                  show.legend = FALSE) +
  
  geom_line(size = 1.2) +
  
  theme_tq() + theme2 + 
  
  scale_color_manual(values = color,
                     labels = c("Mua ngay", "Chờ 1 năm", "Chờ 2 năm",
                                "Chờ 3 năm", "Chờ 4 năm", "Chờ 5 năm")) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  scale_y_continuous(labels = scales::number_format(big.mark     = ".",
                                                    decimal.mark = ",")) +
  
  labs(color = "Danh mục",
       x = "",
       y = "Tổng giá trị tài sản",
       title = "Đầu tư muộn không mang lại nhiều lợi nhuận",
       caption = str_glue("Giả định đầu tư 3.000.000 Đồng/tháng vào thị trường cổ phiếu 
                          bằng cách mua ETF E1VFVN30 ngay tại thời điểm {first(total_asset_tbl$date)} 
                          và so sánh với các trường hợp: 1, 2... 5 năm sau 
                          mới bắt đầu đầu tư vào thị trường cổ phiếu"))
