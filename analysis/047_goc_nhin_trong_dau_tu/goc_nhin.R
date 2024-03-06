# 1. Packages #######
source("header.R")
library(tidyquant)
library(timetk)
library(ggrepel)

# 2. Import data
etf <- get_data_vnstock("047_goc_nhin_trong_dau_tu/01_data/raw/etf_E1VFVN30.csv")

vcbf <- read_csv("047_goc_nhin_trong_dau_tu/01_data/raw/mf_VCBFTBF.csv")



# 3. Calculate growth

# 3.1: Đầu tư 1 lần
calculate_growth <- function(data, start_year, end_year){
  
  data %>% 
    
    filter(date >= paste0(start_year, "-01-01") & date < paste0(end_year, "-01-01")) %>% 
    
    summarise_by_time(.date_var = date,
                      .by = "month",
                      price = first(price),
                      .type = "floor") %>% 
    
    tq_transmute(select     = price,
                 mutate_fun = periodReturn,
                 period     = "monthly",
                 col_rename = "returns") %>% 
    
    mutate(growth = cumprod(1 + returns) - 1) %>% 
    mutate(day    = row_number()) %>% 
    mutate(year   = as_factor(start_year))
  
}

# 3.2: Đầu tư 2 lần

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
    select(day, year, growth = profit_pct)
}



# 4. Phân tích DCA

dca_years <- c(2015, 2018)

dca_tbl <- map_dfr(dca_years, ~ calculate_dca_growth(etf, 3000000, .x, .x + 3)) %>% 
  mutate(name = if_else(year == 2015, "An", "Bảo")) %>% 
  group_by(year) %>% 
  mutate(label_txt = if_else(day == max(day),
                             paste(name, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_))

# Plot:
dca_tbl %>% 
  ggplot(aes(x = day, y = growth, col = year)) +
  
  geom_line() + theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 200,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  geom_hline(yintercept = 0) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  scale_color_manual(values = c("#f94144", "#1d3557")) +
  
  labs(x = "Ngày",
       y = "Tăng trưởng",
       title = "3 năm đầu tư của An và Bảo",
       caption = "An: bắt đầu năm 2015.
       Bảo: bắt đầu năm 2018.
       Cả hai đều đầu tư 3 triệu đồng mỗi tháng vào ETF E1VFVN30.
       Nguồn dữ liệu: tổng hợp bằng vnstock")
  

# Suy nghĩ vẽ plot Bảo +4


# 5. Phân tích 1 lần

# Plot VCBF-TBF 2016 đến 2020

vcbf %>% 
  filter(date >= "2018-01-01" & date < "2021-01-01") %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns) - 1) %>% 
  ggplot(aes(x = date, y = growth)) + 
  
  geom_line(linewidth = 0.8, color = "#00BA38") +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = seq(as.Date("2018-01-01"), as.Date("2023-12-28"), by = "year"), date_labels = "%Y") +
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của quỹ VCBF-TBF: 2018 đến hết 2020",
       caption = "VCBF-TBF là quỹ đầu tư cân bằng  với danh mục bao gồm: cổ phiếu và trái phiếu
       Nguồn: tổng hợp Fmarket và website vcbf.com")


# 3 năm
years <- c(2016, 2018, 2020)

growth_tbl <- map_dfr(years, ~ calculate_growth(vcbf, .x, .x + 3)) %>%
  select(day, year, growth) %>% 
  group_by(year) %>% 
  mutate(label_txt = if_else(day == max(day),
                             paste(year, ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)), NA_character_)) %>%
  ungroup()
  

growth_tbl %>% 
  ggplot(aes(x = day, y = growth, color = year)) +
  
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0) +
  
  theme_tq() + theme +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 15,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x = "Số Tháng",
       y = "Tăng Trưởng",
       title = "Kết quả sau 3 năm đầu tư vào VCBF-TBF",
       subtitle = "Bắt đầu tại ngày giao dịch đầu tiên của ba năm 2016, 2018 và 2020.",
       caption = "VCBF-TBF là quỹ đầu tư cân bằng  với danh mục bao gồm: cổ phiếu và trái phiếu
       Nguồn: tổng hợp Fmarket và website vcbf.com")



growth_tbl %>% 
  ggplot(aes(x = day, y = growth, color = year)) +
  
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0) +
  
  theme_tq() + theme +
  
  
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x = "Số Tháng",
       y = "Tăng Trưởng")

