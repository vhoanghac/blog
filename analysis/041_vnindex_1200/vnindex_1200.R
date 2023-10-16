# 1. Packages -----------------------
source("header.R")
library(tidyquant)
library(timetk)

# 2. Import data
etf <- get_data_vnstock("041_vnindex_1200/01_data/raw/E1VFVN30.csv")

# 3. Tidy
cash <- 3000000

etf_tbl <- etf %>% 
  filter(date >= "2018-04-01") %>% 
  mutate(year  = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  group_by(month, year) %>% 
  mutate(cash = ifelse(date == min(date), cash, 0)) %>% 
  ungroup() %>% 
  
  # DCA plan:
  mutate(q = cash / price,
         total_q             = cumsum(q),
         total_cash_invested = cumsum(cash),
         total_value         = total_q * price,
         profit_pct          = (total_value / total_cash_invested) - 1,
         profit_number       = total_value - total_cash_invested,
         average             = total_cash_invested / total_q) %>% 
  
  mutate(across(contains("total_cash"), ~scales::number(., big.mark = ".", decimal.mark = ","), .names = "{col}_txt")) %>% 
  mutate(across(contains("profit_pct"), ~scales::percent(., accuracy = 0.01, big.mark = ".", decimal.mark = ","), .names = "{col}_txt")) %>% 
  mutate(across(contains("profit_number"), ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1), .names = "{col}_txt")) %>% 
  mutate(across(contains("average"), ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1), .names = "{col}_txt"))


# 4. Analysis

etf_tbl %>% 
  ggplot(aes(x = date,
             y = profit_pct)) +
  
  geom_line(color = "firebrick4") +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +  theme_tq() + theme2 + 
  
  coord_cartesian(xlim = c(min(etf_tbl$date), max(etf_tbl$date) + 200)) +
  
  
  scale_y_continuous(labels = scales::percent, 
                     breaks = scales::pretty_breaks(n = 10)) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) + 
  
  labs(x = "",
       y = "Lợi nhuận (phần trăm)",
       title = "Tỷ lệ lợi nhuận khi DCA liên tục kể từ đỉnh thị trường năm 2018",
       
       subtitle = str_glue("
         Bạn đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(etf_tbl$total_cash_invested_txt)}</span>, lợi nhuận là <span style = 'color:#8b0000;'>{last(etf_tbl$profit_number_txt)}</span>, tương đương <span style = 'color:#8b0000;'>{last(etf_tbl$profit_pct_txt)}</span>"))
