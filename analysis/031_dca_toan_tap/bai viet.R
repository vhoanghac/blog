# 1. Packages #### ####
source("header.R") #Tidyverse, data.table, lubridate, and ggtext
library(tidyquant)

options(scipen = 999)

# 2. Import & clean data #### ####

# 2.1 Raw data tu investing.com
etf <- get_data_investing("031_dca_toan_tap/01_data/raw/E1VFVN30 Historical Data.csv")

mwg <- get_data_investing("031_dca_toan_tap/01_data/raw/MWG Historical Data.csv",
                               format = "%m/%d/%Y")

# 2.2 Raw data Bitcoin
bitcoin <- tq_get("CBBTCUSD", 
                   get  = "economic.data") %>% 
  select(date, price) %>% 
  na.locf()

# Tidied bitcoin data up to 2022-11-25: read_csv("031_dca_toan_tap/01_data/tidied/bitcoin.csv")


# 3. Phan tich:
# Code chua toi uu hoa
# Lua chon data = mwg, etf, bitcoin
# cash: So tien dau tu moi thang. DCA lien tuc
# ls_cash: Lump sum cash. Dau tu mot lan
# dcastop_cash: Dca toi mot moc co dinh

calculateDCA("etf", 
             from         = "2016-01-01",
             cash         = 3000000, 
             ls_cash      = 100000000, 
             dcastop_cash = 100000000,
             plot         = "stop")


# Function:
calculateDCA <- function(data         = "mwg",
                         from         = "2014-10-07",
                         cash         = 3000000,
                         ls_cash      = 100000000,
                         dcastop_cash = 100000000,
                         plot         = c("dca", "lumpsum", "stop")){
  
  x <- get(data)
  
  data_tbl <- x %>% 
    filter(date >= from) %>% 
    mutate(month = month(date),
           year  = year(date)) %>% 
    group_by(month, year) %>% 
    mutate(cash = if_else(date == min(date), cash, 0 )) %>%
    ungroup() %>% 
    
    # 2.0 Calculation #### ####
    # q = quantity. So luong chung khoan
  
    # 2.1 - DCA
    mutate(q = cash / price) %>%
    
    mutate(total_q             = cumsum(q),
           total_cash_invested = cumsum(cash),
           total_value         = total_q * price,
           profit_pct          = (total_value / total_cash_invested) - 1,
           profit_number       = total_value - total_cash_invested,
           average             = total_cash_invested / total_q) %>% 
    
    # 2.2 - Lump sum
    mutate(ls_cash = if_else(date == min(date), ls_cash, 0)) %>% 
    
    mutate(ls_q = ls_cash / price) %>% 
    
    mutate(ls_total_q             = cumsum(ls_q),
           ls_total_cash_invested = cumsum(ls_cash),
           ls_total_value         = ls_total_q * price,
           ls_profit_pct          = (ls_total_value / ls_total_cash_invested) - 1,
           ls_profit_number       = ls_total_value - ls_total_cash_invested,
           ls_average             = ls_total_cash_invested / ls_total_q) %>% 
    
    # 2.3 - DCA stop
    mutate(dca2_cash = if_else(cumsum(cash) < dcastop_cash, cash, 0 )) %>% 
    
    mutate(dca2_q = dca2_cash / price) %>% 
    
    mutate(dca2_total_q             = cumsum(dca2_q),
           dca2_total_cash_invested = cumsum(dca2_cash),
           dca2_total_value         = dca2_total_q * price,
           dca2_profit_pct          = (dca2_total_value / dca2_total_cash_invested) - 1,
           dca2_profit_number       = dca2_total_value - dca2_total_cash_invested,
           dca2_average             = dca2_total_cash_invested / dca2_total_q) %>% 
    
    
    # 3.0 Labels for charts 
    mutate(across(contains("total_cash"), ~scales::number(., big.mark = ".", decimal.mark = ","), .names = "{col}_txt")) %>% 
    mutate(across(contains("profit_pct"), ~scales::percent(., accuracy = 0.01, big.mark = ".", decimal.mark = ","), .names = "{col}_txt")) %>% 
    mutate(across(contains("profit_number"), ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1), .names = "{col}_txt")) %>% 
    mutate(across(contains("average"), ~scales::number(., big.mark = ".", decimal.mark = ",", accuracy = 1), .names = "{col}_txt"))
    
    # select(date, cash,
    #        profit_pct, profit_number, total_cash_txt, profit_pct_txt, profit_number_txt, average_txt,
    #        ls_profit_pct, ls_profit_number, ls_total_cash_txt, ls_profit_pct_txt, ls_profit_number_txt, ls_average_txt,
    #        dca2_profit_pct, dca2_profit_number, dca2_total_cash_txt, dca2_profit_pct_txt, dca2_profit_number_txt, dca2_average_txt)
  
  # PLOT:
  
  if(plot == "dca"){
    
    data_tbl %>% 
      
      ggplot(aes(x = date, y = profit_pct))+
      geom_line(color = "#8b0000") + 
      
      theme_tq() + theme2 + 
      
      scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(n = 6)) +
      scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
      
      labs(x        = "",
           y        = "Lợi nhuận",
           title    = str_glue("Thành quả DCA vào {toupper(data)}"),
           subtitle = str_glue("Đầu tư <span style = 'color:#8b0000;'>{scales::number(cash, big.mark = '.', decimal.mark = ',')} mỗi tháng</span> vào {toupper(data)} từ năm {first(year(x$date))} <br>
                           Thời điểm hiện tại đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(data_tbl$total_cash_invested_txt)}</span>, 
                           với giá trung bình <span style = 'color:#8b0000;'>{last(data_tbl$average_txt)}</span> <br>
                           Lợi nhuận là <span style = 'color:#8b0000;'>{last(data_tbl$profit_number_txt)}</span> , tương đương <span style = 'color:#8b0000;'>{last(data_tbl$profit_pct_txt)}</span>"))
    
  } else if(plot == "lumpsum") {
    
    data_tbl %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = profit_pct, color = "DCA" )) +
      geom_line(aes(y = ls_profit_pct, color = "Lump sum")) +
      scale_colour_manual(values=c("#8b0000", "#0078D7")) +
      
      theme_tq() + theme2 +
      
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(breaks = scales::pretty_breaks(n = 10))  +
      
      labs(color    = "Phương pháp",
           x        = "",
           y        = "Lợi nhuận",
           title    = "So sánh phương pháp DCA và Lump sum",
           subtitle = str_glue("Giả định đầu tư {scales::number(cash, big.mark = '.', decimal.mark = ',')} vào đầu mỗi tháng <span style = 'color:#8b0000;'>(DCA)</span> <br>
                           hoặc đầu tư {scales::number(ls_cash, big.mark = '.', decimal.mark = ',')} trong một lần <span style = 'color:#0078D7;'>(lump sum)</span><br>
                           Với <span style = 'color:#8b0000;'>DCA</span> bạn đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(data_tbl$total_cash_invested_txt)}</span> , lợi nhuận là <span style = 'color:#8b0000;'>{last(data_tbl$profit_number_txt)}</span> , tương đương <span style = 'color:#8b0000;'>{last(data_tbl$profit_pct_txt)}</span> <br>
                           Với <span style = 'color:#0078D7;'>Lump sum</span> bạn đã đầu tư tổng cộng <span style = 'color:#0078D7;'>{last(data_tbl$ls_total_cash_invested_txt)}</span> , lợi nhuận là <span style = 'color:#0078D7;'>{last(data_tbl$ls_profit_number_txt)}</span> , tương đương <span style = 'color:#0078D7;'>{last(data_tbl$ls_profit_pct_txt)}</span>"))
    
  } else {
    
    data_tbl %>%
      ggplot(aes(x = date)) +
      geom_line(aes(y = profit_pct, color = "DCA" )) +
      geom_line(aes(y = dca2_profit_pct, color = "DCA Stop")) +
      geom_line(aes(y = ls_profit_pct, color = "Lump sum")) +
      scale_colour_manual(values=c("#8b0000", "#107C10", "#0078D7")) +
      
      theme_tq() + theme2 +
      
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(breaks = scales::pretty_breaks(n = 10))  +
      
      labs(color    = "Phương pháp",
           x        = "",
           y        = "Lợi nhuận",
           title    = str_glue("Sử dụng phương pháp DCA đến khi nào? ({toupper(data)})"),
           subtitle = str_glue("Chúng ta đầu tư {scales::number(cash, big.mark = '.', decimal.mark = ',')} vào đầu mỗi tháng cho đến khi đạt mốc {scales::number(dcastop_cash, big.mark = '.', decimal.mark = ',')} thì dừng lại. Tạm gọi <span style = 'color:#107C10;'>DCA stop.</span> <br>
                           Với <span style = 'color:#8b0000;'>DCA</span> bạn đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(data_tbl$total_cash_invested_txt)}</span> , lợi nhuận là <span style = 'color:#8b0000;'>{last(data_tbl$profit_number_txt)}</span> , tương đương <span style = 'color:#8b0000;'>{last(data_tbl$profit_pct_txt)}</span> <br>
                           Với <span style = 'color:#0078D7;'>Lump sum</span> bạn đã đầu tư tổng cộng <span style = 'color:#0078D7;'>{last(data_tbl$ls_total_cash_invested_txt)}</span> , lợi nhuận là <span style = 'color:#0078D7;'>{last(data_tbl$ls_profit_number_txt)}</span> , tương đương <span style = 'color:#0078D7;'>{last(data_tbl$ls_profit_pct_txt)}</span> <br>
                           Với <span style = 'color:#107C10;'>DCA stop</span> bạn đã đầu tư tổng cộng <span style = 'color:#107C10;'>{last(data_tbl$dca2_total_cash_invested_txt)}</span> , lợi nhuận là <span style = 'color:#107C10;'>{last(data_tbl$dca2_profit_number_txt)}</span> đồng, tương đương <span style = 'color:#107C10;'>{last(data_tbl$dca2_profit_pct_txt)}</span>"))
    
  }
  
}


# 4. EXTRA: MANUAL ANALYSIS #### ####

# 4.1 DCA 
cash <- 3000000

etf_tbl <- etf %>% 
  mutate(month = month(date),
         year  = year(date)) %>% 
  group_by(month, year) %>% 
  mutate(cash = if_else(date == min(date), 3000000, 0 )) %>%
  ungroup() %>% 
  select(-month, -year) %>% 
  
  # q = quantity. So luong chung khoan
  mutate(q = cash / price) %>% 
  
  mutate(total_q             = cumsum(q),
         total_cash_invested = cumsum(cash),
         total_value         = total_q * price,
         profit_pct          = (total_value / total_cash_invested) - 1,
         profit_number       = total_value - total_cash_invested,
         average             = total_cash_invested / total_q) %>% 
  
  # Labels for charts
  mutate(total_cash_txt      = scales::number(total_cash_invested, big.mark = ".", decimal.mark = ","),
         profit_pct_txt      = scales::percent(profit_pct, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
         profit_number_txt   = scales::number(profit_number, big.mark = ".", decimal.mark = ",", accuracy = 1),
         average_txt         = scales::number(average, big.mark = ".", decimal.mark = ",", accuracy = 1))

# Plot
etf_tbl %>% 
  ggplot(aes(x = date, 
             y = profit_pct))+
  
  geom_line(color = "#8b0000") + theme_tq() + theme2 + 
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x        = "",
       y        = "Lợi nhuận",
       title    = "Thành quả DCA vào ETF",
       subtitle = str_glue("Đầu tư 3.000.000 mỗi tháng vào ETF từ năm 2014 <br>
                           Thời điểm hiện tại đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(etf_tbl$total_cash_txt)}</span> đồng, 
                           giá trung bình mỗi cổ phiếu là <span style = 'color:#8b0000;'>{last(etf_tbl$average_txt)}</span> <br>
                           Lợi nhuận là <span style = 'color:#8b0000;'>{last(etf_tbl$profit_number_txt)}</span> đồng, tương đương <span style = 'color:#8b0000;'>{last(etf_tbl$profit_pct_txt)}</span>"))


# 4.2 Lump sum vs DCA 
# So sanh phuong phap Lump Sum va DCA

ls_cash <- 100000000

etf_lumpsum_tbl <- etf_tbl %>% 
  mutate(ls_cash = if_else(date == min(date), ls_cash, 0)) %>% 
  
  mutate(ls_q = ls_cash / price) %>% 
  
  mutate(ls_total_q = cumsum(ls_q),
         ls_total_cash_invested = cumsum(ls_cash),
         ls_total_value = ls_total_q * price,
         ls_profit_pct = (ls_total_value / ls_total_cash_invested) - 1,
         ls_profit_number = ls_total_value - ls_total_cash_invested,
         ls_average = ls_total_cash_invested / ls_total_q) %>% 
  
  #Labels:
  mutate(ls_total_cash_txt    = scales::number(ls_total_cash_invested, big.mark = ".", decimal.mark = ","),
         ls_profit_pct_txt    = scales::percent(ls_profit_pct, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
         ls_profit_number_txt = scales::number(ls_profit_number, big.mark = ".", decimal.mark = ",", accuracy = 1),
         ls_average_txt       = scales::number(ls_average, big.mark = ".", decimal.mark = ",", accuracy = 1)) %>% 
  
  select(date, price, cash,
         profit_pct, profit_number, total_cash_txt, profit_pct_txt, profit_number_txt, average_txt,
         ls_profit_pct, ls_profit_number, ls_total_cash_txt, ls_profit_pct_txt, ls_profit_number_txt, ls_average_txt)



etf_lumpsum_tbl %>%   
  ggplot(aes(x = date)) +
  
  geom_line(aes(y = profit_pct, color = "DCA" )) +
  geom_line(aes(y = ls_profit_pct, color = "Lump sum")) +
  scale_colour_manual(values=c("#8b0000", "#0078D7")) +
  
  
  theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = scales::pretty_breaks(n = 10))  +
  
  labs(color = "Phương pháp",
       x = "",
       y = "Lợi nhuận",
       title = "So sánh phương pháp DCA và Lump sum",
       subtitle = str_glue("Giả định đầu tư một số tiền 3.000.000 đồng vào đầu mỗi tháng <span style = 'color:#8b0000;'>(DCA)</span> <br>
                           hoặc đầu tư 100.000.000 đồng trong một lần <span style = 'color:#0078D7;'>(lump sum)</span><br>
                           Với <span style = 'color:#8b0000;'>DCA</span> bạn đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(etf_lumpsum_tbl$total_cash_txt)}</span> đồng, lợi nhuận là <span style = 'color:#8b0000;'>{last(etf_lumpsum_tbl$profit_number_txt)}</span> đồng, tương đương <span style = 'color:#8b0000;'>{last(etf_lumpsum_tbl$profit_pct_txt)}</span> <br>
                           Với <span style = 'color:#0078D7;'>lump sum</span> bạn đã đầu tư tổng cộng <span style = 'color:#0078D7;'>{last(etf_lumpsum_tbl$ls_total_cash_txt)}</span> đồng, lợi nhuận là <span style = 'color:#0078D7;'>{last(etf_lumpsum_tbl$ls_profit_number_txt)}</span> đồng, tương đương <span style = 'color:#0078D7;'>{last(etf_lumpsum_tbl$ls_profit_pct_txt)}</span>"))



# 4.3 DCA tới một mốc cố định. Giả định DCA tới 100 triệu rồi dừng.
cash <- 3000000
dca_cash_max <- 100000000

etf_dca2_tbl <- etf_lumpsum_tbl %>% 
  mutate(dca2_cash = if_else(cumsum(cash) < dca_cash_max, cash, 0 )) %>% 
  
  mutate(dca2_q = dca2_cash / price) %>% 
  
  mutate(dca2_total_q             = cumsum(dca2_q),
         dca2_total_cash_invested = cumsum(dca2_cash),
         dca2_total_value         = dca2_total_q * price,
         dca2_profit_pct          = (dca2_total_value / dca2_total_cash_invested) - 1,
         dca2_profit_number       = dca2_total_value - dca2_total_cash_invested,
         dca2_average             = dca2_total_cash_invested / dca2_total_q) %>% 
  
  #Labels:
  mutate(dca2_total_cash_txt    = scales::number(dca2_total_cash_invested, big.mark = ".", decimal.mark = ","),
         dca2_profit_pct_txt    = scales::percent(dca2_profit_pct, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
         dca2_profit_number_txt = scales::number(dca2_profit_number, big.mark = ".", decimal.mark = ",", accuracy = 1),
         dca2_average_txt       = scales::number(dca2_average, big.mark = ".", decimal.mark = ",", accuracy = 1)) %>% 
  
  select(date, price, cash,
         profit_pct, profit_number, total_cash_txt, profit_pct_txt, profit_number_txt, average_txt,
         ls_profit_pct, ls_profit_number, ls_total_cash_txt, ls_profit_pct_txt, ls_profit_number_txt, ls_average_txt,
         dca2_profit_pct, dca2_profit_number, dca2_total_cash_txt, dca2_profit_pct_txt, dca2_profit_number_txt, dca2_average_txt)

  

etf_dca2_tbl %>%   
  ggplot(aes(x = date)) +
  
  geom_line(aes(y = profit_pct, color = "DCA" )) +
  geom_line(aes(y = dca2_profit_pct, color = "DCA Stop")) +
  geom_line(aes(y = ls_profit_pct, color = "Lump sum")) +
  scale_colour_manual(values=c("#8b0000", "#107C10", "#0078D7")) +
  
  
  theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = scales::pretty_breaks(n = 10))  +
  
  labs(color = "Phương pháp",
       x = "",
       y = "Lợi nhuận",
       title = "Sử dụng phương pháp DCA đến khi nào?",
       subtitle = str_glue("Chúng ta đầu tư 3.000.000 đồng vào đầu mỗi tháng cho đến khi đạt mốc 100.000.000 đồng thì dừng lại. Tạm gọi <span style = 'color:#107C10;'>DCA stop.</span> <br>
                           Với <span style = 'color:#8b0000;'>DCA</span> bạn đã đầu tư tổng cộng <span style = 'color:#8b0000;'>{last(etf_dca2_tbl$total_cash_txt)}</span> đồng, lợi nhuận là <span style = 'color:#8b0000;'>{last(etf_dca2_tbl$profit_number_txt)}</span> đồng, tương đương <span style = 'color:#8b0000;'>{last(etf_dca2_tbl$profit_pct_txt)}</span> <br>
                           Với <span style = 'color:#0078D7;'>Lump sum</span> bạn đã đầu tư tổng cộng <span style = 'color:#0078D7;'>{last(etf_dca2_tbl$ls_total_cash_txt)}</span> đồng, lợi nhuận là <span style = 'color:#0078D7;'>{last(etf_dca2_tbl$ls_profit_number_txt)}</span> đồng, tương đương <span style = 'color:#0078D7;'>{last(etf_dca2_tbl$ls_profit_pct_txt)}</span> <br>
                           Với <span style = 'color:#107C10;'>DCA stop</span> bạn đã đầu tư tổng cộng <span style = 'color:#107C10;'>{last(etf_dca2_tbl$dca2_total_cash_txt)}</span> đồng, lợi nhuận là <span style = 'color:#107C10;'>{last(etf_dca2_tbl$dca2_profit_number_txt)}</span> đồng, tương đương <span style = 'color:#107C10;'>{last(etf_dca2_tbl$dca2_profit_pct_txt)}</span>"))