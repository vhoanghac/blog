# 1. Packages #######
source("header.R")
library(tidyquant)



# 2. Import du lieu #######
etf <- get_data_vnstock("045_truoc_khi_dau_tu/01_data/raw/E1VFVN30.csv")

vnindex <- get_data_investing("045_truoc_khi_dau_tu/01_data/raw/vnindex_investing_com.csv", format = "%m/%d/%Y")


# 3. Analysis #######

# 3.1 Không kỳ vọng lợi nhuận trung bình #######

# Tính returns mỗi năm
# Type = arithmetic vì tính tăng trưởng đơn theo từng năm riêng biệt
vni_annual_ret <- vnindex %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "returns",
               type = "arithmetic") %>% 
  filter(date >= "2001-01-01")

# Tính tỷ suất sinh lợi hằng năm (lãi kép)
# Với một năm có 250 ngày giao dịch
vnindex %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  tq_performance(Ra = returns,
                 performance_fun = Return.annualized,
                 scale = 250)

# Plot biểu đồ
vni_annual_ret %>% 
  ggplot(aes(x = year(date), y = returns)) + 
  geom_point(color = "firebrick4", size = 3) + 
  geom_text(aes(label = year(vni_annual_ret$date)), vjust = -1) +
  
  theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(x = "",
       y = "",
       title = "Tăng trưởng của VNINDEX theo từng năm: 2001-2023",
       caption = "Tăng trưởng của từng năm riêng biệt. 
       Được tính bằng cách lấy giá trị cuối cùng chia cho giá trị đầu tiên trong năm đó.
       Giá trị của năm 2023 kết thúc vào tháng 11.
       Nguồn: Investing.com")
  
 
# 3.2 Thị trường tăng mạnh nhưng cũng giảm mạnh #######
etf %>% 
  ggplot(aes(x = date, y = price)) + 
  geom_line(color = "firebrick4") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  
  labs(x = "",
       y = "Giá",
       title = "ETF E1VFVN30 thường xuyên đi lên",
       caption = "ETF E1VFVN30 mô phỏng chỉ số VN30-TRI (có bao gồm cổ tức)
       Nguồn: tổng hợp bằng vnstock")


# 3.3 Drawdowns của VNINDEX #######

vnindex_dd <- vnindex %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  mutate(growth = cumprod(1 + returns),
         max_growth = cummax(growth),
         drawdown = - (1 - growth / max_growth))


vnindex_dd %>% 
  ggplot(aes(x = date, y = drawdown)) +
  geom_area(fill = "firebrick4", color = "white") + theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_x_date(limits = as.Date(c("2001-01-01", "2024-01-01")),
               breaks = as.Date(seq(from = as.Date("2001-01-01"), 
                                    to = as.Date("2024-01-01"), 
                                    by = "2 years")),
               date_labels = "%Y") +
  
  labs(x = "",
       y = "Tỷ lệ (%)",
       title = "Tỷ lệ sụt giảm so với đỉnh của VNINDEX: 2001-2023",
       caption = "Nguồn: Investing.com")


# 3.4 Max Drawdowns và annual returns VNINDEX #######

# Tính max drawdowns
vnindex_max_dd <- vnindex %>% 
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>% 
  group_by(year = year(date)) %>% 
  tq_performance(Ra = returns,
                 performance_fun = maxDrawdown) %>% 
  ungroup() %>% 
  filter(year >= "2001") %>% 
  rename(dd = maxDrawdown.1) %>% 
  mutate(dd = -dd,
         dd_txt = scales::percent(dd, accuracy = 1))

# Gộp Annual returns và Drawdowns
vnindex_ret_dd <- vni_annual_ret %>% 
  mutate(year = year(date),
         returns_txt = scales::percent(returns, accuracy = 1)) %>% 
  select(year, returns, returns_txt) %>% 
  left_join(vnindex_max_dd, by = "year") %>% 
  mutate(year = as_factor(str_sub(year, -2)))


# Plot
vnindex_ret_dd %>% 
  ggplot(aes(x = year))+
  
  geom_col(aes(y = returns), fill = "grey", color = "black") +
  geom_text(aes(y = returns, label = returns_txt), vjust = -1)+

  geom_point(aes(y = dd), color = "firebrick4", size = 3) + 
  geom_text(aes(y = dd, label = dd_txt), vjust = 2, color = "firebrick4", size = 5) +
  
  theme_tq() + theme +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(x = "",
       y = "Tỷ lệ (%)",
       title = "Tăng trưởng và tỷ lệ sụt giảm của VNINDEX trong năm",
       caption = "Giá trị của năm 2023 kết thúc vào tháng 11.
       Tăng trưởng mỗi năm được tính dựa trên điểm của VNINDEX, không bao gồm cổ tức. 
       Tỷ suất sinh lợi hằng năm của VNINDEX tại thời điểm viết bài là 11%/năm.
       Nguồn: Investing.com")
