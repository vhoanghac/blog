# 1. Packages #### ####
source("header.R") #Tidyverse, data.table, lubridate
library(tidyquant)
library(timetk) # Summarise time

# 2. Import data #### ####
# 2.1 Lấy đường dẫn file CSV
paths <- fs::dir_ls("036_etfs_vs_fundmanagers/01_data/tidied/")

# 2.2 Lấy tên dữ liệu để lúc sau đặt tên cho tiện
datanames <-  gsub("\\.csv$","", list.files(path = "036_etfs_vs_fundmanagers/01_data/tidied/",
                                            pattern = "\\.csv$")) %>% 
  tolower()

# 2.3 Gộp các file CSV lại thành một list
list <- paths %>%
  map(function(path) {
    tidytable::fread(path)
  }) %>% 
  set_names(datanames)



# 3. Thêm data cho quỹ #### ####

# Một số quỹ có ngày bắt đầu vào tuần đầu tiên trong năm. Ví dụ ngày 06/01/2020
# Nếu để như vậy tính toán theo cách trong bài viết (sử dụng summarise_by_time với .type = "floor", hay nói cách khác là sử dụng giá cuối cùng trong năm) thì hệ thống sẽ cho rằng năm 2020 không có lợi nhuận.
# Để đơn giản hóa vấn đề, tôi thêm dữ liệu ngày 31/12/2019 với giá NAV giống như ngày đầu tiên.
# Ví dụ VNDAF:
# Ngày đầu tiên là 12/01/2018, giá NAV 10,000
# Thêm dữ liệu 31/12/2017, giá NAV 10,000
# Tính toán tăng trưởng bằng cách lấy giá cuối cùng năm 2018 chia cho giá cuối cùng năm 2017.

# 3.1 BVPF. Ngày đầu tiên 2017-01-02
list$bvpf <- list$bvpf %>% 
  add_row(date = as.IDate("2016-12-31"), price = 10000)

# 3.2 DFVNCAF. Ngày đầu tiên 2019-01-07
list$dfvncaf <- list$dfvncaf %>% 
  add_row(date = as.IDate("2018-12-31"), price = 10000)

# 3.3 VNDAF. Ngày đầu tiên 2018-01-12
list$vndaf <- list$vndaf %>% 
  add_row(date = as.IDate("2017-12-31"), price = 10000)



list$prulink %>% 
  arrange(date) %>%
  summarise_by_time(.date_var = date,
                    .by       = "year",
                    price     = last(price),
                    .type     = "floor")

list$dcds %>% 
  summarise_by_time(.date_var = date,
                    .by       = "year",
                    price     = last(price),
                    .type     = "floor")


# 4. ĐỔI SANG ANNUAL DATA VÀ TÍNH HIỆU SUẤT NĂM #### ####
list_ret <- list %>% 
  
  lapply(function(x) {
    
    x %>% 
      arrange(date) %>% 
      # Lựa chọn giá cuối cùng của năm
      summarise_by_time(.date_var = date,
                        .by       = "year",
                        price     = last(price),
                        .type     = "floor") %>% 
      
      # Tính hiệu suất trong năm.
      # leading = FALSE: Skip và trả về giá trị là NA nếu không có đủ dữ liệu trong 1 năm
      
      tq_transmute(select     = price,
                   mutate_fun = periodReturn,
                   period     = "yearly",
                   col_rename = "returns",
                   leading    = FALSE) 
    })


# 5. PHÂN TÍCH #### ####

returns_tbl <- list_ret %>% 
  enframe() %>% 
  unnest(value) %>% 
  rename(symbol = name) %>% 
  pivot_wider(names_from  = symbol,
              values_from = returns) %>% 
  filter(date >= "2015-01-01" & date < "2023-01-01")


result_tbl <- returns_tbl %>% 
  select(-e1vfvn30) %>% 
  
  # 1: Nếu quỹ cổ phiếu đánh bại ETF VN30
  # 0: Nếu quỹ cổ phiếu không đánh bại ETF VN30
  
  mutate_if(is.numeric, ~case_when(. > returns_tbl$e1vfvn30 ~ 1,
                                    is.na(.) ~ NA_real_,
                                    TRUE ~ 0)) %>%
  select(-date) %>%
  mutate(
    
    # Tính tổng số lượng quỹ cổ phiếu đánh bại ETF VN30
    n_win = rowSums(., na.rm = TRUE),
    
    # Tính tổng số lượng quỹ cổ phiếu bị ETF VN30 đánh bại
    n_loss = rowSums(. == 0, na.rm = TRUE),
    
    total_funds = n_win + n_loss,
    pct_etf_win = n_loss / total_funds,
    date = returns_tbl$date
  ) %>%
  select(date, n_win, n_loss, total_funds, pct_etf_win) %>%
  mutate(date = year(date)) %>% 
  mutate(pct_label = scales::percent(pct_etf_win, accuracy = 1))
  
# Plot:
result_tbl %>% 
  ggplot(aes(x = date, y = pct_etf_win)) +
  geom_col() + theme_tq() + theme +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 10)) +
  
  geom_text(aes(label = pct_label), vjust = -0.5) +
  
  labs(x = "",
       y = "",
       title = "Tỷ lệ các quỹ cổ phiếu bị ETF VN30 đánh bại",
       caption = "Trong bài viết ETF VN30 là E1VFVN30
       Những quỹ trong thống kê là những quỹ đang hoạt động và có dữ liệu
       Những quỹ đã ngừng hoạt động thì mọi thông tin không còn tồn tại")


# 6. TOP 50% QUỸ #### ####

# 6.1 Function chọn top 50% quỹ cổ phiếu hoạt động tốt nhất
select_top_50 <- function(data){
  
  data %>% 
    pivot_longer(names_to = "symbol",
                 values_to = "returns",
                 -date) %>% 
    filter(!is.na(returns)) %>% 
    filter(symbol != "e1vfvn30") %>% 
    arrange(desc(returns)) %>% 
    slice(1 : round(n()/2))
}

# 6.2 Function chọn top quỹ cổ phiếu theo năm

top_list <- lapply(unique(returns_tbl$date), function(d) {
  
  returns_tbl %>% 
    filter(date == d) %>% 
    select_top_50()
})

top_list <- top_list[-c(1,2)]

# 6.3 Gộp lại
top_tbl <- bind_rows(top_list)

# 6.4 Kiểm tra xem top 50% năm 2017 còn xuất hiện ở các năm sau không
for (i in 2:length(top_list)){
  
  print(paste("Top 50% quỹ năm", unique(top_list[[1]]$date), 
              "mà xuất hiện tại năm", unique(top_list[[i]]$date)))
  print(semi_join(top_list[[1]], top_list[[i]], by = "symbol")$symbol)
}


# 7. MANUAL  #### ####

# Top 50% quy mo nam 2017
# Prulink, dcbc, bvfed, dcds, mafeqi
top2017 <- returns_tbl %>% 
  filter(date == "2017-01-01") %>% 
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date ) %>% 
  drop_na() %>% 
  filter(symbol != "e1vfvn30") %>% 
  arrange(desc(returns)) %>% 
  slice(1:round(n()/2)) %>% 
  mutate(date = year(date))

# Top 50% năm 2018
# MBVF, PRUlink, VNDAF, VESAF, VCBF-BCF, ENF, DCDS
top2018 <- returns_tbl %>% 
  filter(date == "2018-01-01") %>% 
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date ) %>% 
  drop_na() %>% 
  filter(symbol != "e1vfvn30") %>% 
  arrange(desc(returns)) %>% 
  slice(1:round(n()/2)) %>% 
  mutate(date = year(date))

# Top 50% năm 2019
# ENF, DCDS, VEOF, VESAF, DCBC, BVPF, Prulink
top2019 <- returns_tbl %>% 
  filter(date == "2019-01-01") %>% 
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date ) %>% 
  drop_na() %>% 
  filter(symbol != "e1vfvn30") %>% 
  arrange(desc(returns)) %>% 
  slice(1:round(n()/2)) %>% 
  mutate(date = year(date))


# Top 50% năm 2020
# DCDS, VESAF, MBVF, DFVN-CAF, SSI-SCA, MAGEF, VCBF-BCF, PRUlink
top2020 <- returns_tbl %>% 
  filter(date == "2020-01-01") %>% 
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date ) %>% 
  drop_na() %>% 
  filter(symbol != "e1vfvn30") %>% 
  arrange(desc(returns)) %>% 
  slice(1:round(n()/2)) %>% 
  mutate(date = year(date))

# Top 50% năm 2021
# VESAF, VEOF, DCDS, SSI-SCA, DCBC, VNDAF, MAFEQI
top2021 <- returns_tbl %>% 
  filter(date == "2021-01-01") %>% 
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date ) %>% 
  drop_na() %>% 
  filter(symbol != "e1vfvn30") %>% 
  arrange(desc(returns)) %>% 
  slice(1:round(n()/2)) %>% 
  mutate(date = year(date))


# Top 50% năm 2022
# BVPF, ENF, SSI-VLGF, MBVF, VCBF-MGF, VNDAF, VEOF, TCEF
top2022 <- returns_tbl %>% 
  filter(date == "2022-01-01") %>% 
  pivot_longer(names_to = "symbol",
               values_to = "returns", !date ) %>% 
  drop_na() %>% 
  filter(symbol != "e1vfvn30") %>% 
  arrange(desc(returns)) %>% 
  slice(1:round(n()/2)) %>% 
  mutate(date = year(date))


