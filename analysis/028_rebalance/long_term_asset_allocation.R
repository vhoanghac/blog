# 1. Packages #### #### #### #### #### 
source("header.R") # tidyverse, data.table, lubridate
library(readxl)
library(tidyquant) #xts
library(ggsci) # colors

library(tidytable)

# 2. Data #### #### #### #### #### 

etf <- get_data_co_phieu(ticker = "E1VFVN30", all = FALSE) %>% 
  pivot_wider(names_from = "symbol",
              values_from = "price") %>% 
  rename("ETF" = "E1VFVN30" ) %>% 
  filter(date >= "2015-09-09" & date <= "2021-07-30") %>% 
  mutate(ETF = ETF * 1000)


tcbf <- read_csv("028_allocation/00_data_raw/tcbf.csv", 
                 col_select = c("date", "TCBF" = "price")) %>% 
  filter(date <= "2021-07-30")


# 3. Tidy  #### #### #### #### #### 

# Join etf va tcbf, fill NA.
data_tbl <- etf %>% 
  left_join(tcbf, by = "date") %>% 
  fill(TCBF)

# convert sang xts (Just testing something)
data_xts <- data_tbl %>% 
  timetk::tk_xts(date_var = "date")

# Tinh returns
returns_xts <- data_xts %>% 
  Return.calculate()

returns_xts$ETF[1] <- 0
returns_xts$TCBF[1] <- 0

# Convert sang tibble
returns_tbl <- returns_xts %>% 
  timetk::tk_tbl(rename_index = "date") %>% 
  gather(symbol, returns, -date)


# 4. Portfolio specs:  #### #### #### #### #### 

# 4.1 Returns data:
returns_tbl_x6 <- returns_tbl %>% 
  group_by(symbol) %>% 
  tq_repeat_df(n = 6)

# 4.1 Weights table
w <- c(  1 ,   0,
       0.8 , 0.2,
       0.6 , 0.4,
       0.4 , 0.6,
       0.2 , 0.8,
       0   ,   1)

symbol <- c("ETF", "TCBF")

w_table <- tibble(symbol) %>% 
  tq_repeat_df(n = 6) %>% 
  bind_cols(tibble(w))

# 4.2 Portfolio Returns
portfolios <- returns_tbl_x6 %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = w_table,
               col_rename = "preturns") %>% 
  
  mutate(cumreturns = cumprod(1 + preturns))


# 4.3 Different periods

# Month
portfolios_m <- portfolios %>% 
  tq_transmute(select = preturns,
               mutate_fun = apply.monthly,
               FUN = Return.cumulative)

# Year
portfolios_y <- portfolios %>% 
  tq_transmute(select = preturns,
               mutate_fun = apply.yearly,
               FUN = Return.cumulative)



# 5. Pha bo tai san va rui ro  #### #### #### ####

color <- pal_locuszoom(alpha = 0.8)(6) # Colors for charts

# 5.1 Plot
portfolios %>% 
  ggplot(aes(x = date, y = cumreturns, color = factor(portfolio))) +
  geom_line(size = 0.8) + theme_tq() + theme2 +
  scale_color_manual(values = color,
                     labels = c("100/0", "80/20", "60/40",
                                "40/60", "20/80", "0/100")) + 
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  labs(color = "Danh mục (ETF/TCBF)",
       x = "",
       y = "Lợi nhuận tích l\u0169y (lần)",
       title = "Lợi nhuận tích l\u0169y của các danh mục",
       caption = str_glue("Với giả \u0111ịnh cùng \u0111ầu t\u01B0 tại thời \u0111iểm {first(portfolios$date)}.
                          Tỷ trọng tài sản của các danh mục không \u0111\u01B0ợc tái cân bằng."))


# 5.2 Table:
# Dem tong so thang
portfolios_m %>% 
  summarise(n_loss = sum(preturns < 0))


# Lo trung binh thang
portfolios_m %>% 
  # filter(date >= "2017-01-01" & date < "2020-01-01") %>%
  mutate(negative_r = case_when(preturns < 0 ~ preturns,
                             TRUE ~ NA_real_)) %>% 
  
  summarise(average = mean(negative_r, na.rm = TRUE),
            median = median(negative_r, na.rm = TRUE),
            min = min(negative_r, na.rm = TRUE),
            max = max(negative_r, na.rm = TRUE))


# Muc sut giam so voi dinh
portfolios %>% 
  tq_performance(Ra = preturns,
                 performance_fun = maxDrawdown)


# 6. Loi nhuan va ty trong #### #### #### ####

# Portfolio tai can bang moi nam:
portfolios_reb <- returns_tbl_x6 %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = w_table,
               col_rename = "preturns",
               rebalance_on = "years") %>% 
  
  mutate(cumreturns = cumprod(1 + preturns))

# Convert to monthly returns:
portfolios_reb_m <- portfolios_reb %>% 
  tq_transmute(select = preturns,
               mutate_fun = apply.monthly,
               FUN = Return.cumulative)

# Table:
portfolios_reb_m %>% 
  # filter(date >= "2017-01-01" & date < "2020-01-01") %>%
  mutate(negative_r = case_when(preturns < 0 ~ preturns,
                                TRUE ~ NA_real_)) %>% 
  
  summarise(average = mean(negative_r, na.rm = TRUE),
            median = median(negative_r, na.rm = TRUE),
            min = min(negative_r, na.rm = TRUE))


# Annualized returns
portfolios_reb_m %>% 
  # filter(date >= "2018-01-01" & date < "2020-01-01") %>%
  tq_performance(Ra = preturns,
                 performance_fun = Return.annualized)

# Plot portfolio tai can bang moi nam
portfolios_reb %>% 
  ggplot(aes(x = date, y = cumreturns, color = factor(portfolio))) +
  geom_line(size = 0.8) + theme_tq() + theme2 +
  scale_color_manual(values = color,
                     labels = c("100/0", "80/20", "60/40",
                                "40/60", "20/80", "0/100")) + 
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  labs(color = "Danh mục (ETF/TCBF)",
       x = "",
       y = "Lợi nhuận tích l\u0169y (lần)",
       title = "Lợi nhuận tích l\u0169y của các danh mục (tái cân bằng mỗi n\u0103m)",
       caption = str_glue("Với giả \u0111ịnh cùng \u0111ầu t\u01B0 tại thời \u0111iểm {first(portfolios$date)}.
                          Tỷ trọng tài sản của các danh mục \u0111\u01B0ợc tái cân bằng mỗi n\u0103m."))


# 7. Portfolio PP #### #### #### ####

#  Prepare data:
returns_tbl_wide <- returns_tbl %>% 
  pivot_wider(id_cols = date,
              names_from = symbol,
              values_from = returns)

# Config:
chenhlech <- 0.1  # Chenh lech ty trong thi bat dau tai can bang
result <- tibble()  # Luu tru ket qua
i0 <- 1 
n_rebalance <- 1 # Dem so lan mua
pf_value <- 1 # Gia tri danh muc

weights <- c(0.8, 0.2)


# Analysis:
for (i in 1:nrow(returns_tbl_wide)) {
  r <- returns_tbl_wide[i0:i,]
  
  j <- 0
  
  r_i <- r %>% 
    mutate_if(is.numeric, list(v = ~ pf_value * weights[j <<- j + 1] * cumprod(1 + .))) %>%
    mutate.(pf = rowSums(select(., contains("_v")))) %>% 
    mutate_at(vars(ends_with("_v")), list(w = ~ ./pf))
  
  touch_upper_band <- any(r_i[nrow(r_i),] %>% select.(ends_with("_w")) %>% unlist() > weights + chenhlech)
  touch_lower_band <- any(r_i[nrow(r_i),] %>% select.(ends_with("_w")) %>% unlist() < weights - chenhlech)
  
  if (touch_upper_band | touch_lower_band | i == nrow(returns_tbl_wide)) {
    i0 <- i + 1
    result <- bind_rows(result, r_i %>% mutate.(n_rebalance = n_rebalance))
    pf_value <- r_i[[nrow(r_i), "pf"]]
    n_rebalance <- n_rebalance + 1
  }
}

# Add thu cong tung ty trong
# portfolio2_pp <- result
# portfolio3_pp <- result
# portfolio4_pp <- result
# portfolio5_pp <- result


# Gop cac portfolio lai. 
portfolios_pp_cumreturns <- portfolios %>% 
  ungroup() %>% 
  filter.(portfolio == 1 ) %>% 
  select.(date, "port1" = cumreturns) %>% 
  left_join.(portfolio2_pp %>% select.(date, "port2" = pf), by = "date") %>% 
  left_join.(portfolio3_pp %>% select.(date, "port3" = pf), by = "date") %>% 
  left_join.(portfolio4_pp %>% select.(date, "port4" = pf), by = "date") %>% 
  left_join.(portfolio5_pp %>% select.(date, "port5" = pf), by = "date") %>% 
  left_join.(portfolios %>% 
               filter(portfolio == 6) %>% 
               select.(date, "port6" = cumreturns), by = "date")
 
# PLOT loi nhuan tich luy cac danh muc theo phuong phap PP
portfolios_pp_cumreturns %>% 
  pivot_longer(cols = c(2:7), names_to = "portfolio", values_to = "cumreturns") %>% 
  
  ggplot(aes(x = date, y = cumreturns, color = portfolio)) +
  geom_line(size = 0.8) + theme_tq() + theme2 +
  scale_color_manual(values = color,
                     labels = c("100/0", "80/20", "60/40",
                                "40/60", "20/80", "0/100")) + 
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  labs(color = "Danh mục (ETF/TCBF)",
       x = "",
       y = "Lợi nhuận tích l\u0169y (lần)",
       title = "Lợi nhuận tích l\u0169y của các danh mục (ph\u01B0\u01A1ng pháp PP)",
       caption = str_glue("Với giả \u0111ịnh cùng \u0111ầu t\u01B0 tại thời \u0111iểm {first(portfolios$date)}.
                          Tỷ trọng tài sản của các danh mục \u0111\u01B0ợc tái cân bằng 
                          theo ph\u01B0\u01A1ng pháp Percentage of Portfolio \u0111\u01B0ợc \u0111ề cập tại: 
                          https://vohoanghac.com/phuong-phap-tai-can-bang-danh-muc-etf/."))


# TABLE Annualized returns
portfolios_pp_returns <- portfolios_pp_cumreturns %>% 
  pivot_longer(cols = c(2:7), names_to = "portfolio", values_to = "cumreturns") %>% 
  group_by(portfolio) %>% 
  tq_transmute(select = cumreturns,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "returns")


portfolios_pp_returns %>% 
  # Lua chon quang thoi gian
  filter(date >= "2020-01-01") %>% 
  
  tq_performance(Ra = returns,
                 performance_fun = Return.annualized)