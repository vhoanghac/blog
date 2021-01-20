# 1. Import Libraries ----
library(tidyverse)
library(tidyquant)
library(timetk)

library(corrr) # Correlation

# Requires:
source("header.R") # Theme

# 2. Import Du lieu: -----

#  Function:

import_investing <- function(data_dir){
  read_csv(data_dir,
           col_types = cols(Date = col_date(format = "%b %d, %Y"))) %>% 
    select(Date, Price) %>% 
    arrange(Date) %>% 
    janitor::clean_names(.)
}

# Import du lieu:
xau_tbl <- import_investing("01_data/XAU_USD_2000_2020.csv")

dxy_tbl <- import_investing("01_data/DXY_2000_2020.csv")

# Left_join:
xau_dxy_tbl <- dxy_tbl %>% 
  left_join(xau_tbl, by = "date") %>% 
  na.locf() %>% 
  rename(DXY = price.x,
         XAU = price.y)

xau_dxy_gather_tbl <- xau_dxy_tbl %>% 
  gather(symbol, value, -date)



# 3. Bai viet ----

# Plot 01: Gia Vang va DXY:
plot_01 <- xau_dxy_gather_tbl %>% 
  group_by(symbol) %>% 
  plot_time_series(date, value,
                   .facet_ncol = 1,
                   .smooth = FALSE,
                   .plotly_slider = TRUE,
                   .line_color = "firebrick4",
                   .title = "Dollar Index & XAU")

htmltools::save_html(plot_01, "plot_01.html")



# Rolling Correlation:

# Correlation:
xau_dxy_tbl %>% 
  select(-date) %>% 
  correlate()
  

# Rolling correlation 180 days:
xau_dxy_rolling_corr <- xau_dxy_tbl %>% 
  tq_mutate_xy(x = DXY,
               y = XAU,
               mutate_fun = runCor, # Rolling Correlation
               
               n   = 180, # 180days
               use = "pairwise.complete.obs",
               
               col_rename = "rolling_correlation")


# Max rolling correlation per year, and > 0.4, add label text:

xau_dxy_rolling_corr_label <- xau_dxy_rolling_corr %>%
  mutate(year  = year(date),
         month = month(date)) %>% 
  
  group_by(year) %>% 
  mutate(max_cor = max(rolling_correlation)) %>% 
  ungroup() %>% 
  
  mutate(max_cor = ifelse(max_cor > 0.4, max_cor, NA_real_)) %>% 
  
  filter(rolling_correlation == max_cor) %>% 
  
  mutate(rolling_correlation = round(rolling_correlation, 2),
         label_text = str_glue("Giá trị: {rolling_correlation}
                               Thời gian: {month}/{year}"))
  


# Plot Rolling correlation
xau_dxy_rolling_corr %>% 
  mutate(correlation = as.numeric(-0.435)) %>% 
  ggplot(aes(x = date)) +
  
  ggrepel::geom_text_repel(data      = xau_dxy_rolling_corr_label, 
                           aes(x     = date,
                               y     = max_cor,
                               label = label_text,
                               size = 0.5)) +
  
  geom_point(data = xau_dxy_rolling_corr_label, aes(x = date, y = max_cor)) +

  geom_line(aes(y = rolling_correlation),
            color = "firebrick4",
            alpha = 0.5) +
  
  geom_hline(aes(yintercept = correlation),
             color = "cornflowerblue",
             size  = 1.5)  +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_tq() + theme +
  
  labs(x     = "",
       y     = "",
       title = "Rolling Correlation XAU & Dollar Index 2000 - 2020",
       caption =  "Chu kỳ tính toán là 180 ngày kể từ n\u0103m 2000")
  
# 4. Phan tich nam 2005: ----

xau_dxy_xts <- xau_dxy_tbl %>% 
  timetk::tk_xts(date_var = date)

# 4.1: DXY va XAU 2005:

plot_2005 <- xau_dxy_gather_tbl %>% 
  mutate(year = year(date)) %>% 
  group_by(symbol) %>% 
  filter(date >= "2005-05-01" & date <= "2006-10-01") %>% 
  plot_time_series(date, value,
                   .facet_ncol = 1,
                   .facet_scales = "free",
                   
                   .smooth = TRUE,
                   .smooth_span = 0.3,
                   .smooth_color = "cornflowerblue",
                   
                   .color_var = year,
                   .plotly_slider = FALSE,
                   
                   .line_color = "firebrick4",
                   .title = "Dollar Index & XAU 2005",
                   .legend_show = FALSE,
                   .interactive = TRUE)


htmltools::save_html(plot_2005, "plot_2005.html")

# 4.2 Chi so lam phat:

cpi_date <- seq(1990, 2006, 1)
cpi_r <- c(0.054, 0.042, 0.03, 0.03, 0.026, 0.028, 0.03, 0.023,
           0.016, 0.022, 0.034, 0.028, 0.016, 0.023, 0.027, 0.034,
           0.032)


date %>% 
  as_tibble() %>% 
  bind_cols(cpi_r) %>% 
  rename(date = value,
         cpi  = ...2) %>% 
  mutate(cpi_label = scales::percent(cpi, accuracy = 0.1),
         highlight = ifelse(date == 2005, "yes", "no")) %>% 
  
  ggplot(aes(x = date,
             y = cpi,
             fill = highlight))+
  
  geom_col() +
  
  scale_fill_manual(values = c("yes" = "firebrick4",
                               "no" = "cornflowerblue"),
                    guide = FALSE) +
  
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = scales::pretty_breaks(n = 5)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(10)) +
  
  geom_text(aes(y = cpi + 0.002, label = cpi_label, size = 2)) + theme_tq() + theme +
  
  labs(x = "",
       y = "Tỉ lệ lạm phát",
       title = "Tỉ lệ lạm phát bình quân của Mỹ 1990 - 2006",
       caption = "Nguồn: Statista")
  

# 4.3 10-year Treasury bonds

treasury_10_y <- tq_get("DGS10", get = "economic.data", from = "2004-01-01") %>%
  na.locf() %>% 
  spread(symbol, price)

tips_10_y <- tq_get("DFII10", get = "economic.data", from = "2004-01-01") %>%
  na.locf() %>% 
  spread(symbol, price)

treasury_10_y %>% 
  left_join(tips_10_y, by = "date") %>% 
  gather(symbol, price, -date) %>% 
  filter(date >= "2005-05-01" & date <= "2006-01-01" & symbol == "DFII10" ) %>% 
  
  ggplot(aes(x = date, y = price)) +
  
  geom_line(color = "firebrick4") +
  
  geom_smooth(method = "loess", span = 0.5, se = FALSE) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 7)) + theme_tq() + theme +
  
  labs(x = "",
       y = "Phần tr\u0103m",
       title = "Lợi tức trái phiếu chính phủ ngừa lạm phát kỳ hạn 10 n\u0103m, n\u0103m 2005",
       caption = "Thời \u0111iểm từ tháng 05/2005 \u0111ến cuối tháng 12/2005\nNguồn: FRED")



# 5. Nam 2010: ----

# 5.1 Gia vang va DXY:
plot_2010 <- xau_dxy_gather_tbl %>% 
  mutate(year = year(date)) %>% 
  group_by(symbol) %>% 
  filter(date >= "2010-02-01" & date <= "2010-08-31") %>% 
  plot_time_series(date, value,
                   .facet_ncol = 1, .color_var = symbol,
                   
                   .smooth = TRUE,
                   .smooth_span = 0.15,
                   .smooth_color = "cornflowerblue",
                   
                   .plotly_slider = FALSE,
                   
                   .line_color = "firebrick4",
                   .title = "Dollar Index & XAU 2010",
                   .legend_show = FALSE,
                   .interactive = TRUE)

htmltools::save_html(plot_2010, "plot_2010.html")


# 5.2 

treasury_10_y %>% 
  left_join(tips_10_y, by = "date") %>% 
  gather(symbol, price, -date) %>% 
  filter(date >= "2009-08-01" & date <= "2011-01-01" & symbol == "DGS10") %>% 
  
  ggplot(aes(x = date, y = price)) +
  
  geom_line() +
  
  geom_smooth(method = "loess", span = 0.25, se = FALSE) +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 13)) + theme_tq() + theme +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)) +
  
  labs(x = "",
       y = "Phần tr\u0103m",
       title = "Lợi tức trái phiếu chính phủ Mỹ kỳ hạn 10 n\u0103m",
       caption = "Thời \u0111iểm từ tháng 08/2009 \u0111ến cuối tháng 12/2010\nNguồn: FRED: https://fred.stlouisfed.org/series/DGS10")


# 6. Nam 2011:

# 6.1 Gia vang va DXY:

plot_2011 <- xau_dxy_gather_tbl %>% 
  mutate(year = year(date)) %>% 
  group_by(symbol) %>% 
  filter(date >= "2011-01-01" & date <= "2015-12-31") %>% 
  plot_time_series(date, value,
                   .facet_ncol = 1, .color_var = year, 
                   
                   .smooth = FALSE,
                   .smooth_span = 0.15,
                   .smooth_color = "cornflowerblue",
                   
                   .plotly_slider = FALSE,
                   
                   .line_color = "firebrick4",
                   .title = "Dollar Index & XAU 2011- 2015",
                   .legend_show = FALSE,
                   .interactive = TRUE)

htmltools::save_html(plot_2011, "plot_2011.html")


# 7. Nam 2019 - 2020

plot_2019 <- xau_dxy_gather_tbl %>% 
  mutate(year = year(date)) %>% 
  group_by(symbol) %>% 
  filter(date >= "2017-01-01" & date <= "2020-12-31") %>% 
  plot_time_series(date, value,
                   .facet_ncol = 1, .color_var = year, .facet_scales = "free",
                   
                   .smooth = TRUE,
                   .smooth_span = 0.15,
                   .smooth_color = "cornflowerblue", 
                   .smooth_size = 0.5,
                   
                   .plotly_slider = FALSE,
                   
                   .line_color = "firebrick4",
                   .title = "Dollar Index & XAU 2019- 2020",
                   .legend_show = FALSE,
                   .interactive = TRUE)

htmltools::save_html(plot_2019, "plot_2019.html")




# 8. TODO:  ----

# Lay months

months <- xau_tbl %>% 
  mutate(months = month(date, label = TRUE, abbr = FALSE)) %>% 
  pull() %>% 
  levels() %>% 
  as.character()

# Monthly Returns
xau_monthly_returns_tbl <- xau_tbl %>% 
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "monthly_returns") %>% 
  
  mutate(year  = year(date),
         month = month(date))

# Min monthly returns:
xau_min_monthly_returns_tbl <- xau_monthly_returns_tbl %>% 
  group_by(year) %>% 
  filter(monthly_returns == min(monthly_returns)) %>% 
  ungroup() %>% 
  filter(monthly_returns <= -0.07)

# Max monthly returns:
xau_max_monthly_returns_tbl <- xau_monthly_returns_tbl %>% 
  group_by(year) %>% 
  filter(monthly_returns == max(monthly_returns)) %>% 
  ungroup() %>% 
  filter(monthly_returns >= 0.07)

# Combined min + max returns:
xau_min_max_monthly_tbl <- xau_min_monthly_returns_tbl %>% 
  bind_rows(xau_max_monthly_returns_tbl) %>% 
  mutate(col_neg = ifelse(monthly_returns < 0, monthly_returns, NA_real_),
         col_pos = ifelse(monthly_returns > 0, monthly_returns,
                          NA_real_))

# Plot:
xau_min_max_monthly_tbl %>% 
  ggplot(aes(x = month)) + 
  
  geom_col(aes(y = col_neg),
           fill = "tomato",
           color = "tomato") +
  
  geom_col(aes(y = col_pos),
           fill = "cornflowerblue",
           color = "cornflowerblue") +
  
  expand_limits(y = 0) +
  
  scale_x_continuous(breaks = 1:12,
                     labels = months) +

  scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                     breaks = scales::pretty_breaks(n = 4)) +
  
  facet_wrap(~ year, ncol = 3, scales = "free_x") +
  
  theme_tq() + theme2 +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))



