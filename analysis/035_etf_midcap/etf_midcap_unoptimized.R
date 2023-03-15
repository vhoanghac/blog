# 1. Packages #### ####
source("header.R") #Tidyverse, data.table, lubridate
library(tidyquant)
library(timetk)
library(ggrepel) #Labels cho chart



# 2. Import data #### ####

data_ind <- get_data_tradingview("035_etf_midcap/01_data/raw/ind_mid_small_vn30.csv") %>% 
  rename(midcap   = price,
         vn30     = vn30_hose_price,
         smallcap = vnsmallcap_hose_price)

data_tri <- get_data_tradingview("035_etf_midcap/01_data/raw/tri_mid_small_vn30.csv") %>% 
  rename(midcap   = price,
         vn30     = vn30tri_hose_price,
         smallcap = vnsmalltri_hose_price)


# 3. FUNCTION Tính tăng trưởng #### ####

calculate_growth <- function(data, symbols) {
  data %>%
    pivot_longer(cols = all_of(symbols),
                 names_to = "symbol",
                 values_to = "price") %>%
    group_by(symbol) %>%
    tq_transmute(select = price,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "returns") %>%
    mutate(growth = cumprod(1 + returns) - 1) %>%
    ungroup() 
}


# 4. Phân tích MIDCAP + VN30 (KHÔNG CỔ TỨC) #### ####

# 4.1 Tính lợi nhuận
data_ind_ret <- calculate_growth(data_ind, c("midcap", "vn30"))


data_ind_cumret <- data_ind_ret %>% 
  select(date, symbol, returns, growth) %>% 
  mutate(label_txt = if_else(date == max(date),
                             paste(toupper(symbol), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_))
# 4.2 Annualized return
data_ind_ret %>% 
  group_by(symbol) %>% 
  tq_performance(Ra = returns,
                 performance_fun = table.AnnualizedReturns)

# 4.2 Plot
data_ind_cumret %>% 
  ggplot(aes(x = date, y = growth, color = symbol)) + 
  
  geom_line(linewidth = 0.8) + theme_tq() + theme +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0, .4))) +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 150,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) + 
  
  scale_color_manual(values = c("#f94144", "#1d3557")) +
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của cổ phiếu \n MIDCAP và VN30 từ năm 2014",
       subtitle = "<span style = 'color:#f94144;'>MIDCAP</span> có tỷ lệ tăng trưởng bình quân <span style = 'color:#f94144;'>9%/năm</span>, trong khi đó <span style = 'color:#1d3557;'>VN30</span> là <span style = 'color:#1d3557;'>5,5%/năm</span>
       ",
       caption = "Nguồn dữ liệu: TradingView.
       Lưu ý: Các chỉ số chỉ phản ánh biến động về giá của cổ phiếu và chưa có bao gồm cổ tức.")



# 5. Phân tích MIDCAP + VN30 (CÓ CỔ TỨC) #### ####

# 5.1 Tính lợi nhuận
data_tri_ret <- calculate_growth(data_tri, c("midcap", "vn30"))


data_tri_cumret <- data_tri_ret %>%
  select(date, symbol, returns, growth) %>% 
  mutate(symbol = ifelse(symbol == "midcap", "midcap-tri", "vn30-tri")) %>% 
  mutate(label_txt = if_else(date == max(date),
                             paste(toupper(symbol), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)),
                             NA_character_))

# 5.2 Plot
data_tri_cumret %>% 
  ggplot(aes(x = date, y = growth, color = symbol)) + 
  
  geom_line(linewidth = 0.8) + theme_tq() + theme +
  
  scale_x_date(breaks = scales::pretty_breaks(n = 10),
               expand = expansion(mult = c(0, .35))) +
  
  geom_label_repel(aes(label = label_txt),
                   force_pull = 0,
                   nudge_x = 150,
                   direction ="y",
                   max.overlaps = 10,
                   segment.size = 0.4,
                   segment.linetype = 2,
                   hjust = 0,
                   size = 7) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) + 
  
  scale_color_manual(values = c("#f94144", "#1d3557")) +
  
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của cổ phiếu \n MIDCAP-TRI và VN30-TRI từ năm 2016",
       subtitle = "Đầu tư vào <span style = 'color:#f94144;'>MIDCAP-TRI</span> mang lại lợi nhuận hằng năm là <span style = 'color:#f94144;'>10,5%/năm</span> <br> 
       Trong khi đó <span style = 'color:#1d3557;'>VN30-TRI</span> là <span style = 'color:#1d3557;'>10,3%/năm</span>
       ",
       caption = "Nguồn dữ liệu: TradingView.
       Lưu ý: Các chỉ số TRI (Total return index) đã bao gồm thu nhập từ các khoản cổ tức.")


# 6. DRAWDOWN MIDCAP-TRI + VN30-TRI #### ####

# 6.1 Data cho biểu đồ
dd_plot <- data_tri_ret %>% 
  group_by(symbol) %>% 
  mutate(maxgrowth = cummax((growth + 1))) %>% 
  mutate(drawdown = (growth + 1) / maxgrowth - 1) %>%
  ungroup()

# 6.2 Tạo label để ghi subtitle
dd_plot_label <- dd_plot %>%
  group_by(symbol) %>% 
  summarise(min = min(drawdown)) %>% 
  mutate(text = scales::percent(min, big.mark = ".", decimal.mark = ",", accuracy = 0.01)) 

dd_mid_txt <- dd_plot_label %>% 
  filter(symbol == "midcap")
dd_vn30_txt <-  dd_plot_label %>% 
  filter(symbol == "vn30")

# 6.3 Plot
dd_plot %>% 
  mutate(symbol = as.factor(if_else(symbol == "midcap", "MIDCAP - TRI", "VN30 - TRI"))) %>% 
  ggplot(aes(x = date, y = drawdown, color = symbol)) +
  
  geom_line(linewidth = 0.8) + 

  geom_hline(yintercept = 0, linetype = "dashed" ) +
  
  
  facet_wrap(~symbol, ncol = 2) + theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  scale_color_manual(values = c("#f94144", "#1d3557")) +
  
  labs(x = "",
       y = "Tỷ lệ",
       title = "Tỷ lệ sụt giảm từ đỉnh \n của MIDCAP-TRI và VN30-TRI",
       subtitle = str_glue("Từ năm 2016 cho đến 2023. <br>
                           Tỷ lệ sụt giảm lớn nhất của <span style = 'color:#f94144;'>MIDCAP-TRI</span> là <span style = 'color:#f94144;'>{dd_mid_txt$text}</span> <br>
                           Trong khi đó của <span style = 'color:#1d3557;'>VN30-TRI</span> là <span style = 'color:#1d3557;'>{dd_vn30_txt$text}</span>"),
       caption = "Nguồn dữ liệu: TradingView.
       Lưu ý: Các chỉ số TRI (Total return index) đã bao gồm thu nhập từ các khoản cổ tức.")
  

# 7. Annualized return + Rolling annualized returns

roll_ann_ret <- data_tri_ret %>% 
  group_by(symbol) %>% 
  tk_augment_slidify(.value = returns,
                     .period = 250,
                     .f = Return.annualized,
                     scale = 250,
                     .names = "roll_timetk",
                     .align = "right") %>% 
  ungroup()
  
roll_ann_ret %>% View()

roll_ann_ret %>% 
  select(-returns, -growth) %>% 
  pivot_wider(names_from = symbol,
              values_from = roll_timetk) %>% 
  mutate(diff = midcap - vn30) %>%
  
  ggplot(aes(x = date, y = diff)) + 
  
  geom_line() + theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(-0.55, 0.55)) + 
  
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  annotate("text", 
           x = as.Date("2019-06-01"), 
           y = 0.5, 
           size = 6,
           label = "Đầu tư MIDCAP-TRI hiệu quả hơn",
           color = "#f94144") +
  
  annotate("text", 
           x = as.Date("2019-06-01"), 
           y = -0.5, 
           size = 6,
           label = "Đầu tư VN30-TRI hiệu quả hơn",
           color = "#1d3557") +
  
  labs(x = "",
       y = "Chênh lệch",
       title = "MIDCAP(TRI) - VN30(TRI) \n Chênh lệch tỷ suất lợi nhuận trung bình năm \n Thời gian nắm giữ: 03 năm",
       caption = "Nguồn dữ liệu: TradingView.
       Lưu ý: Các chỉ số TRI (Total return index) đã bao gồm thu nhập từ các khoản cổ tức.
       Annualized return giả định thời gian nắm giữ là 03 năm (tương đương 750 ngày giao dịch)")





# 7. Yearly return #### ####

year <- data_tri %>% 
  mutate(year = floor(year(date))) %>% 
  group_by(year) %>% 
  summarise(min_date = min(date),
            max_date = max(date)) %>% 
  ungroup()
  
first_values <- data_tri %>% 
  inner_join(year %>% rename(date = min_date) %>% select(date, year)) %>% 
  select(-smallcap)

last_values <- data_tri %>% 
  inner_join(year %>% rename(date = max_date) %>% select(date, year)) %>% 
  rename(last_midcap = midcap,
         last_vn30 = vn30) %>% 
  select(year, contains("last_"))

year_plot <- first_values %>% 
  inner_join(last_values) %>% 
  mutate(diff_midcap = (last_midcap / midcap) - 1,
         diff_vn30 = (last_vn30 / vn30) - 1) %>% 
  select(year, contains("diff_")) %>% 
  gather(-year, key = key, value = value) %>% 
  mutate(key = if_else(key == "diff_midcap", "MIDCAP-TRI", "VN30-TRI"))


year_plot %>% 
  filter(year < 2023) %>% 
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") + theme_tq() + theme2 +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  
  scale_fill_manual(values = c("#f94144", "#1d3557")) +
  
  labs(x = "",
       y = "Hiệu suất (%)",
       title = "Hiệu suất theo từng năm")



# 8. REBALANCE PORTFOLIO
# Set weight
w <- c(0.5, 0.5)

# Portfolio
portfolio <- data_tri_ret %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "years") %>% 
  mutate(symbol = "portfolio") %>% 
  mutate(growth = cumprod(1 + returns) - 1)


portfolio_plot <- rbind(data_tri_ret, portfolio)

portfolio_plot %>% 
  mutate(symbol = case_when(symbol == "midcap" ~ "MIDCAP-TRI",
                            symbol == "vn30" ~ "VN30-TRI",
                            symbol == "portfolio" ~ "PORTFOLIO")) %>% 
  
  
  ggplot(aes(x = date, y = growth, color = symbol)) +
  geom_line() + theme_tq() + theme2 + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) + 
  scale_x_date(breaks = scales::pretty_breaks(n = 8)) +
  scale_color_manual(values = c("#f94144", "#8ac926", "#1d3557")) +
  labs(x = "",
       y = "Tăng trưởng",
       title = "Tăng trưởng của danh mục 50/50 từ năm 2016",
       caption = "Nguồn dữ liệu: TravingView.
       Danh mục PORTFOLIO với tỷ trọng 50% MIDCAP-TRI và 50% VN30-TRI. Tái cân bằng hằng năm.")



# 9. DRAWDOWN PORTFOLIO #### ####

# 9.1 Data cho biểu đồ
portfolio_dd_plot <- portfolio_plot %>% 
  group_by(symbol) %>% 
  mutate(maxgrowth = cummax((growth + 1))) %>% 
  mutate(drawdown = (growth + 1) / maxgrowth - 1) %>%
  ungroup()

# 6.2 Tạo label để ghi subtitle
portfolio_dd_plot_label <- portfolio_dd_plot %>%
  group_by(symbol) %>% 
  summarise(min = min(drawdown)) %>% 
  mutate(text = scales::percent(min, big.mark = ".", decimal.mark = ",", accuracy = 0.01)) 

portfolio_dd_mid_txt <- portfolio_dd_plot_label %>% 
  filter(symbol == "midcap")
portfolio_dd_vn30_txt <-  portfolio_dd_plot_label %>% 
  filter(symbol == "vn30")
portfolio_dd_port_txt <- portfolio_dd_plot_label %>% 
  filter(symbol == "portfolio")


# 6.3 Plot
portfolio_dd_plot %>% 
  mutate(symbol = as.factor(case_when(symbol == "midcap" ~ "MIDCAP-TRI",
                                      symbol == "vn30" ~ "VN30-TRI",
                                      symbol == "portfolio" ~ "PORTFOLIO"))) %>% 
  ggplot(aes(x = date, y = drawdown, color = symbol)) +
  
  geom_line(linewidth = 0.8) + 
  
  geom_hline(yintercept = 0, linetype = "dashed" ) +
  
  
  facet_wrap(~symbol, ncol = 3) + 
  
  
  theme_tq() + theme + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(n = 5)) +
  
  scale_color_manual(values = c("#f94144", "#8ac926", "#1d3557")) +
  
  labs(x = "",
       y = "Tỷ lệ",
       title = "Tỷ lệ sụt giảm từ đỉnh \n của MIDCAP-TRI, VN30-TRI và PORTFOLIO 50/50",
       subtitle = str_glue("Từ năm 2016 cho đến 2023. <br>
                           Tỷ lệ sụt giảm lớn nhất của <span style = 'color:#f94144;'>MIDCAP-TRI</span> là <span style = 'color:#f94144;'>{dd_mid_txt$text}</span> <br>
                           <span style = 'color:#1d3557;'>VN30-TRI</span> là <span style = 'color:#1d3557;'>{dd_vn30_txt$text}</span> <br>
                           <span style = 'color:#8ac926;'>PORTFOLIO</span> là <span style = 'color:#8ac926;'>{portfolio_dd_port_txt$text}</span>"),
       caption = "Nguồn dữ liệu: TradingView.
       Lưu ý: Các chỉ số TRI (Total return index) đã bao gồm thu nhập từ các khoản cổ tức.")
