# 1. Packages #### ####
source("header.R")
library(tidyquant)
library(ggrepel)

# 2. Import data #### #### 
vnm <- get_data_vnstock("037_vnm_etf/01_data/raw/VNM.csv")
etf <- get_data_vnstock("037_vnm_etf/01_data/raw/E1VFVN30.csv")

# 3. Gộp data #### ####
data_tbl <- left_join(etf, vnm, by = "date") %>% 
  drop_na() %>% 
  rename("etf" = price.x,
         "vnm" = price.y) %>% 
  pivot_longer(names_to = "symbol",
               values_to = "price", !date)

# 4. FUNCTION #### ####

plot_growth <- function(data_tbl, year){
  
  data_year <- data_tbl %>% 
    group_by(symbol) %>% 
    filter(date >= paste0(year, "-01-01")) %>% 
    tq_transmute(select = price,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "returns") %>% 
    mutate(growth = cumprod(1 + returns) - 1) %>% 
    ungroup() %>% 
    mutate(label_txt = if_else(date == max(date),
                               paste(toupper(symbol), ":", scales::percent(growth, big.mark = ".", decimal.mark = ",", accuracy = 0.01)),
                               NA_character_))
  
  
  ggplot(data_year, aes(x = date,
                        y = growth,
                        color = symbol)) +
    
    geom_line() + theme_tq() + theme +
    
    geom_label_repel(aes(label = label_txt),
                     force_pull = 0,
                     nudge_x = 150,
                     direction ="y",
                     max.overlaps = 10,
                     segment.size = 0.4,
                     segment.linetype = 2,
                     hjust = 0,
                     size = 7) +
    
    scale_x_date(expand = expansion(mult = c(0.02, 0.3)),
                 breaks = scales::pretty_breaks(n = 8)) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                       breaks = scales::pretty_breaks(n = 8)) +
    
    scale_color_manual(values = c("#f94144", "#1d3557")) +
    
    labs(x = "",
         y = "Tăng trưởng",
         title = paste0("Tăng trưởng của E1VFVN30 và VNM từ năm ", year),
         caption = "Giá cổ phiếu Vinamilk là giá đã điều chỉnh sau khi chia cổ tức.
         Nguồn dữ liệu: TCBS, tổng hợp bằng vnstock")
}

# 5. ANALYSIS #### ####

plot_growth(data_tbl, year = 2022)
