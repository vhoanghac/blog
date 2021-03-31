# 1. Packages
library(tidyverse) 
library(lubridate) #Date, time
library(tidyquant) #Theme, log returns
library(ggrepel) #Label
library(readxl) # Export, import
library(ggsci) # Theme

source("header.R")

# Load data:
source("023_chasing_performance/tidy_data.R")

# Back up:
funds_tbl <- full_data_cleaned

# Funds returns:
funds_r_tbl <- funds_tbl %>% 
  gather(symbol, price, -date) %>% 
  group_by(symbol) %>% 
  
  #Tinh log returns:
  tq_transmute(select     = price,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "returns") %>% 
  ungroup()



# PLOT LAST X DAYS (Nam truoc) #### #### #### #### #### #### 
# Plot function:
plot_performance <- function(n_day = 368){
  
  # Loc data theo so ngay n_day:
  slice <- funds_tbl %>% 
    slice(tail(row_number(), n_day))
  
  # PLOT
  slice %>% 
    select(-VNINDEX) %>% 
    gather(symbol, price, -date) %>% 
    mutate(symbol = symbol %>% as_factor()) %>% 
    group_by(symbol) %>% 
    
    tq_transmute(select = price, 
                 mutate_fun = periodReturn,
                 period = "daily", 
                 type = "log",
                 col_rename = "returns") %>% 
    
    # Tang truong danh muc:
    mutate(growth = exp(cumsum(returns))) %>% 
    
    # Label:
    mutate(label = if_else(date == max(date), as.character(symbol), NA_character_ )) %>% 
    
    ungroup() %>% 
    
    # Plot:
    ggplot(aes(x     = date, 
               y     = growth, 
               color = symbol)) +
    
    geom_line(size = 1.2) +
    
    # Tang kich thuoc chart de hien thi label:
    coord_cartesian(xlim = c(min(slice$date), max(slice$date) + 50)) +
    
    # Them label vao chart
    geom_label_repel(aes(label = label),
                     fontface = "bold",
                     segment.color = NA,
                     nudge_x = 20, 
                     na.rm = TRUE, 
                     label.size = 0.8) +
    
    theme_tq() + theme + scale_color_npg() +
    
    geom_hline(yintercept = 1, linetype = 2) +
    
    labs(x = "Thời gian (ngày)",
         y = "T\u0103ng tr\u01b0ởng",
         title = str_glue("Thành quả \u0111ầu t\u01b0 từ {first(slice$date)}"),
         caption = str_glue("Quãng thời gian từ {first(slice$date)} \u0111ến {last(slice$date)}\nLog returns. 1.2 là lãi 20%"))
  
}

# PLOT PERFORMANCE de lua chon quy cho danh muc:
# 368 = 1 nam
# 736 = 2 nam
# 1103 = 3 nam

plot_performance(n_day = 368)


plot_performance(n_day = 736)


plot_performance(n_day = 1103)


# PORTFOLIOS #### #### #### #### #### #### #### #### #### 

# portfolios returns function:

portfolio_returns <- function(data,
                              name = "best"){
  
  n_stock <- data %>% 
    distinct(symbol) %>% count()
  
  w <- rep(as.numeric(1 / n_stock), n_stock)
  
  portfolio <- data %>% 
    tq_portfolio(assets_col   = symbol,
                 returns_col  = returns,
                 weights      = w,
                 rebalance_on = "months",
                 geometric    = FALSE,
                 col_rename   = "returns") %>% 
    
    mutate(growth = exp(cumsum(returns)))
    
    names(portfolio) <- c("date", "returns", name)
    
    return(portfolio)
}


# PLOT danh muc quy #### #### #### #### #### ####

# Function:
plot_portfolios <- function(good = c("VFMVF4", "VFMVF1", "SSISCA"),
                            bad = c("MBVF", "TCEF", "BVFED")){
  
  good_funds <- funds_r_tbl %>% 
    filter(symbol %in% good)
  
  bad_funds <- funds_r_tbl %>% 
    filter(symbol %in% bad)
  
  good_funds_returns <- good_funds %>% 
    portfolio_returns(name = "Good")
  
  bad_funds_returns <- bad_funds %>% 
    portfolio_returns(name = "Bad")
  
  index_returns <- funds_r_tbl %>% 
    portfolio_returns(name = "Index")
  
  vnindex_returns <- funds_r_tbl %>% 
    filter(symbol == "VNINDEX") %>% 
    portfolio_returns(name = "VNINDEX")
  
  
  # PLOT:
  
  good_funds_returns %>% 
    left_join(bad_funds_returns, by = "date") %>% 
    left_join(index_returns, by = "date") %>% 
    # left_join(vnindex_returns, by = "date") %>% #hide vnindex
    select(date, Good, Bad, Index) %>% 
    gather(symbol, price, -date) %>% 
    mutate(symbol = symbol %>% as_factor()) %>% 
    
    ggplot(aes(x = date, y = price, color = symbol)) + 
    geom_line(size = 1.2) +  theme_tq() + theme2 + 
    
    theme(legend.direction = "vertical",
          legend.text = element_text(face = "bold", size = 13)) +
    
    scale_color_manual(values = c("#00A087FF", "#E64B35FF", "#8491B4B2"),
                       labels = c("Good (tái cân bằng mỗi tháng)",
                                  "Bad (tái cân bằng mỗi tháng)",
                                  "Index quỹ mở")) +
    
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
    
    geom_hline(yintercept = 1, linetype = 2) +
    labs(x = "",
         y = "T\u0103ng tr\u01b0ởng",
         title = "Thành quả hoạt \u0111ộng của các danh mục",
         color = "",
         caption = "Quãng thời gian từ 09/09/2015 \u0111ến 21/03/2021
             Log returns. 1.2 là lãi 20%")
}


# 

plot_portfolios(good = c("VFMVF1", "VFMVF4", "SSISCA"),
                bad = c("MBVF", "TCEF", "BVFED"))
