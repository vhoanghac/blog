# 1. Packages #### #### #### #### 
library(tidyverse)
library(lubridate)
library(readxl)
library(tidyquant)
library(ggrepel) #Label

# Other
source("header.R")

# 2. Import data #### #### #### #### 

# CSV paths:
paths <- fs::dir_ls("027_etfs_mfunds/data")

# Get file names:
datanames <-  gsub("\\.csv$","", list.files(path = "027_etfs_mfunds/data", pattern = "\\.csv$")) %>% 
  tolower()

# Import multiple csv to a list:
list <- paths %>% 
  map(function(path){
    read_csv(path)
  })

# Convert to monthly data:
list %>% 
  set_names(datanames) %>% 

  lapply(function(x){

    x %>%
      
      # First day of each month in the dataset
      summarise_by_time(.date_var = date,
                        .by       = "month",
                        price     = first(price),
                        .type     = "floor") %>% 
      
      # Calculate monthly returns
      tq_transmute(select     = price,
                   mutate_fun = periodReturn,
                   period     = "monthly",
                   col_rename = "returns")

  }) %>%
  
  list2env(envir = .GlobalEnv)


# 3. Phan tich so sanh voi cac quy mo: #### #### #### #### 

# 3.1 Function: 

phantich01 <- function(quy1      = "e1vfvn30", 
                       quy2      = "vndaf",
                       k         = 12,
                       plot_annr = TRUE){
  
  # Merge data
  x <- get(quy1)
  y <- get(quy2)
  
  merged <- x %>%   
    inner_join(y, by = "date")
  merged[1, 2:3] <- 0 

  
  # Convert to long data format
  merged_long <- merged %>% 
    pivot_longer(names_to = "portfolio", values_to = "returns", cols = c(2:3)) %>% 
    group_by(portfolio)
  
  
  # Calculate returns
  data_r <- merged_long %>% 
    
    # Cumulative returns (full period)
    mutate(growth = cumprod(1 + returns) - 1 ) %>% 
    
    # Annual return (Geometric) (k = 12-month period):
    mutate(roll_annr = rollapply(returns, 
                                 FUN   = function(x) prod(1+x) - 1,
                                 width = k,
                                 align = "right",
                                 fill  = NA)) %>% 
    ungroup() %>% 
    
    # Labels for charts:
    mutate(per_text = if_else(date == max(date), 
                              scales::percent(roll_annr, accuracy = 1L),
                              NA_character_),
           
           quy_text = case_when(portfolio == "returns.x" ~ str_glue("{toupper(quy1)}"),
                                portfolio == "returns.y" ~ str_glue("{toupper(quy2)}")),
           
           
           full_text_annual = if_else(date == max(date),
                                      str_glue("{quy_text}: {per_text}"),
                                      NA_character_),
           
           # Cumulative chart:
           cum_r_text = if_else(date == max(date),
                                scales::percent(growth, accuracy = 1L),
                                NA_character_),
           
           full_text_cumr = ifelse(date == max(date), 
                                   str_glue("{quy_text}: {cum_r_text}"),
                                   NA_character_))
  
  # Win/Loss table:
  wl_table <- merged %>%
    mutate(diff = returns.x - returns.y) %>% 
    slice(-1) %>% 
    
    mutate(win  = ifelse(diff > 0, 1, 0),
           loss = ifelse(diff < 0, 1, 0 )) %>% 
    
    summarise(n_win  = sum(win),
              n_loss = sum(loss))
  
  
  # Table: Annualized return (full period)
  annr_table <- merged_long %>%
    tq_performance(Ra              = returns,
                   performance_fun = Return.annualized) %>% 
    
    ungroup() %>% 
    
    mutate(portfolio = case_when(portfolio == "returns.x" ~ str_glue("{quy1}"),
                                 portfolio == "returns.y" ~ str_glue("{quy2}")))
  

  # IF ELSE plot:
  if(plot_annr == TRUE){
    
    plot1 <- data_r %>% 
      
      ggplot(aes(x = date, y = roll_annr, color = portfolio)) +
      
      geom_line(size = 1) + theme_tq() + theme +
      
      coord_cartesian(xlim = c(min(data_r$date), max(data_r$date) + 400)) +
      
      geom_label_repel(aes(label     = full_text_annual),
                       fontface      = "bold",
                       segment.color = NA,
                       nudge_x       = 300,
                       na.rm         = TRUE,
                       label.size    = 0.8) +
     
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                         breaks = scales::pretty_breaks(n = 10)) +
      
      scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
      
      scale_color_manual(values = c("#DC0000B2", "#3C5488B2"),
                         labels = c(str_glue("{toupper(quy1)}"), str_glue("{toupper(quy2)}"))) + 
      
      geom_hline(yintercept = 0, linetype = "dashed") +
      
      labs(x        = "",
           y        = "",
           title    = str_glue("Lợi nhuận \u0111ầu t\u01B0 quỹ {toupper(quy1)} và {toupper(quy2)}"),
           subtitle = str_glue("Chu kỳ nắm giữ: {k} tháng"),
           caption  = str_glue("Rolling returns."))
   
    list(plot1, wl_table, annr_table)
    
  } else {
    
    plot2 <- data_r %>% 
      ggplot(aes(x = date, y = growth, color = portfolio)) +
      
      geom_line(size = 1) + theme_tq() + theme +
      
      coord_cartesian(xlim = c(min(data_r$date), max(data_r$date) + 400)) +
      
      geom_label_repel(aes(label     = full_text_cumr),
                       fontface      = "bold",
                       segment.color = NA,
                       nudge_x       = 300,
                       na.rm         = TRUE,
                       label.size    = 0.8) +
      
      scale_y_continuous(labels = scales::percent_format(),
                         breaks = scales::pretty_breaks(n = 10)) +
      
      scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
      
      scale_color_manual(values = c("#DC0000B2", "#3C5488B2"),
                         labels = c(str_glue("{toupper(quy1)}"), str_glue("{toupper(quy2)}"))) +
      
      geom_hline(yintercept = 0, linetype = "dashed") +
      
      labs(x        = "",
           y        = "",
           title    = str_glue("Lợi nhuận tích l\u0169y của quỹ {toupper(quy1)} và {toupper(quy2)}"),
           subtitle = "",
           caption  = str_glue("Lợi nhuận tích l\u0169y giả \u0111ịnh cùng \u0111ầu t\u01B0 vào hai quỹ tại thời \u0111iểm {first(data_r$date)}"))
    
    list(plot2, wl_table, annr_table)
  }
  
}


# 3.2 Analysis:

phantich01(quy1 = "e1vfvn30", quy2 = "vcbftbf", k = 12, plot_annr = TRUE)

phantich01(quy1 = "e1vfvn30", quy2 = "vcbftbf", k = 12, plot_annr = FALSE)
