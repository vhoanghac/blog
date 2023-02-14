# 1. Packages #### ####
source("header.R") #Tidyverse, data.table, lubridate, and ggtext
library(tidyquant)


options(scipen = 999)

# 2. Import data #### ####
paths <- fs:::dir_ls("032_drawdowns/01_data/tidied/")
datanames <- gsub("\\.csv$","", list.files(path = "032_drawdowns/01_data/tidied",
                                           pattern = "\\.csv$")) %>% 
  tolower()

# 2.1 Gop cac du lieu vao mot list
list <- paths %>% 
  map(function(path){
    fread(path)
  }) %>% 
  set_names(datanames) %>% 
  
  lapply(function(x){
    
    x %>% 
      filter(date >= "2021-06-01") %>% 
      summarise(max_price = max(price),
                current_price = last(price)) %>% 
      mutate(profit = (max_price / current_price) - 1) %>% 
      mutate(profit_txt = scales::percent(profit))
  })
# 2.2 Xuat du lieu ra dang tbl
profit_tbl <- list %>% 
  enframe() %>% 
  unnest(value) %>% 
  mutate(name = fct_reorder(toupper(name), profit))

# 3. Phan tich #### ####

profit_tbl %>% 
  ggplot(aes(x = name, y = profit)) +
  geom_bar(stat = "identity", fill = "#2a9d8f") + 
  coord_flip() +
  geom_text(aes(label = profit_txt), hjust = 1.2, color = "white", size = 5) +
  
  theme_tq() + theme2 + 
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +

  labs(x = "",
       y = "Lợi nhuận",
       title = "Giá giảm là cơ hội để đầu tư",
       subtitle = "Nếu mua bây giờ, bạn sẽ lời bao nhiêu nếu giá về lại đỉnh cũ?",
       caption = "Nguồn dữ liệu: investing.com
       Thời điểm cập nhật: 25/11/2022") 

