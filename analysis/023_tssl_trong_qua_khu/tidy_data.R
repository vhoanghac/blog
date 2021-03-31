# 1. Packages #### #### #### #### 
library(tidyverse)
library(readxl)
library(lubridate)
library(fs) #Import multiple CSV files
library(data.table)

source("header.R")
# 2. Import data #### #### #### #### 

# CSV paths:
paths <- fs::dir_ls("023_chasing_performance/data_raw")

# Get file names:
datanames <-  gsub("\\.csv$","", list.files(path = "023_chasing_performance/data_raw", pattern = "\\.csv$")) %>% 
  tolower()

# Import multiple csv:
list <- paths %>% 
  map(function(path){
    read_csv(path)
  })


list %>% 
  set_names(datanames) %>% 
  map(as_tibble) %>% 
  list2env(envir = .GlobalEnv)

# Import VNINDEX:

vnindex <- get_data_co_phieu(ticker = "^VNINDEX")

vnindex



# 3. TIDY #### #### #### #### #### 

bvfed_d <- bvfed %>% 
  filter(date >= "2015-09-09") %>% 
  rename(BVFED = price)

mbvf_d <- mbvf %>% 
  filter(date >= "2015-09-09") %>%
  rename(MBVF = price)

ssisca_d <- ssisca  %>% 
  filter(date >= "2015-09-09") %>% 
  rename(SSISCA = price)

tcef_d <- tcef %>%
  rename(TCEF = price)

vcbfbcf_d <- vcbfbcf %>% 
  filter(date >= "2015-09-09") %>% 
  rename(VCBFBCF = price)

veof_d <- veof %>% 
  filter(date >= "2015-09-09") %>% 
  rename(VEOF = price)

vfmvf1_d <- vfmvf1 %>% 
  filter(date >= "2015-09-09") %>% 
  rename(VFMVF1 = price)

vfmvf4_d <- vfmvf4 %>% 
  filter(date >= "2015-09-09") %>% 
  rename(VFMVF4 = price)

vfmvf4_d %>% tail()

vnindex_d <- vnindex %>% 
  filter(date >= "2015-09-09" & date <= "2021-03-21") %>% 
  select(date, VNINDEX = price)


vnindex_d %>% tail()

full_data <- bvfed_d %>% 
  full_join(ssisca_d, by = "date") %>% 
  full_join(mbvf_d, by = "date") %>% 
  full_join(tcef_d, by = "date") %>%
  full_join(vcbfbcf_d, by = "date") %>%
  full_join(veof_d, by = "date") %>% 
  full_join(vfmvf1_d, by = "date") %>% 
  full_join(vfmvf4_d, by = "date") %>% 
  
  # Join VNINDEX:
  full_join(vnindex_d, by = "date") %>% 
  arrange(date)

full_data_cleaned <- full_data %>% 
  fill(c(BVFED:VNINDEX)) %>% 
  fill(c(MBVF, VEOF), .direction = "up")


full_data_cleaned %>% tail() %>% View()
