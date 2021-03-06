# IPO 

# Import package

library(tidyverse)
library(tidyquant)
library(readxl)
library(data.table)
library(viridis) # color heat map
library(writexl) # Export to .xlsx
library(scales) # De scale plot line

# Check sheets 
excel_sheets("H:/Dropbox/Work/R/Data/IPO/list.xlsx")

# Chuan bi period
period <- c(2, 3, 4, 5, 10, 30, 60, 120, 180, 360)
prefix <- "Day"
cnames <- paste(prefix, period, sep = " ")

# Chuan bi theme
theme <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"))

point <- format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)


################# 2014 ####################
# Import 2014
data_2014 <- read_excel("H:/Dropbox/Work/R/Data/IPO/list.xlsx", sheet = "2014")
data_2014$date <- as.Date(data_2014$date) # Set lai date

# 2014 daily returns + Cummulative returns
data_2014_returns <- data_2014 %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cum_returns = round((cr - 1)*100, 2))

# Xuat table cum_returns cua tung co phieu
stock2014 <- unique(data_2014$symbol)

table_2014 <- data.table(data_2014_returns) #Convert

table_2014_returns <- table_2014[symbol %chin% stock2014, .(returns = cum_returns[period]), by = symbol]  # Loc co phieu

table_2014_returns <- cbind(table_2014_returns, cnames)
table_2014_returns$cnames <- factor(table_2014_returns$cnames, levels = unique(table_2014_returns$cnames))

# Table 2014 hoan chinh :
y2014 <- spread(table_2014_returns, cnames, returns)

# Export to Excel 
write_xlsx(y2014, "H:\\Dropbox\\Work\\R\\Data\\IPO\\data.xlsx")

# Plot heatmap 2014

ggplot(data = table_2014_returns, mapping = aes(x = cnames, y = symbol, fill = returns) )+
  geom_tile() +
  scale_fill_gradient2(name = "TSSL (%)", low = "#E81123" , high = "#00B294", mid = "#EEEEEE" , space = "Lab") +
  labs(title = "Tỷ suất sinh lợi các cổ phiếu lên HOSE n\u0103m 2014", y = "Cổ phiếu", x = "Ngày") + theme

# Plot line
ggplot(data_2014, aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol) +
  scale_y_log10(labels = point) +
  labs(title = "Giá của cổ phiếu sau khi lên HOSE vào n\u0103m 2014", y = "Giá \u0111ã \u0111iều chỉnh (log)", x = "N\u0103m") + 
  geom_smooth(method = "lm") +
  theme_tq() + theme(legend.position = "none") + theme

### Plot cumreturns
table_2014 %>%
  ggplot(aes(x=date, y = cr, color = symbol))+
  geom_line(size = 1) +
  facet_wrap(~symbol, ncol = 3, scales = "free_y") +
  theme_tq() +
  geom_hline(yintercept = 1, linetype = "dashed", color ="red", size = 1) +
  labs(title = "Giá trị khoản \u0111ầu t\u01B0 1 \u0111ồng từ 2014 \u0111ến 2020", y = "Lợi nhuận", x = "N\u0103m") + theme(legend.position = "none") + theme

################ 2015 #########################
# Import 2015
data_2015 <- read_excel("H:/Dropbox/Work/R/Data/IPO/list.xlsx", sheet = "2015")
data_2015$date <- as.Date(data_2015$date)

# daily returns + cum returns
data_2015_returns <- data_2015 %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cum_returns = round((cr - 1)*100, 2))

# Xuat table cum_returns tung co phieu
stock2015 <- unique(data_2015$symbol)

table_2015 <- data.table(data_2015_returns)

table_2015_returns <- table_2015[symbol %chin% stock2015, .(returns = cum_returns[period]), by = symbol]

table_2015_returns <- cbind(table_2015_returns, cnames)
table_2015_returns$cnames <- factor(table_2015_returns$cnames, levels = unique(table_2015_returns$cnames))

y2015 <- spread(table_2015_returns, cnames, returns)
# Export to Excel 
write_xlsx(y2015, "H:\\Dropbox\\Work\\R\\Data\\IPO\\data2015_1.xlsx")

# Plot heat map 2015

ggplot(data = table_2015_returns, mapping = aes(x = cnames, y = symbol, fill = returns) )+
  geom_tile() +
  scale_fill_gradient2(name = "TSSL (%)", low = "#E81123" , high = "#00B294", mid = "#EEEEEE" , space = "Lab") +
  labs(title = "Tỷ suất sinh lợi các cổ phiếu lên HOSE n\u0103m 2015", y = "Cổ phiếu", x = "Ngày") + theme

### Plot cumreturns
table_2015 %>%
  ggplot(aes(x=date, y = cr, color = symbol))+
  geom_line(size = 1) +
  facet_wrap(~symbol, ncol = 3, scales = "free_y") +
  theme_tq() +
  geom_hline(yintercept = 1, linetype = "dashed", color ="red", size = 1) +
  labs(title = "Giá trị khoản \u0111ầu t\u01B0 1 \u0111ồng từ 2015 \u0111ến 2020", y = "Lợi nhuận", x = "N\u0103m") + theme(legend.position = "none") + theme


################ 2016 #########################
# Import 2016
data_2016 <- read_excel("H:/Dropbox/Work/R/Data/IPO/list.xlsx", sheet = "2016")
data_2016$date <- as.Date(data_2016$date)

# daily returns + cum returns
data_2016_returns <- data_2016 %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cum_returns = round((cr - 1)*100, 2))

# Xuat table cum_returns tung co phieu
stock2016 <- unique(data_2016$symbol)

table_2016 <- data.table(data_2016_returns)

table_2016_returns <- table_2016[symbol %chin% stock2016, .(returns = cum_returns[period]), by = symbol]

table_2016_returns <- cbind(table_2016_returns, cnames)
table_2016_returns$cnames <- factor(table_2016_returns$cnames, levels = unique(table_2016_returns$cnames))

y2016 <- spread(table_2016_returns, cnames, returns)
# Export to Excel 
write_xlsx(y2016, "H:\\Dropbox\\Work\\R\\Data\\IPO\\data2016.xlsx")

# Plot heat map 2016

ggplot(data = table_2016_returns, mapping = aes(x = cnames, y = symbol, fill = returns) )+
  geom_tile() +
  scale_fill_gradient2(name = "TSSL (%)", low = "#E81123" , high = "#00B294", mid = "#EEEEEE" , space = "Lab") +
  labs(title = "Tỷ suất sinh lợi các cổ phiếu lên HOSE n\u0103m 2016", y = "Cổ phiếu", x = "Ngày") + theme

### Plot cumreturns
table_2016 %>%
  ggplot(aes(x=date, y = cr, color = symbol))+
  geom_line(size = 1) +
  facet_wrap(~symbol, ncol = 3, scales = "free_y") +
  theme_tq() +
  geom_hline(yintercept = 1, linetype = "dashed", color ="red", size = 1) +
  labs(title = "Giá trị khoản \u0111ầu t\u01B0 1 \u0111ồng từ 2016 \u0111ến 2020", y = "Lợi nhuận", x = "N\u0103m") + theme(legend.position = "none") + theme


################ 2017 #########################
# Import 2017
data_2017 <- read_excel("H:/Dropbox/Work/R/Data/IPO/list.xlsx", sheet = "2017")
data_2017$date <- as.Date(data_2017$date)

# daily returns + cum returns
data_2017_returns <- data_2017 %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cum_returns = round((cr - 1)*100, 2))

# Xuat table cum_returns tung co phieu
stock2017 <- unique(data_2017$symbol)

table_2017 <- data.table(data_2017_returns)

table_2017_returns <- table_2017[symbol %chin% stock2017, .(returns = cum_returns[period]), by = symbol]

table_2017_returns <- cbind(table_2017_returns, cnames)
table_2017_returns$cnames <- factor(table_2017_returns$cnames, levels = unique(table_2017_returns$cnames))

y2017 <- spread(table_2017_returns, cnames, returns)
# Export to Excel 
write_xlsx(y2017, "H:\\Dropbox\\Work\\R\\Data\\IPO\\data2017.xlsx")

# Plot heat map 2017

ggplot(data = table_2017_returns, mapping = aes(x = cnames, y = symbol, fill = returns) )+
  geom_tile() +
  scale_fill_gradient2(name = "TSSL (%)", low = "#E81123" , high = "#00B294", mid = "#EEEEEE" , space = "Lab") +
  labs(title = "Tỷ suất sinh lợi các cổ phiếu lên HOSE n\u0103m 2017", y = "Cổ phiếu", x = "Ngày") + theme

### Plot cumreturns
table_2017 %>%
  ggplot(aes(x=date, y = cr, color = symbol))+
  geom_line(size = 1) +
  facet_wrap(~symbol, ncol = 3, scales = "free_y") +
  theme_tq() +
  geom_hline(yintercept = 1, linetype = "dashed", color ="red", size = 1) +
  labs(title = "Giá trị khoản \u0111ầu t\u01B0 1 \u0111ồng từ 2017 \u0111ến 2020", y = "Lợi nhuận", x = "N\u0103m") + theme(legend.position = "none") + theme

################ 2018 #########################
# Import 2018
data_2018 <- read_excel("H:/Dropbox/Work/R/Data/IPO/list.xlsx", sheet = "2018")
data_2018$date <- as.Date(data_2018$date)

# daily returns + cum returns
data_2018_returns <- data_2018 %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "returns") %>%
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cum_returns = round((cr - 1)*100, 2))

# Xuat table cum_returns tung co phieu
stock2018 <- unique(data_2018$symbol)

table_2018 <- data.table(data_2018_returns)

table_2018_returns <- table_2018[symbol %chin% stock2018, .(returns = cum_returns[period]), by = symbol]

table_2018_returns <- cbind(table_2018_returns, cnames)
table_2018_returns$cnames <- factor(table_2018_returns$cnames, levels = unique(table_2018_returns$cnames))

y2018 <- spread(table_2018_returns, cnames, returns)
# Export to Excel 
write_xlsx(y2018, "H:\\Dropbox\\Work\\R\\Data\\IPO\\data2018.xlsx")

# Plot heat map 2018
ggplot(data = table_2018_returns, mapping = aes(x = cnames, y = symbol, fill = returns) )+
  geom_tile() +
  scale_fill_gradient2(name = "TSSL (%)", low = "#E81123" , high = "#00B294", mid = "#EEEEEE" , space = "Lab") +
  labs(title = "Tỷ suất sinh lợi các cổ phiếu lên HOSE n\u0103m 2018", y = "Cổ phiếu", x = "Ngày") + theme

### Plot cumreturns
table_2018 %>%
  ggplot(aes(x=date, y = cr, color = symbol))+
  geom_line(size = 1) +
  facet_wrap(~symbol, ncol = 4, scales = "free_y") +
  theme_tq() +
  geom_hline(yintercept = 1, linetype = "dashed", color ="red", size = 1) +
  labs(title = "Giá trị khoản \u0111ầu t\u01B0 1 \u0111ồng từ 2018 \u0111ến 2020", y = "Lợi nhuận", x = "N\u0103m") + theme(legend.position = "none") + theme
