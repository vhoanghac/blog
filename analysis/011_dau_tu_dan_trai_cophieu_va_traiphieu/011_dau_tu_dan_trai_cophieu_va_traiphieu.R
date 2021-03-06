library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(gridExtra)
library(scales)

# Chuan bi theme
theme <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"),
  plot.title = element_text(hjust = 0.5),
  legend.position = "none")

theme2 <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"),
  plot.title = element_text(hjust = 0.5))


# Import data
TCEF <- read_csv("H:/OneDrive/Work/R/Data/Danh muc cac tai san/data/TCEF.csv")

TCBF <- read_csv("H:/OneDrive/Work/R/Data/Danh muc cac tai san/data/TCBF.csv")

# Daily Period
TCEF_2016 <- TCEF %>%
  filter(date >= "2016-01-01")

TCBF_2016 <- TCBF %>%
  filter(date >= "2016-01-01")

# Daily Returns

TCEF_daily_returns <- TCEF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "arithmetic",
               col_rename = "daily.returns")

TCBF_daily_returns <- TCBF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "arithmetic",
               col_rename = "daily.returns")

# Daily XTS Returns (Converted)
TCEF_returns_xts <- xts(TCEF_daily_returns$daily.returns, order.by = TCEF_daily_returns$date)
colnames(TCEF_returns_xts) <- "TCEF_returns"

TCBF_returns_xts <- xts(TCBF_daily_returns$daily.returns, order.by = TCBF_daily_returns$date)
colnames(TCBF_returns_xts) <- "TCBF_returns"


# Daily Log Returns
TCEF_daily_log_returns <- TCEF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "daily.log.returns")

TCBF_daily_log_returns <- TCBF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "daily.log.returns")

# Yearly Log Returns

TCBF_yearly_log_returns <- TCBF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "log",
               col_rename = "yearly.log.returns")

TCBF_yearly_log_returns



TCEF_yearly_log_returns <- TCEF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "log",
               col_rename = "yearly.log.returns")


# Yearly returns
TCBF_yearly_returns <- TCBF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "arithmetic",
               col_rename = "yearly.returns")

TCBF_yearly_returns


# XTS LOG Returns 
TCEF_log_returns_xts <- xts(TCEF_daily_log_returns$daily.log.returns, order.by = TCEF_daily_log_returns$date)
colnames(TCEF_log_returns_xts) <- "TCEF_returns"

TCBF_log_returns_xts <- xts(TCBF_daily_log_returns$daily.log.returns, order.by = TCBF_daily_log_returns$date)
colnames(TCBF_log_returns_xts) <- "TCBF_returns"


## Merge XTS
TCEF_TCBF <- merge(TCEF_returns_xts, TCBF_returns_xts, fill = 0) #xts
names(TCEF_TCBF) <- c("TCEF", "TCBF")

TCEF_TCBF_log <- merge(TCEF_log_returns_xts, TCBF_log_returns_xts)
names(TCEF_TCBF_log) <- c("TCEF","TCBF")


############# Bai viet #############

# TCEF price chart
ggplot(TCEF_2016, aes(x = date, y = price))+
  geom_line(color = "firebrick4") + theme_classic() + theme2 +
  labs(title = "Giá NAV/chứng chỉ quỹ TCEF từ n\u0103m 2016", x = "Thời gian", y = "Giá (VND)") +
  scale_y_continuous(labels=function(x) format(x, big.mark = "."))

## Chart Cumreturns TCEF
chart.CumReturns(TCEF_returns_xts, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào quỹ cổ phiếu TCEF từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận")

## TCEF annual returns
TCEF_annual_returns <- TCEF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "arithmetic")

TCEF_annual_returns %>%
  ggplot(aes(x = date, y = yearly.returns, fill = "")) +
  geom_col() + theme_classic() + theme +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.2,0.4)) +
  labs(title = "Tỷ suất sinh lợi từng năm khi đầu tư vào TCEF", y = "Tỷ suất sinh lợi", x = "Thời gian")

## TCBF + TCEF

chart.CumReturns(TCEF_TCBF, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào hai quỹ từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")

## Portfolio

tpcp_w <- c(0.4, 0.6)

p4060 <- Return.portfolio(R = TCEF_TCBF_log, weights = tpcp_w)
names(p4060) <- c("DM 40cp")

returns_3p <- merge(TCEF_TCBF_log, p4060)

chart.CumReturns(returns_3p, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào danh mục 40/60 từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục")

chart.Drawdown(returns_3p, plot.engine = "ggplot2") +
  theme_classic() + theme2 +
  labs(title = "Tỷ lệ sụt giảm của các danh mục", color = "Danh mục") +
  scale_y_continuous(label = percent) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x = "Giai \u0111oạn 2016 - 2020", y = "Tỷ lệ sụt giảm") 


###########################################



################ ETF

# Import ETF: 
ETF <- read_csv("H:/OneDrive/Work/R/Data/Danh muc cac tai san/data/E1VFVN30.csv")

ETF_2016 <- ETF %>%
  filter(date >= "2016-01-01")

ETF_2016 <- TCBF_2016 %>%
  left_join(ETF_2016, by = "date") %>%
  na.locf() %>%
  select(date, price.y)
colnames(ETF_2016) <- c("date", "ETF")

# Daily returns
ETF_daily_returns <- ETF_2016 %>%
  tq_transmute(select = ETF,
               mutate_fun = periodReturn,
               period = "daily",
               type = "arithmetic",
               col_rename = "daily.returns")

# Convert sang xts
ETF_returns_xts <- xts(ETF_daily_returns$daily.returns, order.by = TCEF_daily_returns$date)
colnames(ETF_returns_xts) <- "ETF_returns"

# Merge xts 3 quy
tcef_tcbf_etf <- merge(TCEF_returns_xts, TCBF_returns_xts, ETF_returns_xts, fill = 0)
colnames(tcef_tcbf_etf) <- c("TCEF", "TCBF", "ETF")

chart.CumReturns(tcef_tcbf_etf, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào ba quỹ từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")


# Daily log returns
ETF_daily_log_returns <- ETF_2016 %>%
  tq_transmute(select = ETF,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "daily.log.returns")

# Convert sang xts
ETF_log_returns_xts <- xts(ETF_daily_log_returns$daily.log.returns, order.by = ETF_daily_log_returns$date)
colnames(ETF_log_returns_xts) <- "ETF_returns"

# Merge
ETF_TCBF_log <- merge(ETF_log_returns_xts, TCBF_log_returns_xts)
names(ETF_TCBF_log) <- c("ETF","TCBF")

# ETF + TCBF

p_etf_4060 <- Return.portfolio(R = ETF_TCBF_log, weights = tpcp_w)
names(p_etf_4060) <- "DM 40etf"

returns_3pp <- merge(p4060, p_etf_4060)

chart.CumReturns(returns_3pp, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào danh mục 40/60 từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục")

returns_4pp <- merge(ETF_returns_xts, returns_3p, p_etf_4060 )
names(returns_4pp) <- c("ETF", "TCEF", "TCBF", "DM.40cp", "DM.40etf")

chart.Drawdown(returns_4pp, plot.engine = "ggplot2") +
  theme_classic() + theme2 +
  labs(title = "Tỷ lệ sụt giảm của các danh mục", color = "Danh mục") +
  scale_y_continuous(label = percent) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x = "Giai \u0111oạn 2016 - 2020", y = "Tỷ lệ sụt giảm") 






##########################################################
##################### TEST RETURN
##########################################################


#Cach 1:
TCEF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "log",
               col_rename = "yearly.log.returns")
# Cach 2:
TCEF_2016 %>%
  tq_transmute(select = price,
                 mutate_fun = yearlyReturn,
               type = "log", 
                 col_rename = "yearly.Return")
# Cach 3:

TCEF_daily_log_returns %>%
  tq_transmute(select = daily.log.returns,
               mutate_fun = apply.yearly,
               FUN = sum,
               col_rename = "yearly.Return")

# Cach 4:
TCEF_2016 %>%
  tq_transmute(select = price,
               mutate_fun = annualReturn,
               type = "log",
               col_rename = "yearly.Return")


#####################  TEST  Standard deviation 
t(TCEF_daily_log_returns %>%
  filter(format(TCEF_daily_log_returns$date, "%Y") == "2016") %>%  #Format 2016  
  tq_performance(Ra = daily.log.returns,
                 Rb = NULL,
                 performance_fun = table.Variability))


#################### TEST SharpeRatio
t(TCEF_daily_log_returns %>%
    filter(format(TCEF_daily_log_returns$date, "%Y") == "2016") %>%  #Format 2016  
    tq_performance(Ra = daily.log.returns,
                   Rb = NULL,
                   performance_fun = SharpeRatio))

# Cach 2: 
portfolio_excess_returns <- TCEF_daily_log_returns %>%
  filter(format(TCEF_daily_log_returns$date, "%Y") == "2016") %>%  #Format 2016  
  tq_transmute(select = daily.log.returns,
               mutate_fun = Return.excess,
               Rf = 0)

portfolio_excess_returns

sharpe_ratio_manual <- round(mean(portfolio_excess_returns$`daily.log.returns > Rf`)/StdDev(portfolio_excess_returns$`daily.log.returns > Rf`), 4)

sharpe_ratio_manual


##########################################################
##################### END TEST RETURN
##########################################################


## Tinh bang Excel:

# Annual Returns DM40 cp.
DM40cp_yearly <- apply.yearly(returns_3p$DM.40cp, sum)
DM40cp_yearly

# Standard deviation
t(table.Variability(returns_3pp$DM.40cp["2016"]))  #DM 40% cp 60% bond
t(table.Variability(returns_3pp$DM.40etf["2020"])) #DM 40% etf 60% bond

# Sharpe
SharpeRatio(returns_3pp$DM.40cp["2020"], FUN = "StdDev", annualize = TRUE)

SharpeRatio(returns_3pp$DM.40etf["2020"], FUN = "StdDev", annualize = TRUE)

# Sortino
SortinoRatio(returns_3pp$DM.40cp["2020"], MAR = 0)
SortinoRatio(returns_3pp$DM.40etf["2020"], MAR = 0)


# Maximum Drawdown
maxDrawdown(returns_3pp$DM.40cp["2020"])
maxDrawdown(returns_3pp$DM.40etf["2020"])

maxDrawdown(returns_3pp$DM.40etf)

# Trung nhau.

tq_performance_fun_options()


##########################################################

###########################################################
################### VANG 
###########################################################

# Import XAU
XAU <- read_csv("H:/OneDrive/Work/R/Data/Danh muc cac tai san/data/giavang.csv")

XAU_2016 <- XAU %>%
  filter(date >= "2016-01-01")
names(XAU_2016) <- c("date", "price")

# XAU Daily returns (normal)
XAU_daily_returns <- XAU_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "arithmetic",
               col_rename = "daily.returns")

# XAU Daily log returns 
XAU_daily_log_returns <- XAU_2016 %>%
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "daily.log.returns")


# Convert daily returns to xts
XAU_returns_xts <- xts(XAU_daily_returns$daily.returns, order.by = XAU_daily_returns$date)
colnames(XAU_returns_xts) <- "XAU_returns"

XAU_log_returns_xts <- xts(XAU_daily_log_returns$daily.log.returns, order.by = XAU_daily_log_returns$date)
colnames(XAU_log_returns_xts) <- "XAU_returns"

# Merge de tinh cum returns. Arithmetic
cum_4p <- merge(TCEF_returns_xts, TCBF_returns_xts, ETF_returns_xts, XAU_returns_xts, fill = 0)
names(cum_4p) <- c("TCEF", "TCBF", "ETF", "XAU")

# Chart CumReturns

chart.CumReturns(cum_4p, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục")

### Phan tich hai danh muc


#tpcp_w <- c(0.4, 0.6)

#p4060 <- Return.portfolio(R = TCEF_TCBF_log, weights = tpcp_w)
#names(p4060) <- c("DM 40cp")

## TCEF + Vang:

TCEF_XAU_log <- merge(TCEF_log_returns_xts, XAU_log_returns_xts, fill = 0) #xts
names(TCEF_XAU_log) <- c("TCEF", "XAU")

p_40cp_60vang <- Return.portfolio(R = TCEF_XAU_log, weights = tpcp_w)
names(p_40cp_60vang) <- "DM.40cp.60vang"


## ETF  Vang:
ETF_XAU_log <- merge(ETF_log_returns_xts, XAU_log_returns_xts, fill = 0)
names(ETF_XAU_log) <- C("ETF", "XAU")

p_40etf_60vang <- Return.portfolio(R = ETF_XAU_log, weights = tpcp_w)
names(p_40etf_60vang) <- "DM.40etf.60vang"

# Merge 2 danh muc

dm_60vang <- merge(p_40cp_60vang, p_40etf_60vang, fill = 0)

# Chart Cum Returns 2 danh muc 60% vang

chart.CumReturns(dm_60vang, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào danh mục 40/60 từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục")

# So danh 4 danh muc
so_sanh_4dm <- merge(p4060, p_etf_4060, p_40cp_60vang, p_40etf_60vang, fill = 0 )

chart.CumReturns(so_sanh_4dm, wealth.index = TRUE, plot.engine = "ggplot2")+
  theme_classic() + theme2 +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 vào danh mục 40/60 từ n\u0103m 2016 \u0111ến 08/2020", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục")


chart.Drawdown(so_sanh_4dm, plot.engine = "ggplot2") +
  theme_classic() + theme2 +
  labs(title = "Tỷ lệ sụt giảm của các danh mục", color = "Danh mục") +
  scale_y_continuous(label = percent) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(x = "Thời gian", y = "Tỷ lệ sụt giảm") 


# Return yearly các danh mục

table.AnnualizedReturns(R = so_sanh_4dm, Rf = 0)

dm.40cp.yearly <- apply.yearly(so_sanh_4dm$DM.40cp, sum)
dm.40etf.yearly <- apply.yearly(so_sanh_4dm$DM.40etf, sum)
dm.40cp.60vang.yearly <- apply.yearly(so_sanh_4dm$DM.40cp.60vang, sum)
dm.40etf.60vang.yearly <- apply.yearly(so_sanh_4dm$DM.40etf.60vang, sum)

annual_returns_4dm <- merge(dm.40cp.yearly, dm.40etf.yearly, dm.40cp.60vang.yearly, dm.40etf.60vang.yearly)

t(annual_returns_4dm)

### Standard + sharpe + Sortino

# Standard deviation
t(table.Variability(so_sanh_4dm["2020"]))

# Sharpe Ratio:
t(SharpeRatio(so_sanh_4dm["2020"], FUN = "StdDev", annualize = TRUE))

# Max Drawdown
maxDrawdown(so_sanh_4dm["2020"])

# Sortino
SortinoRatio(so_sanh_4dm["2020"], MAR = 0)



###########################################################
################### CO PHIEU NUOC NGOAI
###########################################################

AAPL_raw <- read_csv("H:/OneDrive/Work/R/Data/Danh muc cac tai san/data/AAPL.csv",
                     col_types = cols(date = col_datetime("%b %d, %Y")))
AAPL <- AAPL_raw %>%
  arrange(date) %>%
  select(date, price) %>%
  filter(date >= "2016-01-05")

AAPL$date <- as.Date(AAPL$date)

