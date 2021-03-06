# Packages
library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(gridExtra) # Grid 
library(tidyquant) # theme

# Chuan bi theme
theme <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"))

# Function Import
import <- function(url){
  as.xts(read.zoo(url, sep=",", header = TRUE, format = "%Y-%m-%d", drop = FALSE))
}

# Import Quy co phieu
TCEF <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/TCEF_1520.csv")
TCEF <- TCEF[!duplicated(index(TCEF), fromLast = TRUE)]

BVFED <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/BVFED_1620.csv")

VCBFBCF <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/VCBFBCF_1420.csv")
VCBFBCF <- VCBFBCF[!duplicated(index(VCBFBCF), fromLast = TRUE)]

VEOF <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/VEOF_1420.csv")
VEOF <- VEOF[!duplicated(index(VEOF), fromLast = TRUE)]

VFMVF4 <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/VFMVF4_1420.csv")

# Import ETF:
VFMVN30 <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/ETF_E1VFVN30.csv")

SSIVN50 <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/ETF_SSIAMVNX50.csv")

# Import Quy trai phieu
SSIBF <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/SSIBF_1720.csv")

TCBF <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/TCBF_1520.csv")

# Import hybrid
SSISCA <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/SSISCA_1420.csv")
SSISCA <- SSISCA[!duplicated(index(SSISCA), fromLast = TRUE)]

VCBFTBF <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/VCBFTBF_1420.csv")
VCBFTBF <- VCBFTBF[!duplicated(index(VCBFTBF), fromLast = TRUE)]

VFMVF1 <- import("https://raw.githubusercontent.com/vhoanghac/blog/master/data/VFMVF1_1520.csv")

# Merge
data <- merge(TCEF, BVFED, VCBFBCF, VFMVF4, VFMVN30, SSIVN50, SSIBF, TCBF, VEOF, SSISCA, VCBFTBF, VFMVF1)

names(data) <- c("TCEF", "BVFED", "VCBFBCF", "VFMVF4", "ETF_VFMVN30", "ETF_SSIVN50", "SSIBF", "TCBF", "VEOF", "SSISCA", "VCBFTBF", "VFMVF1")

cophieu.cols = c(1,2,3,4)
etf.cols = c(5,6)
traiphieu.cols = c(7,8)
hybrid.cols = c(9,10,11,12)

# Period
data_2016 <- data["2016/2020"]
data_2018 <- data["2018/2020"]


# Convert to monthly
data_2016_m <- apply.monthly(data_2016, mean, na.rm = TRUE)
data_2018_m <- apply.monthly(data_2018, mean, na.rm = TRUE)


# Calculate returns
data_2016_m_simple <- Return.calculate(data_2016_m)
data_2016_m_simple <- na.fill(data_2016_m_simple, fill = 0)

data_2018_m_simple <- Return.calculate(data_2018_m)
data_2018_m_simple <- na.fill(data_2018_m_simple, fill = 0)


######################################

data_2016_replace <- na.locf(data_2016, fromLast = TRUE)
data_2016_replace <- na.locf(data_2016_simple)

data_2016_rep_returns <- Return.calculate(data_2016_replace)

chart.CumReturns(data_2016_rep_returns[,cophieu.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ cổ phiếu từ n\u0103m 2016 \u0111ến 2017", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0") + ylim(0.7, 2.2)



##############################################

# Dau tu 2016 -> 2017 quy co phieu
chart.CumReturns(data_2016_m_simple[,cophieu.cols]["2016/2017"], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ cổ phiếu từ n\u0103m 2016 \u0111ến 2017", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")

table.AnnualizedReturns(data_2016_m_simple[,cophieu.cols]["2016/2017"])


# Dau tu 2018 -> 2020
chart.CumReturns(data_2018_m_simple[,cophieu.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ cổ phiếu từ n\u0103m 2018 \u0111ến 2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0") +
  geom_hline(yintercept = 1, linetype = "dashed", color ="black", size = 0.5)


# Dau tu 2016 -> 2020:
chart.CumReturns(data_2016_m_simple[,cophieu.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ cổ phiếu từ n\u0103m 2016 \u0111ến 2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0") +
  ylim(0.7, 2.2)

# Drawdown

chart.Drawdown(data_2016_m_simple[, cophieu.cols], plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Mức sụt giảm từ \u0111ỉnh của các quỹ từ n\u0103m 2016 \u0111ến 2020", x = "Thời gian", color = "Quỹ \u0111ầu t\u01B0")

VaR(R = data_2016_m_simple, p = 0.95)

##############################################
# Trai phieu

# Dau tu 2016 -> 2017 
chart.CumReturns(data_2016_m_simple[, c(1,2,3,4,8)]["2016/2017"], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2016 \u0111ến 2017", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")

table.AnnualizedReturns(data_2016_m_simple[, c(1,2,3,4,8)]["2016/2017"])

# Dau tu 2018 -> 2020
chart.CumReturns(data_2018_m_simple[,c(cophieu.cols, traiphieu.cols)], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2018 \u0111ến 2020", 
       x = "Thời gian", 
       y = "Lợi nhuận", 
       color = "Quỹ \u0111ầu t\u01B0") +
  geom_hline(yintercept = 1, linetype = "dashed", color ="black", size = 0.5)

traiphieu_2018 <- chart.CumReturns(data_2018_m_simple[, traiphieu.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(x = "Thời gian", 
       y = "Lợi nhuận", 
       color = "Quỹ trái phiếu") +
  geom_hline(yintercept = 1, linetype = "dashed", color ="black", size = 0.5)

table.AnnualizedReturns(data_2018_m_simple[,c(cophieu.cols, traiphieu.cols)])

# Dau tu 2016 -> 2020
chart.CumReturns(data_2016_m_simple[,c(cophieu.cols, traiphieu.cols)], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2016 \u0111ến 2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")

################################################
# Hybrid

# Dau tu 2016 -> 2017 
chart.CumReturns(data_2016_m_simple[, hybrid.cols]["2016/2017"], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2016 \u0111ến 2017", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")

# Dau tu 2018 -> 2020

hybrid_2018 <- chart.CumReturns(data_2018_m_simple[, hybrid.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2018 \u0111ến 2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ lai") +
  geom_hline(yintercept = 1, linetype = "dashed", color ="black", size = 0.5)

cophieu_2018 <- chart.CumReturns(data_2018_m_simple[,cophieu.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(x = "Thời gian", y = "Lợi nhuận", color = "Quỹ cổ phiếu") +
  geom_hline(yintercept = 1, linetype = "dashed", color ="black", size = 0.5)
grid.arrange(hybrid_2018, cophieu_2018, ncol = 2)

table.AnnualizedReturns(data_2016_m_simple[,c(cophieu.cols, hybrid.cols)])

# Dau tu 2016 -> 2020
chart.CumReturns(data_2016_m_simple[, c(cophieu.cols, hybrid.cols)], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2016 \u0111ến 2020", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ \u0111ầu t\u01B0")

##################################################

# ETF

# Dau tu 2016 -> 2017 
chart.CumReturns(data_2016_m_simple[, c(cophieu.cols, etf.cols)]["2016/2017"], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(title = "Hiệu quả \u0111ầu t\u01B0 các quỹ từ n\u0103m 2016 \u0111ến 2017", x = "Thời gian", y = "Lợi nhuận", color = "Quỹ ETF")

# Dau tu 2018 -> 2020
etf_2018 <- chart.CumReturns(data_2018_m_simple[,etf.cols], wealth.index = TRUE, plot.engine = "ggplot2") + theme_classic() + theme +
  labs(x = "Thời gian", y = "Lợi nhuận", color = "Quỹ ETF") +
  geom_hline(yintercept = 1, linetype = "dashed", color ="black", size = 0.5)


grid.arrange(hybrid_2018, etf_2018, cophieu_2018, traiphieu_2018, ncol = 2)

##################################################

# Tong ket

table.AnnualizedReturns(data_2016_m_simple)

t(table.CalendarReturns(data_2016_m_simple))

table.Stats(data_2016_m_simple)

chart.Boxplot(R = data_2016_m_simple, sort.by = "median")

t(maxDrawdown(data_2016_m_simple))

StdDev(data_2016_m_simple)

chart.RiskReturnScatter(data_2016_m_simple["2019"], Rf= 0.05/12, add.sharpe = 1, colorset = c(rep("gray",2), "blue","red","green", rep("gray",2), "purple", rep("gray",3),"orange"), xlim = c(0, 0.22), ylim = c(0, 0.33), main = "2016 -> 2020", xlab = "Rủi ro", ylab = "Lợi nhuận")

# Last x days return
last_x_days <- data_2016_m[c(7,19,31,43,49, 52, 55),]

test <- coredata(last_x_days)

last_3_months <- test[7,] / test[6,] - 1 
last_6_months <- test[7,] / test[5,] - 1
last_12_months <- test[7,] / test[4,] - 1
last_24_months <- test[7,] / test[3,] - 1
last_36_months <- test[7,] / test[2,] - 1

last_x_months <- rbind(last_3_months,last_6_months,last_12_months, last_24_months, last_36_months)

data_last <- t(last_x_months)

################################################

# EXPORT

write.zoo(x = data_2016_m_simple, file ="data_2016.csv", index.name = "Index")

# Cumreturns
cumreturns <- cumprod(1 + data_2016_m_simple)
plot(test)
write.zoo(x = cumreturns, file = "cumreturns.csv", index.name = "Index")

write.zoo(x = data_last, file = "data_last.csv")

write.zoo(x = data, file = "data_quy_dau_tu.csv", index.name = "date")
