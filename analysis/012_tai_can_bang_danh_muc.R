# Packages
library(xts)
library(PerformanceAnalytics)
library(tidyverse)  # theme
library(scales) # percent

# Chuan bi theme
theme <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  legend.position = "none")

theme2 <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5, size = 12),
  plot.caption = element_text(size = 10, color = "black", face = "italic"))

# Function Import
import <- function(url){
  as.xts(read.zoo(url, sep=",", header = TRUE, format = "%Y-%m-%d", drop = FALSE))
}


# Import
VNMAST <- import("H:/OneDrive/Work/R/Data/RebalanceDM/VNMAST.csv")
VNMPNJ <- import("H:/OneDrive/Work/R/Data/RebalanceDM/VNMPNJ.csv")
VNMPNJVCB <- import("H:/OneDrive/Work/R/Data/RebalanceDM/VNMPNJVCB.csv")
VPVH <- import("H:/OneDrive/Work/R/Data/RebalanceDM/VPVH.csv")

TCEFTCBF <- import("H:/OneDrive/Work/R/Data/RebalanceDM/TCEFTCBF.csv")
ETF <- import("H:/OneDrive/Work/R/Data/RebalanceDM/ETF.csv")

# Weight
wp = c(0.5, 0.5)
wp_4060 = c(0.4, 0.6)
wp_6040 = c(0.6, 0.4)

wp_3cp = c(1/3, 1/3, 1/3)
wp_4cp <- c(0.25, 0.25, 0.25, 0.25)


################### Phan tich VNM + AST  ###################
# Returns danh muc
VNMAST_daily_log_returns <- Return.calculate(VNMAST, method = "log")

P_VNMAST <- Return.portfolio(VNMAST_daily_log_returns, weights = wp)

chart.CumReturns(P_VNMAST, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 50% VNM và 50% AST từ n\u0103m 2018 \u0111ến 11/2020", x = "Thời gian", y = "Lợi nhuận") + geom_hline(yintercept = 1, linetype = "dashed", color = "black") 

# Xem weight 
P_VNMAST_verbose <- Return.portfolio(VNMAST_daily_log_returns, weights = wp, verbose = TRUE)
P_VNMAST_weight <- P_VNMAST_verbose$EOP.Weight

autoplot.zoo(P_VNMAST_weight$AST) + theme_classic() + theme + 
  labs(title = "Biến \u0111ộng tỷ trọng của AST qua thời gian", x = "Thời gian", y = "Tỷ trọng") +
  geom_smooth(method = "loess")

# Gia su dau tu ty trong khac
P_VNMAST_4060 <- Return.portfolio(VNMAST_daily_log_returns, weights = wp_4060)
P_VNMAST_6040 <- Return.portfolio(VNMAST_daily_log_returns, weights = wp_6040)

P_VNMAST_Full <- merge(P_VNMAST_4060, P_VNMAST, P_VNMAST_6040)
names(P_VNMAST_Full) <- c("Danh muc 40", "Danh muc 50", "Danh muc 60")

chart.CumReturns(P_VNMAST_Full, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 của các danh mục", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess")

###### Rebalance VNM + AST
P_VNMAST_reb <- Return.portfolio(VNMAST_daily_log_returns, weights = wp, rebalance_on = "quarters")

P_VNMAST_2dm <- merge(P_VNMAST, P_VNMAST_reb)
names(P_VNMAST_2dm) <- c("DM.50", "DM tai can bang")

chart.CumReturns(P_VNMAST_2dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess")


P_VNMAST_reb_verbose <- Return.portfolio(VNMAST_daily_log_returns, weights = wp, rebalance_on = "quarters", verbose = TRUE)

P_VNMAST_reb_weight <- P_VNMAST_reb_verbose$EOP.Weight

autoplot.zoo(P_VNMAST_reb_weight$AST) + theme_classic() + theme + 
  labs(title = "Biến \u0111ộng tỷ trọng của AST qua thời gian", x = "Thời gian", y = "Tỷ trọng") +
  geom_smooth(method = "loess")

################### VNM + PNJ 2010 ###################
# Daily log returns
VNMPNJ_daily_log_returns <- Return.calculate(VNMPNJ, method = "log")

P_VNMPNJ <- Return.portfolio(VNMPNJ_daily_log_returns, weights = wp)

P_VNMPNJ_reb <- Return.portfolio(VNMPNJ_daily_log_returns, weights = wp, rebalance_on = "quarters")

P_VNMPNJ_2dm <- merge(P_VNMPNJ, P_VNMPNJ_reb)
names(P_VNMPNJ_2dm) <- c("DM.50", "DM tai can bang")

# Chart
chart.CumReturns(P_VNMPNJ_2dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 50% VNM và 50% PNJ", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")

chart.RelativePerformance(P_VNMPNJ, P_VNMPNJ_reb, 
                          main = "So sánh hiệu suất \u0111ầu t\u01B0 danh mục 2cp buy&hold so với tái cân bằng",
                          colorset = "#8b0000",
                          elementcolor = "gray")

tail(Return.relative(P_VNMPNJ, P_VNMPNJ_reb))

################### VNM + PNJ + VCB ####################
# Daily log returns

VPV_daily_log_returns <- Return.calculate(VNMPNJVCB, method = "log")

P_VPV <- Return.portfolio(VPV_daily_log_returns, weights = wp_3cp)
P_VPV_reb <- Return.portfolio(VPV_daily_log_returns, weights = wp_3cp, rebalance_on = "quarters")

P_VPV_3dm <- merge(P_VPV, P_VPV_reb)
names(P_VPV_3dm) <- c("DM.buy&hold", "DM tai can bang")

# Chart
chart.CumReturns(P_VPV_3dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 1/3 VNM, PNJ và VCB", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")

chart.RelativePerformance(P_VPV, P_VPV_reb, 
                          main = "So sánh hiệu suất \u0111ầu t\u01B0 danh mục 3cp buy&hold so với tái cân bằng",
                          colorset = "#8b0000",
                          elementcolor = "gray")



################### PNJ + VCB ###########################

PNJ_daily_log_returns <- Return.calculate(VNMPNJ$PNJ, method = "log")
VCB_daily_log_returns <- Return.calculate(VNMPNJVCB$VCB, method = "log")

PNJVCB <- merge(PNJ_daily_log_returns, VCB_daily_log_returns)

P_PNJVCB <- Return.portfolio(PNJVCB, weights = wp)
P_PNJVCB_reb <- Return.portfolio(PNJVCB, weights = wp, rebalance_on = "quarters")

P_PNJVCB_2dm <- merge(P_PNJVCB, P_PNJVCB_reb)
names(P_PNJVCB_2dm) <- c("DM.50", "DM tai can bang")

chart.CumReturns(P_PNJVCB_2dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 50% PNJ và 50% VCB", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")

chart.RelativePerformance(P_PNJVCB, P_PNJVCB_reb, 
                          main = "So sánh hiệu suất \u0111ầu t\u01B0 danh mục 2cp buy&hold so với tái cân bằng",
                          colorset = "#8b0000",
                          elementcolor = "gray")

#################### PNJ + VCB + HPG #####################


HPG_daily_log_returns <- Return.calculate(VPVH$HPG, method = "log")

PVH <- merge(PNJ_daily_log_returns, VCB_daily_log_returns, HPG_daily_log_returns)

P_PVH <- Return.portfolio(PVH, weights = wp_3cp)
P_PVH_reb <- Return.portfolio(PVH, weights = wp_3cp, rebalance_on = "quarters")

P_PVH_3dm <- merge(P_PVH, P_PVH_reb)
names(P_PVH_3dm) <- c("DM.buy&hold", "DM tai can bang")

chart.CumReturns(P_PVH_3dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 1/3 PNJ, VCB và HPG", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")


chart.RelativePerformance(P_PVH, P_PVH_reb, 
                          main = "So sánh hiệu suất \u0111ầu t\u01B0 danh mục 3cp buy&hold so với tái cân bằng",
                          colorset = "#8b0000",
                          elementcolor = "gray")

################### 4 cp###########################
VPVH_daily_log_returns <- Return.calculate(VPVH, method = "log")

P_VPVH <- Return.portfolio(VPVH_daily_log_returns, weights = wp_4cp)
P_VPVH_reb <- Return.portfolio(VPVH_daily_log_returns, weights = wp_4cp, rebalance_on = "quarters")

P_VPVH_4dm <- merge(P_VPVH, P_VPVH_reb)
names(P_VPVH_4dm) <- c("DM.buy.hold", "DM tai can bang")
  
chart.CumReturns(P_VPVH_4dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 25% VNM, PNJ, VCB và HPG", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")

chart.RelativePerformance(P_VPVH, P_VPVH_reb, 
                          main = "So sánh hiệu suất \u0111ầu t\u01B0 danh mục 4cp buy&hold so với tái cân bằng",
                          colorset = "#8b0000",
                          elementcolor = "gray")

tail(Return.relative(P_VPV, P_VPV_reb))


############################### ETF ############################

TCEFTCBF <- TCEFTCBF[.indexwday(TCEFTCBF) %in% 1:5] # Loai bo ngay cuoi tuan
TCEFTCBF_2016 <- TCEFTCBF["2016/2020"]

write.zoo(TCEFTCBF, "TCEFTCBF.csv", sep = ",") # Export ra file csv


ETF <- ETF[.indexwday(ETF) %in% 1:5]
ETF_2016 <- ETF["2016/2020"]
names(ETF_2016) <- "ETF"

write.zoo(ETF, "ETF.csv", sep = ",") # Export

QuyDauTu <- merge(ETF_2016, TCEFTCBF_2016, join = "left")

#### Tinh toan:

QuyDauTu_daiy_log_returns <- Return.calculate(QuyDauTu, method = "log")

P_TCB <- Return.portfolio(QuyDauTu_daiy_log_returns[,2:3], weights = wp_4060)
P_TCB_reb <- Return.portfolio(QuyDauTu_daiy_log_returns[, 2:3], weights = wp_4060, rebalance_on = "years")

P_TCB_2dm <- merge(P_TCB, P_TCB_reb)
names(P_TCB_2dm) <- c("DM.buy.hold", "DM tai can bang")

chart.CumReturns(P_TCB_2dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 40% TCEF và 60% TCBF", subtitle ="So sánh danh mục bình th\u01B0ờng và danh mục tái cân bằng", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")



## So sanh 4 danh muc :
P_TCB_2dm_sosanh <- merge(P_TCB_2dm, QuyDauTu_daiy_log_returns$TCEF, QuyDauTu_daiy_log_returns$TCBF, all = TRUE)
P_TCB_2dm_sosanh <- na.fill(P_TCB_2dm_sosanh, fill = 0)


chart.CumReturns(P_TCB_2dm_sosanh, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 40/60", subtitle ="So sánh với các danh mục Buy&hold, 100% TCEF và 100% TCBF", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")



## So sanh 3 danh muc :

P_TCB_3dm_sosanh <- merge(P_TCB_2dm, QuyDauTu_daiy_log_returns$TCBF, all = TRUE)
P_TCB_3dm_sosanh <- na.fill(P_TCB_3dm_sosanh, fill = 0)


chart.CumReturns(P_TCB_3dm_sosanh, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 40/60", subtitle ="So sánh với các danh mục Buy&hold, 100% TCEF và 100% TCBF", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")




#### Danh muc 60 40

P_TCB_60 <- Return.portfolio(QuyDauTu_daiy_log_returns[,2:3], weights = wp_6040)
P_TCB_60_reb <- Return.portfolio(QuyDauTu_daiy_log_returns[,2:3], weights = wp_6040, rebalance_on = "years")

P_TCB_60_2dm <- merge(P_TCB_60, P_TCB_60_reb)
names(P_TCB_60_2dm) <- c("DM.buy.hold", "DM tai can bang")

chart.CumReturns(P_TCB_60_2dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 40/60", subtitle ="So sánh với các danh mục Buy&hold, 100% TCEF và 100% TCBF", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")


P_TCB_60_2dm_sosanh <- merge(P_TCB_60_2dm, QuyDauTu_daiy_log_returns$TCEF, QuyDauTu_daiy_log_returns$TCBF, all = TRUE)
P_TCB_60_2dm_sosanh <- na.fill(P_TCB_60_2dm_sosanh, fill = 0)

chart.CumReturns(P_TCB_60_2dm_sosanh, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 60/40", subtitle ="So sánh với các danh mục Buy&hold, 100% TCEF và 100% TCBF", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")


#### ETF + TCBF

P_ETF_40 <- Return.portfolio(QuyDauTu_daiy_log_returns[, c(1, 3)], weights = wp_4060)
P_ETF_40_reb <- Return.portfolio(QuyDauTu_daiy_log_returns[, c(1,3)], weights = wp_4060, rebalance_on = "quarters")

P_ETF_40_2dm <- merge(P_ETF_40, P_ETF_40_reb)
names(P_ETF_40_2dm) <- c("DM.buy.hold", "DM tai can bang")

chart.CumReturns(P_ETF_40_2dm, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng mỗi quý 40/60", 
       subtitle ="Tỷ trọng 40% ETF và 60% TCBF",
       x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")


chart.RelativePerformance(P_ETF_40, P_ETF_40_reb, 
                          main = "So sánh hiệu suất \u0111ầu t\u01B0 danh mục 4cp buy&hold so với tái cân bằng",
                          colorset = "#8b0000",
                          elementcolor = "gray")



# So sanh 4 danh muc

P_ETF_4dm_sosanh <- merge(P_ETF_40_2dm, QuyDauTu_daiy_log_returns$ETF, QuyDauTu_daiy_log_returns$TCBF, all = TRUE)
P_ETF_4dm_sosanh <- na.fill(P_ETF_4dm_sosanh, fill = 0)


chart.CumReturns(P_ETF_4dm_sosanh, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng mỗi quý 40% ETF và 60% TCBF", 
       subtitle ="So sánh với các danh mục Buy&hold, 100% ETF và 100% TCBF", 
       caption = "Chú thích: Tái cân bằng mỗi quý",
       x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")




#### ETF + TCBF 60/40

P_ETF_60 <- Return.portfolio(QuyDauTu_daiy_log_returns[, c(1, 3)], weights = wp_6040)
P_ETF_60_reb <- Return.portfolio(QuyDauTu_daiy_log_returns[, c(1,3)], weights = wp_6040, rebalance_on = "years")

P_ETF_60_4dm_sosanh <- merge(P_ETF_60, P_ETF_60_reb, QuyDauTu_daiy_log_returns$ETF, QuyDauTu_daiy_log_returns$TCBF, all = TRUE)
P_ETF_60_4dm_sosanh <- na.fill(P_ETF_60_4dm_sosanh, fill = 0)
names(P_ETF_60_4dm_sosanh) <- c("DM.buy.hold", "DM tai can bang", "ETF", "TCBF")


chart.CumReturns(P_ETF_60_4dm_sosanh[,c(1:2)], wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 60% ETF và 40% TCBF", 
       subtitle ="So sánh với các danh mục Buy&hold và tái cân bằng",
       caption = "Chú thích: Tái cân bằng mỗi n\u0103m",
       x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")



chart.CumReturns(P_ETF_60_4dm_sosanh, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 60% ETF và 40% TCBF", 
       subtitle ="So sánh với các danh mục Buy&hold, 100% ETF và 100% TCBF",
       caption = "Chú thích: Tái cân bằng mỗi n\u0103m",
       x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")


########## Quang thoi gian 2018

QuyDauTu_daiy_log_returns_2018 <- Return.calculate(QuyDauTu["2018/2020"], method = "log")

P_ETF_40_2018 <- Return.portfolio(QuyDauTu_daiy_log_returns_2018[,c(1,3)], weights = wp_4060)
P_ETF_40_2018_reb <- Return.portfolio(QuyDauTu_daiy_log_returns_2018[,c(1,3)], weights = wp_4060, rebalance_on = "quarters")

P_ETF_40_2dm_2018 <- merge(P_ETF_40_2018, P_ETF_40_2018_reb)
names(P_ETF_40_2dm_2018) <- c("DM.buy.hold", "DM tai can bang")

chart.CumReturns(P_ETF_40_2dm_2018, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 40/60", subtitle ="So sánh với các danh mục Buy&hold, 100% TCEF và 100% TCBF", x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")

P_ETF_4dm_sosanh_2018 <- merge(P_ETF_40_2dm_2018, QuyDauTu_daiy_log_returns_2018$ETF, QuyDauTu_daiy_log_returns_2018$TCBF, all = TRUE)
P_ETF_4dm_sosanh_2018 <- na.fill(P_ETF_4dm_sosanh_2018, fill = 0)

chart.CumReturns(P_ETF_4dm_sosanh_2018, wealth.index = TRUE, geometric = TRUE, plot.engine = "ggplot2") +
  theme_classic() + theme2 + 
  labs(title = "Hiệu quả \u0111ầu t\u01B0 danh mục tái cân bằng 40% ETF và 60% TCBF", 
       subtitle ="So sánh với các danh mục Buy&hold, 100% ETF và 100% TCBF",
       caption = "Chú thích: Tái cân bằng mỗi quý",
       x = "Thời gian", y = "Lợi nhuận", color = "Danh mục") + geom_hline(yintercept = 1, linetype = "dashed", color = "black")
