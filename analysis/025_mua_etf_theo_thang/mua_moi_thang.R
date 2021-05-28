# 1. Packages ####
library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggrepel)
library(patchwork)
library(cowplot)


source("header.R") 

# 2. Import data ####

etf <- get_data_co_phieu(ticker = "E1VFVN30", all = FALSE) %>% 
  spread(symbol, price) %>% 
  mutate(E1VFVN30 = E1VFVN30 * 1000)


tcbf <- read_csv("025_dca_lumpsum/data/TCBF.csv") %>% 
  filter(date >= "2015-09-30")


# Normal Growth:
etf_returns <- etf %>% 
  filter(date >= "2014-10-31") %>% 
  tq_transmute(select = E1VFVN30,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "returns") %>% 
  
  mutate(index = cumprod(1 + returns))


etf_matrix <- as.matrix(etf_returns[, "index"])


# 3. ETF E1VFVN30 ####

# 3.1 Function 

test <-  function(n_month, plot = FALSE){
  
  final_results <- data.frame(date = etf_returns[1:(nrow(etf_returns) - n_month), "date"])
  
  
  for(i in 1:(length(etf_matrix[, 1]) - n_month)) {
    
    last_row <- i + n_month
    
    beg_index <- etf_matrix[i, "index"]
    
    end_index <- etf_matrix[last_row]
    
    end_ls <- (end_index / beg_index * 1)

    
    # DCA:
  
    etf_growth <- end_index / etf_matrix[i:(last_row - 1)]
    
    dca_etf <- sum(etf_growth * (1 / n_month) * 1)
    
    
    ls  <- paste0("ls_", n_month, "m")
    dca <- paste0("dca_", n_month, "m")
    out <- paste0("dca_outperformance")
    
    
    final_results[i, ls] <- end_ls - 1  
    final_results[i, dca] <- dca_etf - 1
    final_results[i, out] <- dca_etf / end_ls - 1
  
  }
  

  # PLOT:
  if (plot == TRUE){
    
    
    labels <- data.frame()
    labels[1, "perf_col"] <- 0.45
    labels[1, "label"] <- "Mua từng tháng có hiệu quả"
    labels[1, "date"] <- as.Date("2015-01-01")
    labels[2, "perf_col"] <- -0.45
    labels[2, "label"] <- "Mua từng tháng không hiệu quả"
    labels[2, "date"] <- as.Date("2015-01-01")

    
    mean_perf <- mean(final_results$dca_outperformance)
    
    dca_underperf <- (final_results %>% 
      filter(dca_outperformance < 0) %>% 
      nrow()) / nrow(final_results)
    
    if (mean_perf > 0){
      perf_result <- "hiệu quả h\u01a1n"
    } else {
      perf_result <- "kém hiệu quả h\u01a1n"
      
      mean_perf <- abs(mean_perf)
    }
    
    caption <- str_glue("Trung bình danh mục mua từng tháng {perf_result} danh mục mua một lần {round(mean_perf * 100, 2)}%.
                        Việc mua từng tháng {perf_result} mua một lần chiếm {round(dca_underperf *100,2)}% tổng số các quan sát.")
    
  p1 <- final_results %>% 
    ggplot(aes(x = date,
               y = dca_outperformance)) +
    geom_line(color = "#DC0000B2", size = 1.2) + theme_tq() + theme +
    
    geom_hline(yintercept = 0, linetype = "dashed") +
    
    geom_text_repel(data = labels, aes(x=date, y=perf_col),
                    color = "black",
                    label = labels$label,
                    family = "serif", size = 6,
                    max.iter = 1) +
  
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                       breaks = seq(-0.5, 0.5, 0.1)) +
    
    scale_x_date(breaks = scales::pretty_breaks(n = 6)) + 
    
    labs(x = "",
         y = "") 
    
  
  
  p2 <- final_results %>% 
    ggplot(aes(x = dca_outperformance)) +
    geom_density(fill = "#DC0000B2") + theme_tq() + theme + theme(axis.text.y = element_blank()) + 
    
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
    
    scale_x_continuous(limits = c(-0.5, 0.5),
                       breaks = scales::pretty_breaks(n = 8),
                       labels = scales::percent_format(accuracy = 1L)) +
    
    labs(x = "Mua từng tháng hiệu quả (%)",
         y = "Tần suất")
  
  plot <- (p1 + p2) + plot_annotation(subtitle = str_glue("Quãng thời gian so sánh: {n_month} tháng"),
                              theme = theme,
                              tag_levels = "I",
                              caption = caption)

  return(plot)

  
  }
  
  return(final_results)
}

# 3.2 Plot

a1 <- test(n_month = 3, plot = TRUE)
a2 <- test(n_month = 6, plot = TRUE)
a3 <- test(n_month = 12, plot = TRUE)
a4 <- test(n_month = 18, plot = TRUE)
a5 <- test(n_month = 24, plot = TRUE)
a6 <- test(n_month = 36, plot = TRUE)


a3 + plot_annotation(title = "Hiệu quả của danh mục mua E1VFVN30 từng tháng so với mua một lần")


plot_grid(a1, a2, ncol = 1)




# 4. Danh muc 60/40 #### 

# Portfolio Growth:

raw_port <- tcbf %>% 
  left_join(etf, by = "date") %>% 
  fill(E1VFVN30) %>% 
  
  gather(symbol, price, -date) %>% 

  group_by(symbol) %>% 
  
  tq_transmute(select = price,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "returns") %>% 
  
  mutate(growth = cumprod(1 + returns)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = date,
              names_from = symbol,
              values_from = c("returns", "growth"))

# 4.1 Function:

port_matrix <- as.matrix(raw_port[, 4:5])

etf_col_matrix <- grepl("E1VFVN30", colnames(port_matrix))
tcbf_col_matrix <- grepl("TCBF", colnames(port_matrix))


test2 <- function(n_month, w_etf, plot = FALSE){
  
  w_tcbf <- 1 - w_etf
  
  # Matrix de dien du lieu vao
  results <- data.frame(date = raw_port[1:(nrow(raw_port) - n_month), "date"])
  

  for(i in 1:(length(port_matrix[,2]) - n_month)){
    
    last_row <- i + n_month
    
    # Dau tu mot lan:
    
    beg_etf <- port_matrix[i, etf_col_matrix]
    beg_tcbf <- port_matrix[i, tcbf_col_matrix]
    
    end_etf <- port_matrix[last_row, etf_col_matrix]
    end_tcbf <- port_matrix[last_row, tcbf_col_matrix]
    
    end_ls <- (end_etf / beg_etf * w_etf) + (end_tcbf / beg_tcbf * w_tcbf)
    
    
    
    # DCA:
    
    etf_growth <- end_etf / port_matrix[i:(last_row - 1), etf_col_matrix]
    tcbf_growh <- end_tcbf / port_matrix[i:(last_row - 1), tcbf_col_matrix]
    
    dca_etf <- sum(etf_growth * (1 / n_month) * w_etf)
    dca_tcbf <- sum(tcbf_growh * (1 / n_month) * w_tcbf)
    
    end_dca <- dca_etf + dca_tcbf
    
    # Data frame:
    
    ls  <- paste0("ls_", n_month, "m")
    dca <- paste0("dca_", n_month, "m")
    out <- paste0("dca_outperformance")
    
    
    results[i, ls] <- end_ls - 1  
    results[i, dca] <- end_dca - 1
    results[i, out] <- end_dca / end_ls - 1
    
    
  }
  
  if(plot == TRUE){
    
    labels <- data.frame()
    labels[1, "perf_col"] <- 0.45
    labels[1, "label"] <- "Mua từng tháng có hiệu quả"
    labels[1, "date"] <- as.Date("2016-01-01")
    labels[2, "perf_col"] <- -0.45
    labels[2, "label"] <- "Mua từng tháng không hiệu quả"
    labels[2, "date"] <- as.Date("2016-01-01")
    
    
    mean_perf <- mean(results$dca_outperformance)
    
    dca_underperf <- (results %>% 
                        filter(dca_outperformance < 0) %>% 
                        nrow()) / nrow(results)
    
    if (mean_perf > 0){
      perf_result <- "hiệu quả h\u01a1n"
    } else {
      perf_result <- "kém hiệu quả h\u01a1n"
      
      mean_perf <- abs(mean_perf)
    }
    
    caption <- str_glue("Trung bình danh mục mua từng tháng {perf_result} danh mục mua một lần {round(mean_perf * 100, 2)}%.
                        Việc mua từng tháng {perf_result} mua một lần chiếm {round(dca_underperf *100,2)}% tổng số các quan sát.")
    
    p1 <- results %>% 
      ggplot(aes(x = date,
                 y = dca_outperformance)) +
      
      geom_line(color = "#DC0000B2", size = 1.2) + theme_tq() + theme +
      
      geom_hline(yintercept = 0, linetype = "dashed") +
      
      geom_text_repel(data = labels, aes(x=date, y=perf_col),
                      color = "black",
                      label = labels$label,
                      family = "serif", size = 6,
                      max.iter = 1) +
      
      scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                         breaks = seq(-0.5, 0.5, 0.1)) +
      
      scale_x_date(breaks = scales::pretty_breaks(n = 6)) + 
      
      labs(x = "",
           y = "") 
    
    
    p2 <- results %>% 
      ggplot(aes(x = dca_outperformance)) +
      geom_density(fill = "#DC0000B2") + theme_tq() + theme + theme(axis.text.y = element_blank()) + 
      
      geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
      
      scale_x_continuous(limits = c(-0.5, 0.5),
                         breaks = scales::pretty_breaks(n = 8),
                         labels = scales::percent_format(accuracy = 1L)) +
      
      labs(x = "Mua từng tháng hiệu quả (%)",
           y = "Tần suất")
    
    
    plot <- (p1 + p2) + plot_annotation(subtitle = str_glue("Quãng thời gian so sánh: {n_month} tháng"),
                                        theme = theme,
                                        tag_levels = "I",
                                        caption = caption)
    
    return(plot)
    
    
  }
  
  return(results)
  
  
}

# 4.2 Plot:


b1 <- test2(n_month = 3, w_etf = 0.4, plot = TRUE)
b2 <- test2(n_month = 6, w_etf = 0.4, plot = TRUE)
b3 <- test2(n_month = 12, w_etf = 0.4, plot = TRUE)
b4 <- test2(n_month = 18, w_etf = 0.4, plot = TRUE)
b5 <- test2(n_month = 24, w_etf = 0.4, plot = TRUE)
b6 <- test2(n_month = 36, w_etf = 0.4, plot = TRUE)


b3 + plot_annotation(title = "Hiệu quả của danh mục 60/40 mua từng tháng so với mua một lần")

# 4.3 Something cool

tt <- test2(n_month = 12, w_etf = 0.4, plot = FALSE)

tt %>% 
  select(date, dca_12m, ls_12m) %>% 
  gather(symbol, growth, -date) %>% 
  
  ggplot(aes(x = date, y = growth, col = symbol)) +
  geom_line(size = 1.2) + theme_tq() + theme2 + theme(legend.text = element_text(size = 14)) +
  
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) + 
  
  scale_color_manual(values = c("#3C5488B2", "#DC0000B2"),
                     labels = c("Mua từng tháng", "Mua một lần")) + 
    
  labs(x = "",
       y = "T\u0103ng tr\u01b0ởng",
       color = "",
       title = "So sánh danh mục 60/40 mua từng tháng và mua một lần",
       subtitle = "Quãng thời gian so sánh: 12 tháng") 
