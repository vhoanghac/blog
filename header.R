# Github:
github_raw <- "https://raw.githubusercontent.com/vhoanghac/blog/master/"

# Data dir:
data_dir <- paste0(github_raw,"data")

# Themes:
theme <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  legend.position = "none",
  plot.caption = element_text(size = 10, color = "black", face = "italic"))  # No Legends

theme2 <- theme(
  text = element_text(family = "serif", size = 14),
  title = element_text(color = "#8b0000"),
  axis.text.y = element_text(face = "bold"),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5, size = 12),
  plot.caption = element_text(size = 10, color = "black", face = "italic"))
