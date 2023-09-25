rm(list=ls())
cat("\014")  

library("plotly")
library("viridis")
setwd("/home/rooda/Dropbox/Patagonia/")

data_q <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")

f <- list(family = "Times New Roman", size = 38)
f2 <- list(family = "Times New Roman", size = 34)

x <- list(titlefont = f, tickfont = f2, title = "Bias correction factor (PP BFC)", ticks = "outside", dtick = 0.5)
bins <- list(start = 0, end = 2.01, size = 0.1) 

viridis_mod <- viridis(11, direction = -1)
viridis_mod <- c(rep(viridis_mod[1], 8), viridis_mod, viridis_mod[11])

y1 <- list(title = "Basins (n)", titlefont = f, dtick = 5, tickfont = f2, ticks = "outside", orientation = "v", zeroline = FALSE, autorange="reversed")
fig1 <- plot_ly(data_q, y = ~BF_PMET, type = "histogram", marker = list(color = viridis_mod), ybins = bins)
fig1 <- fig1 %>% layout(xaxis = y1, yaxis = x, showlegend = FALSE, bargap=0.1)
fig1 <- fig1 %>% layout(plot_bgcolor="rgba(0, 0, 0, 0)")

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig1, file = "MS1 Results/FigureS4_BCF_hist.png", width = 350, height = 1000, scale = 4)
