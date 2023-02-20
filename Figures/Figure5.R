rm(list=ls())
cat("\014")  

library("plotly")
library("viridis")
setwd("/home/rooda/Dropbox/Patagonia/")

data_raw <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
data_q <- data_raw[c("PP_ERA5","PP_CR2MET", "PP_MSWEP", "PP_W5D5", "PP_PMET")]/data_raw$PP_TRUE
data_q$PP_PMET <- data_raw$BF_PMET 

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, title = "Bias correction factor (PP BFC)", ticks = "outside", range = c(0, 3))
bins <- list(start = 0, end = 3, size = 0.1) 

y1 <- list(title = "PMET (%)", titlefont = f, tickfont = f2, ticks = "outside", orientation = "v", zeroline = FALSE, range = c(0, 0.149), tickformat = ".0%")
fig1 <- plot_ly(data_q, x = ~PP_PMET, type = "histogram", marker = list(color = viridis(9)[1]), histnorm = "probability", opacity = 0.7, xbins = bins, bingroup=1)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y1, showlegend = FALSE, bargap=0.1)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y2 <- list(title = "ERA5 (%)", titlefont = f, tickfont = f2, ticks = "outside", orientation = "v", zeroline = FALSE, range = c(0, 0.149), tickformat = ".0%")
fig2 <- plot_ly(data_q,  x = ~PP_ERA5, type = "histogram",  marker = list(color = viridis(9)[3]), histnorm = "probability",  opacity = 0.7, bingroup=1)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE, bargap=0.1)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y3 <- list(title = "CR2MET (%)", titlefont = f, tickfont = f2, ticks = "outside", orientation = "v", zeroline = FALSE, range = c(0, 0.149), tickformat = ".0%")
fig3 <- plot_ly(data_q,  x = ~PP_CR2MET, type = "histogram", marker = list(color = viridis(9)[5]), histnorm = "probability",  opacity = 0.7, bingroup=1)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE, bargap=0.1)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y4 <- list(title = "MSWEP (%)", titlefont = f, tickfont = f2, ticks = "outside", orientation = "v", zeroline = FALSE, range = c(0, 0.149), tickformat = ".0%")
fig4 <- plot_ly(data_q, x = ~PP_MSWEP, type = "histogram", marker = list(color = viridis(9)[7]), histnorm = "probability",  opacity = 0.7, bingroup=1)
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE, bargap=0.1)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y5 <- list(title = "W5D5 (%)", titlefont = f, tickfont = f2, ticks = "outside", orientation = "v", zeroline = FALSE, range = c(0, 0.149), tickformat = ".0%")
fig5 <- plot_ly(data_q, x = ~PP_MSWEP, type = "histogram", marker = list(color = viridis(9)[9]), histnorm = "probability",  opacity = 0.7, bingroup=1)
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE, bargap=0.1)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")


fig <- c(0.04, 0.04, 0.01, 0.01)
fig <- subplot(fig1, fig2, fig3, fig4, fig5, nrows = 5, shareX = T, shareY = T, titleY = T, margin = fig)
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/Figure5_BCF_hist.png", width = 600, height = 1000, scale = 4)
