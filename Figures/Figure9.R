rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))

data_q<-read.csv("C:/Users/rooda/Dropbox/Patagonia/MS1 Results/Q_performance.csv")
data_q$Model <- factor(data_q$Model, levels = c("ERA5d - C", "ERA5d - V", "MSWEP - C", "MSWEP - V","CR2MET - C","CR2MET - V","PMET - C", "PMET - V"))

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0, 1))

fig1 <- plot_ly(data_q, y = ~r, x = ~Model, type = "box", color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)
fig1

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0, 3),
           tickfont = f2, dtick = 1, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_q, y = ~Beta, x = ~Model, type = "box", color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)
fig2

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0.3, 1.5),
           tickfont = f2, dtick = 0.3, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_q, y = ~Gamma, x = ~Model, type = "box", color = ~Model, colors =  brewer.pal(4, 'Dark2'))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
           tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE, range = c(-1, 1))

fig4 <- plot_ly(data_q, y = ~KGE, x = ~Model, type = "box", color = ~Model, colors =  brewer.pal(4, 'Dark2'))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4)

fig <- subplot(fig1, fig2,  fig3, fig4,nrows = 2, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))
fig

server <- orca_serve()
setwd("C:/Users/rooda/Dropbox/Patagonia/Figures/")
server$export(fig, file = "Figure9_TUWmodel.pdf", width = 1200, height = 800, scale = 4)
server$export(fig, file = "Figure9_TUWmodel.png", width = 1200, height = 800, scale = 4)
server$close()