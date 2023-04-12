rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
setwd("/home/rooda/Dropbox/Patagonia/")

models   <- c("ERA5d", "W5E5", "MSWEP", "CR2MET", "PMET")
data_q  <- read.csv("MS1 Results/Q_performance.csv")
data_q$Model[data_q$Model== 'ERA5'] <- 'ERA5d'
data_q$Model  <- factor(data_q$Model, levels = models)
data_q_cal <- subset(data_q, Stage == "Calibration")
data_q_val <- subset(data_q, Stage == "Validation")

data_q_val$Model  <- factor(data_q_val$Model, levels = models)

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title1 <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.2, 1))

fig1 <- plot_ly(data_q_cal, y = ~r, x = ~Model, type = "box", color = ~Model, colors = brewer.pal(5, 'Dark2'), boxmean = T, offsetgroup = "A")
fig1 <- fig1 %>% add_boxplot(y = data_q_val$r, x = data_q_val$Model, type = "box", color = data_q_val$Model, boxmean = T, offsetgroup = "B", opacity = 0.6)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title1, boxmode = "group", boxgroupgap = 0.15)
fig1

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0, 2),
           tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_q_cal, y = ~Beta, x = ~Model, type = "box", color = ~Model, colors = brewer.pal(5, 'Dark2'), boxmean = T, offsetgroup = "A")
fig2 <- fig2 %>% add_boxplot(y = data_q_val$Beta, x = data_q_val$Model, type = "box", color = data_q_val$Model, boxmean = T, offsetgroup = "B", opacity = 0.6)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2, boxmode = "group", boxgroupgap = 0.15)

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0.6, 1.4),
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_q_cal, y = ~Gamma, x = ~Model, type = "box", color = ~Model, colors =  brewer.pal(5, 'Dark2'), boxmean = T, offsetgroup = "A")
fig3 <- fig3 %>% add_boxplot(y = data_q_val$Gamma, x = data_q_val$Model, type = "box", color = data_q_val$Model, boxmean = T, offsetgroup = "B", opacity = 0.6)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3, boxmode = "group", boxgroupgap = 0.15)

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
           tickfont = f2, dtick = 0.25, ticks = "outside", zeroline = FALSE, range = c(0.1, 1))

fig4 <- plot_ly(data_q_cal, y = ~KGE, x = ~Model, type = "box", color = ~Model, colors =  brewer.pal(5, 'Dark2'), boxmean = T, offsetgroup = "A")
fig4 <- fig4 %>% add_boxplot(y = data_q_val$KGE, x = data_q_val$Model, type = "box", color = data_q_val$Model, boxmean = T, offsetgroup = "B", opacity = 0.6)
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4, boxmode = "group", boxgroupgap = 0.15)

fig <- c(0.04, 0.04, 0.01, 0.01)
fig <- subplot(fig1, fig2,  fig3, fig4, nrows = 2, shareX = T, titleY = T, margin = fig)
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/Figure9_TUWmodel.png", width = 1200, height = 800, scale = 4)
