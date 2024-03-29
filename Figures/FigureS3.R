rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")

setwd("/home/rooda/Dropbox/Patagonia/")

data_pet <- read.csv("Data/Evapotranspiration/Ep_Validation.csv")
data_pet$Zone <- factor(data_pet$Zone, levels = c("Northern", "Center", "Southern"))

f  <- list(family = "Times New Roman", size = 24)
f2 <- list(family = "Times New Roman", size = 20)
bg_colour <- "rgb(245, 245, 245)"

hline <- function(y = 0, color = "black") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", opacity = 0.4,
       y0 = y, y1 = y,line = list(color = color, dash="dot", width = 2.5))}

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.05, ticks = "outside", zeroline = FALSE, range = c(0.8, 1))
  
fig1 <- plot_ly(data_pet, y = ~GLEAM_r, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'), boxmean = TRUE)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor=bg_colour)
fig1 <- fig1 %>% layout(annotations = title)
fig1 <- fig1 %>% layout(shapes = list(hline(1)))

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0, 2),
          tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_pet, y = ~GLEAM_Beta, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'), boxmean = TRUE)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor=bg_colour)
fig2 <- fig2 %>% layout(annotations = title2)
fig2 <- fig2 %>% layout(shapes = list(hline(1)))

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0, 2),
           tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_pet, y = ~GLEAM_Alpha, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'), boxmean = TRUE)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor=bg_colour)
fig3 <- fig3 %>% layout(annotations = title3)
fig3 <- fig3 %>% layout(shapes = list(hline(1)))

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
          tickfont = f2, dtick = 0.25, ticks = "outside", zeroline = FALSE, range = c(0, 1))

fig4 <- plot_ly(data_pet, y = ~GLEAM_KGE, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'), boxmean = TRUE)
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor=bg_colour)
fig4 <- fig4 %>% layout(annotations = title4)
fig4 <- fig4 %>% layout(shapes = list(hline(1)))

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.02, 0.02))
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/FigureS3_Validation_Ep.png", width = 1100, height = 800, scale = 4)