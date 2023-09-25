rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
setwd("/home/rooda/Dropbox/Patagonia/")

vars_pp  <- c("KGE", "r", "Beta", "Gamma")
data_pp  <- read.csv("Data/Precipitation/PP_Validation.csv")
data_pp  <- rbind(setNames(data_pp[paste0("ERA5_",   vars_pp)], vars_pp),
                  setNames(data_pp[paste0("MERRA2_", vars_pp)], vars_pp),
                  setNames(data_pp[paste0("CSFR_",   vars_pp)], vars_pp),
                  setNames(data_pp[paste0("CR2REG_", vars_pp)], vars_pp))
n        <-  nrow(data_pp)/4
data_pp  <- cbind(data_pp, Model = c(rep("ERA5", n), rep("MERRA2", n), rep("CSFR", n), rep("CR2REG", n)))
data_pp$Model <- factor(data_pp$Model, levels = c("CSFR", "ERA5", "MERRA2", "CR2REG")) # original order 

vars_t2m  <- c("ME", "rSD")
data_t2m <- read.csv("Data/Temperature/Tavg_Validation.csv")
data_t2m  <- rbind(setNames(data_t2m[paste0("ERA5_",   vars_t2m)], vars_t2m),
                  setNames(data_t2m[paste0("MERRA2_", vars_t2m)], vars_t2m),
                  setNames(data_t2m[paste0("CSFR_",   vars_t2m)], vars_t2m),
                  setNames(data_t2m[paste0("CR2REG_", vars_t2m)], vars_t2m))
n        <-  nrow(data_t2m)/4
data_t2m  <- cbind(data_t2m, Model = c(rep("ERA5", n), rep("MERRA2", n), rep("CSFR", n), rep("CR2REG", n)))
data_t2m$Model <- factor(data_t2m$Model, levels = c("CSFR", "ERA5", "MERRA2", "CR2REG")) # original order 

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)
bg_colour <- "rgb(245, 245, 245)"

hline <- function(y = 0, color = "black") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", opacity = 0.4,
       y0 = y, y1 = y,line = list(color = color, dash="dot", width = 2.5))}

x     <- list(titlefont = f, tickfont = f2, ticks = "outside")
y     <- list(title = "Correlation (r)", titlefont = f, tickfont = f2, dtick = 0.2, 
              ticks = "outside", zeroline = FALSE, range = c(0, 1))
title <- list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)

  
fig1 <- plot_ly(data_pp, y = ~r, x = ~Model, type = "box", color = ~Model, 
                colors = brewer.pal(4, 'Dark2'), boxmean = T)
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor=bg_colour)
fig1 <- fig1 %>% layout(annotations = title)
fig1 <- fig1 %>% layout(shapes = list(hline(1)))

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0, 4),
          tickfont = f2, dtick = 1, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_pp, y = ~Beta, x = ~Model, type = "box", color = ~Model, 
                colors = brewer.pal(4, 'Dark2'), boxmean = T)
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor=bg_colour)
fig2 <- fig2 %>% layout(annotations = title2)
fig2 <- fig2 %>% layout(shapes = list(hline(1)))

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0.3, 1.2),
           tickfont = f2, dtick = 0.3, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_pp, y = ~Gamma, x = ~Model, type = "box", color = ~Model, 
                colors = brewer.pal(4, 'Dark2'), boxmean = T)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor=bg_colour)
fig3 <- fig3 %>% layout(annotations = title3)
fig3 <- fig3 %>% layout(shapes = list(hline(1)))

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
          tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE, range = c(-1, 1))

fig4 <- plot_ly(data_pp, y = ~KGE, x = ~Model, type = "box",  color = ~Model, 
                colors = brewer.pal(4, 'Dark2'), boxmean = T)
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor=bg_colour)
fig4 <- fig4 %>% layout(annotations = title4)
fig4 <- fig4 %>% layout(shapes = list(hline(1)))

title5 <-list(text = "e)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.91)
y5 <- list(title = "Mean error (β')", titlefont = f, 
           tickfont = f2, dtick = 2, ticks = "outside", zeroline = FALSE, range = c(-4, 2))

fig5 <- plot_ly(data_t2m, y = ~ME, x = ~Model, type = "box", color = ~Model, 
                colors = brewer.pal(4, 'Dark2'), boxmean = T)
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor=bg_colour)
fig5 <- fig5 %>% layout(annotations = title5)
fig5 <- fig5 %>% layout(shapes = list(hline(0)))

title6 <-list(text = "f)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.91)
y6 <- list(title = "Variability (γ')", titlefont = f, 
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.6, 1.2))

fig6 <- plot_ly(data_t2m, y = ~rSD, x = ~Model, type = "box", color = ~Model, 
                colors = brewer.pal(4, 'Dark2'), boxmean = T)
fig6 <- fig6 %>% layout(xaxis = x, yaxis = y6, showlegend = FALSE)
fig6 <- fig6 %>% layout(plot_bgcolor=bg_colour)
fig6 <- fig6 %>% layout(annotations = title6)
fig6 <- fig6 %>% layout(shapes = list(hline(1)))

fig <- c(0.04, 0.04, 0.01, 0.01)
fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, nrows = 3, shareX = T, titleY = T, margin = fig)
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/FigureS1_Validation.png", width = 1200, height = 1000, scale = 4)
