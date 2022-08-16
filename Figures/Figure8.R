rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
setwd("/home/rooda/Dropbox/Patagonia/")

models <- c("ERA5d", "MSWEP", "CR2MET","PMET")
vars_pp  <- c("KGE", "r", "Beta", "Gamma")
vars_t2m  <- c("ME", "rSD")

data_pp  <- read.csv("Data/Precipitation/Validation_PP.csv")
data_pp  <- rbind(setNames(data_pp[paste0("ERA5d_",   vars_pp)], vars_pp),
                  setNames(data_pp[paste0("MSWEP_", vars_pp)], vars_pp),
                  setNames(data_pp[paste0("CR2MET_",   vars_pp)], vars_pp),
                  setNames(data_pp[paste0("PMET_", vars_pp)], vars_pp))
n        <-  nrow(data_pp)/4
data_pp  <- cbind(data_pp, Model = c(rep("ERA5d", n), rep("MSWEP", n), rep("CR2MET", n), rep("PMET", n)))
data_pp$Model  <- factor(data_pp$Model, levels = models)

data_t2m <- read.csv("Data/Temperature/Validation_T2M.csv")
data_t2m  <- rbind(setNames(data_t2m[paste0("ERA5d_",   vars_t2m)], vars_t2m),
                   setNames(data_t2m[paste0("CR2MET_",   vars_t2m)], vars_t2m),
                   setNames(data_t2m[paste0("PMET_", vars_t2m)], vars_t2m))
n         <-  nrow(data_t2m)/3
data_t2m  <- cbind(data_t2m, Model = c(rep("ERA5d", n), rep("CR2MET", n), rep("PMET", n)))
data_t2m$Model <- factor(data_t2m$Model, levels = models)

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.2, 1))

fig1 <- plot_ly(data_pp, y = ~r, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0, 3),
           tickfont = f2, dtick = 1, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_pp, y = ~Beta, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0.3, 1.5),
           tickfont = f2, dtick = 0.3, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_pp, y = ~Gamma, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
           tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE, range = c(-1, 1))

fig4 <- plot_ly(data_pp, y = ~KGE, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4)

title5 <-list(text = "e)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.91)
y5 <- list(title = "Mean error (β')", titlefont = f, 
           tickfont = f2, dtick = 2, ticks = "outside", zeroline = FALSE, range = c(-4, 2))

fig5 <- plot_ly(data_t2m, y = ~ME, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig5 <- fig5 %>% layout(annotations = title5)

title6 <-list(text = "f)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.91)
y6 <- list(title = "Variability (γ')", titlefont = f, 
           tickfont = f2, dtick = 0.1, ticks = "outside", zeroline = FALSE, range = c(0.8, 1.2))

fig6 <- plot_ly(data_t2m, y = ~rSD, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig6 <- fig6 %>% layout(xaxis = x, yaxis = y6, showlegend = FALSE)
fig6 <- fig6 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig6 <- fig6 %>% layout(annotations = title6)

fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, nrows = 3, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))
fig

reticulate::use_miniconda('r-reticulate')
save_image(fig, file = "Figures/Figure8_Validation_v2.pdf", width = 1200, height = 1000, scale = 4)
save_image(fig, file = "Figures/Figure8_Validation_v2.png", width = 1200, height = 1000, scale = 4)
