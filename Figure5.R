rm(list=ls())
cat("\014")  

library("xlsx")
library("readxl")
library("plotly")
library("RColorBrewer")

Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))

data_pp<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Figures/Figure5_Validation.xlsx", sheet = "data_pp")
data_t2m<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Figures/Figure5_Validation.xlsx", sheet = "data_t2m")

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0, 1))
  
fig1 <- plot_ly(data_pp, y = ~r, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (B)", titlefont = f, range = c(0, 4),
          tickfont = f2, dtick = 1, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_pp, y = ~Beta, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (y)", titlefont = f, range = c(0.3, 1.2),
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
y5 <- list(title = "Bias (degC)", titlefont = f, 
           tickfont = f2, dtick = 2, ticks = "outside", zeroline = FALSE, range = c(-8, 2))

fig5 <- plot_ly(data_t2m, y = ~Beta2, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig5 <- fig5 %>% layout(annotations = title5)

title6 <-list(text = "f)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.91)
y6 <- list(title = "Variability (y)", titlefont = f, 
           tickfont = f2, dtick = 0.2, ticks = "outside", zeroline = FALSE, range = c(0.6, 1.2))

fig6 <- plot_ly(data_t2m, y = ~Gamma2, x = ~Model, type = "box", 
                color = ~Model, colors = brewer.pal(4, 'Dark2'))
fig6 <- fig6 %>% layout(xaxis = x, yaxis = y6, showlegend = FALSE)
fig6 <- fig6 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig6 <- fig6 %>% layout(annotations = title6)

fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, 
               nrows = 3, shareX = T, titleY = T, 
               margin = c(0.04, 0.04, 0.01, 0.01))
fig

orca(fig, file = "Figure5_Validation.pdf", width = 1200, height = 1000)
orca(fig, file = "Figure5_Validation.png", width = 1200, height = 1000)
