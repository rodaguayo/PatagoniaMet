rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))

data_pet<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Evapotranspiration/Data_Evapotranspiration_v10.xlsx", sheet = "info")
data_pet$Zone <- factor(data_pet$Zone, levels = c("Northern", "Center", "Southern"))

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside")
title <-list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y <- list(title = "Correlation (r)", titlefont = f, 
          tickfont = f2, dtick = 0.05, ticks = "outside", zeroline = FALSE, range = c(0.8, 1))
  
fig1 <- plot_ly(data_pet, y = ~r, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)
fig1

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.99)
y2 <- list(title = "Bias (β)", titlefont = f, range = c(0, 2),
          tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE)

fig2 <- plot_ly(data_pet, y = ~Beta, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)
fig2

title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y3 <- list(title = "Variability (γ)", titlefont = f, range = c(0, 2),
           tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE)

fig3 <- plot_ly(data_pet, y = ~Gamma, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)
fig3

title4 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.95)
y4 <- list(title = "KGE", titlefont = f, 
          tickfont = f2, dtick = 0.25, ticks = "outside", zeroline = FALSE, range = c(0, 1))

fig4 <- plot_ly(data_pet, y = ~KGE, x = ~Zone, type = "box", 
                color = ~Zone, colors = brewer.pal(3, 'Dark2'))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4)
fig4

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.015, 0.015))
fig

server <- orca_serve()
server$export(fig, file = "FigureS1_Validation.pdf", width = 1200, height = 700)
server$export(fig, file = "FigureS1_Validation.png", width = 1200, height = 700)
server$close()





