rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))

data_q<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Streamflow/Data_Streamflow_v10.xlsx", sheet = "info")
data_q<-stack(data_q[,24:30])
data_q$values<-as.numeric(data_q$values)

levels(data_q$ind) <- c("ERA5", "MERRA2", "CSFR", "CR2REG", "MSWEP", "CR2MET", "PMET" )

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x <- list(titlefont = f, tickfont = f2, ticks = "outside", orientation = "v")
y <- list(title = "Bias correction factor (BFC)", titlefont = f, 
          tickfont = f2, dtick = 0.5, ticks = "outside", zeroline = FALSE, range = c(0,3))

fig1 <- plot_ly(data_q, y = ~values, x = ~ind, type = "box", color = ~ind, colors = brewer.pal(7, 'Dark2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1

server <- orca_serve()
setwd("C:/Users/rooda/Dropbox/Patagonia/Figures/")
server$export(fig1, file = "Figure6_BCF.pdf", width = 800, height = 700, scale = 4)
server$export(fig1, file = "Figure6_BCF.png", width = 800, height = 700, scale = 4)
server$close()