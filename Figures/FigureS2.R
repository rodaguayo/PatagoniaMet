rm(list=ls())
cat("\014")  

library("readxl")
library("plotly")
library("RColorBrewer")

Sys.setenv(PATH=paste0("C:/Users/rooda/AppData/Local/Programs/orca;", Sys.getenv("PATH")))

data_pp<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/Data_Precipitation_v10.xlsx", sheet = "importance")
data_t2m<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Temperature/Data_Temperature_v10.xlsx", sheet = "importance")
data_q<-read_xlsx("C:/Users/rooda/Dropbox/Patagonia/Data/Streamflow/Data_Streamflow_v10.xlsx", sheet = "importance")

data<-rbind(data_pp,data_t2m, data_q)
data$variable <- factor(data$variable, levels = c("Raw PP", "Raw T2M", "Elevation", "Distance to coast", "Climate class"
                                                   , "Aspect", "Cloud cover", "West gradient"))
data$index <- factor(data$index, levels = c("\u03b1 PP", "\u03b2 PP", "\u03b1 T2M", "\u03b2 T2M", "BCF"))

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

y <- list(titlefont = f, tickfont = f2, ticks = "outside")
x <- list(title = "Relative importance (%)", titlefont = f, 
          tickfont = f2, dtick = 10, ticks = "outside", zeroline = FALSE, range = c(0, 40))
fig1 <- plot_ly(data, x = ~value, y = ~variable, type = "bar", split = ~index,
                color = ~index, colors = brewer.pal(5, 'Paired'), 
                marker = list(line = list(color = "rgba(0,0,0,0.5)", width = 1.5)))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(legend = list(x = 0.79, y = 0.99, font = f2, bgcolor = 'rgba(0,0,0,0.1)'))

fig1


server <- orca_serve()
server$export(fig1, file = "FigureS2_Random_Forest.pdf", width = 700, height = 1000, scale = 4)
server$export(fig1, file = "FigureS2_Random_Forest.png", width = 700, height = 1000, scale = 4)
server$close()