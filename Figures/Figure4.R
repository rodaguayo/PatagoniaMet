rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
library("reshape2")

setwd("/home/rooda/Dropbox/Patagonia/")

vars     <- c("aspect", "climate_class", "distance_coast", "elevation", "t2m", "pp", "cloud_cover", "west_gradient")
data_pp  <- read.csv("MS1 Results/RF_PP_importance.csv")
data_pp  <- melt(data_pp, id.vars = "parameter", measure.vars = vars, na.rm = TRUE)
levels(data_pp$variable) <-  c("Aspect", "Climate class", "Distance to coast", "Elevation", "Temperature", "Precipitation", "Cloud cover", "West gradient")
data_pp$parameter[data_pp$parameter=="a linear"] <- "PP α"
data_pp$parameter[data_pp$parameter=="b linear"] <- "PP β"

vars     <- c("climate_class", "distance_coast", "elevation", "t2m", "pp", "cloud_cover")
data_q   <- read.csv("MS1 Results/RF_PP_factor_importance.csv")
data_q   <- melt(data_q, id.vars = "parameter", measure.vars = vars)
levels(data_q$variable) <-  c("Climate class", "Distance to coast", "Elevation", "Temperature", "Precipitation", "Cloud cover")
data_q$parameter[data_q$parameter=="pp_factor"] <- "PP BCF"

vars      <- c("climate_class", "distance_coast", "elevation", "t2m", "pp", "cloud_cover", "west_gradient")
data_t2m  <- read.csv("MS1 Results/RF_T2M_importance.csv")
data_t2m  <- melt(data_t2m, id.vars = c("parameter", "var"), measure.vars = vars, na.rm = TRUE)
levels(data_t2m$variable) <-  c("Climate class", "Distance to coast", "Elevation", "Temperature", "Precipitation", "Cloud cover",  "West gradient")
data_t2m$parameter[data_t2m$parameter=="me"] <-  "T2M β"
data_t2m$parameter[data_t2m$parameter=="rSD"] <- "T2M α" 
data_tmax <- subset(data_t2m, var == "Tmax")
data_tmin <- subset(data_t2m, var == "Tmin")

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x     <- list(titlefont = f, tickfont = f2, ticks = "outside")
y     <- list(titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 20, range = c(-5,60))
title <- list(text = "a) Precipitation", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)

fig1 <- plot_ly(data_pp, y = ~value, x = ~variable, type = "box", color = ~parameter, colors = brewer.pal(5, 'Blues')[c(2,4)])
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = TRUE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)
fig1 <- fig1 %>% add_trace(x = data_q$variable, y = data_q$value, color = data_q$parameter, 
                           type = 'scatter',  mode = 'markers', 
                           marker = list(size = 13, color = brewer.pal(9, 'Blues')[8] ))
fig1

title2 <-list(text = "b) Maximum temperature", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.96)
y2 <- list(title = "Importance (%)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 20, range = c(-10,50))

fig2 <- plot_ly(data_tmax, y = ~value, x = ~variable, type = "box", color = ~parameter, 
                colors = brewer.pal(6, 'Paired')[3:4])
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = TRUE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)

title3 <-list(text = "c) Minimum temperature", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.91)
y3 <- list(titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 20, range = c(-10,50))

fig3 <- plot_ly(data_tmin, y = ~value, x = ~variable, type = "box", color = ~parameter, 
                colors = brewer.pal(6, 'Paired')[3:4], legendgroup = ~parameter, showlegend = FALSE)
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)

fig <- c(0.04, 0.04, 0.01, 0.01)
fig <- subplot(fig1, fig2, fig3, nrows = 3, shareX = T, titleY = T, margin = fig)
fig <- fig %>% layout(legend = list(orientation = 'h', x = 0.86, y = 0.99, font = f2, bgcolor = "rgb(235, 235, 235)"))
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/Figure4_Random_Forest.png", width = 1000, height = 1000, scale = 4)
