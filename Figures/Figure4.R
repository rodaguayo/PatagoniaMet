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
data_pp_alpha <- subset(data_pp, parameter == "PP α")
data_pp_beta  <- subset(data_pp, parameter == "PP β")

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
data_tmax_alpha <- subset(data_t2m, var == "Tmax" & parameter == "T2M α")
data_tmax_beta  <- subset(data_t2m, var == "Tmax" & parameter == "T2M β")
data_tmin_alpha <- subset(data_t2m, var == "Tmin" & parameter == "T2M α")
data_tmin_beta  <- subset(data_t2m, var == "Tmin" & parameter == "T2M β")

data_pp_param    <- read.csv("MS1 Results/PP_parameters.csv")
data_t2m_param   <- read.csv("MS1 Results/T2M_parameters.csv")
data_tmax_param  <- subset(data_t2m_param, var == "Tmax")
data_tmin_param  <- subset(data_t2m_param, var == "Tmax")

f <- list(family = "Times New Roman", size = 20)
f2 <- list(family = "Times New Roman", size = 16)

x     <- list(titlefont = f, tickfont = f2, ticks = "outside")
y     <- list(title = "Importance (%)", standoff=0, titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 20, range = c(-5,60))
title <- list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.04, y = 0.99)

fig1 <- plot_ly(type = 'box', y = data_pp_alpha$value, x = data_pp_alpha$variable, 
                offsetgroup = "A", showlegend = TRUE,  color = I("#a6bddb"), name = "PP α")
fig1 <- fig1 %>% add_boxplot(y = data_pp_beta$value, x = data_pp_beta$variable, offsetgroup = "B", color = I("#3690c0"), name = "PP β")
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = TRUE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1 <- fig1 %>% layout(annotations = title)
fig1 <- fig1 %>% add_trace(x = data_q$variable, y = data_q$value, color = data_q$parameter, name = "PP BCF",
                           type = 'scatter',  mode = 'markers', 
                           marker = list(size = 13, color = "#034e7b"))

title2 <-list(text = "d)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.04, y = 0.96)
y2 <- list(title = "Importance (%)", standoff=0, titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 20, range = c(-10,40))

fig2 <- plot_ly(y = data_tmax_alpha$value, x = data_tmax_alpha$variable, type = "box", 
                offsetgroup = "A",  color = I("#a1d99b"),  name = "Tmax α")
fig2 <- fig2 %>% add_boxplot(y = data_tmax_beta$value, x = data_tmax_beta$variable, offsetgroup = "B", color = I("#238b45"),  name = "Tmax β")
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig2 <- fig2 %>% layout(annotations = title2)


title3 <-list(text = "f)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.04, y = 0.91)
y3 <- list(title = "Importance (%)", standoff=0, titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 20, range = c(-10,40))

fig3 <- plot_ly(y = data_tmin_alpha$value, x = data_tmin_alpha$variable, type = "box", 
                offsetgroup = "A", color = I("#fdae6b"),  name = "Tmin α")
fig3 <- fig3 %>% add_boxplot(y = data_tmin_beta$value, x = data_tmin_beta$variable, offsetgroup = "B", color = I("#f16913"),  name = "Tmin β")
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig3 <- fig3 %>% layout(annotations = title3)

x2     <- list(title= "Month",titlefont = f, tickfont = f2, ticks = "outside", dtick = 1)
title4 <- list(text = "a) Precipitation", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.99)
y4     <- list(title = "PP β", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 0.4, range = c(0.2,1.7))
fig4 <- plot_ly(type = "box", y = data_pp_param$b_linear, x = data_pp_param$month, showlegend = FALSE, offsetgroup = "A", color = I("#3690c0"))
fig4 <- fig4 %>% layout(xaxis = x2, yaxis = y4)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig4 <- fig4 %>% layout(annotations = title4)

title5 <- list(text = "c) Maximum temperature", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.95)
y5     <- list(title = "Tmax β", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 2, range = c(-5,4))
fig5 <- plot_ly(type = "box", y = data_tmax_param$me, x = data_tmax_param$month, showlegend = FALSE, offsetgroup = "A", color = I("#238b45"))
fig5 <- fig5 %>% layout(xaxis = x2, yaxis = y5)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig5 <- fig5 %>% layout(annotations = title5)

title6 <- list(text = "e) Minimum temperature", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.01, y = 0.91)
y6     <- list(title = "Tmax β", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 2, range = c(-5,4))
fig6 <- plot_ly(type = "box", y = data_tmin_param$me, x = data_tmin_param$month, showlegend = FALSE, offsetgroup = "A", color = I("#f16913"))
fig6 <- fig6 %>% layout(xaxis = x2, yaxis = y6)
fig6 <- fig6 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig6 <- fig6 %>% layout(annotations = title6)


fig_a <- subplot(fig4, fig5, fig6, nrows = 3, shareX = T, titleY = T, margin =  c(0.04, 0.04, 0.01, 0.01))
fig_b <- subplot(fig1, fig2, fig3, nrows = 3, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))

fig <- subplot(fig_a, fig_b, nrows = 1, shareX = T, titleY = T, margin = c(0.06, 0.04, 0.01, 0.01))
fig <- fig %>% layout(boxmode = "group", boxgroupgap = 0.1, legend = list(orientation = 'h', x = 0.15, y = 1.05, font = f2))
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/Figure4_Random_Forest.png", width = 1000, height = 1000, scale = 4)
