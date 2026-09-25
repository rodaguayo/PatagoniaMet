rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
library("reshape2")

setwd("/home/rooda/Dropbox/Patagonia/")

# list of all predictors
vars     <- c("distance_coast", "elevation", "t2m", "pp", "cloud_cover",
              "west_gradient", "aspect", "aridity_index")
vars_name <- c("Distance to coast", "Elevation", "Temperature", "Precipitation", 
               "Cloud cover", "West gradient", "Aspect",  "Aridity index")

data_pp  <- read.csv("MS1 Results/RF_PP_importance.csv")
data_pp  <- melt(data_pp, id.vars = "parameter", measure.vars = vars, na.rm = FALSE)
levels(data_pp$variable) <-  vars_name
data_pp$parameter[data_pp$parameter=="a_linear"] <- "PP α"
data_pp$parameter[data_pp$parameter=="b_linear"] <- "PP β"
data_pp_alpha <- subset(data_pp, parameter == "PP α")
data_pp_beta  <- subset(data_pp, parameter == "PP β")

data_q   <- read.csv("MS1 Results/RF_PP_factor_importance.csv")
data_q$aspect   <- NA
data_q  <- melt(data_q, id.vars = "parameter", measure.vars = vars, na.rm = FALSE)
levels(data_q$variable) <-  vars_name

data_t2m  <- read.csv("MS1 Results/RF_T2M_importance.csv")
data_t2m  <- melt(data_t2m, id.vars = c("parameter", "var"), measure.vars = vars, na.rm = TRUE)
levels(data_t2m$variable) <-  vars_name
data_t2m$parameter[data_t2m$parameter=="me"] <-  "T2M β"
data_t2m$parameter[data_t2m$parameter=="rSD"] <- "T2M α" 
data_tmax_alpha <- subset(data_t2m, var == "Tmax" & parameter == "T2M α")
data_tmax_beta  <- subset(data_t2m, var == "Tmax" & parameter == "T2M β")
data_tmin_alpha <- subset(data_t2m, var == "Tmin" & parameter == "T2M α")
data_tmin_beta  <- subset(data_t2m, var == "Tmin" & parameter == "T2M β")


f <- list(family = "Times New Roman", size = 24)
f2 <- list(family = "Times New Roman", size = 18)
bg_colour <- "rgb(245, 245, 245)"

x     <- list(titlefont = f, tickfont = f2, ticks = "outside")
y     <- list(title = "Importance (%)", standoff=0, titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 10, range = c(0,30))
title <- list(text = "a)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0, y = 0.99)

fig1 <- plot_ly(type = 'box', y = data_pp_alpha$value, x = data_pp_alpha$variable, 
                offsetgroup = "A", showlegend = TRUE,  color = I("#a6bddb"), name = "PP α")
fig1 <- fig1 %>% add_boxplot(y = data_pp_beta$value, x = data_pp_beta$variable, offsetgroup = "B", color = I("#3690c0"), name = "PP β")
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = TRUE)
fig1 <- fig1 %>% layout(plot_bgcolor=bg_colour)
fig1 <- fig1 %>% layout(annotations = title)
fig1 <- fig1 %>% add_trace(x = data_q$variable, y = data_q$value, color = data_q$parameter, name = "PP BCF",
                           type = 'scatter',  mode = 'markers', 
                           marker = list(size = 13, color = "#034e7b"))

title2 <-list(text = "b)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0, y = 0.96)
y2 <- list(title = "Importance (%)", standoff=0, titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 10, range = c(0,30))

fig2 <- plot_ly(y = data_tmax_alpha$value, x = data_tmax_alpha$variable, type = "box", 
                offsetgroup = "A",  color = I("#a1d99b"),  name = "Tmax α")
fig2 <- fig2 %>% add_boxplot(y = data_tmax_beta$value, x = data_tmax_beta$variable, offsetgroup = "B", color = I("#238b45"),  name = "Tmax β")
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2)
fig2 <- fig2 %>% layout(plot_bgcolor=bg_colour)
fig2 <- fig2 %>% layout(annotations = title2)


title3 <-list(text = "c)", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0, y = 0.91)
y3 <- list(title = "Importance (%)", standoff=0, titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, dtick = 10, range = c(0,30))

fig3 <- plot_ly(y = data_tmin_alpha$value, x = data_tmin_alpha$variable, type = "box", 
                offsetgroup = "A", color = I("#fdae6b"),  name = "Tmin α")
fig3 <- fig3 %>% add_boxplot(y = data_tmin_beta$value, x = data_tmin_beta$variable, offsetgroup = "B", color = I("#f16913"),  name = "Tmin β")
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3)
fig3 <- fig3 %>% layout(plot_bgcolor=bg_colour)
fig3 <- fig3 %>% layout(annotations = title3)


fig <- subplot(fig1, fig2, fig3, nrows = 3, shareX = T, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))
fig <- fig %>% layout(boxmode = "group", boxgroupgap = 0.07, legend = list(orientation = 'h', x = 0.02, y = 1.04, font = f2))
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/FigureS2_Random_Forest.png", width = 800, height = 1000, scale = 4)
