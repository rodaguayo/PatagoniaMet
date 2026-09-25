rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")

setwd("/home/rooda/Dropbox/Patagonia/")

data_q  <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")
colnames(data_q)

f <- list(size = 18)
f2 <- list( size = 18)
bg_colour <- "rgb(245, 245, 245)"

y <- list( titlefont = f, tickfont = f2,  ticks = "outside", zeroline = FALSE, standoff = 0)

x1 <- list(title = list(text = "Area (km2)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig1 <- plot_ly(data_q, x = ~total_area, type = "histogram", colors = brewer.pal(8, 'Dark2')[1],  histnorm = "probability")
fig1 <- fig1 %>% layout(xaxis = x1, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)
fig1

x2 <- list(title = list(text = "Elevation (masl)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig2 <- plot_ly(data_q, x = ~elev_median, type = "histogram", colors = brewer.pal(8, 'Dark2')[2],  histnorm = "probability")
fig2 <- fig2 %>% layout(xaxis = x2, yaxis = y, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x3 <- list(title = list(text = "Glacier cover (%)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig3 <- plot_ly(data_q, x = ~glacier_cover, type = "histogram", colors = brewer.pal(8, 'Dark2')[3],  histnorm = "probability")
fig3 <- fig3 %>% layout(xaxis = x3, yaxis = y, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x4 <- list(title = list(text = "PP (mm yr-1)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig4 <- plot_ly(data_q, x = ~p_mean_PMET, type = "histogram", colors = brewer.pal(8, 'Dark2')[4],  histnorm = "probability")
fig4 <- fig4 %>% layout(xaxis = x4, yaxis = y, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x5 <- list(title = list(text = "Ep (mm yr-1)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig5 <- plot_ly(data_q, x = ~pet_mean_PMET, type = "histogram", colors = brewer.pal(8, 'Dark2')[5],  histnorm = "probability")
fig5 <- fig5 %>% layout(xaxis = x5, yaxis = y, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x6 <- list(title = list(text = "Aridity index (-)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig6 <- plot_ly(data_q, x = ~aridity_PMET, type = "histogram", colors = brewer.pal(8, 'Dark2')[6],  histnorm = "probability")
fig6 <- fig6 %>% layout(xaxis = x6, yaxis = y, showlegend = FALSE)
fig6 <- fig6 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x7 <- list(title = list(text = "Freq. high PP (days)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig7 <- plot_ly(data_q, x = ~high_prec_freq_PMET, type = "histogram", colors = brewer.pal(8, 'Dark2')[7],  histnorm = "probability")
fig7 <- fig7 %>% layout(xaxis = x7, yaxis = y, showlegend = FALSE)
fig7 <- fig7 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x8 <- list(title = list(text = "Freq. low PP (days)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig8 <- plot_ly(data_q, x = ~low_prec_freq_PMET, type = "histogram", colors = brewer.pal(8, 'Dark2')[8],  histnorm = "probability")
fig8 <- fig8 %>% layout(xaxis = x8, yaxis = y, showlegend = FALSE)
fig8 <- fig8 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

x9 <- list(title = list(text = "Fracion solid PP (%)",  standoff = 0), titlefont = f, tickfont = f2, ticks = "outside")
fig9 <- plot_ly(data_q, x = ~frac_snow_PMET, type = "histogram", colors = brewer.pal(8, 'Dark2')[1],  histnorm = "probability")
fig9 <- fig9 %>% layout(xaxis = x9, yaxis = y, showlegend = FALSE)
fig9 <- fig9 %>% layout(plot_bgcolor=bg_colour, bargap=0.1)

fig <- c(0.01, 0.01, 0.05, 0.05)
fig <- subplot(fig1, fig2,  fig3, fig4, fig5, fig6, fig7, fig8, fig9, 
               nrows = 3, shareX = F, shareY = T, titleX = T, titleY = T,margin = fig)
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/FigureX_basins_attr.png", width = 1000, height = 750, scale = 4)
