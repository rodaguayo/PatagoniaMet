rm(list=ls())
cat("\014")  

library("exactextractr")
library("plotly")
library("terra")
library("sf")

setwd("/home/rooda/Dropbox/Patagonia/")
basin_shp  <- st_read("GIS South/Basins_PMET_v10_int.shp")
basin_pp   <- data.frame(matrix(0, ncol = 2, nrow = 0))
basin_t2m  <- data.frame(matrix(0, ncol = 2, nrow = 0))
basin_snow <- data.frame(matrix(0, ncol = 2, nrow = 0))

# list of datasets
pp_stacks <- list(ERA5d  = rast("Data/Precipitation/PP_ERA5_hr_1980_2020m.nc"),
                  MSWEP  = rast("Data/Precipitation/PP_MSWEPv28_1979_2020m.nc"),
                  CR2MET = rast("Data/Precipitation/PP_CR2MET_1960_2021m.nc"),
                  W5E5   = rast("Data/Precipitation/PP_W5E5_1979_2019m.nc"),
                  PMET   = rast("Data/Precipitation/PP_PMET_1980_2020m.nc"))

t2m_stacks <- list(ERA5d  = rast("Data/Temperature/Tavg_ERA5_hr_1980_2020m.nc"),
                   MSWEP  = rast("Data/Temperature/Tavg_MSWX_1979_2021m.nc"),
                   CR2MET = rast("Data/Temperature/Tavg_CR2MET_1960_2021m.nc"),
                   W5E5   = rast("Data/Temperature/Tavg_W5E5_1979_2019m.nc"),
                   PMET   = rast("Data/Temperature/Tavg_PMET_1980_2020m.nc"))

# use a common period
period     <- c(as.Date("1990-01-01"), as.Date("2019-12-31"))

for (i in 1:length(pp_stacks)) {
  stack <- pp_stacks[[i]]
  time(stack) <- as.Date(time(stack))
  stack <- stack[[time(stack) >= period[1] & time(stack) <= period[2]]]
  stack <- mean(tapp(stack, strftime(time(stack),format="%Y"), fun = sum, na.rm = TRUE))
  basin_pp <- rbind(basin_pp, cbind(names(t2m_stacks)[i], exact_extract(stack, basin_shp, "mean")))
  print(names(pp_stacks)[[i]])
}

for (i in 1:length(t2m_stacks)) {
  stack <- t2m_stacks[[i]]
  time(stack) <- as.Date(time(stack))
  stack <- stack[[time(stack) >= period[1] & time(stack) <= period[2]]]
  stack <- mean(tapp(stack, strftime(time(stack),format="%Y"), fun = mean, na.rm = TRUE))
  basin_t2m <- rbind(basin_t2m, cbind(names(t2m_stacks)[i], exact_extract(stack, basin_shp, "mean")))
  print(names(t2m_stacks)[[i]])
}

for (i in 1:length(pp_stacks)) {
  stack_pp  <- pp_stacks[[i]]
  stack_t2m <- t2m_stacks[[i]]
  save_time <- time(stack_pp)
  stack_t2m <- resample(stack_t2m, stack_pp, method = "bilinear")
  stack_pp[stack_t2m > 0] <- 0
  stack_pp <- mean(tapp(stack_pp, strftime(save_time,format="%Y"), fun = sum, na.rm = TRUE))
  basin_snow <- rbind(basin_snow, cbind(names(pp_stacks)[i], exact_extract(stack_pp, basin_shp, "mean")))
  print(names(pp_stacks)[[i]])
}

stack <- rast("Data/Evapotranspiration/PET_GLEAM36a_1980_2021m.nc")
stack <- stack[[time(stack) >= period[1] & time(stack) <= period[2]]]
stack <- mean(tapp(stack, strftime(time(stack),format="%Y"), fun = sum, na.rm = TRUE))
basin_pet <- as.data.frame(cbind("GLEAM", exact_extract(stack, basin_shp, "mean")))
basin_pet <- as.numeric(basin_pp$V2) / as.numeric(basin_pet$V2)

basin_pp_true <- read.csv("Data/Streamflow/Q_PMETobs_v10_metadata.csv")$PP_TRUE
basin_pp_true <- basin_pp_true/as.numeric(basin_pp$V2)

models   <- c("ERA5d", "W5E5", "MSWEP", "CR2MET", "PMET")
basin_pp$V1   <- factor(basin_pp$V1,   levels = models)
basin_t2m$V1  <- factor(basin_t2m$V1,  levels = models)
basin_snow$V1 <- factor(basin_snow$V1, levels = models)

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)
x <- list(titlefont = f, tickfont = f2,  ticks = "outside")

y1 <- list(title = "Mean precipitation (mm yr-1)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0, 5000))
fig1 <- plot_ly(x = basin_pp$V1, y = as.numeric(basin_pp$V2), type = "box", color = basin_pp$V1, colors = brewer.pal(5, 'Dark2'))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y1, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y2 <- list(title = "Mean temperature (ºC)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(2, 10), dtick = 2)
fig2 <- plot_ly(x = basin_t2m$V1, y = as.numeric(basin_t2m$V2), type = "box", color = basin_pp$V1, colors = brewer.pal(5, 'Dark2'))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y3 <- list(title = "Aridity index", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0, 8), dtick = 2)
fig3 <- plot_ly(x = basin_pp$V1, y = basin_pet, type = "box", color = basin_pp$V1, colors = brewer.pal(5, 'Dark2'))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y4 <- list(title = "Snow accumulation (mm)", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(-50, 1000))
fig4 <- plot_ly(x = basin_snow$V1, y = as.numeric(basin_snow$V2), type = "box", color = basin_snow$V1, colors = brewer.pal(5, 'Dark2'))
fig4 <- fig4 %>% layout(xaxis = x, yaxis = y4, showlegend = FALSE)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y5 <- list(title = "BCF", titlefont = f, tickfont = f2, ticks = "outside", zeroline = FALSE, range = c(0, 3))
fig5 <- plot_ly(x = basin_pp$V1, y = basin_pp_true, type = "box", color = basin_pp$V1, colors = brewer.pal(4, 'Dark2'))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

fig <- c(0.04, 0.04, 0.02, 0.02)
fig <- subplot(fig1, fig2, fig4, fig3, nrows = 2, shareX = T, shareY = F, titleY = T, margin = fig)
fig

reticulate::use_miniconda('r-reticulate')
reticulate::py_run_string("import sys") # https://github.com/plotly/plotly.R/issues/2179
save_image(fig, file = "MS1 Results/Figure7_comparison.png", width = 1000, height = 800, scale = 4)

