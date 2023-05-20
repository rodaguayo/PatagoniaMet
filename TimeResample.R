# Code for monthly aggregation --------------------------------------------------------------------
# Developed by Rodrigo Aguayo (2020-2023)

# monthly aggregation
MonthlyResample <- function(daily_data, days_min, FUN) {
  
  daily_data_c <- daily_data
  coredata(daily_data_c)[is.finite(daily_data_c)] <- 1
  monthly_data_c  <- aggregate(daily_data_c, strftime(time(daily_data_c), "%Y-%m-01"), sum, na.rm = TRUE)
  monthly_data    <- aggregate(daily_data,   strftime(time(daily_data),   "%Y-%m-01"), FUN, na.rm = TRUE)
  coredata(monthly_data)[monthly_data_c < days_min] <- NA
  coredata(monthly_data) <- round(monthly_data, 3)
  index(monthly_data) <- as.Date(time(monthly_data))
  monthly_data
}

# annual aggregation
AnnualResample <- function(monthly_data, months_min, FUN) {
  
  monthly_data_c <- monthly_data
  coredata(monthly_data_c)[is.finite(monthly_data_c)] <- 1
  annual_data_c  <- aggregate(monthly_data_c, strftime(time(monthly_data_c), "%Y-01-01"), sum, na.rm = TRUE)
  annual_data    <- aggregate(monthly_data,   strftime(time(monthly_data),   "%Y-01-01"), FUN, na.rm = TRUE)
  coredata(annual_data)[annual_data_c < months_min] <- NA
  coredata(annual_data) <- round(annual_data, 3)
  index(annual_data) <- as.Date(time(annual_data))
  annual_data
}
