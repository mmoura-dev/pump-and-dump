get_series <- function(df, series_name) {
  if (series_name == "PRICE") {
    return(df$avg_usd_price)
  }
  else if (series_name == "VOLUME") {
    return(df$sum_usd_volume)
  }
  else if (series_name == "ORDERS") {
    return(df$num_orders)
  }
  else if (series_name == "VOLUME_CUMSUM") {
    return(cumsum(df$sum_usd_volume))
  }
  else if (series_name == "ORDERS_CUMSUM") {
    return(cumsum(df$num_orders))
  }
  else {
    stop("Invalid: series_name")
  }
}

data_cleaning <- function(data) {
  return(data[-c(1, nrow(data)), ])
}
