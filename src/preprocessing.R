preprocess_data <- function(df, series_name) {
  df <- data_cleaning(df)
  event_column <- df$event
  
  if (series_name == "PRICE") {
    series_column <- df$avg_usd_price
  }

  else if (series_name == "VOLUME") {
    series_column <- df$sum_usd_volume
  }

  else if (series_name == "ORDERS") {
    series_column <- df$num_orders
  }

  else if (series_name == "VOLUME_CUMSUM") {
    series_column <- cumsum(df$sum_usd_volume)
  }

  else if (series_name == "ORDERS_CUMSUM") {
    series_column <- cumsum(df$num_orders)
  }

  else if (series_name == "PRICE_DIFF") {
    series_column <- diff(df$avg_usd_price)
    event_column <- event_column[2:length(event_column)]
  }

  else {
    stop("Invalid: series_name")
  }
  
  return(data.frame(
    series = series_column,
    event = event_column
  ))
}

data_cleaning <- function(data) {
  return(data[-c(1, nrow(data)), ])
}
