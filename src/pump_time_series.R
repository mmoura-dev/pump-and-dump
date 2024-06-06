# =============================================================================
# Warnings:
#   - Read the function in reverse order.
#   - BASE_PATH/cached_data/pre_processed_series directory should be created
#     before running.
#   - Depends on btc_price_history.R
#   - 
# =============================================================================


concat_event_column <- function(result_df, event_datetime, chunk_size) {
  result <- result_df %>%
    mutate(event = FALSE) %>%
    mutate(event = case_when(
      bin > event_datetime & bin <= (event_datetime + as.difftime(
        chunk_size, units = "secs")) ~ TRUE,
      TRUE ~ FALSE
    ))
  
  return(result)
}

group_orders_by_bin <- function(pristine_df, bins_df) {
  grp_df <- pristine_df %>%
    group_by(bin) %>%
    summarise(num_orders = n(),
              avg_usd_price = mean(usd_price),
              sum_usd_volume = sum(usd_volume))
  
  result <- left_join(bins_df, grp_df, by = "bin")
  result$avg_usd_price <- na.locf(result$avg_usd_price, na.rm = FALSE)
  result$sum_usd_volume[is.na(result$sum_usd_volume)] <- 0
  result$num_orders[is.na(result$num_orders)] <- 0
  result <- as.data.frame(result)
  
  result$num_orders <- as.numeric(result$num_orders)
  result$avg_usd_price <- as.numeric(result$avg_usd_price)
  result$sum_usd_volume <- as.numeric(result$sum_usd_volume)
  
  return(result)
}

generate_bins_df <- function(pristine_df, chunk_size) {
  datetime_range <- range(pristine_df$datetime)
  amplitude_in_seconds <- as.numeric(diff(datetime_range), units = "secs")
  number_of_bins <- round(amplitude_in_seconds / chunk_size)
  
  result <- data.frame(
    bin = seq(datetime_range[1], datetime_range[2],
              by = as.difftime(chunk_size, units = "secs"))
  )
  
  return(result)
}

generate_bin_column <- function(pristine_df, bin_df, chunk_size) {
  index_ <- 1
  pristine_df$bin <- sapply(pristine_df$datetime, function(dt) {
    while (index_ <= length(bin_df$bin) && bin_df$bin[index_] < dt) {
      index_ <<- index_ + 1
    }
    return(bin_df$bin[index_])
  })
  
  return(as.POSIXct(pristine_df$bin, tz = "UTC"))
}

merge_buy_orders_and_btc_prices <- function(buys_df, btc_prices_df) {
  buys_df$btc_price_in_usd <- sapply(buys_df$datetime, function(dt) {
    btc_index <- which(btc_prices_df$datetime > dt)[1]
    if (is.na(btc_index)) {
      btc_index <- which(btc_prices_df$datetime == max(btc_prices_df$datetime))
    }
    return(btc_prices_df$usd_price[btc_index])
  })
  buys_df$usd_volume = buys_df$btc_volume * buys_df$btc_price_in_usd
  
  pristine_df <- data.frame(
    datetime = buys_df$datetime,
    usd_price = buys_df$usd_volume / buys_df$amount,
    usd_volume = buys_df$usd_volume
  )

  return(pristine_df)
}

get_btc_usd_price_history <- function(start_datetime, end_datetime) {
  start_timestamp <- as.integer(start_datetime) * 1000
  end_timestamp <- as.integer(end_datetime) * 1000

  result <- get_btc_price_history(start_timestamp, end_timestamp) %>%
    mutate(
      datetime = as.POSIXct(date / 1000, tz = "UTC"),
      usd_price = rate,
      .keep = "none"
    )
  
  return(result)
}

get_event_datetime <- function(pump_history_file_name) {
  splitted_file_name <- unlist(strsplit(pump_history_file_name, "_"))
  event_time_str <- gsub("\\.", ":", gsub(".csv", "", splitted_file_name[3]))
  event_datetime_str <- paste(splitted_file_name[2], event_time_str)

  return(as.POSIXct(event_datetime_str, tz = "UTC"))
}

cache_pump_time_series <- function(pump_file, chunk_size, file_name) {
  event_datetime <- get_event_datetime(pump_file)
  orders_history_df <- read.csv(paste0(BASE_PATH,
                                       "lamorgia/binance_orders_history/",
                                       pump_file))

  buy_orders_history_df <- orders_history_df %>%
    filter(side == "buy") %>%
    select(-side, -price) %>%
    mutate(
      datetime = as.POSIXct(timestamp / 1000, tz = "UTC"),
      .keep = "unused"
    )
  btc_usd_price_history <- get_btc_usd_price_history(min(
    buy_orders_history_df$datetime), max(buy_orders_history_df$datetime))
  pristine_df <- merge_buy_orders_and_btc_prices(buy_orders_history_df,
                                                 btc_usd_price_history)
  
  bins_df <- generate_bins_df(pristine_df, chunk_size)
  pristine_df$bin <- generate_bin_column(pristine_df, bins_df, chunk_size)
  result <- group_orders_by_bin(pristine_df, bins_df)
  result <- concat_event_column(result, event_datetime, chunk_size)

  write.csv(result,
            file = file_name,
            row.names = FALSE)
}

get_pump_time_series <- function(pump_file_name, chunk_size) {

  path <- paste0(BASE_PATH, "cached_data/pre_processed_series/", "chunk_size-",
                 chunk_size, "/")
  target_file_name <- paste0(pump_file_name, "_", chunk_size, "s", ".csv")
  
  if (!(target_file_name %in% list.files(path, pattern = "*.csv"))) {
    print(paste(target_file_name, "cache miss"))
    
    if (!file.exists(path)) {
      dir.create(path)
    }
    
    cache_pump_time_series(paste0(pump_file_name, ".csv"), chunk_size,
                           paste0(path, target_file_name))
  }

  return(read.csv(paste0(path, target_file_name)))
}
