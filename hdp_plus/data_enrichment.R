get_btc_price_history <- function(start_timestamp, end_timestamp) {
  
  query_api <- function(path, file_name) {
    result <- response <- POST(
      "https://api.livecoinwatch.com/coins/single/history",
      body = list(
        currency = "USD",
        code = "BTC",
        start = start_timestamp,
        end = end_timestamp,
        meta = FALSE
      ),
      encode = "json",
      add_headers("x-api-key" = LIVE_COIN_WATCH_API_KEY,
                  "content-type" = "application/json")
    )
    
    if (status_code(response) != 200) {
      stop("Live Coin Watch request has failed.")
    }
    
    json_data <- content(response, as = "text")
    parsed_json <- fromJSON(json_data)
    write.csv(parsed_json$history,
              file = paste0(path, file_name),
              row.names = FALSE)
  }
  
  path <- "cached_data/btc_price_history/"
  file_name <- paste0("btc-", start_timestamp, "-", end_timestamp, ".csv")
  cached_file_names <- list.files(path, pattern = "*.csv")
  
  if (!(file_name %in% cached_file_names)) {
    
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    
    query_api(path, file_name)
  }
  
  return(read.csv(paste0(path, file_name)))
}
