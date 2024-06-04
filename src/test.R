BASE_PATH = "~/pump-and-dump/"

source(paste0(BASE_PATH, "src/btc_price_history.R"))
source(paste0(BASE_PATH, "src/pump_time_series.R"))

file_names <- basename(list.files(
  path = paste0(BASE_PATH, "lamorgia/binance_orders_history"),
  pattern = "*.csv",
  full.names = TRUE))
file_names <- gsub(".csv", "", file_names)

for (file_name in file_names) {
  data <- get_pump_time_series(file_name, 3600, FALSE)
  print(head(data))
  break
}
