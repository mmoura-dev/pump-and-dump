BASE_PATH = "~/pump-and-dump/"
PLOTTING = FALSE
TIMEFRAME_IN_SECONDS = 180000 #  If null the entire pump dataset is used

library(dplyr)
library(daltoolbox) 
library(harbinger)
library(ggplot2)
library(zoo)

source(paste0(BASE_PATH, "src/btc_price_history.R"))
source(paste0(BASE_PATH, "src/pump_time_series.R"))
source(paste0(BASE_PATH, "src/preprocessing.R"))
source(paste0(BASE_PATH, "src/model_factory.R"))
source(paste0(BASE_PATH, "src/strategies.R"))
source(paste0(BASE_PATH, "src/metrics.R"))


pump_main <- function(chunk_size, strategy_name, series_name = NULL) {

  file_names <- basename(list.files(
    path = paste0(BASE_PATH, "lamorgia/binance_orders_history"),
    pattern = "*.csv",
    full.names = TRUE))
  file_names <- gsub(".csv", "", file_names)
  
  la_morgia_used_files <- read.csv(
    paste0(BASE_PATH, "lamorgia/lamorgia_used_pumps.csv"))$file_name
  file_names <- la_morgia_used_files
  # file_names <- file_names[1:17]

  hard_metrics <- data.frame(
    file_name = file_names,
    tp = vector("numeric", length(file_names)),
    fp = vector("numeric", length(file_names)),
    tn = vector("numeric", length(file_names)),
    fn = vector("numeric", length(file_names))
  )

  i <- 1
  for (file_name in file_names) {
    data <- get_pump_time_series(file_name, chunk_size)
    hard_metrics[i, 2:ncol(hard_metrics)] <- get_strategy_result(data,
                                                                 strategy_name,
                                                                 series_name)
    print(head(hard_metrics[i,]))
    i <- i + 1
    cat("\n")
  }
  
  if (!is.null(series_name)) {
    strategy_name <- paste0(strategy_name, "_", series_name)
  }

  hard_metrics <- calculate_metrics(hard_metrics)
  result <- data.frame(
    strategy = strategy_name,
    chunk_size = chunk_size,
    avg_accuracy = mean(hard_metrics$accuracy, na.rm = TRUE),
    avg_precision = mean(hard_metrics$precision, na.rm = TRUE),
    avg_recall = mean(hard_metrics$recall, na.rm = TRUE),
    avg_f1_score = mean(hard_metrics$f1_score, na.rm = TRUE),
    support = length(file_names)
  )

  print(result)
  current_datetime_str <- format(Sys.time(), "%d-%m-%Y_%H-%M-%S")
  write.csv(result,
            file = paste0(BASE_PATH, "results/", strategy_name, "_chunk-",
                          chunk_size, "s_timeframe-", TIMEFRAME_IN_SECONDS, "_",
                          current_datetime_str, ".csv"),
            row.names = FALSE)
  return(result)
}

pump_main(3600, "DUMMY", "RUSH_ORDERS")