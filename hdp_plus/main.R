# HD Pump Plus (HDP+)

# Configs
PLOTTING = FALSE # If the used method should generate pdf plots
TIMEFRAME_SECONDS = 180000 #  If null the entire pump data set is used
# Time interval around the event to be used in detection
CHUNK_SIZE_SECONDS = 3600 # Interval for grouping observations
STRATEGY_NAME = "HDP_PLUS" # The event detection method
DATA_PREPROCESSING_NAME = NULL # Selects data preprocessing
LIVE_COIN_WATCH_API_KEY = Sys.getenv("LCW_API_KEY") # data enrichment api key

# Instructions:
# - Set the repository root as the R working directory
# - Set the desired configs above
# - Set the env var LCW_API_KEY
#   - For reproducibility you may download the cached data and skip this step
# - Run this script
# - Run main() on the R console


library(dplyr)
library(daltoolbox) 
library(harbinger)
library(ggplot2)
library(zoo)
library(httr)
library(jsonlite)

source("hdp_plus/data_enrichment.R")
source("hdp_plus/pump_series.R")
source("hdp_plus/preprocessing.R")
source("hdp_plus/model_factory.R")
source("hdp_plus/strategies.R")
source("hdp_plus/metrics.R")

main <- function() {
  
  file_names <- read.csv("lamorgia/lamorgia_used_pumps.csv")$file_name
  # file_names <- file_names[1:17]
  
  confusion_matrices <- data.frame(
    file_name = file_names,
    tp = vector("numeric", length(file_names)),
    fp = vector("numeric", length(file_names)),
    tn = vector("numeric", length(file_names)),
    fn = vector("numeric", length(file_names))
  )
  
  i <- 1
  for (file_name in file_names) {
    data <- get_pump_time_series(file_name, CHUNK_SIZE_SECONDS)
    confusion_matrices[i, 2:ncol(confusion_matrices)] <- get_strategy_result(data,
                                                                             STRATEGY_NAME,
                                                                             DATA_PREPROCESSING_NAME)
    print(head(confusion_matrices[i,]))
    i <- i + 1
    cat("\n")
  }
  
  if (!is.null(DATA_PREPROCESSING_NAME)) {
    STRATEGY_NAME <- paste0(STRATEGY_NAME, "_", DATA_PREPROCESSING_NAME)
  }
  
  hard_metrics <- calculate_metrics(confusion_matrices)
  result <- data.frame(
    strategy = STRATEGY_NAME,
    chunk_size = CHUNK_SIZE_SECONDS,
    avg_accuracy = mean(hard_metrics$accuracy, na.rm = TRUE),
    avg_precision = mean(hard_metrics$precision, na.rm = TRUE),
    avg_recall = mean(hard_metrics$recall, na.rm = TRUE),
    avg_f1_score = mean(hard_metrics$f1_score, na.rm = TRUE),
    support = length(file_names)
  )
  
  print(result)
  current_datetime_str <- format(Sys.time(), "%d-%m-%YT%H-%M-%S")
  write.csv(result,
            file = paste0("results/hdp_plus/", STRATEGY_NAME, "_chunk-",
                          CHUNK_SIZE_SECONDS, "s_timeframe-", TIMEFRAME_SECONDS,
                          "s_", current_datetime_str, ".csv"),
            row.names = FALSE)
  return(result)
}
