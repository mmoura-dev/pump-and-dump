get_strategy_result <- function(df, strategy_name, series_name = NULL) {

  if (strategy_name %in% MODELS_AVAILABLE && !is.null(series_name)) {
    data <- preprocess_data(df, series_name)
    series <- data$series
    model <- fit(build_model(strategy_name), series)
    detection <- detect(model, series)
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }

  else if (strategy_name == "LEFT_REMD" && !is.null(series_name)) {
    data <- preprocess_data(df, series_name)
    series <- data$series
    model <- fit(build_model("REMD"), series)
    detection <- detect(model, series)
    event_true_indexes <- filter(detection, event == TRUE)$idx

    for (index in event_true_indexes) {
      left_index <- max(min(length(series), index - 1), 1)
      detection$event[left_index] <- TRUE
    }

    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }
  
  else if (strategy_name == "RECALL_GFT" && series_name %in% c("ORDERS_CUMSUM", "VOLUME_CUMSUM")) {
    data <- preprocess_data(df, series_name)
    series <- data$series
    model <- fit(build_model("GFT"), series)
    detection <- detect(model, series)
    event_true_indexes <- filter(detection, event == TRUE)$idx
    
    for (index in event_true_indexes) {
      left <- max(min(length(series), index - 1), 1)
      right <- max(min(length(series), index + 1), 1)
      detection$event[left] <- TRUE
      detection$event[right] <- TRUE
    }
    
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }
  
  else if (strategy_name == "EVIDENT_PUMP") {
    data <- preprocess_data(df, "VOLUME_CUMSUM")
    series <- data$series
    model <- fit(build_model("GFT"), series)
    detection <- detect(model, series)
    event_true_indexes <- filter(detection, event == TRUE)$idx
    
    for (index in event_true_indexes) {
      left <- max(min(length(series), index - 1), 1)
      right <- max(min(length(series), index + 1), 1)
      detection$event[left] <- TRUE
      detection$event[right] <- TRUE
    }

    volume_cumsum_df <- preprocess_data(df, "VOLUME_CUMSUM")
    volume_cumsum <- volume_cumsum_df$series
    chow <- fit(build_model("CHOW"), volume_cumsum)
    chow_detection <- detect(chow, volume_cumsum)

    detection$event <- detection$event & chow_detection$event
    detection$type <- chow_detection$type
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }

  else {
    stop(paste("Invalid strategy_name:", strategy_name))
  }
  
  if (PLOTTING) {
    plot_files <- basename(list.files(
      path = paste0(BASE_PATH, "plots"),
      pattern = "^plot_\\d+.pdf",
      full.names = FALSE))
    plot_number <- 1
    
    if (length(plot_files) > 0) {
      plot_numbers <- as.numeric(sub("plot_(\\d+).pdf", "\\1", plot_files))
      plot_number <- max(plot_numbers, na.rm = TRUE) + 1
    }

    grf <- har_plot(model, series, detection, data$event)
    ggsave(paste0(BASE_PATH, "plots/plot_", plot_number, ".pdf"), plot = grf)
  }
  
  dont_ask_me <- conf_matrix[TRUE == TRUE]
  return(as.numeric(c(dont_ask_me[2], dont_ask_me[5], dont_ask_me[6],
                      dont_ask_me[3])))
}
