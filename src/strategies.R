get_strategy_result <- function(df, strategy_name, series_name = NULL) {

  if (strategy_name %in% MODELS_AVAILABLE && !is.null(series_name)) {
    data <- preprocess_data(df, series_name)
    series <- data$series
    model <- fit(build_model(strategy_name), series)
    detection <- detect(model, series)
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }

  else if (strategy_name == "LEFT_REMD" && !is.null(series_name)) {
    result <- left_remd_detect(df, series_name)
    model <- result$model
    detection <- result$detection
    data <- result$data
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

  else if (strategy_name == "MASTER_LEAGUE") {
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

    # Right Chow
    volume_cumsum_df <- preprocess_data(df, "VOLUME_CUMSUM")
    volume_cumsum <- volume_cumsum_df$series
    chow <- fit(build_model("CHOW"), volume_cumsum)
    chow_detection <- detect(chow, volume_cumsum)
    event_true_indexes <- filter(chow_detection, event == TRUE)$idx
    for (index in event_true_indexes) {
      right <- max(min(length(volume_cumsum), index + 1), 1)
      chow_detection$event[right] <- TRUE
    }
    
    detection$event <- (detection$event & chow_detection$event)
    detection$type <- chow_detection$type
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }

  else if (strategy_name == "SUPER_DIFF") {
    data <- preprocess_data(df, "PRICE_DIFF")
    series <- data$series
    model <- fit(build_model("GARCH"), series)
    detection <- detect(model, series)
    event_true_indexes <- filter(detection, event == TRUE)$idx
    
    for (index in event_true_indexes) {
      left <- max(min(length(series), index - 1), 1)
      detection$event[left] <- TRUE
      detection$event[left + 1] <- FALSE
    }
    event_true_indexes <- filter(detection, event == TRUE)$idx

    # RED
    red <- fit(build_model("RED"), series)
    red_detection <- detect(red, series)
    red_event_true_indexes <- filter(red_detection, event == TRUE)$idx
    red_detection_plus <- red_detection
    for (index in red_event_true_indexes) {
      left <- max(min(length(series), index - 1), 1)
      red_detection_plus$event[left] <- TRUE
      red_detection_plus$event[left + 1] <- FALSE
    }

    detection$event <- (detection$event & red_detection$event) | (detection$event & red_detection_plus$event)
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }
  
  else if (strategy_name == "test") {
    data <- preprocess_data(df, "PRICE_DIFF")
    series <- data$series
    model <- fit(build_model("GARCH"), series)
    detection <- detect(model, series)
    event_true_indexes <- filter(detection, event == TRUE)$idx
    
    for (index in event_true_indexes) {
      left <- max(min(length(series), index - 1), 1)
      detection$event[left] <- TRUE
      detection$event[left + 1] <- FALSE
    }
    event_true_indexes <- filter(detection, event == TRUE)$idx
    
    # RED
    red <- fit(build_model("RED"), series)
    red_detection <- detect(red, series)
    red_event_true_indexes <- filter(red_detection, event == TRUE)$idx
    red_detection_plus <- red_detection
    for (index in red_event_true_indexes) {
      left <- max(min(length(series), index - 1), 1)
      red_detection_plus$event[left] <- TRUE
      red_detection_plus$event[left + 1] <- FALSE
    }

    # SUPER_DIFF result
    detection$event <- (detection$event & red_detection$event) | (detection$event & red_detection_plus$event)
    super_diff_result <- detection
    
    # Master league
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
    
    # Right Chow
    volume_cumsum_df <- preprocess_data(df, "VOLUME_CUMSUM")
    volume_cumsum <- volume_cumsum_df$series
    chow <- fit(build_model("CHOW"), volume_cumsum)
    chow_detection <- detect(chow, volume_cumsum)
    event_true_indexes <- filter(chow_detection, event == TRUE)$idx
    for (index in event_true_indexes) {
      right <- max(min(length(volume_cumsum), index + 1), 1)
      chow_detection$event[right] <- TRUE
    }
    
    detection$event <- (detection$event & chow_detection$event)
    detection$type <- chow_detection$type
    
    # Merge
    supa_positive_indexes <- (filter(super_diff_result, event == TRUE)$idx) + 1
    print(supa_positive_indexes)
    if (length(supa_positive_indexes) == 1 && detection$event[supa_positive_indexes[1]] == TRUE) {
      detection$event <- rep(FALSE, length(detection$event))
      detection$event[supa_positive_indexes[1]] <- TRUE
    }
    
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }

  else {
    stop(paste("Invalid strategy_name:", strategy_name))
  }
  
  if (PLOTTING) {
    custom_plot(model, series, detection, data)
  }
  
  dont_ask_me <- conf_matrix[TRUE == TRUE]
  return(as.numeric(c(dont_ask_me[2], dont_ask_me[5], dont_ask_me[6],
                      dont_ask_me[3])))
}


left_remd_detect <- function(df, series_name) {
  data <- preprocess_data(df, series_name)
  series <- data$series
  model <- fit(build_model("REMD"), series)
  detection <- detect(model, series)
  event_true_indexes <- filter(detection, event == TRUE)$idx
  
  for (index in event_true_indexes) {
    left_index <- max(min(length(series), index - 1), 1)
    detection$event[left_index] <- TRUE
  }
  
  return(list(
    model = model,
    detection = detection,
    data = data
  ))
}

custom_plot <- function(model, series, detection, data) {
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

