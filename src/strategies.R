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
    series <- data$series
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }
  
  else if (strategy_name == "RECALL_GFT") {
    result <- recall_gft_detect(df, series_name)
    recall_gft <- result$model
    recall_gft_detection <- result$detection
    recall_gft_data <- result$data
    series <- recall_gft_data$series
    conf_matrix <- evaluate(recall_gft, recall_gft_detection$event,
                            recall_gft_data$event)$confMatrix
  }

  else if (strategy_name == "RIGHT_CHOW") {
    result <- right_chow_detect(df, series_name)
    right_chow <- result$model
    right_chow_detection <- result$detection
    right_chow_data <- result$data
    series <- right_chow_data$series
    conf_matrix <- evaluate(right_chow, right_chow_detection$event,
                            right_chow_data$event)$confMatrix
  }

  else if (strategy_name == "MASTER_LEAGUE") {
    result <- master_league_detect(df, series_name)
    model <- result$model
    detection <- result$detection
    data <- result$data
    series <- data$series
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }

  else if (strategy_name == "SUPER_DIFF") {
    # Prefered timeframe: 200000
    # Has some randomness in precision
    result <- super_diff_detect(df)
    model <- result$model
    detection <- result$detection
    data <- result$data
    series <- data$series
    conf_matrix <- evaluate(model, detection$event, data$event)$confMatrix
  }
  
  else if (strategy_name == "SUPER_LEAGUE") {
    result <- super_diff_detect(df)
    super_diff_model <- result$model
    super_diff_detection <- result$detection
    super_diff_data <- result$dat

    result <- master_league_detect(df, "VOLUME_CUMSUM")
    master_league_model <- result$model
    master_league_detection <- result$detection
    master_league_data <- result$data
    series <- master_league_data$series

    # +1 because diff removes the first observation
    super_diff_true_indexes <- (filter(super_diff_detection,
                                       event == TRUE)$idx) + 1

    if (length(super_diff_true_indexes) == 1 &&
        master_league_detection$event[super_diff_true_indexes[1]] == TRUE) {
      master_league_detection$event <- rep(FALSE,
                                           length(master_league_detection$event))
      master_league_detection$event[super_diff_true_indexes[1]] <- TRUE
    }
    
    conf_matrix <- evaluate(master_league_model, master_league_detection$event,
                            master_league_data$event)$confMatrix
  }

  else if (strategy_name == "DUMMY") {
    result <- dummy_detect(df, series_name)
    model <- result$model
    detection <- result$detection
    data <- result$data
    series <- data$series
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

recall_gft_detect <- function(df, series_name) {
  change_point_series_name_validation(series_name)

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

  return(list(
    model = model,
    detection = detection,
    data = data
  ))
}

right_chow_detect <- function(df, series_name) {
  change_point_series_name_validation(series_name)

  data <- preprocess_data(df, series_name)
  series <- data$series
  chow <- fit(build_model("CHOW"), series)
  chow_detection <- detect(chow, series)

  event_true_indexes <- filter(chow_detection, event == TRUE)$idx
  for (index in event_true_indexes) {
    right <- max(min(length(series), index + 1), 1)
    chow_detection$event[right] <- TRUE
  }

  return(list(
    model = chow,
    detection = chow_detection,
    data = data
  ))
}

master_league_detect <- function(df, series_name) {
  result <- recall_gft_detect(df, series_name)
  recall_gft <- result$model
  recall_gft_detection <- result$detection
  recall_gft_data <- result$data
  
  result <- right_chow_detect(df, series_name)
  right_chow_detection <- result$detection
  
  recall_gft_detection$event <- (recall_gft_detection$event &
                                   right_chow_detection$event)
  recall_gft_detection$type <- right_chow_detection$type

  return(list(
    model = recall_gft,
    detection = recall_gft_detection,
    data = recall_gft_data
  ))
}

super_diff_detect <- function(df) {
  price_diff_data <- preprocess_data(df, "PRICE_DIFF")
  price_diff_series <- price_diff_data$series
  garch <- fit(build_model("GARCH"), price_diff_series)
  garch_detection <- detect(garch, price_diff_series)

  for (index in filter(garch_detection, event == TRUE)$idx) {
    left <- max(min(length(price_diff_series), index - 1), 1)
    garch_detection$event[left] <- TRUE
    garch_detection$event[left + 1] <- FALSE
  }

  red <- fit(build_model("RED"), price_diff_series)
  red_detection <- detect(red, price_diff_series)
  red_event_true_indexes <- filter(red_detection, event == TRUE)$idx
  left_red_detection_only <- red_detection
  for (index in red_event_true_indexes) {
    left <- max(min(length(price_diff_series), index - 1), 1)
    left_red_detection_only$event[left] <- TRUE
    left_red_detection_only$event[left + 1] <- FALSE
  }

  garch_detection$event <- (garch_detection$event &
                              red_detection$event) | (garch_detection$event &
                                                        left_red_detection_only$event)
  return(list(
    model = garch,
    detection = garch_detection,
    data = price_diff_data
  ))
}

dummy_detect <- function(df, series_name) {
  data <- preprocess_data(df, series_name)
  series <- data$series
  model <- fit(build_model("ARIMA"), series)
  detection <- detect(model, series)
  detection$event <- find_outliers(series, 30)

  return(list(
    model = model,
    detection = detection,
    data = data
  ))
}

find_outliers <- function(series, k) {
  calculate_stats <- function(subseq) {
    return(list(mean = mean(subseq), sd = sd(subseq)))
  }

  result <- rep(FALSE, length(series))

  for (i in 1:length(series)) {
    prev_start <- max(1, i - k)
    prev_end <- i - 1
    next_start <- i + 1
    next_end <- min(length(series), i + k)

    if (prev_start < prev_end) {
      prev_stats <- calculate_stats(series[prev_start:prev_end])
      prev_mean <- prev_stats$mean
      prev_sd <- prev_stats$sd
    } else {
      prev_mean <- series[i]
      prev_sd <- 0
    }

    if (next_start < next_end) {
      next_stats <- calculate_stats(series[next_start:next_end])
      next_mean <- next_stats$mean
      next_sd <- next_stats$sd
    } else {
      next_mean <- series[i]
      next_sd <- 0
    }

    if (abs(series[i] - prev_mean) > prev_sd &&
        abs(series[i] - next_mean) > next_sd) {
      result[i] <- TRUE
    }
  }

  return(result)
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

change_point_series_name_validation <- function(series_name) {
  if (!(series_name %in% c("ORDERS_CUMSUM", "VOLUME_CUMSUM"))) {
    print(paste("series_name:", series_name))
    stop("Invalid series_name for change point strategy.")
  }
}
