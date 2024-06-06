get_strategy_result <- function(df, strategy_name, series_name = NULL) {

  if (strategy_name %in% MODELS_AVAILABLE && !is.null(series_name)) {
    series <- get_series(df, series_name)
    model <- fit(build_model(strategy_name), series)
    detection <- detect(model, series)
    conf_matrix <- evaluate(model, detection$event, df$event)$confMatrix
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

    grf <- har_plot(model, series, detection, df$event)
    ggsave(paste0(BASE_PATH, "plots/plot_", plot_number, ".pdf"), plot = grf)
  }
  
  dont_ask_me <- conf_matrix[TRUE == TRUE]
  return(as.numeric(c(dont_ask_me[2], dont_ask_me[5], dont_ask_me[6],
                      dont_ask_me[3])))
}
