get_strategy_result <- function(df, strategy_name, series_name = NULL) {
  BASE_MODELS <- c("ARIMA", "GARCH", "EMD", "REMD", "CHOW", "GFT")

  if (strategy_name %in% BASE_MODELS && !is.null(series_name)) {
    series <- get_series(df, series_name)
    model <- fit(build_model(strategy_name), series)
    detection <- detect(model, series)
    conf_matrix <- evaluate(model, detection$event, df$event)$confMatrix
    dont_ask_me <- conf_matrix[TRUE == TRUE]
    return(as.numeric(c(dont_ask_me[2], dont_ask_me[5], dont_ask_me[6],
                        dont_ask_me[3])))
  }
}