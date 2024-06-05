calculate_metrics <- function(df) {
  if (!all(c("file_name", "tp", "fp", "tn", "fn") %in% colnames(df))) {
    stop("The df must contain the columns: file_name, tp, fp, tn, and fn")
  }

  df$accuracy <- with(df, (tp + tn) / (tp + fp + tn + fn))
  df$precision <- with(df, ifelse((tp + fp) > 0, tp / (tp + fp), NA))
  df$recall <- with(df, ifelse((tp + fn) > 0, tp / (tp + fn), NA))
  df$f1_score <- with(df,
                      ifelse((precision + recall) > 0,
                             2 * (precision * recall) / (precision + recall),
                             NA))

  return(df)
}
