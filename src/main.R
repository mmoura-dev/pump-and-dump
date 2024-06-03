library(dplyr)
library(httr)
library(jsonlite)
library(zoo)
library(daltoolbox) 
library(harbinger)

EXPERIMENT_NAME <- "test"
TIMEFRAME <- FALSE
CHUNK_SIZE <- 3600
SERIES_NAMES <- c("avg_usd_price", "sum_usd_volume", "cumsum_usd_volume",
                  "num_orders")
MODEL_NAMES <- c("ARIMA", "EMD", "REMD", "CHOW", "GFT")

get_btc_price <- function(start_timestamp, end_timestamp){
  path <- "~/pump-and-dump/cached_data/btc_price_history/"
  target_file_name <- paste0("btc-", start_timestamp, "-", end_timestamp,
                             ".csv")
  file_names <- list.files(path,
                           pattern = "*.csv")
  
  if (!(target_file_name %in% file_names)) {
    response <- POST(
      "https://api.livecoinwatch.com/coins/single/history",
      body = list(
        currency = "USD",
        code = "BTC",
        start = start_timestamp,
        end = end_timestamp,
        meta = FALSE
      ),
      encode = "json",
      add_headers("x-api-key" = Sys.getenv("API_KEY"),
                  "content-type" = "application/json")
    )
    
    if (status_code(response) != 200) {
      stop("Live Coin Watch request has failed.")
    }
    
    json_data <- content(response, as = "text")
    parsed_json <- fromJSON(json_data)
    write.csv(parsed_json$history,
              file = paste0(path, target_file_name),
              row.names = FALSE)
  }
  
  return(read.csv(paste0(path, target_file_name)))
}

cached_get_pump_ts <- function(file_name, seconds_to_group, timeframe) {
  path <- paste0("~/pump-and-dump/cached_data/pre_processed_series/", "cs-", seconds_to_group, "_tf-",
                 timeframe, "/")
  target_file_name <- paste0(file_name, "_", seconds_to_group, "_", timeframe,
                             ".csv")
  
  if (!(target_file_name %in% list.files(path, pattern = "*.csv"))) {
    print(target_file_name)
    stop("TS cache miss")
  }
  
  return(read.csv(paste0(path, target_file_name)))
}

build_model <- function(model_name) {
  if (model_name == "ARIMA") {
    return(hanr_arima())
  }
  else if (model_name == "GARCH") {
    return(hanr_garch())
  }
  else if (model_name == "RED") {
    return(hanr_red())
  }
  else if (model_name == "REMD") {
    return(hanr_remd())
  }
  else if (model_name == "EMD") {
    return(hanr_emd())
  }
  else if (model_name == "CHOW") {
    return (hcp_chow())
  }
  else if (model_name == "GFT") {
    return (hcp_gft())
  } else {
    stop(paste("Model not found: ", model_code_int))
  }
}

file_names <- basename(list.files(
  path = "~/pump-and-dump/lamorgia/binance_orders_history", pattern = "*.csv",
  full.names = TRUE))
file_names <- gsub(".csv", "", file_names[1:3])
result_dfs <- sapply(SERIES_NAMES, function(name) list())

for (file_name in file_names) {
  print(file_name)
  df <- cached_get_pump_ts(file_name, CHUNK_SIZE, TIMEFRAME)
  # print(head(df))
  
  for (i in 1:length(SERIES_NAMES)) {
    print(SERIES_NAMES[i])
    
    model_names_copy <- MODEL_NAMES
    if (SERIES_NAMES[i] == "cumsum_usd_volume") {
      model_names_copy <- c("ARIMA", "CHOW", "GFT")
    }
    
    series <- df[[SERIES_NAMES[i]]]
    # print(head(series))
    
    hard_evals <- data.frame(
      model_name = model_names_copy,
      tp = vector("numeric", length(model_names_copy)),
      fp = vector("numeric", length(model_names_copy)),
      tn = vector("numeric", length(model_names_copy)),
      fn = vector("numeric", length(model_names_copy))
    )

    j <- 1
    for (model_name in model_names_copy) {
      model <- fit(build_model(model_name), series)
      detection <- detect(model, series)
      
      if (model_name == "GFT") {
        event_true_indexes <- filter(detection, event == TRUE)$idx

        arima <- fit(build_model("ARIMA"), series)
        detection <- detect(arima, series) %>%
          mutate(event = ifelse(row_number() < event_true_indexes[1] | row_number() > event_true_indexes[2], FALSE, event))
      }

      # if (model_name == "CHOW") {
      #   remd <- fit(build_model("REMD"), series)
      #   detection <- detect(remd, series)
      #   plot(har_plot(model, series, detection, df$event))
      #   event_true_indexes <- filter(detection, event == TRUE)$idx
      # 
      #   detection$event <- FALSE
      #   for (index in event_true_indexes) {
      #     OFFSETT <- 10
      #     start_index <- max(1, index - OFFSETT)
      #     end_index <- min(length(series), start_index + (2 * OFFSETT))
      #     
      #     model <- fit(build_model("CHOW"), series[start_index:end_index])
      #     detection$event[start_index:end_index] <- detect(model, series[start_index:end_index])$event
      #   }
      #   plot(har_plot(model, series, detection, df$event))
      #   print(evaluate(model, detection$event, df$event)$confMatrix)
      # }
      
      # if (model_name == "REMD") {
      #   plot(har_plot(model, series, detection, df$event))
      #   print(evaluate(model, detection$event, df$event)$confMatrix)
      # }
      # 
      # if (model_name == "CHOW" && SERIES_NAMES[i] == "cumsum_usd_volume") {
      #   plot(har_plot(model, series, detection, df$event))
      #   print(evaluate(model, detection$event, df$event)$confMatrix)
      # }
      
      if (model_name == "CHOW" && SERIES_NAMES[i] == "avg_usd_price") {
        remd <- fit(build_model("REMD"), series)
        detection <- detect(remd, series)
        event_true_indexes <- filter(detection, event == TRUE)$idx

        for (index in event_true_indexes) {
          end_index <- min(length(series), index - 1)
          detection$event[end_index] <- TRUE
        }
        # print(evaluate(remd, detection$event, df$event)$confMatrix)
      }
      
      hard_conf_matrix <- evaluate(model, detection$event, df$event)$confMatrix
      hard_evals[j, ] <- c(model_name, hard_conf_matrix[2], hard_conf_matrix[5],
                           hard_conf_matrix[6], hard_conf_matrix[3])

      # plot(har_plot(model, series, detection, df$event))
      # print(model_name)
      # print(hard_conf_matrix)

      j <- j + 1
    }

    # print(hard_evals)
    result_dfs[[i]] <- append(result_dfs[[i]], list(hard_evals))
  }
}

for (i in 1:length(SERIES_NAMES)) {
  combined_df <- bind_rows(result_dfs[i], .id = "source_df")
  combined_df$tp <- as.numeric(combined_df$tp)
  combined_df$fp <- as.numeric(combined_df$fp)
  combined_df$tn <- as.numeric(combined_df$tn)
  combined_df$fn <- as.numeric(combined_df$fn)
  avg_df <- combined_df %>%
    group_by(model_name) %>%
    summarise(avg_hard_tp = mean(tp),
              avg_hard_fp = mean(fp),
              avg_hard_tn = mean(tn),
              avg_hard_fn = mean(fn))

  print(SERIES_NAMES[i])
  print(avg_df)

  path <- paste0("~/pump-and-dump/results/",
                 EXPERIMENT_NAME, "/")
  if (!file.exists(path)) {
    dir.create(path)
  }

  write.csv(avg_df,
            file = paste0(path, SERIES_NAMES[i], "_", TIMEFRAME, "_",
                          CHUNK_SIZE, "sec.csv"),
            row.names = FALSE)
}
