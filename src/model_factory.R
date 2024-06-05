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
    stop(paste("Model not found:", model_name))
  }
}
