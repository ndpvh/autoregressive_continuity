# Function that will run the lag-1 autoregressive model for a particular dataset,
# person, and item
#
# @param data The dataset for a particular person within a particular condition
# @param id Character denoting the column that contains the id of the participant
# @param item Character denoting the column in which the items are contained
run_ar1_model <- function(data, 
                          id = "id", 
                          item = "item") {

  # Check how many responses are used for the estimation
  idx <- !is.na(data$response) & !is.na(data$response_lagged)
  number_usable <- sum(idx)

  # Compute the average time interval for these data. Used to derive the 
  # continuous-time predictions
  time_interval <- mean(
    data$time_interval[idx], 
    na.rm = TRUE
  ) 

  # Compute the AR(1) using least-squares estimation
  ar1_model_lm = lm(
    response ~ response_lagged,
    data = data
  ) |>
    try()

  # Compute the AR(1) using the ARIMA function. Is less error-prone
  ar1_model_arima = arima(
    data$response, 
    order = c(1, 0, 0)
  ) |>
    try()

  # Check whether the model failed to converge, and if so, set the autoregressive
  # effect to NA
  ar1_lm_parameter <- dplyr::if_else(
    class(ar1_model_lm) != "try-error",
    coef(ar1_model_lm)[2],
    NA
  )
  ar1_arima_parameter <- dplyr::if_else(
    class(ar1_model_arima) != "try-error",
    coef(ar1_model_arima)[1],
    NA
  )

  ar1_lm_parameter <- dplyr::if_else(
    is.na(ar1_lm_parameter) | abs(ar1_lm_parameter) >= 1, 
    NA, 
    ar1_lm_parameter
  )
  ar1_arima_parameter <- dplyr::if_else(
    is.na(ar1_arima_parameter) | abs(ar1_arima_parameter) >= 1, 
    NA, 
    ar1_arima_parameter
  )

  # Create a data.frame containing the results and return
  results_df = data.frame(
    "id" = unique(data[, idcol]),
    "item" = unique(data[, itemcol]),
    "condition" = unique(data$condition),
    "ar_lm" = ar1_lm_parameter,
    "ar_arima" = ar1_arima_parameter,
    "number_usable_pairs" = number_usable,
    "time_interval" = time_interval
  )

  return(results_df)
}