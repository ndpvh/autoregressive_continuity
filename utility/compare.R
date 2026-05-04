# Function to compare the autoregressive effects across conditions and with the 
# continuous-time estimate. 
#
# Comparisons happen in the following direction:
#   - Interval comparison: Long - Short
#   - CT comparisons: Condition - Expectation
#
# @param data The dataset containing the item for which to do the comparison
# @param paired Logical denoting whether to do a paired t-test or a non-paired 
# one. Paired is needed for the within-subject datasets and unpaired for the
# between-subject ones
compare <- function(dataset, 
                    paired){

  # Compute summary statistics for the autoregressive effects and time intervals
  # for each condition. 
  #
  # Note that we set the negative autocorrelations to NA for this. This is to 
  # ensure the continuous-time predictions follows the real-valued OU model's 
  # predictions, in which negative autoregressive effects cannot happen. 
  ar <- dataset |>
    dplyr::group_by(condition) |>
    dplyr::mutate(
      ar_arima = ifelse(
        ar_arima < 0, 
        NA, 
        ar_arima
      )
    ) |>
    dplyr::summarise(
      "mean" = mean(ar_arima, na.rm = TRUE),
      "sd" = sd(ar_arima, na.rm = TRUE),
      "dt" = mean(time_interval, na.rm = TRUE)
    ) |>
    tidyr::pivot_wider(
      names_from = condition,
      values_from = mean:dt
    )
  
  # Define the exponents to which the autoregressive effect should be raised to 
  # transform from a short time interval to a long one and the other way around.
  # For short -> long, this is defined as:
  #
  #   \Delta t_long / \Delta t_short
  #
  # within the assumed OU model. For long -> short, this is just the inverse.
  exponent <- ar$dt_long_interval / ar$dt_short_interval

  # Compute the continuous-time prediction going from short -> long and from 
  # long -> short using the exponent computed above
  ct_s2l <- ar$mean_short_interval^exponent
  ct_l2s <- ar$mean_long_interval^(exponent^(-1))
  
  # Distinguish between paired comparisons and unpaired comparisons
  if(paired){
    # Transform dataset so that it has two columns containing all of the 
    # estimates
    dataset <-  dataset |>
      tidyr::pivot_wider(
        id_cols = c(id, item), 
        names_from = condition,
        values_from = ar_arima
      )

    # Perform the paired-sample t-test: Differences across conditions
    test_1 <- t.test(
      dataset$long_interval,
      dataset$short_interval, 
      paired = TRUE
    )
    
    # Perform the one-sample t-tests: Difference with continuous time prediction
    test_2 <- t.test(
      dataset$long_interval,
      mu = ct_s2l
    )
    test_3 <- t.test(
      dataset$short_interval,
      mu = ct_l2s
    )

    # Compute effect sizes for these differences
    d <- cohen_d(
      test_1$statistic, 
      nrow(dataset),
      paired = TRUE
    )
    d_s2l <- cohen_d(
      test_2$statistic,
      nrow(dataset), 
      paired = FALSE
    )
    d_l2s <- cohen_d(
      test_3$statistic,
      nrow(dataset), 
      paired = FALSE
    )
    
    # Changed, but kept in for posterity (if we need to make this choice again).
    # In this piece of code, we compare each individual's expected value within
    # the continuous-time framework to their actual values, meaning that we are
    # comparing distributions to distributions. For the between-subject analysis,
    # we do no such thing, therefore leading me to choose a different approach
    # (point-estimates instead of distributions), keeping the analyses the same
    # across datasets.
    #
    # dataset = dataset |>
    #   rowwise() |>
    #   mutate(expected_ar = shortinterval^exponent) |>
    #   ungroup()
    
    # expectedar_comparison = tryCatch(t.test(dataset$longinterval,
    #                                dataset$expected_ar,
    #                                paired = TRUE)$p.value, 
    #                                error = function(e) NA)    
  } else {
    
    # Perform the independent-samples t-test: Differences across conditions
    test_1 <- t.test(
      ar_arima ~ condition,
      data = dataset
    )
    
    # Perform the one-sample t-tests: Difference with continuous time prediction
    test_2 <- t.test(
      dataset$ar_arima[dataset$condition == "long_interval"],
      mu = ct_s2l
    )
    test_3 <- t.test(
      dataset$ar_arima[dataset$condition == "short_interval"],
      mu = ct_l2s
    )

    # Compute effect sizes for these differences
    d <- cohen_d(
      test_1$statistic, 
      c(
        sum(dataset$condition == "long_interval"), 
        sum(dataset$condition == "short_interval")
      ),
      paired = FALSE
    )
    d_s2l <- cohen_d(
      test_2$statistic,
      sum(dataset$condition == "long_interval"), 
      paired = FALSE
    )
    d_l2s <- cohen_d(
      test_3$statistic,
      sum(dataset$condition == "short_interval"), 
      paired = FALSE
    )
  }

  # Create a single-row data.frame with all relevant information
  results <- data.frame(
    item = unique(dataset$item),

    ar_short = ar$mean_short_interval, 
    ar_long = ar$mean_long_interval, 
    ar_s2l = ct_s2l, 
    ar_l2s = ct_l2s,

    dt_short = ar$dt_short_interval,
    dt_long = ar$dt_long_interval,

    condition_t = test_1$statistic,
    condition_df = test_1$parameter,
    condition_p = test_1$p.value,
    condition_q025 = test_1$conf.int[1],
    condition_q975 = test_1$conf.int[2],
    condition_d = d$d, 
    condition_dvar = d$dvar,

    continuous_s2l_t = test_2$statistic,
    continuous_s2l_df = test_2$parameter,
    continuous_s2l_p = test_2$p.value, 
    continuous_s2l_q025 = test_2$conf.int[1],
    continuous_s2l_q975 = test_2$conf.int[2],
    continuous_s2l_d = d_s2l$d, 
    continuous_s2l_dvar = d_s2l$dvar,

    continuous_l2s_t = test_3$statistic,
    continuous_l2s_df = test_3$parameter,
    continuous_l2s_p = test_3$p.value, 
    continuous_l2s_q025 = test_3$conf.int[1],
    continuous_l2s_q975 = test_3$conf.int[2],
    continuous_l2s_d = d_l2s$d, 
    continuous_l2s_dvar = d_l2s$dvar
  )

  return(results)
}