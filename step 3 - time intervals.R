################################################################################
# PURPOSE:
#
# Comparison of the length of the time intervals between the different
# conditions within the datasets. Ideally, these time intervals are significantly
# different so that they are sufficiently apart.
#
# This serves as a sanity check for the results obtained in the other steps.
#
#
# Authors: Niels Vanhasbroeck
################################################################################

# Load the estimates for the different datasets
data <- list(
  "BUCHNER" = readRDS(
    here::here(
      "results", 
      "BUCHNER_estimates.Rds"
    )
  ),
  "DEJONCKHEERE" = readRDS(
    here::here(
      "results", 
      "DEJONCKHEERE_estimates.Rds"
    )
  ),
  "CLOOS" = readRDS(
    here::here(
      "results", 
      "CLOOS_estimates.Rds"
    )
  )
)

# Retain only one row for each participant, select only the relevant columns, 
# and bind all dataset together
data <- lapply(
  seq_along(data), 
  function(i) {
    results <- data[[i]] |>
      # Add name of the dataset to the data.frame
      dplyr::mutate(
        dataset = names(data)[i]
      ) |>

      # Select only the relevant columns
      dplyr::select(
        dataset, 
        id, 
        condition, 
        number_usable_pairs, 
        time_interval
      ) |>

      # Exclude participants with insufficient number of usable pairs. Ensures
      # that all analyses (time interval + autoregressive effects) happen on the
      # same data
      dplyr::group_by(id) |>
      dplyr::mutate(to_keep = min(number_usable_pairs) >= 20) |>
      dplyr::ungroup() |>
      dplyr::filter(to_keep) |>
      dplyr::select(-to_keep) |>

      # Retain only a single row for each participant
      dplyr::group_by(id, condition) |>
      dplyr::mutate(row = dplyr::row_number()) |> 
      dplyr::filter(row == 1) |>
      dplyr::select(-row) |>
      dplyr::ungroup()

    return(results)
  }
) |>
  `names<-` (names(data))

# Perform the different tests of interest and add the results in a data.frame.
# Here, we only focus on the time intervals. Later, we'll concern ourselves with
# the number of usable pairs.
results <- lapply(
  seq_along(data),
  function(i) {
    # Extract the data
    data_i <- data[[i]]

    # Perform the t-test. Note that we need it paired or non-paired depending 
    # on the dataset (interestingly, I am not actually sure about this, given 
    # that time intervals are manipulated, not measured)
    result <- t.test(
      data_i$time_interval[data_i$condition == "long_interval"],
      data_i$time_interval[data_i$condition == "short_interval"],
      paired = !(names(data)[i] == "BUCHNER")
    )
    
    # Put in a data.frame and return
    result <- data_i |>
      dplyr::group_by(condition) |>
      dplyr::summarize(
        mean = mean(time_interval), 
        sd = sd(time_interval),
        min = min(time_interval),
        q025 = quantile(time_interval, 0.025),
        q975 = quantile(time_interval, 0.975),
        max = max(time_interval)
      ) |>
      tidyr::pivot_wider(
        names_from = condition, 
        values_from = mean:max
      ) |>
      dplyr::mutate(
        dataset = names(data)[i],
        t = result$statistic, 
        df = result$parameter, 
        p = result$p.value
      ) |>
      dplyr::relocate(dataset)

    return(result)
  }
)
results <- do.call("rbind", results)
View(results)
