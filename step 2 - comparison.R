################################################################################
# PURPOSE:
#
# Comparisons central to answering the research questions, them being:
#   - Comparing the autoregressive effects across conditions
#   - Comparing the autoregressive effects of the "long_interval" condition to
#     a continuous-time estimate derived from the autoregressive effects of the 
#     "short_interval" condition
#
# All analyses are performed per dataset and per item. We used (paired) t-tests
# for our purposes.
#
#
# Authors: Simran K. Johal, Henri Lo, & Manuel Rein
# Reviewed and adapted by: Niels Vanhasbroeck
################################################################################

library(tidyverse)
library(here)

# Read in the comparison function
source(
  here::here(
    "utility",
    "compare.R"
  )
)

# Read in all results 
data <- list(
  "BUCHNER" = readRDS(
    here::here(
      "results",
      "BUCHNER_estimates.Rds"
    )
  ),
  "CLOOS" = readRDS(
    here::here(
      "results",
      "CLOOS_estimates.Rds"
    )
  ),
  "DEJONCKHEERE" = readRDS(
    here::here(
      "results",
      "DEJONCKHEERE_estimates.Rds"
    )
  )
)

# Loop over all datasets to perform the comparisons
for(i in seq_along(data)) {
  # Peform comparison and filter on the number of useable pairs. Save the results
  # in an Rds file
  results <- data[[i]] |>
    dplyr::filter(number_usable_pairs >= 20) |>
    dplyr::group_by(item) |>
    dplyr::group_split(.keep = TRUE) |>
    purrr::map_dfr(
      ~ compare(
        ., 
        paired = !(names(data)[i] == "BUCHNER")
      )
    )

  # Save the results
  saveRDS(
    results, 
    here::here(
      "results",
      paste0(
        names(data)[i],
        "_comparison.Rds"
      )
    )
  )
}
