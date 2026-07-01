################################################################################
# PURPOSE:
#
# Estimation of the autoregressive effects for each dataset, person, item, and
# condition. These are used in later steps of the analysis to answer our 
# research questions.
#
#
# Authors: Simran K. Johal, Henri Lo, & Manuel Rein
# Reviewed and adapted by: Niels Vanhasbroeck
################################################################################

library(tidyverse)
library(patchwork)
library(ggpubr)
library(here) 

# Read in the estimation functions
source(here::here("utility", "estimation.R"))

# Read in all datasets. 
data <- list(
  "BUCHNER" = readRDS(
    here::here(
      "data_processed",
      "BUCHNER.Rds"
    )
  ),
  "CLOOS" = readRDS(
    here::here(
      "data_processed",
      "CLOOS.Rds"
    )
  ),
  "DEJONCKHEERE" = readRDS(
    here::here(
      "data_processed",
      "DEJONCKHEERE.Rds"
    )
  )
)

# Run the analysis on the different datasets and save them
for(i in seq_along(data)) {
  # Give some indication of where we are
  print(names(data)[i])

  # Perform the estimation
  results <- data[[i]] |>
    dplyr::group_by(id, condition, item) |>
    dplyr::group_split(.keep = TRUE) |>
    purrr::map_dfr(~ run_ar1_model(.))

  # Save the results
  saveRDS(
    results,
    here::here(
      "results", 
      paste0(
        names(data)[i], 
        "_estimates.Rds"
      )
    )
  )
}
