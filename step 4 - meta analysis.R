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
# Authors: Simran K. Johal, Henri Lo, & Manuel Rein
# Reviewed and adapted by: Niels Vanhasbroeck
################################################################################

# Load the comparison information for the different datasets. Contains the 
# effect sizes that we are interested in
data <- list(
  "BUCHNER" = readRDS(
    here::here(
      "results", 
      "BUCHNER_comparison.Rds"
    )
  ),
  "DEJONCKHEERE" = readRDS(
    here::here(
      "results", 
      "DEJONCKHEERE_comparison.Rds"
    )
  ),
  "CLOOS" = readRDS(
    here::here(
      "results", 
      "CLOOS_comparison.Rds"
    )
  )
)

# Define the columns containing the information for each of the separate 
# comparisons performed
type <- c(
  "condition",
  "continuous_s2l",
  "continuous_l2s"
)

# Loop over the data and run the meta-analysis
set.seed(345) # Moneen - The Long Count
for(i in seq_along(data)) {
  for(j in seq_along(type)) {
    # Create the columnnames specifying the standard deviation and effect size 
    # for this type of comparison
    cols <- c(
      "item",
      paste(type[j], c("d", "dvar"), sep = "_")
    )

    # Select the data of interest
    data_ij <- data[[i]][, cols] |>
      `colnames<-` (c("item", "yi", "vi"))

    # Run the meta-analysis
    result <- metafor::rma(
      yi = yi, 
      vi = vi,
      data = data_ij, 
      method = "REML"
    )

    # Save this result
    saveRDS(
      result, 
      here::here(
        "results",
        paste0(
          names(data)[i], 
          "_meta-",
          type[j],
          ".Rds"
        )
      )
    )
  }
}

