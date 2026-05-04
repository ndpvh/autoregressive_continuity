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

# Define the limits for the plots. Based on the 99% confidence interval 
# of the estimates, ensuring some space is left (as the 95%CI is plotted)
estimate <- lapply(
  data,
  function(x) x[, paste(type, "d", sep = "_")] |>
    unlist() |>
    as.numeric() 
) |>
  unlist() |>
  as.numeric()
se <- lapply(
  data,
  function(x) x[, paste(type, "dvar", sep = "_")] |>
  unlist() |>
  as.numeric() |>
  sqrt()
) |>
  unlist() |>
  as.numeric()
bounds <- c(
  estimate - 2.576 * se, 
  estimate + 2.567 * se
)
alims <- range(bounds) |>
  round(2)
xlims <- alims + c(-0.5, 0.5) * diff(alims)

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

    # Create forest plots that summarize the meta-analysis
    pdf(
      here::here(
        "figures", 
        paste0(
          "forest_", 
          names(data)[i], 
          "_meta-", 
          type[j], 
          ".pdf"
        )
      ),
      width = 8, 
      height = 5 * nrow(data_ij) / 19 + 1.5
    )

    par(mar = rep(2, 4))

    metafor::forest(
      result, 
      xlab = "Cohen's d",
      slab = data_ij$item, 
      header = "Item",
      alim = alims,
      xlim = xlims,
      refline = ifelse(
        0 > lims[1] & 0 < lims[2],
        0,
        NA
      ),
      width = c(0.1, 0.8, 0.1),
      psize = 1,
      cex = 1
    )

    dev.off()
  }
}

