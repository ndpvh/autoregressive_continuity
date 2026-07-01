################################################################################
# TABLE 1: AR ESTIMATES AND RESULTS OF ANALYSIS
################################################################################

# Load the estimates for the different conditions
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

# Define the number of bootstraps to draw
N <- 10000

# Loop over this list, the items, and the conditions and create summary 
# statistics of interest
set.seed(154) # A Great Fire - Silverstein
results <- lapply(
    seq_along(data), 
    function(i) {
        # Define unique items and unique conditions
        conditions <- expand.grid(
            unique(data[[i]]$item),
            unique(data[[i]]$condition),
            stringsAsFactors = FALSE
        )

        # Loop over each row in this unique mapping
        results <- lapply(
            seq_len(nrow(conditions)),
            function(j) {
                # Select the relevant data and remove NAs. Additionally filter 
                # on the number of useable pairs and on the negative values of 
                # the arima estimates, providing us with the relevant summary 
                # statistics for the interpretation of the t-tests.
                selection <- data[[i]] |>
                    dplyr::filter(
                        item == conditions$Var1[j],
                        condition == conditions$Var2[j]
                    ) |>
                    dplyr::filter(!is.na(ar_arima)) |>

                    # Ensure that only the relevant ones are used, also within
                    # the within-subject designs
                    dplyr::group_by(id) |>
                    dplyr::mutate(to_keep = min(number_usable_pairs) >= 20) |>
                    dplyr::ungroup() |>
                    dplyr::filter(to_keep) |>
                    dplyr::select(-to_keep)

                # Perform a bootstrap on the mean of the AR estimates
                boot <- matrix(
                    sample(
                        selection$ar_arima, 
                        N * nrow(selection),
                        replace = TRUE
                    ),
                    nrow = N, 
                    ncol = nrow(selection)
                ) |>
                    rowMeans()

                # Compute the observed mean and derive the 95%CI
                return(
                    data.frame(
                        data = names(data)[i],
                        item = conditions$Var1[j],
                        condition = conditions$Var2[j],
                        mean = mean(selection$ar_arima),
                        q025 = quantile(boot, 0.025),
                        q975 = quantile(boot, 0.975)
                    )
                )
            }
        )

        return(do.call("rbind", results))
    }
)
results <- do.call("rbind", results)
View(results)

# Load the results of the comparisons 
comparisons <- list(
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

# Loop over the comparison data and extract some useful resuls
for(i in seq_along(comparisons)) {
  cat(names(comparisons)[i], ":\n\n")

  # Number of significant differences per dataset
  cat("Number of significances\n")
  cat("  condition: ")
  cat(sum(comparisons[[i]]$condition_p <= 0.05))
  cat("\n")

  cat("  short to long: ")
  cat(sum(comparisons[[i]]$continuous_s2l_p <= 0.05))
  cat("\n")

  cat("  long to short: ")
  cat(sum(comparisons[[i]]$continuous_l2s_p <= 0.05))
  cat("\n\n")

  # Inspection of the Cohen's d's per dataset
  cat("Cohen's d\n")
  cat("  condition: ")
  cat(range(comparisons[[i]]$condition_d[comparisons[[i]]$condition_p < 0.05]))
  cat("\n")

  cat("  short to long: ")
  cat(range(comparisons[[i]]$continuous_s2l_d[comparisons[[i]]$continuous_s2l_p < 0.05]))
  cat("\n")

  cat("  long to short: ")
  cat(range(comparisons[[i]]$continuous_l2s_d[comparisons[[i]]$continuous_l2s_p < 0.05]))
  cat("\n\n")
}



################################################################################
# DEMOGRAPHICS
################################################################################

####################
# BUCHNER

# For BUCHNER. Note that these data are not available online and only upon 
# request. Hence, the code here will not work.
demo <- read.csv(
  here::here(
    "data", 
    "BUCHNER DEMOGRAPHICS.csv"
  )
)

# Change the participant id's in the same way as for the actual data, ensuring 
# a correct mapping between the two
demo <- demo |>
  dplyr::mutate(
    session = session |>
      as.factor() |>
      as.numeric()
  )

# Go to the estimation results and filter out those with an insufficient number 
# of datapoints
data <- readRDS(
  here::here(
    "results",
    "BUCHNER_estimates.Rds"
  )
) |>
  dplyr::filter(number_usable_pairs >= 20)

# Retain only the demographics of those participants kept in the data and check
# them
demo <- demo |>
  dplyr::filter(session %in% data$id)

sum(demo$gender == 0) / nrow(demo) # Female
sum(demo$gender == 1) / nrow(demo) # Male

mean(demo$age)
sd(demo$age)

t.test(
  demo$age[demo$condition == "A"],
  demo$age[demo$condition == "B"]
)

demo |>
  dplyr::filter(gender %in% c(0, 1)) |>
  dplyr::group_by(condition) |>
  dplyr::summarize(
    female = sum(gender == 0),
    male = sum(gender == 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(female, male) |>
  unlist() |>
  as.numeric() |>
  matrix(nrow = 2, ncol = 2) |> 
  chisq.test(p = c(sum(demo$gender == 0), sum(demo$gender == 1)) / sum(demo$gender %in% c(0, 1)))



####################
# DEJONCKHEERE

# Note that these data are not available online and only upon request. Hence, 
# the code here will not work.
demo <- read.csv(
  here::here(
    "data", 
    "DEJONCKHEERE DEMOGRAPHICS.csv"
  ),
  sep = ";",
  header = FALSE
)[-c(1:3), ] |>
  `colnames<-` (c("date", "id", "gender", "age", "compliance")) |>
  dplyr::filter(compliance != "")

# Unfortunately, the id's in the demographic file do not match those of the 
# raw data. Hence, we need to figure out who is who in both files. We will do 
# this based on the dates indicated in the demographic file and the dates of 
# the informed consent provided in the raw datafile.
#
# Read in the raw data and retain only those rows in which the informed consent
# columns have values
data <- read.csv(
  here::here(
    "data", 
    "DEJONCKHEERE.csv"
  ),
  sep = ";"
) |>
  dplyr::filter(
    grepl("IC", questionListName, fixed = TRUE),
    !is.na(consent_5_yesno)
  ) |>
  dplyr::select(
    connectionId,
    timeStampStart
  ) |>
  `colnames<-` (c("id", "time_start"))

# Transform the time stamp to an actual date in the raw dataset and delete a 
# double row for remaining participants
data <- data |>
  dplyr::mutate(
    time_start = lubridate::as_datetime(time_start)
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarize(
    time_start = time_start[1]
  ) |>
  dplyr::ungroup() 

# Assume that putting the ids in ascending order in both datasets matches the 
# id's across datasets. The dates match up between both files, but within dates
# we don't have sufficient information within the demographic dataset to make 
# this inference (outside of common sense)
demo$id <- data$id

# Read in the DEJONCKHEERE results and filter based on the 
data <- readRDS(
  here::here(
    "results", 
    "DEJONCKHEERE_estimates.Rds"
  )
) |>
  dplyr::group_by(id) |>
  dplyr::mutate(to_keep = min(number_usable_pairs) >= 20) |>
  dplyr::ungroup() |>
  dplyr::filter(to_keep) 

# Show the demographics of those participants kept in the data
demo <- demo |>
  dplyr::filter(id %in% data$id) |> 
  dplyr::mutate(age = as.numeric(age))

table(demo$gender) / nrow(demo)

mean(demo$age)
sd(demo$age)




####################
# CLOOS

# Note that these data are not available online and only upon request. Hence, 
# the code here will not work.
demo <- read.csv(
  here::here(
    "data", 
    "CLOOS DEMOGRAPHICS.csv"
  ),
  sep = ";"
) |>
  dplyr::select(
    id,
    UserLanguage:Nationality
  ) |>
  `colnames<-` (c("id", "language", "age", "birthday", "gender", "nationality"))

# Read in the CLOOS results and filter based on the number of usable pairs
data <- readRDS(
  here::here(
    "results", 
    "CLOOS_estimates.Rds"
  )
) |>
  dplyr::group_by(id) |>
  dplyr::mutate(to_keep = min(number_usable_pairs) >= 20) |>
  dplyr::ungroup() |>
  dplyr::filter(to_keep) 

# Rework the ids of the data so that they match up with those of the demographic
# dataset
data <- data |>
  dplyr::mutate(id = as.numeric(id))

# Show the demographics of those participants kept in the data
demo <- demo |>
  dplyr::filter(id %in% data$id) |> 
  dplyr::mutate(age = as.numeric(age))

table(demo$gender) / nrow(demo)

mean(demo$age)
sd(demo$age)



################################################################################
# USABLE PAIRS
################################################################################

# Load the estimates
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

# Get out the usable pairs
results <- lapply(
  seq_along(data),
  function(i) {
    return(
      data[[i]] |>
        dplyr::group_by(id, condition) |>
        dplyr::filter(dplyr::row_number() == 1) |>
        dplyr::ungroup() |>
        dplyr::mutate(dataset = names(data)[i]) |>
        dplyr::group_by(id) |>
        dplyr::mutate(kept = min(number_usable_pairs) >= 20) |>
        dplyr::ungroup() |>
        dplyr::summarize(
          dataset = dataset[i],
          mean = mean(number_usable_pairs),
          sd = sd(number_usable_pairs),
          min = min(number_usable_pairs),
          max = max(number_usable_pairs),
          retained = 100 * sum(kept) / length(kept)
        )
    )
  }
)
results <- do.call("rbind", results)
View(results)



################################################################################
# FAILED ESTIMATIONS
################################################################################

# Load the estimates
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

# Get out the number of NA estimations
results <- lapply(
  seq_along(data),
  function(i) {
    return(
      data[[i]] |>
        dplyr::group_by(id) |>
        dplyr::mutate(kept = min(number_usable_pairs) >= 20) |>
        dplyr::ungroup() |>
        dplyr::group_by(item) |>
        dplyr::summarize(
          number = sum(is.na(ar_arima)),
          percentage = number / length(ar_arima)
        ) |>
        dplyr::mutate(dataset = names(data)[i])
    )
  }
)
results <- do.call("rbind", results)
View(results)



################################################################################
# META-ANALYSIS
################################################################################

# Load the estimates of the meta-analyses for the three different conditions
type <- c("condition", "continuous_s2l", "continuous_l2s")
dataset <- c("BUCHNER", "DEJONCKHEERE", "CLOOS")

data <- lapply(
  type, 
  function(x) lapply(
    dataset, 
    function(y) readRDS(
      here::here(
        "results", 
        paste0(y, "_meta-", x, ".Rds")
      )
    )
  ) |>
    `names<-` (dataset)
) |>
  `names<-` (type)

# Loop over all these lists and retain those results that are of interest to 
# us
results <- lapply(
  seq_along(data),
  function(i) {
    # Loop over the different datasets
    results <- lapply(
      seq_along(data[[i]]),
      function(j) {
        # Extract the current result
        results <- data[[i]][[j]]

        # Create a single row in a data.frame
        return(
          data.frame(
            dataset = names(data[[i]])[j],
            condition = names(data)[i],
            estimate = results$beta[1],
            se = results$se[1],
            z_value = results$zval[1],
            p = results$pval[1],
            q025 = results$ci.lb[1],
            q975 = results$ci.ub[1], 

            tau_2 = results$tau2,
            I_2 = results$I2, 
            Q_estimate = results$QE, 
            Q_df = results$k - 1,
            Q_p = results$QEp
          )
        )
      }
    )

    # Bind together and return
    return(do.call("rbind", results))
  }
)
results <- do.call("rbind", results)
View(results)



################################################################################
# NEGATIVE AUTOCORRELATION
################################################################################

# Load all estimates
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

# Loop over all datasets and identify the number of negative autocorrelations 
# per item per condition
results <- lapply(
  seq_along(data),
  function(i) {
    # Only retain those participants who have more than 20 usable pairs. 
    # Furthermore delete missing values
    selected <- data[[i]] |>
      dplyr::group_by(id) |>
      dplyr::mutate(to_keep = min(number_usable_pairs) >= 20) |>
      dplyr::ungroup() |>
      dplyr::filter(to_keep) |>
      dplyr::filter(!is.na(ar_arima))

    # Summarize number of negative autocorrelations for each dataset separately
    # and add a percentage to this
    selected <- selected |>
      dplyr::group_by(condition, item) |>
      dplyr::summarize(
        frequency = sum(ar_arima < 0),
        percentage = frequency / length(ar_arima),
        total = length(ar_arima)
      )

    # Return as is
    return(selected)
  }
)

# Summarize these results and display them
summarized <- lapply(
  seq_along(results),
  function(i) results[[i]] |>
    dplyr::ungroup() |>
    dplyr::summarize(
      data = names(data)[i],
      mean = mean(percentage),
      sd = sd(percentage),
      min = min(percentage),
      max = max(percentage)
    )
)
summarized <- do.call("rbind", summarized)

# Perform a t-test to find out whether there is a difference between conditions 
# regarding negative autoregressive effects
t.test(
  results[[1]]$percentage[results[[1]]$condition == "short_interval"],
  results[[1]]$percentage[results[[1]]$condition == "long_interval"]
)
t.test(
  results[[2]]$percentage[results[[2]]$condition == "short_interval"],
  results[[2]]$percentage[results[[2]]$condition == "long_interval"],
  paired = TRUE
)
t.test(
  results[[3]]$percentage[results[[3]]$condition == "short_interval"],
  results[[3]]$percentage[results[[3]]$condition == "long_interval"],
  paired = TRUE
)
