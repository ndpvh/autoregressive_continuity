################################################################################
# PURPOSE:
#
# This script contains the code for reproducing all statistics reported in the 
# manuscript. It draws heavily upon the results obtained through the scripts 
# contained at the top-level.
#
# This script is subdivided in several sections, each of which will contain a
# small description of the purpose of the section and where to find the results
# in the manuscript. Each section is included in chronological order of the 
# manuscript.
#
# Author: Niels Vanhasbroeck
################################################################################

################################################################################
# Demographics
#
# Code used to derive the demographic information of participants from the 
# data. For privacy reasons, however, we cannot include these demographic data
# within the repository. Hence, this code will not work for you unless you issue
# a request for these data to the authors of the original datasets.
#
# For each of the datasets, the pipeline looks the same:
#   1. Read in the demographic data 
#   2. Read in the estimation results 
#   3. Filter out those participants with an insufficient number of datapoints 
#      as determined by the number of useable pairs
#   4. Get the demographics of those participants kept in the data
#
# For the Büchner dataset, we additionally check whether these demographics 
# differ between the long and short interval conditions.  
#
# The results can be found under the section "Datasets" in the manuscript.

##########################################
# BUCHNER

# Read in the demographic data of the BUCHNER dataset
demo <- read.csv(
  here::here(
    "data", 
    "BUCHNER DEMOGRAPHICS.csv"
  )
)

# Change the participant id's in the same way as for the actual data (see the 
# `step 0 - preprocessing` script), ensuring a correct mapping between the two
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

# Check whether the demographics differ between the long and short interval. 
# This is first done for age (using a t-test) and then for gender (using a 
# chi-square test)
t.test(
  demo$age[demo$condition == "A"],
  demo$age[demo$condition == "B"]
)

demo |>
  # For the analysis, only consider those who indicated "female" or "male"
  dplyr::filter(gender %in% c(0, 1)) |>

  # Count up each gender per condition
  dplyr::group_by(condition) |>
  dplyr::summarize(
    female = sum(gender == 0),
    male = sum(gender == 1)
  ) |>
  dplyr::ungroup() |>

  # Rework this data.frame for use in the chisq.test function. As an additional 
  # argument in this function, we include the proportion of each gender across
  # the total sample, checking whether the distribution of gender differs 
  # between the two conditions while accounting for the base rate.
  dplyr::select(female, male) |>
  unlist() |>
  as.numeric() |>
  matrix(nrow = 2, ncol = 2) |> 
  chisq.test(p = c(sum(demo$gender == 0), sum(demo$gender == 1)) / sum(demo$gender %in% c(0, 1)))



##########################################
# DEJONCKHEERE

# Read in the demographic data of the DEJONCKHEERE dataset
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




##########################################
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
# Manipulation check
#
# For each of the datasets, we needed to check whether the observed time intervals
# were indeed different across the two conditions. We did this through either a
# paired or independent t-test, depending on the study's design. 
#
# This analysis is included in full in `step 3 - time intervals.R` and will 
# therefore not be reproduced here. Instead, we source to the original script,
# which makes the user view the results automatically. 
#
# The results can be found under the section "Manipulation check" in the
# manuscript.

source(
  here::here(
    "step 3 - time intervals.R"
  )
)



################################################################################
# Number of usable pairs
#
# We checked how many usable pairs each participant had within each dataset, as
# a retention of consecutive pairs was crucial for our analyses. This pertains
# to just a descriptive analysis, reporting means, standard deviations, but also
# percentage of people that did not meet the criterium of having at least 20 
# usable pairs (needed for stability of the ARIMA estimations).
#
# The results can be found under the section "Preprocessing" in the Methods 
# section of the manuscript.

# Load the estimates. Note that within these estimates, the invalid pairs have 
# already been removed
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

# Define the total number of participants in each dataset 
sample_sizes <- c(
  "BUCHNER" = 1126,
  "DEJONCKHEERE" = 96,
  "CLOOS" = 115
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
          sample_size = length(unique(id)),
          removed = 1 - length(unique(id)) / sample_sizes[i],
        )
    )
  }
)
results <- do.call("rbind", results)
View(results)



################################################################################
# Failed estimations
#
# We found that not all estimations for the AR(1) model were successful, leading
# to missing values in the temporal dependence estimate. These were removed from
# the subsequent analysis. Here, we report descriptive statistics on these 
# exclusions over and above those that were already excluded based on the 
# number of usable pairs.
#
# The results of this analysis can be found in the section "Estimating 
# autoregressive models" in the Methods section

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

# Summarize the results per dataset
results |>  
  dplyr::summarize(
    mean = mean(number), 
    sd = sd(number), 
    min = min(number), 
    max = max(number), 
    .by = dataset
  )



################################################################################
# Negative autoregressive effects
#
# The one-dimensional Ornstein-Uhlenbeck model only allows for exponential 
# regulation towards a baseline while its discrete-time alternative -- the AR(1)
# -- allows for sawtooth-like dynamics as well. While the majority of estimated
# autoregressive effects are positive (as expected by the OU), some were 
# estimated to be negative, a type of dynamic that the OU cannot capture.
#
# We adapted our analyses to account for these negative autoregressive effects 
# (see the manuscript). Here, we report descriptive statistics on their 
# occurrence.
#
# The results of this analysis are not included in the manuscript, but 
# in the Supplementary Materials under section B, subsection "Oscillatory 
# behavior". In the manuscript, we include more information on negative 
# autoregressive effects under "Exponential assumption" in the Methods section.

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
      dplyr::summarize(
        frequency = sum(ar_arima < 0),
        percentage = frequency / length(ar_arima),
        total = length(ar_arima), 
        .by = c(condition, item)
      )

    # Return as is
    return(selected)
  }
) |> 
  `names<-` (names(data))

# Summarize these results and display them
summarized <- lapply(
  seq_along(results),
  function(i) results[[i]] |>
    dplyr::summarize(
      data = names(data)[i],
      mean = mean(percentage),
      sd = sd(percentage),
      min = min(percentage),
      max = max(percentage)
    )
)
do.call("rbind", summarized)

# Perform a t-test to find out whether there is a difference between conditions 
# regarding negative autoregressive effects
for(x in names(data)) {
  # Provide an indicator of the study we're talking about
  cat("\n--------------------------------\n")
  cat("Results for the", x, "study\n\n")

  # Perform the independent-sample or paired-sample t-test
  t.test(
    results[[x]]$percentage[results[[x]]$condition == "short_interval"],
    results[[x]]$percentage[results[[x]]$condition == "long_interval"],
    paired = x != "BUCHNER"
  ) |> 
    print()
}



################################################################################
# Estimates and results for the comparisons
#
# Our results were obtained through three analyses: A comparison of the 
# autoregressive effects across conditions (`condition`), a comparison of the 
# observed autoregressive effects to a prediction from the OU for the long 
# interval condition (`continuous_s2l`), and for the short interval condition 
# (`continuous_l2s`). This bit of code reproduces these analyses and Table 1
# published in the manuscript.
#
# These result can be found under the Results section of the manuscript. Table 
# 1 in the manuscript contains the results for the `condition` and 
# `continuous_s2l` analysis. For the results of `continuous_l2s`, one should
# consulte the Supplementary Materials (C).

# First, a summary of the estimates ############################################

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



# Now, a summary of the comparisons ############################################

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

# Loop over the comparison data and extract some useful results. These are 
# reported in their respective sections under Results
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

# For a full view of the results, you can inspect the contents of the list 
# containing the comparisons. The columns contain the following meaning:
#   - ar_short, ar_long: Average estimate of the autoregressive effect used to 
#     derive the continuous-time predictions. Note that these may differ from the 
#     averages reported in Table 1 and above because negative autoregressive 
#     effects have been removed from this analysis. Note that for all comparison,
#     negative autoregressive effects were retained in the analysis: They were
#     only removed to compute the continuous-time predictions. 
#   - ar_s2l, ar_l2s: Predictions for the long and short interval conditions resp.
#   - dt_<>: Average time interval per condition
#   - <>_t, <>_df, <>_p, <>_q025, <>_q975: Results of the t-tests per comparison,
#     respectively the t-statistic, degrees of freedom, p-value, and 95%CI of the
#     difference in the comparison
#   - <>_d, <>_dvar: Cohen's d and its variability per comparison
View(comparisons$BUCHNER)
View(comparisons$DEJONCKHEERE)
View(comparisons$CLOOS)



################################################################################
# Meta-analysis
#
# To provide a general answer to our research questions, we aggregated our
# item-level results within a dataset to a singular result using a meta-
# analysis. 
#
# Results of this meta-analysis are reported in the article under Results. 
# Additionally, an analysis of the heteroskedasticity is included in the 
# Supplementary Materials (C). Both results are obtained through executing this
# code.
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

# You can inspect the results though this call to View(). The columns mean the 
# following:
#   - estimate, se: Estimate of the meta-analyzed Cohen's d and its standard 
#     error
#   - z_value, p: Value of the test-statistic used in `metafor` and its 
#     associated p-value
#   - q025, q975: 95%CI of the estimate of the meta-analyzed Cohen's d
#   - tau_2, I_2: Measures of heteroskedasticity across items
#   - Q_estimate, Q_df, Q_p: Cochran's Q, its associated degrees of freedom, and
#     the p-value 
View(results)
