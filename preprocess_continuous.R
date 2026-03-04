################################################################################
# CONTINUOUS -- LEONIE
################################################################################

# Set Up ####

library(tidyverse)
library(zoo)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the dataset
data <- read.csv(
  file.path("data", "data drawing.csv"),
  row.names = 1
)

data <- data %>%
  filter(!is.na(missed))

data <- data %>%
  select(id:missed, beep_pa:burst_na) %>%
  mutate(time = ymd_hms(time)) # ensure POSIXct

# reshape data to some kind of long format and assign measurement count (within condition)
# exclude beeps that are faulty as being answerd in the wrong direction by participants
# (e.g., frist beep 6 than beep 5) and only keep first (timewise)
data_long <- data %>%
  mutate(
    condition = if_else(
      is.na(beep.no),
      "burst",
      "beep"
    ),
    na = if_else(
      condition == "beep",
      beep_na,
      burst_na
    ),
    pa = if_else(
      condition == "beep",
      beep_pa,
      burst_pa
    )
  ) |>
  arrange(id, time) |>
  group_by(id, date, condition) %>%
  mutate(
    meas_no = as.integer(as.factor(time)),
    test = beep.no - meas_no
  ) |>
  filter(test != -1 | is.na(test)) |>
  mutate(
    meas_no = as.integer(as.factor(time)),
  ) |>
  select(
    id,
    connectionId,
    date,
    time,
    condition,
    meas_no,
    missed,
    na,
    pa
  ) |>
  arrange(id, condition, time)

# minimum no of valid paris per person and condition
min_pairs <- 20

# calculate time intervals ####

# compute lagged values of missing, PA, and NA to identify valid pairs of consecutive prompts
# compute time intervals between valid pairs of consecutive prompts (i.e., both prompts not missed) and store in new variable time_interval
data_long_difftime <- data_long %>%
  group_by(id, date, condition) %>%
  mutate(
    time_lag = lag(time),
    missed_lag = lag(missed),
    pa_lag = lag(pa),
    na_lag = lag(na),
    valid_pair = if_else(
      missed == FALSE & missed_lag == FALSE,
      TRUE,
      FALSE,
      missing = FALSE
    ),
    time_interval = if_else(
      valid_pair,
      as.numeric(difftime(time, time_lag, units = "mins")),
      NA
    )
  ) %>%
  ungroup()

# Limits to time intervals ####

data_valid <- data_long_difftime %>%
  filter(valid_pair)

## Functions ####

### Function to apply outlier detection and filtering ####
apply_limit_rule <- function(data, rule_fn, min_pairs) {
  
  # Calculate bounds once per condition
  bounds_df <- data %>%
    group_by(condition) %>%
    summarise(
      bounds_list = list(rule_fn(time_interval)),
      .groups = "drop"
    ) %>%
    mutate(
      lower_bound = map_dbl(bounds_list, "lower"),
      upper_bound = map_dbl(bounds_list, "upper")
    ) %>%
    select(condition, lower_bound, upper_bound)


  # Apply bounds and filter
  data %>%
    left_join(bounds_df, by = "condition") %>%
    filter(
      time_interval >= lower_bound & time_interval <= upper_bound
    ) %>%
    group_by(id, condition) %>%
    mutate(n_valid_pairs = n()) |>
    group_by(id) %>%
    filter(all(n_valid_pairs >= min_pairs)) %>%
    ungroup() %>%
    select(-lower_bound, -upper_bound)
  }

# Define limit rules ####
sd_rule <- function(x, multiplier = 2, na.rm = TRUE) {
  mean_val <- mean(x, na.rm = na.rm)
  sd_val <- sd(x, na.rm = na.rm)
  list(
    lower = mean_val - multiplier * sd_val,
    upper = mean_val + multiplier * sd_val
  )
}

iqr_rule <- function(x, multiplier = 1.5, na.rm = TRUE) {
  median_val <- median(x, na.rm = na.rm)
  iqr_val <- IQR(x, na.rm = na.rm)
  list(
    lower = median_val - multiplier * iqr_val,
    upper = median_val + multiplier * iqr_val
  )
}

# Function to generate summary and save outputs ####
summarize_and_save <- function(data, rule_name, figure_dir = "figures") {
  # Create figures directory if it doesn't exist
  if (!dir.exists(figure_dir)) {
    dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Create histogram and save as PNG
  p <- data %>%
    ggplot(aes(x = time_interval, fill = condition)) +
    geom_histogram(
      position = "identity",
      alpha = 0.5,
      bins = 60,
      color = "white"
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      title = paste("Distribution of Time Intervals Between Prompts", rule_name),
      x = "Time interval (minutes)",
      y = "Count",
      fill = "Condition"
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "top"
    )

  ggsave(
    filename = file.path(
      figure_dir,
      paste0("time_intervals_", rule_name, ".png")
    ),
    create.dir = TRUE,
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )

  # Generate summary statistics
  summary_intervals <- data |>
    summarise(
      min_interval = min(time_interval, na.rm = TRUE),
      mean_interval = mean(time_interval, na.rm = TRUE),
      sd_interval = sd(time_interval, na.rm = TRUE),
      max_interval = max(time_interval, na.rm = TRUE),
      .by = condition
    )

  summary_pairs <- data %>%
    select(id, condition, n_valid_pairs) %>%
    distinct() %>%
    group_by(condition) %>%
    summarise(
      min_pairs = min(n_valid_pairs),
      mean_pairs = mean(n_valid_pairs),
      sd_pairs = sd(n_valid_pairs),
      max_pairs = max(n_valid_pairs)
    )

  n_participants <- data |>
    summarise(n_participants = n_distinct(id), .by = condition)

  # Combine all summaries
  result <- n_participants %>%
    left_join(summary_intervals, by = "condition") %>%
    left_join(summary_pairs, by = "condition")

  result
}

## No Limits ####

data_nolim <- data_valid %>%
  group_by(id, condition) %>%
  mutate(n_valid_pairs = n()) |>
  group_by(id) %>%
  filter(all(n_valid_pairs >= min_pairs)) %>%
  ungroup()

summary_nolim <- summarize_and_save(data_nolim, "no_limits")

## fixed limits ###
data_fixlim <- data_valid %>%
  group_by(id, condition) %>%
  filter((condition == "beep" & time_interval >= 80 & time_interval <= 200) | (condition == "burst" & time_interval >= 0 & time_interval <= 40)) %>%
  mutate(n_valid_pairs = n()) |>
  group_by(id) %>%
  filter(all(n_valid_pairs >= min_pairs)) %>%
  ungroup()

summary_fixlim <- summarize_and_save(data_fixlim, "fixed limits")

## 3 * SD criterion ####

data_3sd <- apply_limit_rule(
  data_valid,
  rule_fn = function(x) sd_rule(x, multiplier = 3),
  min_pairs
)

summary_3sd <- summarize_and_save(data_3sd, "3sd")

## 2 * SD criterion ####

data_2sd <- apply_limit_rule(
  data_valid,
  function(x) sd_rule(x, multiplier = 2),
  min_pairs
)

summary_2sd <- summarize_and_save(data_2sd, "2sd")

## Box-plot criterion ####

data_boxpl <- apply_limit_rule(
  data_valid,
  iqr_rule,
  min_pairs
)

summary_boxpl <- summarize_and_save(data_boxpl, "IQR")

overview_summary <- bind_rows(
  summary_nolim,
  summary_fixlim,
  summary_3sd,
  summary_2sd,
  summary_boxpl
) %>%
  mutate(rule = rep(c("no_limits", "fixlim", "3sd", "2sd", "boxplot"), each = 2)) %>%
  select(rule, everything())
