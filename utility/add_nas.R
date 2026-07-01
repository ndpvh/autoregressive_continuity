# Function that handles the following stuff for the datasets:
#   - Computation of time intervals (delta)
#   - The imputation of NAs whenever the time intervals fall out of the 
#     prespecified (intended) bounds
# 
# @param dat The dataset
# @param id Character denoting the column that contains the participant id
# @param min_diff, max_diff Numerics defining the expected minimum and maximum 
# bound of the time between beeps
# @param time_units Character denoting the unit of time for the time interval
# @param time_var Character denoting the column containing the time
# @param alter_time_var Character denoting a, potential, alternative time 
# variable that can be used in conjunction with the original time_var. Useful 
# when NAs may occur in one but not the other variable
add_nas <- function(dat, 
                    id, 
                    min_diff, 
                    max_diff,
                    time_units = "mins", 
                    time_var, 
                    alter_time_var = NULL) {
  
  # 1) Standardize names once
  names(dat)[names(dat) == id]       <- "id"
  names(dat)[names(dat) == time_var] <- "time"
  if (!is.null(alter_time_var)) {
    names(dat)[names(dat) == alter_time_var] <- "alter_time"
    dat$time <- if_else(is.na(dat$time), dat$alter_time, dat$time)
  }
  
  # 2) Compute deltas and flags
  dat <- dat %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(
      row_id = row_number(),
      time_lag = lag(time),
      delta = as.numeric(difftime(time, time_lag, units = time_units)),
      new_row_needed = !is.na(delta) & (delta < min_diff | delta > max_diff)
    ) %>%
    ungroup()
  
  # 3) Make all NA rows at once (keep id; blank everything else)
  na_rows <- dat %>%
    filter(new_row_needed) %>%
    mutate(.insert_order = 0) %>%                 # NA goes before original
    mutate(across(-c(id, row_id, .insert_order), ~ NA))
  
  # 4) Originals with order tag
  originals <- dat %>%
    mutate(.insert_order = 1)
  
  # 5) Bind and order so NA is before flagged original
  out <- bind_rows(originals, na_rows) %>%
    arrange(id, row_id, .insert_order) %>%
    select(-.insert_order)
  
  out
}

# Function created as an add-on for add_na, specific to the DEJONCKHEERE dataset.
#
# It handles a specific cross-condition problem that `add_nas` couldn't catch, 
# where `add_nas` is blind to the type of beep that came before another one, 
# which can become problematic in the within-subject designs. Specifically, 
# this function catches the scenario where a mood questionnaire is sent close to
# the followup, consequently being counted as a followup when considering time 
# only. 
#
# Function checks whethter the previous non-NA row a "followup" and whether 
# they were less than a particular time apart, catching the case of interest. 
#
# @param data The dataset
# @param min_minutes Minimum number of minutes needed to define a "beep" instead
# of a "followup"
insert_na_for_mood <- function(data, 
                               min_minutes = 20) {

  result_df <- data

  # Find all mood rows that violate timing. We start from the bottom and go 
  # to the top
  for (i in nrow(result_df):2) {
    # Select the current row
    curr_row <- result_df[i, ]

    # Skip if current row is NA or if it is not a beep
    if (is.na(curr_row$condition)) {
      next
    }
    
    if (curr_row$condition != "mood") {
      next
    }

    # Find previous non-NA row
    prev_idx <- NA
    for (j in (i - 1):1) {
      if (!is.na(result_df$condition[j])) {
        prev_idx <- j
        break
      }
    }

    # Skip if no previous row found
    if (is.na(prev_idx)) {
      next
    }

    prev_row <- result_df[prev_idx, ]

    # Check if previous was "followup"
    if (prev_row$condition != "followup") {
      next
    }

    # Check if time difference < 45 minutes
    minutes_diff <- as.numeric(
      difftime(curr_row$time, prev_row$time, units = "mins")
    )

    if (minutes_diff >= min_minutes) {
      next
    }

    # Create NA row
    na_row <- result_df[i, ]
    na_row[1, ] <- NA
    na_row$id <- result_df$id[i]
    na_row$time_lag <- prev_row$time

    # Insert before current row
    result_df <- bind_rows(
      result_df[1:(i - 1), ],
      na_row,
      result_df[i:nrow(result_df), ]
    )
  }

  # Recalculate row_id
  result_df <- result_df |>
    mutate(row_id = row_number())

  return(result_df)
}


