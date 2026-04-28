################################################################################
# PURPOSE:
#
# Preprocessing of the various raw datafiles in the folder `data/` to datasets
# containing the useable pairs for the estimation routine per person, per item,
# per condition. The results are saved in the folder `data_processed/` under the
# same names.
#
# Note that the data will only contain the information necessary for the AR(1)
# estimation. Demographic information will not be available in the preprocessed
# data.
#
#
# Authors: Leonie Cloos & Peer-Ole Jacobsen
# Reviewed and adapted by: Niels Vanhasbroeck
################################################################################

library(tidyverse)
library(here)
source(
  here(
    "utility",
    "add_nas.R"
  )
)

# Define the minimal number of useable pairs needed for the estimation
min_pairs <- 20

# Define the unit in which the time intervals should be computed
time_unit <- "mins"



################################################################################
# BUCHNER

# Load the ESM data and the demographics
data <- readr::read_csv(
  here::here(
    "data", 
    "BUCHNER.csv"
  )
)

# Distinguish between the long and short interval conditions and add NAs to 
# those values that are deemed to fall outside of the intended intervals 
# (e.g., due to technical difficulties). We take intervals of 2 to 4 hours
# (long) or 1 to 2 hours (short) with an additional 10min for participants to 
# complete the beep as the cutoff values.
data_a <- data |> 
  dplyr::filter(condition == "A") |> 
  add_nas(
    id = "session",
    min_diff = 60, 
    max_diff = 130,
    time_units = time_unit, 
    time_var = "ended", 
    alter_time_var = "created"
  )

data_b <- data |> 
  dplyr::filter(condition == "B") |> 
  add_nas(
    id = "session",
    min_diff = 120, 
    max_diff = 250,
    time_units = time_unit, 
    time_var = "ended", 
    alter_time_var = "created"
  )

# Merge the datasets, only keep those variables of interest, rename the 
# conditions to be more informative (and in line with the other datasets), and
# convert the participant ids to numbers
data <- dplyr::bind_rows(data_a, data_b) |>
  dplyr::select(
    id,
    time, 
    delta,
    condition, 
    soci:stres
  ) |>
  dplyr::mutate(
    condition = ifelse(
      condition == "A",
      "short_interval",
      "long_interval"
    ),
    id = id |>
      as.factor() |>
      as.numeric()
  ) |>
  dplyr::rename(time_interval = delta)

# Delete all participants that do not have a sufficient number of valid pairs 
# that can be made with the data.
data <- data |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    n_valid_pairs = sum(!is.na(time_interval))
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(n_valid_pairs >= min_pairs) |> 
  dplyr::select(-n_valid_pairs)

# Create lagged data to be used for the estimation.
data <- data |>
  dplyr::group_by(id) |>
  tidyr::nest() |>
  dplyr::mutate(
    data = data |>
      as.data.frame() |>
      dplyr::arrange(time) |>
      dplyr::mutate(
        dplyr::across(
          soci:stres, 
          dplyr::lag,
          .names = "{.col}_lag"
        )
      ) |>
      list()
  ) |>
  tidyr::unnest(data)

# Put in long format with the response and the lagged response next to each 
# other.
lagged <- data |>
  dplyr::select(
    id:condition, 
    soci_lag:stres_lag
  ) |>
  tidyr::pivot_longer(
    soci_lag:stres_lag, 
    names_to = "item", 
    values_to = "response_lag"
  ) |>
  dplyr::mutate(
    item = stringr::str_replace(
      item, 
      "_lag", 
      ""
    )
  )

data <- data |>
  dplyr::select(id:stres) |>
  tidyr::pivot_longer(
    soci:stres, 
    names_to = "item", 
    values_to = "response"
  ) |>
  dplyr::full_join(
    lagged, 
    by = c("id", "time", "time_interval", "condition", "item")
  )

# Save the processed data in the corresponding folder
saveRDS(
  data, 
  here("data_processed", "BUCHNER.Rds")
)



################################################################################
# DEJONCKHEERE

# Read in the dataset
data <- readr::read_delim(
  here(
    "data", 
    "DEJONCKHEERE.csv"
  ),
  delim = ";"
)

# Filter only ESM data (i.e. drop data from the informed consent and debriefing)
# and rename the columns that we are interested in
data <- data |>
  dplyr::filter(grepl("Mood", questionListName, fixed = TRUE)) |>
  dplyr::rename(
    id = connectionId,
    condition = questionListName,
    scheduledBeep = scheduledBeepId,
    time_start = timeStampStart,
    time_stop = timeStampStop,
    relaxed = ontspannen_sliderNegPos,
    frustrated = gefrustreerd_sliderNegPos,
    sad = droevig_sliderNegPos,
    at_ease = rustig_sliderNegPos,
    stressed = gestresseerd_sliderNegPos,
    enthusiastic = enthousiast_sliderNegPos,
    bored = verveeld_sliderNegPos,
    somber = neerslachtig_sliderNegPos,
    happy = blij_sliderNegPos,
    elated = opgewekt_sliderNegPos,
    content = tevreden_sliderNegPos,
    anxious = angstig_sliderNegPos
  ) |>
  dplyr::select(
    id:time_stop, 
    relaxed:anxious
  )

# M-path questionnaire names need to be changed. Followup in m-path means that 
# within that questionnaire, there is a follow up item, so that within this 
# questionnaire there is an item indicating that the "mood" questionnaire needs 
# to be triggered the next time. In other words, "Mood" is actually the follow 
# up
data <- data |>
  dplyr::mutate(
    condition = dplyr::case_match(
      condition,
      "Mood_followup" ~ "mood",
      "Mood" ~ "followup",
      .default = condition
    )
  )

# Sort the data and convert the times to timestamps. Several problems arose: 
#   - Missed beeps dont have a timestamp. So time_start does not allow us to sort 
#     the missed beeps into the right order. Sorting the data based on a person's
#     id will add the missed beeps at the bottom of their data
#   - Data from m-Path is already in the time zone of the participant (Brussels), 
#     but are not in UNIX timestamps. Rather, the timestamps represent the 
#     number of seconds since 1970-01-01 compared to the participant's local 
#     time zone. Converting them leaves them in UTC, which is not that 
#     problematic unless you combine it with other data.
data <- data |>
  dplyr::arrange(id) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    time_start = lubridate::as_datetime(time_start),
    time_stop = lubridate::as_datetime(time_stop),
    .after = "condition"
  ) |>
  dplyr::arrange(id, time_start) |> 
  dplyr::mutate(
    dplyr::across(
      time_start:time_stop, 
      function(x) lubridate::force_tz(
        x, 
        tzone = "Europe/Brussels"
      )
    )
  )

# Add the date, total beep duration, day number, beep number (within day), and 
# information on whether a beep was missed
data <- data |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    date = lubridate::as_date(time_start),
    onbeep.mins = lubridate::time_length(
      time_stop - time_start, 
      time_unit
    ),
    .after = "id"
  ) |>
  dplyr::filter(!is.na(date)) |> # filter is NA
  dplyr::group_by(id) |>
  tidyr::nest() |>
  dplyr::mutate(
    data = purrr::map(
      data, 
      ~ dplyr::group_by(
        ., 
        date
      )
    ),
    data = purrr::map(
      data, 
      ~ dplyr::mutate(
        ., 
        day.no = dplyr::cur_group_id()
      )
    )
  ) |>
  tidyr::unnest(data) |>
  dplyr::ungroup() |>
  dplyr::relocate(
    day.no, 
    .after = date
  ) |>
  dplyr::group_by(id, day.no) |>
  dplyr::mutate(beep.no = seq.int(1, n(), 1)) |>
  dplyr::arrange(id, time_start) |>
  dplyr::relocate(beep.no, .after = day.no) |>
  dplyr::mutate(missed = is.na(time_start)) |>
  dplyr::ungroup()

# Compute time intervals
data <- data |>
  dplyr::group_by(id, day.no) |>
  dplyr::arrange(time_stop) |>
  dplyr::mutate(
    time_interval = lubridate::time_length(
      time_stop - lag(time_stop), 
      time_unit
    ),
    .after = time_stop
  ) |>
  dplyr::ungroup()

# Build the dataset containing only the followups, building on the add_na and 
# insert_na_for_mood functions
followup <- data |>
  add_nas(
    id = "id",
    min_diff = 1,
    max_diff = 20,
    time_units = time_unit,
    time_var = "time_stop"
  ) |>
  insert_na_for_mood(min_minutes = 20) |>
  dplyr::rename(
    time_stop = time, 
    time_interval_original = time_interval
  ) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    time_interval = difftime(time_stop, lag(time_stop), units = time_unit) |>
      as.numeric(),
    n_valid_pairs = sum(!is.na(time_interval)), 
    .after = time_interval_original
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(dataset = "shortinterval") |>
  dplyr::select(-row_id, -new_row_needed)

# Build the full dataset, using only the original beeps, preprocessing them, 
# and then combining them with the followup data
data <- data |>
  dplyr::filter(condition == "mood") |>
  add_nas(
    id = "id",
    min_diff = 45,
    max_diff = 175,
    time_units = time_unit,
    time_var = "time_stop"
  ) |>
  dplyr::rename(
    time_stop = time, 
    time_interval_original = time_interval
  ) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    time_interval = difftime(time_stop, lag(time_stop), units = time_unit) |>
      as.numeric(),
    n_valid_pairs = sum(!is.na(time_interval)), 
    .after = time_interval_original
  ) |>
  dplyr::ungroup() |> 
  dplyr::mutate(dataset = "longinterval") |>
  dplyr::select(-row_id, -new_row_needed) |>
  dplyr::bind_rows(followup)

# Filter out participants who do not have the prespecified number of pairs in 
# every condition. 
data <- data |>
  dplyr::filter(n_valid_pairs >= min_pairs) |>
  dplyr::select(
    id, 
    time_stop,
    time_interval, 
    relaxed:anxious, 
    dataset
  ) |>
  dplyr::rename(time = time_stop)

# Create lagged data to be used for the estimation.
data <- data |>
  dplyr::group_by(id, dataset) |>
  tidyr::nest() |>
  dplyr::mutate(
    data = data |>
      as.data.frame() |>
      dplyr::arrange(time) |>
      dplyr::mutate(
        dplyr::across(
          relaxed:anxious, 
          dplyr::lag,
          .names = "{.col}_lag"
        )
      ) |>
      list()
  ) |>
  tidyr::unnest(data)

# Put in long format with the response and the lagged response next to each 
# other.
lagged <- data |>
  dplyr::select(
    id:time_interval, 
    relaxed_lag:anxious_lag
  ) |>
  tidyr::pivot_longer(
    relaxed_lag:anxious_lag, 
    names_to = "item", 
    values_to = "response_lag"
  ) |>
  dplyr::mutate(
    item = stringr::str_replace(
      item, 
      "_lag", 
      ""
    )
  )

data <- data |>
  dplyr::select(
    id:time_interval, 
    relaxed:anxious
  ) |>
  tidyr::pivot_longer(
    relaxed:anxious, 
    names_to = "item", 
    values_to = "response"
  ) |>
  dplyr::full_join(
    lagged, 
    by = c("id", "time", "time_interval", "dataset", "item")
  ) |>
  dplyr::rename(condition = dataset)

# Save the processed data in the corresponding folder
saveRDS(
  data, 
  here("data_processed", "DEJONCKHEERE.Rds")
)



################################################################################
# CLOOS

# Read in the dataset
data <- readr::read_csv(
  here(
    "data", 
    "CLOOS.csv"
  )
)[, -1]

# Filter out those results that come from the continuous measurement scale used
# in the study through the "missed" variable, select only the relevant columns,
# and ensure POSIXct format for the time variable
data <- data |>
  dplyr::filter(!is.na(missed)) |>
  dplyr::select(
    id:missed, 
    beep_pa:burst_na
  ) |>
  dplyr::mutate(time = lubridate::ymd_hms(time))

# Reshape data to long format with PA and NA measurements of beeps and bursts 
# taken together in their own respective columns, distinguishing between the two
# kinds of responses in a condition column. Assign measurement count (within 
# condition), exclude beeps that are faulty (i.e., answered in the wrong 
# direction; e.g., frist beep 6 than beep 5), and only keep first timewise beep
# to disambiguate
data <- data |>
  dplyr::mutate(
    condition = dplyr::if_else(
      is.na(beep.no),
      "burst",
      "beep"
    ),
    na = dplyr::if_else(
      condition == "beep",
      beep_na,
      burst_na
    ),
    pa = dplyr::if_else(
      condition == "beep",
      beep_pa,
      burst_pa
    )
  ) |>
  dplyr::arrange(id, time) |>
  dplyr::group_by(id, date, condition) |>
  dplyr::mutate(
    meas_no = time |>
      as.factor() |>
      as.integer(),
    test = beep.no - meas_no
  ) |> 
  dplyr::filter(test != -1 | is.na(test)) |>
  dplyr::mutate(
    meas_no = time |> 
      as.factor() |>
      as.integer(),
  ) |>
  dplyr::select(
    id:time,
    condition,
    meas_no,
    missed,
    na,
    pa
  ) |>
  dplyr::arrange(id, condition, time)

# Put lagged values of PA/NA and current PA/NA within same row, compute the time 
# intervals and filter on them with a fixed range, letting beeps fall within
# 120 - 160 min and bursts into 0 - 40 min
data <- data |>
  dplyr::group_by(id) |>
  tidyr::nest() |>
  dplyr::mutate(
    data = data |>
      as.data.frame() |>
      dplyr::arrange(time) |>
      dplyr::mutate(
        dplyr::across(
          na:pa, 
          dplyr::lag,
          .names = "{.col}_lag"
        )
      ) |>
      dplyr::mutate(
        time_lag = dplyr::lag(time),
        time_interval = difftime(time, time_lag, units = time_unit) |>
          as.numeric()
      ) |>
      dplyr::select(-time_lag) |>
      list()
  ) |>
  tidyr::unnest(data)

# Filter the data according to missed beeps and the previously defined time 
# intervals
data <- data |>
  dplyr::filter(
    !is.na(pa), 
    !is.na(pa_lag),
    !is.na(time_interval)
  ) |>
  dplyr::filter(
    dplyr::if_else(
      condition == "beep", 
      time_interval >= 120 & time_interval <= 160, 
      time_interval <= 40
    )
  )

# Delete all participants that do not have a sufficient number of valid pairs 
# that can be made with the data.
data <- data |>
  dplyr::group_by(id, condition) |>
  dplyr::mutate(
    n_valid_pairs = sum(!missed)
  ) |>
  dplyr::ungroup() |> 
  dplyr::filter(n_valid_pairs >= min_pairs) |> 
  dplyr::select(-n_valid_pairs, -missed) 

# Put in long format with the response and the lagged response next to each 
# other.
lagged <- data |>
  dplyr::select(
    id,
    time, 
    condition,
    na_lag:pa_lag,
    time_interval
  ) |>
  tidyr::pivot_longer(
    na_lag:pa_lag, 
    names_to = "item", 
    values_to = "response_lag"
  ) |>
  dplyr::mutate(
    item = stringr::str_replace(
      item, 
      "_lag", 
      ""
    )
  )

data <- data |>
  dplyr::select(
    id, 
    time:meas_no, 
    na:pa, 
    time_interval
  ) |>
  tidyr::pivot_longer(
    na:pa, 
    names_to = "item", 
    values_to = "response"
  ) |>
  dplyr::full_join(
    lagged, 
    by = c("id", "time", "time_interval", "condition", "item")
  )


# Save the processed data in the corresponding folder
saveRDS(
  data, 
  here("data_processed", "CLOOS.Rds")
)

