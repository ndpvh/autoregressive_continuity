################################################################################
# EGON -- LEONIE
################################################################################
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the dataset
data <- data.table::fread(
    file.path("data", "DEJONCKHEERE.csv"),
    data.table = FALSE
)

# Filter only ESM data, i.e. drop data from the informed consent and debriefing
data <- data |>
    filter(
        !(questionListName %in%
            c("IC and Baseline Questionnaire (Birte)", "Debriefing"))
    )

# Select only the columns that we are interested in
data <- data |>
    dplyr::select(
        connectionId,
        questionListName, # Questionnaire and condition
        timeStampStart,
        timeStampStop,
        ontspannen_sliderNegPos:angstig_sliderNegPos
    ) |>
    dplyr::rename(
        id = connectionId,
        condition = questionListName,
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
    )


#Sort the data before starting
data <- data |>
    arrange(id, time_start)

# Data from m-Path is already in the time zone of the participant, probably Europe/Brussels.
# Thus, these timestamps are _not_ UNIX timestamps (as they are in UTC), but rather the number of
# seconds since 1970-01-01 compared to the participant's local time zone. Converting them leaves
# them in UTC, which is not that problematic unless you combine it with other data.
# Transform timestamp to POSIXct

data <- data |>
    group_by(id) |>
    mutate(
        time_start = as_datetime(time_start),
        time_stop = as_datetime(time_stop),
        .after = "condition"
    ) |>
    arrange(id, time_start) |>
    # Compute time intervals per person per beep. Importantly, NAs should be
    # accounted for as well in consecutive time intervals: They will be deleted
    # afterwards
    mutate(
        time_interval = time_length(time_start - lag(time_start), "mins"),
        .after = time_stop
    ) |>
    mutate(across(time_start:time_stop, \(x) {
        force_tz(x, tzone = "Europe/Brussels")
    }))


# Retain only time columns and columns with no missing or empty ("") values
# Note: We need to put `!is.POSIXt(x)` in the `where()` clause, as POSIXt cannot be compared against
# string literals.
data <- data |>
    dplyr::select(
        id:time_interval,
        where(\(x) !is.POSIXt(x) && any(!is.na(x)) && any(x != ""))
    )

# Create the date, and the total beep duration.
data <- data |>
    group_by(id) |>
    mutate(
        date = as_date(time_start),
        onbeep.mins = time_length(time_stop - time_start, "mins"),
        .after = "id"
    )

# Add `day.no` to represent the current day in their study for each `id`
data <- data |>
    group_by(id) %>%
    nest() %>%
    mutate(data = map(data, ~ group_by(., date))) %>%
    mutate(data = map(data, ~ mutate(., day.no = cur_group_id()))) %>%
    unnest(data) %>%
    ungroup()

# Add the beep number (per day)
data <- data |>
    group_by(id, day.no) |>
    mutate(data = seq.int(1, n(), 1)) |>
    unnest(data) |>
    arrange(id, time_start)

# Whether the beep was missed entirely
data <- data |>
    mutate(missed = is.na(time_start))

# To Do:
## Do we need a beep number for follow up and non_followup ?
## Finding the pairs !!

# write.csv(
#     data,
#     file = file.path("data", "preprocessed_dejonckheere.csv"),
#     row.names = FALSE
# )
