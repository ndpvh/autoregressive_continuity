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

# Select only the columns that we are interested in
data <- data %>%
    dplyr::select(
        connectionId,
        questionListName, # Questionnaire and condition
        timeStampSent,
        timeStampStart,
        timeStampStop,
        ontspannen_sliderNegPos:angstig_sliderNegPos
    ) %>%
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

# Compute time intervals per person per beep. Importantly, NAs should be
# accounted for as well in consecutive time intervals: They will be deleted
# afterwards
data %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(time_start) %>%
    dplyr::mutate(
        time_interval = time_stop - lag(time_stop),
        .after = time_stop
    ) %>%
    View()

#Sort the data before starting
data <- data |>
    arrange(id, time_start)

# Data from m-Path is already in the time zone of the participant, probably Europe/Brussels.
# Thus, these timestamps are _not_ UNIX timestamps (as they are in UTC), but rather the number of
# seconds since 1970-01-01 compared to the participant's local time zone. Converting them leaves
# them in UTC, which is not that problematic unless you combine it with other data.
# Transform timestamp to POSIXct
data <- data |>
    mutate(
        time_start = as_datetime(time_start),
        time_stop = as_datetime(time_stop),
        .after = "condition"
    ) |>
    mutate(across(time_start:time_stop, \(x) {
        force_tz(x, tzone = "Europe/Brussels")
    }))


# Filter only ESM data, i.e. drop data from the informed consent and debriefing
# Retain only time columns and columns with no missing or empty ("") values
# Note: We need to put `!is.POSIXt(x)` in the `where()` clause, as POSIXt cannot be compared against
# string literals.
data <- data |>
    filter(
        !(condition %in%
            c("IC and Baseline Questionnaire (Birte)", "Debriefing"))
    ) |>
    dplyr::select(
        id,
        time_start,
        time_stop,
        where(\(x) !is.POSIXt(x) && any(!is.na(x)) && any(x != ""))
    )

#test
