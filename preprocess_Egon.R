################################################################################
# EGON -- LEONIE
################################################################################
library(tidyverse)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in the dataset
data <- data.table::fread(
    file.path("data", "DEJONCKHEERE.csv"),
    data.table = FALSE
)

# Filter only ESM data, i.e. drop data from the informed consent and debriefing
data <- data %>%
    filter(
        !(questionListName %in%
            c("IC and Baseline Questionnaire (Birte)", "Debriefing"))
    )

# Select only the columns that we are interested in
data <- data %>%
    dplyr::select(
        connectionId,
        scheduledBeepId,
        questionListName, # Questionnaire and condition
        timeStampStart,
        timeStampStop,
        ontspannen_sliderNegPos:angstig_sliderNegPos
    ) %>%
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
    )

# M-path questionnaire names need to be changed (followup in m-path means that in that questionnaire there is a follow up item;
# but this means that in this questionnaire there is an item indicating that the "mood" questionnaire needs to be triggered the next time)
# so Mood is actually the follow up
data <- data %>%
    mutate(
        condition = case_match(
            condition,
            "Mood_followup" ~ "mood",
            "Mood" ~ "followup",
            .default = condition
        )
    )

data %>%
    group_by(id, condition) %>%
    summarise(beeps = n())

# Sort the data: because time_start does not allow us to sort the missed beeps into the right order, we would need to sort by scheduled beep. But this does not wor because the mood and mood_followup
# are not continuous in their scheduled beep ids. So we need to first
data <- data %>%
    arrange(id)

# Data from m-Path is already in the time zone of the participant, probably Europe/Brussels.
# Thus, these timestamps are _not_ UNIX timestamps (as they are in UTC), but rather the number of
# seconds since 1970-01-01 compared to the participant's local time zone. Converting them leaves
# them in UTC, which is not that problematic unless you combine it with other data.
# Transform timestamp to POSIXct

data <- data %>%
    group_by(id) %>%
    mutate(
        time_start = as_datetime(time_start),
        time_stop = as_datetime(time_stop),
        .after = "condition"
    ) %>%
    arrange(id, time_start) %>%
    mutate(across(time_start:time_stop, \(x) {
        force_tz(x, tzone = "Europe/Brussels")
    }))


# Retain only time columns and columns with no missing or empty ("") values
# Note: We need to put `!is.POSIXt(x)` in the `where()` clause, as POSIXt cannot be compared against
# string literals.
data <- data %>%
    dplyr::select(
        id:time_stop,
        where(\(x) !is.POSIXt(x) && any(!is.na(x)) && any(x != ""))
    )

# Create the date, and the total beep duration.
data <- data %>%
    group_by(id) %>%
    mutate(
        date = as_date(time_start),
        onbeep.mins = time_length(time_stop - time_start, "mins"),
        .after = "id"
    )

# Add `day.no` to represent the current day in their study for each `id`
data <- data %>%
    group_by(id) %>%
    nest() %>%
    mutate(data = map(data, ~ group_by(., date))) %>%
    mutate(data = map(data, ~ mutate(., day.no = cur_group_id()))) %>%
    unnest(data) %>%
    ungroup() %>%
    relocate(day.no, .after = date)

# Add the beep number (per day)
data <- data %>%
    group_by(id, day.no) %>%
    mutate(beep.no = seq.int(1, n(), 1)) %>%
    arrange(id, time_start) %>%
    relocate(beep.no, .after = day.no)

# Whether the beep was missed entirely
data <- data %>%
    mutate(missed = is.na(time_start))


# Compute time intervals per person per beep. Importantly, NAs should be
# accounted for as well in consecutive time intervals: They will be deleted
# afterwards
data <- data %>%
    group_by(id, day.no) %>%
    mutate(
        time_interval = time_length(time_stop - lag(time_stop), "mins"),
        .after = time_stop
    )

data %>%
    filter(condition == "mood") %>%
    ggplot2::ggplot(aes(x = time_interval)) +
    ggplot2::geom_histogram(bins = 100)


data %>%
    filter(condition == "followup") %>%
    ggplot2::ggplot(aes(x = time_interval)) +
    ggplot2::geom_histogram(bins = 100) +
    ggplot2::geom_vline(xintercept = 20)

data %>%
    group_by(condition) %>%
    summarise(
        mean_interval = mean(time_interval, na.rm = TRUE),
        median_interval = median(time_interval, na.rm = TRUE),
        sd_interval = sd(time_interval, na.rm = TRUE)
    )

## Do we need a differentiation for follow up and non_followup ?
# Determine the condition based on the time interval
data <- data %>%
    group_by(id, day.no) %>%
    mutate(
        pair = case_when(
            time_interval < 20 ~ "followup_pair",
            time_interval >= 45 & time_interval < 120 ~ "non_followup_pair",
            .default = NA_character_
        ),
        .after = condition
    )


## Finding the pairs
dataset_followups =
    add_nas(
        dat = data,
        id = "id",
        min_diff = 1,
        max_diff = 20,
        time_units = "mins",
        time_var = "time_start"
    )

dataset_beeps =
    add_nas(
        dat = data,
        id = "id",
        min_diff = 45,
        max_diff = 175,
        time_units = "mins",
        time_var = "time_start"
    )

write.csv(
    dataset_followups,
    file = file.path("data_processed", "dataset_short_interval"),
    row.names = FALSE
)

write.csv(
    dataset_beeps,
    file = file.path("data_processed", "dataset_long_interval"),
    row.names = FALSE
)

# To Do: Leo preeprocessing steps
# figuring out intervals,
# proposing how to split the datasets
# decision rules for follow up vs non follow up
