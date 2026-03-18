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
    # dplyr::select(
    #     connectionId,
    #     scheduledBeepId,
    #     questionListName, # Questionnaire and condition
    #     timeStampStart,
    #     timeStampStop,
    #     ontspannen_sliderNegPos:angstig_sliderNegPos
    #) %>%
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

# M-path questionnaire names need to be changed (followup in m-path means that withinin that questionnaire there is a follow up item;
# this means that in this questionnaire there is an item indicating that the "mood" questionnaire needs to be triggered the next time)
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

# Sort the data: Problem that msised beeps dont ahve a timestamp sent and timestamp start is of course empty. So time_start does not allow us to sort the missed beeps into the right order, they are actually useless.
# The missed beeps will just be at bottom of each persons data

data <- data %>%
    arrange(id)

# Data from m-Path is already in the time zone of the participant, Europe/Brussels.
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
    ) %>%
    select(-timeStampSent)

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
    filter(!is.na(date)) %>% # filter is NA
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

# ---- Inspect study duration ----
# Note: some participants have 8-9 days despite planned 7, reason unknown
freqtable = data %>%
    group_by(id) %>%
    summarise(
        total_beeps = n(),
        days = max(day.no)
    )

table(freqtable$days)


# Compute time intervals per person per beep.
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

# =============================================================================
# INSERT NA BREAKS
# NA rows are inserted to prevent lag time from being computed across invalid
# gaps. When lag() hits an NA row it returns NA for the next row, breaking
# the pair.
# 
# Goes through all rows and inserts an NA row when the time gap between two consecutive beeps is either less than 1 minute (too fast, probably a duplicate) or more than 20 minutes
# (too long, the pair is too far apart to be meaningful). The NA row acts as a break — it prevents a lag time from being computed across that gap later.
# =============================================================================

add_nas <- function(
    dat,
    id,
    min_diff,
    max_diff,
    time_units = "mins",
    time_var,
    alter_time_var = NULL
) {
    # 1) Standardize names once
    names(dat)[names(dat) == id] <- "id"
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
            new_row_needed = !is.na(delta) &
                (delta < min_diff | delta > max_diff)
        ) %>%
        ungroup()

    # 3) Make all NA rows at once (keep id; blank everything else)
    na_rows <- dat %>%
        filter(new_row_needed) %>%
        mutate(.insert_order = 0) %>% # NA goes before original
        mutate(across(-c(id, row_id, .insert_order), ~NA))

    # 4) Originals with order tag
    originals <- dat %>%
        mutate(.insert_order = 1)

    # 5) Bind and order so NA is before flagged original
    out <- bind_rows(originals, na_rows) %>%
        arrange(id, row_id, .insert_order) %>%
        select(-.insert_order)

    out
}




# =============================================================================
# This handles a specific cross-condition problem that `add_nas` couldn't catch because `add_nas` is blind to what *type* of beep came before. The scenario it catches is:
# a mood quesitonnaire being too close to a followup and being counted as followup condition.
# Function checks: was the previous non-NA row a "followup", and were they less than 20 minutes apart / this is what the precious add_na function would be missing as we defined the interval 1-20
# =============================================================================

insert_na_for_mood <- function(df, min_minutes = 20) {
    result_df <- df

    # Find all mood rows that violate timing
    for (i in nrow(result_df):2) {
        # Start from bottom, go backwards

        curr_row <- result_df[i, ]

        # Skip if current row is NA
        if (is.na(curr_row$condition)) {
            next
        }

        # Skip if current is not "mood"
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
    result_df <- result_df %>%
        mutate(row_id = row_number())

    return(result_df)
}

# ---- Build dataset_followups ----
# ---- dataset_followups: all beeps, short-interval breaks ----
dataset_followups = add_nas(
    dat = data,
    id = "id",
    min_diff = 1,
    max_diff = 20,
    time_units = "mins",
    time_var = "time_stop"
)

dataset_followups = insert_na_for_mood(
    dataset_followups,
    min_minutes = 20
)

dataset_followups <- dataset_followups %>%
    rename(time_stop = time) %>%
    group_by(id) %>%
    mutate(
        time_interval = as.numeric(difftime(time_stop, lag(time_stop), units = "mins")),
        n_valid_pairs = sum(!is.na(time_interval))
    ) %>%
    ungroup() %>%
    select(-row_id, -label_jhxa_followUp)



# ---- Build dataset_beeps ----
# ---- dataset_beeps: mood only, long-interval breaks ----

dataset_beeps <- add_nas(
    dat        = data %>% filter(condition == "mood"),
    id         = "id",
    min_diff   = 45,
    max_diff   = 175,
    time_units = "mins",
    time_var   = "time_stop"
)

dataset_beeps <- dataset_beeps %>%
    rename(time_stop = time) %>%
    group_by(id) %>%
    mutate(
        time_interval = as.numeric(difftime(time_stop, lag(time_stop), units = "mins")),
        n_valid_pairs = sum(!is.na(time_interval))
    ) %>%
    ungroup() %>%
    select(-row_id, -label_jhxa_followUp)

# =============================================================================
# COMBINE, SUMMARISE, FILTER
# =============================================================================
data_combined <- bind_rows(
    dataset_beeps     %>% mutate(dataset = "long_interval"),
    dataset_followups %>% mutate(dataset = "short_interval")
)

# ---- Summary statistics and plots ----
# ---- Plot before filtering ----
p <- data_combined %>%
    filter(!is.na(time_interval)) %>%
  ggplot(aes(x = time_interval, fill = dataset)) +
  geom_histogram(
    position = "identity",
    alpha    = 0.5,
    bins     = 60,
    color    = "white"
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Distribution of Time Intervals Between Prompts Dejonckheere",
    x     = "Time interval (minutes)",
    y     = "Count",
    fill  = "Dataset"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

ggsave(
  filename   = file.path("figures", "time_intervals_combined.png"),
  create.dir = TRUE,
  plot       = p,
  width      = 12,
  height     = 6,
  dpi        = 300
)

# ---- Summary statistics before filtering ----
summary_intervals <- data_combined %>%
    summarise(
        min_interval  = min(time_interval,  na.rm = TRUE),
        mean_interval = mean(time_interval, na.rm = TRUE),
        sd_interval   = sd(time_interval,   na.rm = TRUE),
        max_interval  = max(time_interval,  na.rm = TRUE),
        .by = c(dataset)
    )

summary_pairs <- data_combined %>%
    select(id, dataset, n_valid_pairs) %>%
    distinct() %>%
    summarise(
        min_pairs  = min(n_valid_pairs),
        mean_pairs = mean(n_valid_pairs),
        sd_pairs   = sd(n_valid_pairs),
        max_pairs  = max(n_valid_pairs),
        .by = c(dataset)
    )

n_participants <- data_combined %>%
    summarise(
        n_participants = n_distinct(id),
        .by = c(dataset)
    )

print(summary_intervals)
print(summary_pairs)
print(n_participants)

# ---- Filter: keep only participants with >= 20 valid pairs in every condition ----
# remove people with less than 20 pairs in either condition

data_combined_valid = data_combined %>%
    group_by(id) %>%
    filter(all(n_valid_pairs >= 20)) %>%
    ungroup()

# Summary statistics
summary_intervals_valid <- data_combined_valid %>%
    summarise(
        min_interval  = min(time_interval,  na.rm = TRUE),
        mean_interval = mean(time_interval, na.rm = TRUE),
        sd_interval   = sd(time_interval,   na.rm = TRUE),
        max_interval  = max(time_interval,  na.rm = TRUE),
        .by = c(dataset)
    )

summary_pairs_valid <- data_combined_valid %>%
    select(id, dataset, n_valid_pairs) %>%
    distinct() %>%
    summarise(
        min_pairs  = min(n_valid_pairs),
        mean_pairs = mean(n_valid_pairs),
        sd_pairs   = sd(n_valid_pairs),
        max_pairs  = max(n_valid_pairs),
        .by = c(dataset)
    )

n_participants_valid <- data_combined_valid %>%
    summarise(
        n_participants = n_distinct(id),
        .by = c(dataset)
    )

print(summary_intervals_valid)
print(summary_pairs_valid)
print(n_participants_valid)

p <- data_combined_valid %>%
    filter(!is.na(time_interval)) %>%
  ggplot(aes(x = time_interval, fill = dataset)) +
  geom_histogram(
    position = "identity",
    alpha    = 0.5,
    bins     = 60,
    color    = "white"
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Distribution of Time Intervals Between Prompts Dejonckheere",
    x     = "Time interval (minutes)",
    y     = "Count",
    fill  = "Dataset"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

ggsave(
  filename   = file.path("figures", "time_intervals_valid participants.png"),
  create.dir = TRUE,
  plot       = p,
  width      = 12,
  height     = 6,
  dpi        = 300
)


# =============================================================================
# SAVE
# =============================================================================

dataset_dejonckheere_short_interval = 
    data_combined_valid %>%
    filter(dataset == "short_interval") 


write.csv(
    dataset_dejonckheere_short_interval,
    file = file.path("data_processed", "dataset_dejonckheere_short_interval.csv"),
    row.names = FALSE
)

dataset_dejonckheere_long_interval = 
    data_combined_valid %>%
    filter(dataset == "long_interval")

write.csv(
    dataset_dejonckheere_long_interval,
    file = file.path("data_processed", "dataset_dejonckheere_long_interval.csv"),
    row.names = FALSE
)

