################################################################################
# CONTINUOUS -- LEONIE
################################################################################
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)

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


data <- data %>%
  mutate(condition = ifelse(is.na(beep.no), "burst", "beep"), .after = time) %>%
  dplyr::group_by(id, date) %>%
  nest(data = !condition) %>%
  mutate(
    data = ifelse(
      condition == "burst",
      map(data, ~ mutate(.x, burst.no = seq.int(1, n(), 1))),
      map(data, ~ mutate(.x, burst.no = NA))
    )
  ) |>
  unnest(data) %>%
  arrange(id, time) %>%
  relocate(burst.no, .after = beep.no)

data <- data %>%
  group_by(id, date) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    # time_valid = if_else(missed, as.POSIXct(NA), time),
    # last_valid_time = zoo::na.locf(time_valid, na.rm = FALSE),
    time_lag = lag(time),
    time_interval = if_else(
      missed,
      NA,
      as.numeric(difftime(time, time_lag, units = "mins"))
    )
  )

# remove data points where the lag is from a different condition
data <- data %>%
  mutate(time_interval = ifelse(condition != lag(condition), NA, time_interval))

# make histogram of time intervals for conditions

data %>%
  ggplot(aes(x = time_interval, fill = condition)) +
  geom_histogram(
    position = "identity",
    alpha = 0.5,
    bins = 60,
    color = "white"
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Distribution of Time Intervals Between Prompts",
    x = "Time interval (minutes)",
    y = "Count",
    fill = "Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# provide info how many valid time intervals there are per person
data %>%
  group_by(id, condition) %>%
  summarise(
    n_total = n(),
    n_valid_pairs = sum(!is.na(time_interval))
  ) %>%
  ggplot(aes(x = n_valid_pairs, fill = condition)) +
  geom_histogram(
    position = "identity",
    alpha = 0.6,
    bins = 40,
    color = "white"
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Number of Valid Time Intervals per Participant",
    x = "Valid intervals per person",
    y = "Count",
    fill = "Condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

data %>%
  group_by(id, condition) %>%
  summarise(
    n_total = n(),
    n_valid_pairs = sum(!is.na(time_interval))
  ) %>%
  group_by(condition) %>%
  summarise(
    mean_valid_pairs = mean(n_valid_pairs),
    sd_valid_pairs = sd(n_valid_pairs)
  )

write.csv(
  data %>% filter(!is.na(burst.no)),
  file = file.path("data_processed", "dataset_cloos_burst_short"),
  row.names = FALSE
)

write.csv(
  data %>% filter(!is.na(beep.no)),
  file = file.path("data_processed", "dataset_cloos_beeps_long"),
  row.names = FALSE
)
