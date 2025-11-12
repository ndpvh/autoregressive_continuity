################################################################################
# CONTINUOUS -- LEONIE
################################################################################
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)

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
    time_valid = if_else(missed, as.POSIXct(NA), time),
    last_valid_time = zoo::na.locf(time_valid, na.rm = FALSE),
    time_lag = lag(last_valid_time),
    time_interval = if_else(
      missed,
      NA_real_,
      as.numeric(difftime(time, time_lag, units = "mins"))
    )
  )

# remove data points where the lag is from a different condition
data <- data %>%
  mutate(time_interval = ifelse(condition != lag(condition), NA, time_interval))

# make histogram of time intervals for conditions

data %>%
  ggplot2::ggplot(aes(x = time_interval, fill = condition)) +
  ggplot2::geom_density(bins = 100)


# provide info how many valid time intervals there are per person
data %>%
  group_by(id, condition) %>%
  summarise(
    n_total = n(),
    n_valid_intervals = sum(!is.na(time_interval))
  ) %>%
  group_by(condition) %>%
  summarise(
    mean_valid_intervals = mean(n_valid_intervals),
    sd_valid_intervals = sd(n_valid_intervals)
  )
