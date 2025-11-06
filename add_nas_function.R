add_nas <- function(dat, id, min_diff, max_diff,
                           time_units = "mins", time_var, alter_time_var = NULL) {
  
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


