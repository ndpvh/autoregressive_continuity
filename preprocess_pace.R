library(tidyverse)
library(here)
source("add_nas_function.R")

# load data
dat <- read_csv(here("data", "pace_esm_data_s1.csv"))
demo <- read_csv(here("data", "pace_baseline_data_s1.csv"))


# Data prep ---------------------------------------------------------------
# Condition A 
dat_a <- dat %>% 
  filter(condition == "A") %>% 
  add_nas(id = "session",min_diff =60, max_diff=130, # 1-2 hours + 10 mins to complete
          time_units = "mins", time_var = "ended", alter_time_var = "created")

dat_b <- dat %>% 
  filter(condition == "B") %>% 
  add_nas(id = "session",min_diff =120, max_diff=250, # 2-4 hours + 10 mins to complete
          time_units = "mins", time_var = "ended", alter_time_var = "created")

#merge datasets
dat_t <- bind_rows(dat_a, dat_b) 

# only keep some variables
dat_s <- dat_t %>% select(id:condition, soci:stres)

# combine with demographic information; rename session to id for merging
dat_c <- demo %>% 
  rename(id = session) %>% 
  select(id, gender, age) %>%  # join with ESM data, keep all rows of ESM data (left join)
  left_join(dat_s, by = "id")

write_csv(dat_c, here("data", "ESM_dat_processed_timelag.csv"))

