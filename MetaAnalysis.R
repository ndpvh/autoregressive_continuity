#### 0) Libraries ####
library(tidyverse)
library(metafor)

#### 1) Load data ####
load("data/PACE_ARParameterEstimates.RData")         # pace_arresults_full
load("data/CloosData_ARParameterEstimates.RData")    # cloos_arresults_full
load("data/EgonData_ARParameterEstimates.RData")     # dejonckheere_arresults_full

#### 2) Recode interval labels ####
pace_arresults_full <- pace_arresults_full %>%
  mutate(intervalcondition = case_when(
    condition == "B" ~ "longinterval",
    condition == "A" ~ "shortinterval",
    TRUE ~ NA_character_
  ))

cloos_arresults_full <- cloos_arresults_full %>%
  mutate(intervalcondition = case_when(
    condition == "beep"  ~ "longinterval",
    condition == "burst" ~ "shortinterval",
    TRUE ~ NA_character_
  ))

dejonckheere_arresults_full <- dejonckheere_arresults_full %>%
  mutate(intervalcondition = condition)

#### 3) Fixed thresholds ####
pace_cut  <- 50
cloos_cut <- 35
egon_cut  <- 25

#### 4) Compute effect size and variance from t and n ####
# Paired: Cohen dz = t / sqrt(n_pairs)
# Independent two group: Cohen d = t * sqrt(1/n1 + 1/n2)
# Independent one sample: Cohen d = t / sqrt(n)
# yi = effect size for a variable
# vi = variance of yi
# Variances are standard approximations

es_paired_dz <- function(tval, n){
  if (is.na(tval) || is.na(n) || n < 2) return(tibble(yi = NA_real_, vi = NA_real_))
  yi <- as.numeric(tval) / sqrt(n)
  vi <- (1 / n) + (yi^2 / (2 * n))
  tibble(yi = yi, vi = vi)
}

es_indep_d <- function(tval, n1, n2){
  if (is.na(tval) || is.na(n1) || is.na(n2) || n1 < 2 || n2 < 2) return(tibble(yi = NA_real_, vi = NA_real_))
  yi <- as.numeric(tval) * sqrt(1 / n1 + 1 / n2)
  vi <- (n1 + n2) / (n1 * n2) + (yi^2 / (2 * (n1 + n2)))
  tibble(yi = yi, vi = vi)
}

es_onesample_d <- function(tval, n){
  if (is.na(tval) || is.na(n) || n < 2) return(tibble(yi = NA_real_, vi = NA_real_))
  yi <- as.numeric(tval) / sqrt(n)
  vi <- (1 / n) + (yi^2 / (2 * n))
  tibble(yi = yi, vi = vi)
}

#### 5) Per item function: returns primitives + effect sizes for both targets ####
item_results <- function(df_item, dataset_name, paired_design){
  
  item_name <- unique(df_item$item)
  
  # Summaries for reporting (means and sds by interval)
  summ <- df_item %>%
    group_by(intervalcondition) %>%
    summarise(
      n = sum(!is.na(ar_arima)),
      mean = mean(ar_arima, na.rm = TRUE),
      sd = sd(ar_arima, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = intervalcondition, values_from = c(n, mean, sd), names_sep = "_")
  
  mean_long  <- summ$mean_longinterval  %||% NA_real_
  mean_short <- summ$mean_shortinterval %||% NA_real_
  sd_long    <- summ$sd_longinterval    %||% NA_real_
  sd_short   <- summ$sd_shortinterval   %||% NA_real_
  n_long     <- summ$n_longinterval     %||% NA_real_
  n_short    <- summ$n_shortinterval    %||% NA_real_
  
  # Expected CT value uses (mean_short)^2 at the item level for independent case
  expected_mean <- if (!is.na(mean_short)) mean_short^2 else NA_real_
  
  if (paired_design) {
    
    wide <- df_item %>%
      pivot_wider(id_cols = c(id, item), names_from = intervalcondition, values_from = ar_arima)
    
    # n pairs for long vs short
    n_pairs_A <- sum(!is.na(wide$longinterval) & !is.na(wide$shortinterval))
    
    # Target A test: long vs short (paired)
    ttA <- tryCatch(t.test(wide$longinterval, wide$shortinterval, paired = TRUE),
                    error = function(e) NULL)
    tA  <- if (!is.null(ttA)) as.numeric(ttA$statistic) else NA_real_
    dfA <- if (!is.null(ttA)) as.numeric(ttA$parameter) else NA_real_
    pA  <- if (!is.null(ttA)) as.numeric(ttA$p.value) else NA_real_
    
    # mean difference for Target A at participant level
    diffA_mean <- if (n_pairs_A >= 1) mean(wide$longinterval - wide$shortinterval, na.rm = TRUE) else NA_real_
    
    esA <- es_paired_dz(tA, n_pairs_A)
    
    # Target B: long vs short^2 (paired)
    wide <- wide %>%
      mutate(expected_ar = shortinterval^2)
    
    n_pairs_B <- sum(!is.na(wide$longinterval) & !is.na(wide$expected_ar))
    
    ttB <- tryCatch(t.test(wide$longinterval, wide$expected_ar, paired = TRUE),
                    error = function(e) NULL)
    tB  <- if (!is.null(ttB)) as.numeric(ttB$statistic) else NA_real_
    dfB <- if (!is.null(ttB)) as.numeric(ttB$parameter) else NA_real_
    pB  <- if (!is.null(ttB)) as.numeric(ttB$p.value) else NA_real_
    
    diffB_mean <- if (n_pairs_B >= 1) mean(wide$longinterval - wide$expected_ar, na.rm = TRUE) else NA_real_
    
    esB <- es_paired_dz(tB, n_pairs_B)
    
    bind_rows(
      tibble(
        dataset = dataset_name,
        item = item_name,
        target = "interval",
        test_type = "paired",
        n_pairs = n_pairs_A,
        n_long = NA_real_,
        n_short = NA_real_,
        mean_long = mean_long,
        mean_short = mean_short,
        sd_long = sd_long,
        sd_short = sd_short,
        expected_mean = NA_real_,
        mean_diff = diffA_mean,
        t = tA, df = dfA, p = pA,
        yi = esA$yi, vi = esA$vi
      ),
      tibble(
        dataset = dataset_name,
        item = item_name,
        target = "arsq",
        test_type = "paired",
        n_pairs = n_pairs_B,
        n_long = NA_real_,
        n_short = NA_real_,
        mean_long = mean_long,
        mean_short = mean_short,
        sd_long = sd_long,
        sd_short = sd_short,
        expected_mean = NA_real_,
        mean_diff = diffB_mean,
        t = tB, df = dfB, p = pB,
        yi = esB$yi, vi = esB$vi
      )
    )
    
  } else {
    
    # Target A: independent t-test long vs short
    ttA <- tryCatch(t.test(ar_arima ~ intervalcondition, data = df_item),
                    error = function(e) NULL)
    tA  <- if (!is.null(ttA)) as.numeric(ttA$statistic) else NA_real_
    dfA <- if (!is.null(ttA)) as.numeric(ttA$parameter) else NA_real_
    pA  <- if (!is.null(ttA)) as.numeric(ttA$p.value) else NA_real_
    
    diffA_mean <- if (!is.na(mean_long) && !is.na(mean_short)) mean_long - mean_short else NA_real_
    esA <- es_indep_d(tA, n_long, n_short)
    
    # Target B: one-sample test on longinterval against expected_mean = (mean_short)^2
    long_vals <- df_item$ar_arima[df_item$intervalcondition == "longinterval"]
    n_long_obs <- sum(!is.na(long_vals))
    
    ttB <- NULL
    if (!is.na(expected_mean) && n_long_obs > 1) {
      ttB <- tryCatch(t.test(long_vals, mu = expected_mean),
                      error = function(e) NULL)
    }
    tB  <- if (!is.null(ttB)) as.numeric(ttB$statistic) else NA_real_
    dfB <- if (!is.null(ttB)) as.numeric(ttB$parameter) else NA_real_
    pB  <- if (!is.null(ttB)) as.numeric(ttB$p.value) else NA_real_
    
    diffB_mean <- if (!is.na(mean_long) && !is.na(expected_mean)) mean_long - expected_mean else NA_real_
    
    # Effect size for one-sample test
    esB <- es_onesample_d(tB, n_long_obs)
    
    bind_rows(
      tibble(
        dataset = dataset_name,
        item = item_name,
        target = "interval",
        test_type = "independent",
        n_pairs = NA_real_,
        n_long = n_long,
        n_short = n_short,
        mean_long = mean_long,
        mean_short = mean_short,
        sd_long = sd_long,
        sd_short = sd_short,
        expected_mean = NA_real_,
        mean_diff = diffA_mean,
        t = tA, df = dfA, p = pA,
        yi = esA$yi, vi = esA$vi
      ),
      tibble(
        dataset = dataset_name,
        item = item_name,
        target = "arsq",
        test_type = "one_sample",
        n_pairs = NA_real_,
        n_long = n_long_obs,
        n_short = NA_real_,
        mean_long = mean_long,
        mean_short = mean_short,
        sd_long = sd_long,
        sd_short = sd_short,
        expected_mean = expected_mean,
        mean_diff = diffB_mean,
        t = tB, df = dfB, p = pB,
        yi = esB$yi, vi = esB$vi
      )
    )
  }
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#### 6) Prepare meta-analytic dataset ####
pace_fixed <- pace_arresults_full %>%
  filter(number_usable_pairs > pace_cut) %>%
  group_by(item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr(~ item_results(.x, dataset_name = "PACE", paired_design = FALSE))

cloos_fixed <- cloos_arresults_full %>%
  filter(number_usable_pairs > cloos_cut) %>%
  group_by(item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr(~ item_results(.x, dataset_name = "Cloos", paired_design = TRUE))

egon_fixed <- dejonckheere_arresults_full %>%
  filter(number_usable_pairs > egon_cut) %>%
  group_by(item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr(~ item_results(.x, dataset_name = "Egon", paired_design = TRUE))

es_fixed <- bind_rows(pace_fixed, cloos_fixed, egon_fixed)

# Save the per item table
if (!dir.exists("meta_outputs")) dir.create("meta_outputs")
write_csv(es_fixed, file.path("meta_outputs", "per_item_effects_fixed_threshold.csv"))

#### 7) Quick checks: significance counts and effect summaries ####
es_fixed %>%
  group_by(dataset, target) %>%
  summarise(
    n_items = n(),
    n_sig = sum(p < 0.05, na.rm = TRUE),
    mean_yi = mean(yi, na.rm = TRUE),
    .groups = "drop"
  )

#### 8) Meta analysis at fixed thresholds (random effects) ####
run_meta <- function(df, dataset_name, target_name){
  dat <- df %>%
    filter(dataset == dataset_name, target == target_name, !is.na(yi), !is.na(vi))
  
  if (nrow(dat) < 3) {
    message("Not enough items for meta: ", dataset_name, " ", target_name)
    return(NULL)
  }
  
  res <- rma(yi = yi, vi = vi, data = dat, method = "REML")
  print(summary(res))
  
  # forest plot to pdf
  pdf(file.path("meta_outputs", paste0("forest_", dataset_name, "_", target_name, "_fixed.pdf")),
      width = 8, height = 6)
  forest(res, slab = dat$item)
  dev.off()
  
  return(res)
}

m_pace_interval <- run_meta(es_fixed, "PACE", "interval")
m_pace_arsq     <- run_meta(es_fixed, "PACE", "arsq")

m_cloos_interval <- run_meta(es_fixed, "Cloos", "interval")
m_cloos_arsq     <- run_meta(es_fixed, "Cloos", "arsq")

m_egon_interval <- run_meta(es_fixed, "Egon", "interval")
m_egon_arsq     <- run_meta(es_fixed, "Egon", "arsq")

#### End ####



