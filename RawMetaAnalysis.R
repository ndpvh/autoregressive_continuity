#### 0) Libraries ####
library(tidyverse)
library(metafor)

#### 1) Load data ####
load("data/PACE_CleanedARExpectations.RData")     # pace_expectations
load("data/Cloos_CleanedARExpectations.RData")    # cloos_expectations
load("data/Egon_CleanedARExpectations.RData")     # egon_expectations

pace_expectations <- pace_expectations %>%
  mutate(
    expected_shortar = ifelse(is.na(ar_arima_longinterval), NA, expected_shortar),
    expected_longar  = ifelse(is.na(ar_arima_shortinterval), NA, expected_longar)
  )


#### 2) Fixed thresholds ####
pace_cut  <- 20
cloos_cut <- 20
egon_cut  <- 20

#### 3) Prepare each dataset ####
prep_expectations <- function(df, dataset_name, cutoff) {
  
  required_cols <- c(
    "id",
    "item",
    "number_usable_pairs",
    "ar_arima_longinterval",
    "ar_arima_shortinterval",
    "expected_shortar",
    "expected_longar"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required columns in ", dataset_name, ": ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  df %>%
    mutate(
      dataset = dataset_name,
      id = as.character(id),
      item = as.character(item),
      number_usable_pairs = as.numeric(number_usable_pairs),
      ar_arima_longinterval = as.numeric(ar_arima_longinterval),
      ar_arima_shortinterval = as.numeric(ar_arima_shortinterval),
      expected_shortar = as.numeric(expected_shortar),
      expected_longar = as.numeric(expected_longar)
    ) %>%
    filter(number_usable_pairs >= cutoff)
}

pace_fixed  <- prep_expectations(pace_expectations,  "PACE",  pace_cut)
cloos_fixed <- prep_expectations(cloos_expectations, "Cloos", cloos_cut)
egon_fixed  <- prep_expectations(egon_expectations,  "Egon",  egon_cut)

#### 4) Helpers for per-item contrasts ####
# For each item and each contrast, compute the mean difference, its uncertainty, and a t-test.
# Contrasts:
#   1) ar_arima_shortinterval - ar_arima_longinterval
#   2) ar_arima_shortinterval - expected_shortar
#   3) ar_arima_longinterval  - expected_longar

paired_contrast_stats <- function(x, y, dataset_name, item_name, contrast_name, n_pairs_summary = NA_real_) {
  
  ok <- is.finite(x) & is.finite(y)
  x_ok <- x[ok]
  y_ok <- y[ok]
  
  n_obs <- length(x_ok)
  # If there are fewer than 2 paired observations, returns row = NA.
  if (n_obs < 2) {
    return(tibble(
      dataset = dataset_name,
      item = item_name,
      contrast = contrast_name,
      design = "paired",
      n_obs = n_obs,
      n_x = NA_real_,
      n_y = NA_real_,
      number_usable_pairs = n_pairs_summary,
      mean_x = ifelse(n_obs >= 1, mean(x_ok, na.rm = TRUE), NA_real_),
      mean_y = ifelse(n_obs >= 1, mean(y_ok, na.rm = TRUE), NA_real_),
      yi = NA_real_,
      vi = NA_real_,
      sd_diff = NA_real_,
      se_diff = NA_real_,
      t_value = NA_real_,
      df = NA_real_,
      p_value = NA_real_
    ))
  }
  
  d <- x_ok - y_ok
  mean_diff <- mean(d, na.rm = TRUE)
  sd_diff <- sd(d, na.rm = TRUE)
  se_diff <- sd_diff / sqrt(n_obs)
  vi_diff <- se_diff^2 # sampling variance
  
  # Compute a one-sample t test  
  t_value <- if (!is.na(se_diff) && se_diff > 0) mean_diff / se_diff else NA_real_
  df_value <- n_obs - 1
  p_value <- if (!is.na(t_value)) {
    2 * pt(abs(t_value), df = df_value, lower.tail = FALSE)
  } else {
    NA_real_
  }
  
  tibble(
    dataset = dataset_name,
    item = item_name,
    contrast = contrast_name,
    design = "paired",
    n_obs = n_obs,
    n_x = NA_real_,
    n_y = NA_real_,
    number_usable_pairs = n_pairs_summary,
    mean_x = mean(x_ok, na.rm = TRUE),
    mean_y = mean(y_ok, na.rm = TRUE),
    yi = mean_diff,
    vi = ifelse(is.finite(vi_diff) && vi_diff > 0, vi_diff, NA_real_),
    sd_diff = sd_diff,
    se_diff = se_diff,
    t_value = t_value,
    df = df_value,
    p_value = p_value
  )
}

independent_contrast_stats <- function(x, y, dataset_name, item_name, contrast_name, n_pairs_summary = NA_real_) {
  
  x_ok <- x[is.finite(x)]
  y_ok <- y[is.finite(y)]
  
  n_x <- length(x_ok)
  n_y <- length(y_ok)
  
  if (n_x < 2 || n_y < 2) {
    return(tibble(
      dataset = dataset_name,
      item = item_name,
      contrast = contrast_name,
      design = "independent",
      n_obs = n_x + n_y,
      n_x = n_x,
      n_y = n_y,
      number_usable_pairs = n_pairs_summary,
      mean_x = ifelse(n_x >= 1, mean(x_ok, na.rm = TRUE), NA_real_),
      mean_y = ifelse(n_y >= 1, mean(y_ok, na.rm = TRUE), NA_real_),
      yi = NA_real_,
      vi = NA_real_,
      sd_diff = NA_real_,
      se_diff = NA_real_,
      t_value = NA_real_,
      df = NA_real_,
      p_value = NA_real_
    ))
  }
  
  mean_x <- mean(x_ok, na.rm = TRUE)
  mean_y <- mean(y_ok, na.rm = TRUE)
  sd_x <- sd(x_ok, na.rm = TRUE)
  sd_y <- sd(y_ok, na.rm = TRUE)
  
  mean_diff <- mean_x - mean_y
  se_diff <- sqrt((sd_x^2 / n_x) + (sd_y^2 / n_y))
  vi_diff <- se_diff^2
  
  tt <- tryCatch(
    t.test(x_ok, y_ok, paired = FALSE),
    error = function(e) NULL
  )
  
  t_value <- if (!is.null(tt)) as.numeric(tt$statistic) else NA_real_
  df_value <- if (!is.null(tt)) as.numeric(tt$parameter) else NA_real_
  p_value <- if (!is.null(tt)) as.numeric(tt$p.value) else NA_real_
  
  tibble(
    dataset = dataset_name,
    item = item_name,
    contrast = contrast_name,
    design = "independent",
    n_obs = n_x + n_y,
    n_x = n_x,
    n_y = n_y,
    number_usable_pairs = n_pairs_summary,
    mean_x = mean_x,
    mean_y = mean_y,
    yi = mean_diff,
    vi = ifelse(is.finite(vi_diff) && vi_diff > 0, vi_diff, NA_real_),
    sd_diff = NA_real_,
    se_diff = se_diff,
    t_value = t_value,
    df = df_value,
    p_value = p_value
  )
}

#### 5) Per-item difference function ####
item_results_raw <- function(df_item) {
  
  dataset_name <- unique(df_item$dataset)
  item_name <- unique(df_item$item)
  
  n_pairs_summary <- suppressWarnings(max(df_item$number_usable_pairs, na.rm = TRUE))
  if (!is.finite(n_pairs_summary)) n_pairs_summary <- NA_real_
  
  if (dataset_name == "PACE") {
    out1 <- independent_contrast_stats(
      x = df_item$ar_arima_shortinterval,
      y = df_item$ar_arima_longinterval,
      dataset_name = dataset_name,
      item_name = item_name,
      contrast_name = "short_minus_long",
      n_pairs_summary = n_pairs_summary
    )
    
    out2 <- independent_contrast_stats(
      x = df_item$ar_arima_shortinterval,
      y = df_item$expected_shortar,
      dataset_name = dataset_name,
      item_name = item_name,
      contrast_name = "short_minus_expectedshort",
      n_pairs_summary = n_pairs_summary
    )
    
    out3 <- independent_contrast_stats(
      x = df_item$ar_arima_longinterval,
      y = df_item$expected_longar,
      dataset_name = dataset_name,
      item_name = item_name,
      contrast_name = "long_minus_expectedlong",
      n_pairs_summary = n_pairs_summary
    )
  } else {
    out1 <- paired_contrast_stats(
      x = df_item$ar_arima_shortinterval,
      y = df_item$ar_arima_longinterval,
      dataset_name = dataset_name,
      item_name = item_name,
      contrast_name = "short_minus_long",
      n_pairs_summary = n_pairs_summary
    )
    
    out2 <- paired_contrast_stats(
      x = df_item$ar_arima_shortinterval,
      y = df_item$expected_shortar,
      dataset_name = dataset_name,
      item_name = item_name,
      contrast_name = "short_minus_expectedshort",
      n_pairs_summary = n_pairs_summary
    )
    
    out3 <- paired_contrast_stats(
      x = df_item$ar_arima_longinterval,
      y = df_item$expected_longar,
      dataset_name = dataset_name,
      item_name = item_name,
      contrast_name = "long_minus_expectedlong",
      n_pairs_summary = n_pairs_summary
    )
  }
  
  bind_rows(out1, out2, out3)
}

#### 6) Create the per-item meta-analytic table ####
pace_raw_es <- pace_fixed %>%
  group_by(dataset, item) %>%
  group_split(.keep = TRUE) %>%
  purrr::map_dfr(item_results_raw)

cloos_raw_es <- cloos_fixed %>%
  group_by(dataset, item) %>%
  group_split(.keep = TRUE) %>%
  purrr::map_dfr(item_results_raw)

egon_raw_es <- egon_fixed %>%
  group_by(dataset, item) %>%
  group_split(.keep = TRUE) %>%
  purrr::map_dfr(item_results_raw)

raw_es <- bind_rows(pace_raw_es, cloos_raw_es, egon_raw_es) %>%
  mutate(
    contrast_label = dplyr::recode(
      contrast,
      short_minus_long = "AR short - AR long",
      short_minus_expectedshort = "AR short - Expected short",
      long_minus_expectedlong = "AR long - Expected long"
    )
  )

if (!dir.exists("raw_meta_outputs")) dir.create("raw_meta_outputs")

write_csv(raw_es, file.path("raw_meta_outputs", "per_item_signed_differences.csv"))

#### 7) Quick checks ####
quick_checks <- raw_es %>%
  group_by(dataset, contrast, contrast_label) %>%
  summarise(
    n_items_total = n(),
    n_items_testable = sum(!is.na(yi)),
    n_sig = sum(p_value < 0.05, na.rm = TRUE),
    mean_yi = mean(yi, na.rm = TRUE),
    .groups = "drop"
  )

print(quick_checks)
write_csv(quick_checks, file.path("raw_meta_outputs", "quick_checks_signed.csv"))

#### 8) Meta-analysis helpers ####
extract_meta <- function(res, dataset_name, contrast_name, k_items) {
  tibble(
    dataset = dataset_name,              
    contrast = contrast_name,
    k_items = k_items,                    # number of items
    estimate = as.numeric(coef(res)[1]),  # pooled effect size from the random-effects model
    se = res$se[1],                       # standard error of that pooled estimate
    ci_lb = res$ci.lb,                    # lower confidence limits
    ci_ub = res$ci.ub,                    # upper confidence limits
    pval = res$pval,                      # p-value for the pooled effect
    tau2 = res$tau2,                      # between-item variance (how much the item effects vary around the mean
    I2 = res$I2,                          # proportion of total variability due to heterogeneity rather than sampling error
    H2 = res$H2,                          # heterogeneity index
    QE = res$QE,                          # Cochran’s Q test for heterogeneity
    QEp = res$QEp                         # Cochran’s Q test for heterogeneity
  )
}

run_meta <- function(df, dataset_name, contrast_name) {
  
  dat <- df %>%
    filter(
      dataset == dataset_name,
      contrast == contrast_name,
      !is.na(yi),
      !is.na(vi),
      vi > 0
    )
  
  if (nrow(dat) < 2) {
    message("Not enough items for meta: ", dataset_name, " | ", contrast_name)
    return(NULL)
  }
  
  res <- rma.uni(
    yi = dat$yi,
    vi = dat$vi,
    method = "REML",
    test = "knha",
    slab = dat$item
  )
  
  pdf(
    file.path("raw_meta_outputs", paste0("forest_", dataset_name, "_", contrast_name, ".pdf")),
    width = 8,
    height = 6
  )
  forest(
    res,
    slab = dat$item,
    xlab = "Signed raw mean difference",
    main = paste(dataset_name, "|", unique(dat$contrast_label)),
    alim = c(-0.5, 0.5),
    xlim = c(-0.8, 0.8),
    at = seq(-0.5, 0.5, by = 0.1),
    header = "Item"
  )
  dev.off()
  
  list(
    model = res,
    summary = extract_meta(res, dataset_name, contrast_name, nrow(dat))
  )
}

#### 9) Run dataset-specific meta-analyses ####
datasets <- c("PACE", "Cloos", "Egon")
contrasts <- c(
  "short_minus_long",
  "short_minus_expectedshort",
  "long_minus_expectedlong"
)

meta_results <- list()
meta_summary_rows <- list()

idx <- 1
for (ds in datasets) {
  for (ct in contrasts) {
    tmp <- run_meta(raw_es, ds, ct)
    meta_results[[paste(ds, ct, sep = "::")]] <- tmp
    if (!is.null(tmp)) {
      meta_summary_rows[[idx]] <- tmp$summary
      idx <- idx + 1
    }
  }
}

meta_summary <- bind_rows(meta_summary_rows) %>%
  mutate(
    contrast_label = dplyr::recode(
      contrast,
      short_minus_long = "AR short - AR long",
      short_minus_expectedshort = "AR short - Expected short",
      long_minus_expectedlong = "AR long - Expected long"
    )
  ) %>%
  relocate(contrast_label, .after = contrast)

print(meta_summary)
write_csv(meta_summary, file.path("raw_meta_outputs", "meta_summary_by_dataset.csv"))

#### End ####
