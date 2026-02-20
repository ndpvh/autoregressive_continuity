library(tidyverse) # data wrangling
library(patchwork) # graphing
library(ggpubr)# graphing

#### Load in Data #### 

pace_esm = read.csv(
  file.path(
    "data_processed",
    "ESM_dat_processed_timelag.csv"
  )
)

cloos_long = read.csv(
  file.path(
    "data_processed",
    "dataset_cloos_beeps_long.txt"
  ), 
  header = TRUE
)
cloos_short = read.csv(
  file.path(
    "data_processed",
    "dataset_cloos_burst_short.txt"
  ), 
  header = TRUE
)

dejonckheere_longinterval = read.csv(
  file.path(
    "data_processed",
    "dataset_dejonckheere_long_interval.txt"
  ), 
  header = TRUE
)
dejonckheere_shortinterval = read.csv(
  file.path(
    "data_processed",
    "dataset_dejonckheere_short_interval.txt"
  ), 
  header = TRUE
)

## Subset Leonie's data so aligns with run_ar1_function

cloos_combined = rbind(cloos_short, cloos_long)

# want cols for: id, condition, and then separate cols for outcome variable
# NAs have already been added for the night lags and missing data

cloos_combined = cloos_combined %>%
  dplyr::select(condition, id, beep.no, burst.no, beep_pa, burst_pa,
                beep_na, burst_na) %>%
  rowwise() %>%
  mutate(response.no = coalesce(beep.no, burst.no),
         pa = coalesce(beep_pa, burst_pa),
         na = coalesce(beep_na, burst_na)) %>%
  ungroup() %>%
  dplyr::select(condition, id, response.no, pa, na)

### Subset Egon's data for run_ar1_function

dejonckheere_longinterval$intervalcondition = "longinterval"
dejonckheere_shortinterval$intervalcondition = "shortinterval"

dejonckheere_longinterval = dejonckheere_longinterval %>%
  dplyr::select(id, relaxed:anxious, intervalcondition)

dejonckheere_shortinterval = dejonckheere_shortinterval %>%
  dplyr::select(id, relaxed:anxious, intervalcondition)

#### Run Autoregressive Models ####

# Notes: There needs to be a column in the dataset that gives the condition
# for the time interval, and that column needs to be called "condition"
# The itemcol will be a column that contains all the items being measured
# The scores for those items at each timepoint needs to be in a column called response

run_ar1_model = function(dataset, idcol, itemcol){
  
  dataset_lagged = dataset %>%
    dplyr::select(condition, response) %>%
    fill(condition, .direction = "updown") %>%
    mutate("response_lagged" = dplyr::lag(response))
  
  number_usable = sum(!is.na(dataset_lagged$response) & !is.na(dataset_lagged$response_lagged))
  
  ar1_model_lm = try(lm(as.formula("response ~ response_lagged"), data = dataset_lagged))
  
  ar1_model_arima = try(arima(dataset[ , "response"],
                              order = c(1, 0, 0)))
  
  # if either model fails OR AR parameter is greater than 1 in abs value, set
  # AR parameter to NA
  
  if(class(ar1_model_lm) != "try-error"){
    
    ar1_lm_parameter = coef(ar1_model_lm)[2]
    
    if(is.na(ar1_lm_parameter) | abs(ar1_lm_parameter) >= 1){
      ar1_lm_parameter = NA
    }
  }else{
      ar1_lm_parameter = NA
    }
  
  if(class(ar1_model_arima) != "try-error"){
    
    ar1_arima_parameter = coef(ar1_model_arima)[1]
    
    if(is.na(ar1_arima_parameter) | abs(ar1_arima_parameter) >= 1){
      ar1_arima_parameter = NA
    }
  }else{
    ar1_arima_parameter = NA
  }
  
  results_df = data.frame("ID" = unique(dataset[ , idcol]), 
                          "item" = unique(dataset[ , itemcol]), 
                          "condition" = unique(dataset_lagged[ , "condition"]),
                          "ar_lm" = ar1_lm_parameter, "ar_arima" = ar1_arima_parameter,
                          "number_usable_pairs" = number_usable)
  
  return(results_df)
  
}

### Run on PACE Dataset ####

pace_esm_analysis = pace_esm %>%
  dplyr::select(session, condition, n_day, n_survey_per_day, 
         soci, asse, acti, trus, kind,
         poli, crea, curi, arti, lazy, orga, reli, irri,
         rela, care, hung, tire, happ, stres) %>%
  pivot_longer(cols = names(.)[-c(1:4)], names_to = "item",
               values_to = "response")

pace_ar_datasets = pace_esm_analysis %>%
  group_by(session, item) %>%
  group_split(.keep = TRUE)

pace_arresults_full = pace_esm_analysis %>%
  group_by(session, item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr( ~ run_ar1_model(., idcol = "session", itemcol = "item"))

save(
  pace_arresults_full, 
  file = file.path(
    "results",
    "ar_estimates",
    "PACE_ARParameterEstimates.RData"
  )
)

#### Run on Leonie's Dataset ####

cloos_combined = cloos_combined %>%
  pivot_longer(cols = c("pa", "na"),
               names_to = "item", values_to = "response")

cloos_arresults_full = cloos_combined %>%
  group_by(id, condition, item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr( ~ run_ar1_model(., idcol = "id", itemcol = "item"))

save(
  cloos_arresults_full, 
  file = file.path(
    "results",
    "ar_estimates",
    "CloosData_ARParameterEstimates.RData"
  )
)

#### Run on Egon's Dataset ####

dejonckheere_fulldata = rbind(dejonckheere_longinterval,
                              dejonckheere_shortinterval)

names(dejonckheere_fulldata)[ncol(dejonckheere_fulldata)] = "condition"

dejonckheere_fulldata = dejonckheere_fulldata %>%
  pivot_longer(cols = relaxed:anxious, names_to = "item", 
               values_to = "response")

dejonckheere_arresults_full = dejonckheere_fulldata %>%
  group_by(id, condition, item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr( ~ run_ar1_model(., idcol = "id", itemcol = "item"))

save(
  dejonckheere_arresults_full, 
  file = file.path(
    "results",
    "ar_estimates",
    "EgonData_ARParameterEstimates.RData"
  )
)
