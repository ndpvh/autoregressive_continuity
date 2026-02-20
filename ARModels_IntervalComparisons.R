#### Libraries #####

library(tidyverse)

#### Read in AR Estimates ####

load(file.path("results", "ar_estimates", "PACE_ARParameterEstimates.RData")) # pace_arresults_full
load(file.path("results", "ar_estimates", "CloosData_ARParameterEstimates.RData")) # cloos_arresults_full
load(file.path("results", "ar_estimates", "EgonData_ARParameterEstimates.RData")) # dejonckheere_arresults_full

#### Compare AR Estimates B/w Short and Long Intervals ####

# Changed this in the original file
#
# # B = 4x/day (longer interval), A = 8x/day (shorter interval)
# pace_arresults_full = pace_arresults_full %>%
#   mutate("condition" = case_when(
#     condition == "B" ~ "longinterval",
#     condition == "A" ~ "shortinterval",
#     TRUE ~ NA_character_
#   ))

# # Beep = long interval, Burst = short interval

# cloos_arresults_full = cloos_arresults_full %>%
#   mutate("condition" = case_when(
#     condition == "beep" ~ "longinterval",
#     condition == "burst" ~ "shortinterval",
#     TRUE ~ NA_character_
#   ))

# dejonckheere_arresults_full$condition = dejonckheere_arresults_full$condition

intervalcomparison_function = function(dataset, paired){
  
  # dataset has already been subsetted to specific item
  # and a specific number of usable pairs
  # paired is TRUE/FALSE statement for whether the data are paired
  
  ar_summarystats = dataset %>%
    group_by(condition) %>%
    summarise("ARMean" = mean(ar_arima, na.rm = TRUE),
              "ARSD" = sd(ar_arima, na.rm = TRUE),
              "time_interval" = mean(time_interval, na.rm = TRUE)) %>%
    pivot_wider(names_from = condition,
                values_from = c(ARMean, ARSD, time_interval))
  
  exponent <- ar_summarystats$time_interval_longinterval / ar_summarystats$time_interval_shortinterval
  expected_mean = ar_summarystats$ARMean_shortinterval^exponent
  
  if(paired){

    dataset = dataset %>%
      pivot_wider(id_cols = c(id, item), names_from = condition,
                  values_from = ar_arima)
    
    ttest_pvalue = tryCatch(t.test(dataset$longinterval,
                          dataset$shortinterval,
                          paired = TRUE)$p.value,
                          error = function(e) NA)
    
    # also compare to the square of the short interval
    
    dataset = dataset %>%
      rowwise() %>%
      mutate(expected_ar = shortinterval^exponent) %>%
      ungroup()
    
    expectedar_comparison = tryCatch(t.test(dataset$longinterval,
                                   dataset$expected_ar,
                                   paired = TRUE)$p.value, 
                                   error = function(e) NA)
    
    results = cbind(data.frame(item = unique(dataset[ , "item"]),
                               interval_comparison = ttest_pvalue,
                               arsquared_comparison = expectedar_comparison), ar_summarystats)
    
    
  }else if(!paired){
    
    # independent samples
    
    ttest_pvalue = t.test(ar_arima ~ condition,
                             data = dataset)$p.value
    
    # it could be that the average AR estimates are sig
    # different from each other, but the AR estimate for
    # the long interval is not what we would expect (square of phi_shortinterval)
    
    expectedar_comparison = t.test(dataset$ar_arima[dataset$condition == "longinterval"],
                              mu = expected_mean)$p.value
    
    results = cbind(data.frame(item = unique(dataset[ , "item"]),
                               interval_comparison = ttest_pvalue,
                               arsquared_comparison = expectedar_comparison), ar_summarystats)
    
    
  }
  
  return(results)
  
  
}

pace_intervalcomparisons = pace_arresults_full %>%
  filter(number_usable_pairs > 50) %>%
  group_by(item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr( ~ intervalcomparison_function(., paired = FALSE))

# in most ideal world, the interval comparison is nonsig 
# and the arsquared comparison is sig (no diff between intervals,
# and the longer interval is not the shorter interval squared)
pace_intervalcomparisons %>%
  rowwise() %>%
  mutate(intervalcomparison_sig = case_when(interval_comparison < .05 ~ 1,
                              interval_comparison >= .05 ~ 0,
                              TRUE ~ NA_real_),
         arsquared_sig = case_when(arsquared_comparison < .05 ~ 1,
                                   arsquared_comparison >= .05 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  ungroup() %>%
  summarise(sum(intervalcomparison_sig), sum(arsquared_sig))
# so the long interval phi is significantly diff from the short
# interval in 12/19 cases; but also the short interval phi is sig diff
# from what we would expect (in 18/19 cases)

cloos_intervalcomparisons = cloos_arresults_full %>%
  filter(number_usable_pairs > 35) %>%
  group_by(item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr( ~ intervalcomparison_function(., paired = TRUE))

cloos_intervalcomparisons %>%
  rowwise() %>%
  mutate(intervalcomparison_sig = case_when(interval_comparison < .05 ~ 1,
                                            interval_comparison >= .05 ~ 0,
                                            TRUE ~ NA_real_),
         arsquared_sig = case_when(arsquared_comparison < .05 ~ 1,
                                   arsquared_comparison >= .05 ~ 0,
                                   TRUE ~ NA_real_)) %>%
  ungroup() %>%
  summarise(sum(intervalcomparison_sig), sum(arsquared_sig))
# interval lengths sig differ, and long interval *does not*
# differ from the square of the short interval

dejonckheere_intervalcomparisons = dejonckheere_arresults_full %>%
  filter(number_usable_pairs > 25) %>%
  group_by(item) %>%
  group_split(.keep = TRUE) %>%
  map_dfr( ~ intervalcomparison_function(., paired = TRUE))

dejonckheere_intervalcomparisons %>%
  rowwise() %>%
  mutate(intervalcomparison_sig = case_when(interval_comparison < .05 ~ 1,
                                            interval_comparison >= .05 ~ 0,
                              TRUE ~ NA_real_),
         arsquared_sig = case_when(arsquared_comparison < .05 ~ 1,
                                   arsquared_comparison >= .05 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  ungroup() %>%
  summarise(sum(intervalcomparison_sig), sum(arsquared_sig, na.rm = TRUE))
# out of 12 items
# If only include participants with more than 25 timepoints,
# then half items have different ar estimates across the intervals
# (half are not significantly different); but only
# 1 item (enthusiastic) is sig different from what would expect by squaring

##### Comparisons Figure #####

ggplot(data = pace_arresults_full,
       aes(x = ar_arima, color = condition, fill = condition))+
  geom_density(alpha = 0.3)+
  facet_wrap(~ item)+
  theme_classic()+
  theme(legend.position = "top")+
  scale_color_discrete(labels = c("Long Interval", "Short Interval"))+
  scale_fill_discrete(labels = c("Long Interval", "Short Interval"))+
  labs(x = "AR Estimate", y = "Density", fill = "Interval Length",
       color = "Interval Length")

ggsave(file.path("figures", "PACE_AREstimates_Visual.jpeg"),
       width = 10, height = 8)

ggplot(data = cloos_arresults_full,
       aes(x = ar_arima, color = condition, fill = condition))+
  geom_density(alpha = 0.3)+
  facet_wrap(~ item)+
  theme_classic()+
  theme(legend.position = "top")+
  xlim(c(-1.5, 1.5))+
  scale_color_discrete(labels = c("Long Interval", "Short Interval"))+
  scale_fill_discrete(labels = c("Long Interval", "Short Interval"))+
  labs(x = "AR Estimate", y = "Density", fill = "Interval Length",
       color = "Interval Length")

ggsave(file.path("figures", "Cloos_AREstimates_Visual.jpeg"),
       width = 8, height = 6)

ggplot(data = dejonckheere_arresults_full,
       aes(x = ar_arima, color = condition, fill = condition))+
  geom_density(alpha = 0.3)+
  facet_wrap(~ item)+
  theme_classic()+
  theme(legend.position = "top")+
  xlim(c(-1.5, 1.5))+
  scale_color_discrete(labels = c("Long Interval", "Short Interval"))+
  scale_fill_discrete(labels = c("Long Interval", "Short Interval"))+
  labs(x = "AR Estimate", y = "Density", fill = "Interval Length",
       color = "Interval Length")

ggsave(file.path("figures", "Dejonckheere_AREstimates_Visual.jpeg"),
       width = 10, height = 8)

ggplot(data = pace_arresults_full %>% filter(item == "soci"),
       aes(x = ar_arima, color = condition,
           fill = condition))+
  geom_density(alpha = 0.5)+
  geom_vline(xintercept = pace_intervalcomparisons$ARMean_longinterval[pace_intervalcomparisons$item == "soci"],
             color = "#9888A5")+
  geom_vline(xintercept = pace_intervalcomparisons$ARMean_shortinterval[pace_intervalcomparisons$item == "soci"],
             color = "#4E7147", alpha = 0.8)+
  geom_vline(xintercept = pace_intervalcomparisons$ARMean_longinterval[pace_intervalcomparisons$item == "soci"]^2,
             linetype = "dashed")+
  scale_fill_manual(values = c("#9888A5", "#4E7147"),
                    labels = c("Long Interval", "Short Interval"))+
  scale_color_manual(values = c("#9888A5", "#4E7147"),
                     labels = c("Long Interval", "Short Interval"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(x = "AR Estimates (Sociable)", y = "Density", fill = "Interval Length",
       color = "Interval Length")

ggsave("CTAR_ARComparisonsExample.jpeg",
       height = 6, width = 12, units = "in")


