library(tidyverse)
library(ggtext)
library(ggpubr)

##################################
# Figure - Time Intervals ######
##################################

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
    "dataset_cloos_beep_long.txt"
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

# Process PACE data

pace_esm_wide = pace_esm %>%
  select(id, time, condition) %>%
  mutate(time = str_replace_all(time, "T|Z", " "),
         time = as.POSIXct(time, format="%Y-%m-%d %H:%M:%S"))%>%
  mutate(condition = ifelse(condition == "A",
                                   "shortinterval",
                                   "longinterval"),
         lagtime = lag(time),
         time_interval = as.numeric(difftime(time, lagtime, units = "mins")))|>
  drop_na(time_interval)|>
  filter(time_interval >= 60 & time_interval <= 250)

# Process Egon's data

dejonckheere_longinterval$intervalcondition = "longinterval"
dejonckheere_shortinterval$intervalcondition = "shortinterval"

dejonckheere_longinterval = dejonckheere_longinterval %>%
  dplyr::select(id, relaxed:anxious, intervalcondition, delta)

dejonckheere_shortinterval = dejonckheere_shortinterval %>%
  dplyr::select(id, relaxed:anxious, intervalcondition, delta)

dejonckheere_fulldata = rbind(dejonckheere_longinterval,
                              dejonckheere_shortinterval) |>
  dplyr::rename(time_interval = delta,
                condition = intervalcondition)  

# Process Leonie's data

cloos_combined = rbind(cloos_short, cloos_long)

# want cols for: id, condition, and then separate cols for outcome variable
# NAs have already been added for the night lags and missing data

cloos_combined = cloos_combined %>%
  dplyr::select(condition, id, meas_no, pa, na, time_interval) %>%
  dplyr::mutate(condition = ifelse(condition == "burst",
                                   "shortinterval",
                                   "longinterval"))  

# Make the plot

data <- list("Büchner et al. (preprint)" = pace_esm_wide,
             "Dejonckheere et al. (2022)" = dejonckheere_fulldata,
             "Cloos et al. (2024)" = cloos_combined)

plt <- lapply(
  seq_along(data),
  function(i){
    
    dataset_lagged = data[[i]] %>%
      mutate("interval_lagged" = dplyr::lag(time_interval))|>
      filter(!is.na(interval_lagged) & !is.na(time_interval))|>
      mutate(condition = factor(condition, levels = c("shortinterval", "longinterval")))
    
    plot <- ggplot(dataset_lagged, aes(x = time_interval, fill = condition))+
      scale_x_continuous(limits = c(-20,270), breaks = seq(0, 250, by = 50), expand = c(0,0))+
      geom_histogram(aes(color = condition), 
                     binwidth = 5, position = "identity", center = 2.5 )+
      scale_color_discrete(palette = c("#716080", "#4E7147"))+
      scale_fill_discrete(palette = c("#9888A5", "#889c84"),
                          labels = c("Short interval", "Long interval"))
    
    build = ggplot_build(plot)$data[[1]]
    max_count_short = max(build$count[build$group == 1])
    max_count_long = max(build$count[build$group == 2])
    max_count = max(build$count)
    max_limit = max_count + max_count/2
    yticks = case_when(i == 1 ~ c(0, 2500, 5000, 7500),
                       i == 2 ~ c(0, 200, 400, 600),
                       i == 3 ~ c(0, 300, 600, 900))
    
    plot <- plot +
      scale_y_continuous(limits = c(0, max_limit), expand = c(0,0), breaks = yticks)+
      geom_segment(x = mean(dataset_lagged$time_interval[dataset_lagged$condition == "shortinterval"]),
                   y = 0, yend = max_count_short, linewidth = 0.7, col = "white", lty = "11" )+
      geom_segment(x = mean(dataset_lagged$time_interval[dataset_lagged$condition == "longinterval"]),
                   y = 0, yend = max_count_long, linewidth = 0.7, col = "white", lty = "11" )+
      annotate("richtext",
               label = paste("<i>M</i> = ", round(mean(dataset_lagged$time_interval[dataset_lagged$condition == "shortinterval"]), 1)),
               colour = "#716080",
               label.colour = "white",
               #fill = NA,
               fontface = 2,
               x = mean(dataset_lagged$time_interval[dataset_lagged$condition == "shortinterval"]),
               y = max_count_short + max_count/4)+
      annotate("richtext",
               label = paste("<i>M</i> = ", round(mean(dataset_lagged$time_interval[dataset_lagged$condition == "longinterval"]), 1)),
               colour = "#4E7147",
               label.colour = "white",
               #fill = NA,
               fontface = 2,
               x = mean(dataset_lagged$time_interval[dataset_lagged$condition == "longinterval"]),
               y = max_count_long + max_count/4)+
      guides(color = "none")+
      labs(x = "", 
           y = "Count",
           fill = "Condition",
           title = names(data)[i])+
      theme(
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(
          fill = NA,
          color = "#6b7068",
          linewidth = 1
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          size = 12,
          hjust = 0
        ),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.title = element_text(face = "bold")
      )
      
    if (i == 3){
      plot <- plot +
        labs(x = "Time Interval (Minutes)")+
        theme(
          axis.title.x = element_text(size = 12)
        )
    }
    
    return(plot)
  }
)

time_intervals <- ggarrange(
  plotlist = plt,
  ncol = 1,
  common.legend = TRUE,
  legend = "bottom",
  align = "v",
  heights = c(0.85, 0.85, 1)
)

ggsave(
  file.path("manuscript", "fig2_time_intervals.jpg"),
  time_intervals,
  width = 500*3,
  height = 600*3,
  unit = "px"
)

