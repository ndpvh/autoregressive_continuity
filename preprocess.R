################################################################################
# EGON
################################################################################

library(tidyverse)

# Read in the dataset
data <- data.table::fread(
    file.path("data", "DEJONCKHEERE_unprocessed.csv"),
    data.table = FALSE
)

# Select only the columns that we are interested in
data <- data %>% 
    dplyr::select(
        connectionId,
        questionListName,
        timeStampStart, 
        timeStampStop,
        ontspannen_sliderNegPos:angstig_sliderNegPos
    ) %>%
    dplyr::rename(
        id = connectionId, 
        condition = questionListName, 
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

# Compute time intervals per person per beep. Importantly, NAs should be 
# accounted for as well in consecutive time intervals: They will be deleted 
# afterwards
data %>% 
    dplyr::group_by(id) %>% 
    dplyr::arrange(time_start) %>% 
    dplyr::mutate(
        time_interval = time_stop - lag(time_stop)
    ) %>% 
    dplyr::relocate(
        id:time_stop, 
        time_interval
    ) %>% View()

