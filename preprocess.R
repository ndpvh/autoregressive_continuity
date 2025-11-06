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
        ontspannen = ontspannen_sliderNegPos, 
        gefrustreerd = gefrustreerd_sliderNegPos,
        droevig = droevig_sliderNegPos, 
        rustig = rustig_sliderNegPos,
        gestresseerd = gestresseerd_sliderNegPos,
        enthousiast = enthousiast_sliderNegPos,
        verveeld = verveeld_sliderNegPos,
        neerslachtig = neerslachtig_sliderNegPos,
        blij = blij_sliderNegPos,
        opgewekt = opgewekt_sliderNegPos,
        tevreden = tevreden_sliderNegPos,
        angstig = angstig_sliderNegPos
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

