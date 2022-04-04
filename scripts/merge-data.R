library(tidyverse)

survey <- read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE) %>% as_tibble

registry <- read.csv('./data/SPARCO DATABASE_HU EXTRACT.csv', header=TRUE) %>% as_tibble %>% filter(redcap_repeat_instrument != 'patient_follow_up')

studyData <- as_tibble(merge(survey, registry, by.x='patient_record_id', by.y='record_id'))

