library(tidyverse)

survey <- as_tibble(read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE))
registry <- read.csv('./data/SPARCO DATABASE_HU EXTRACT.csv', header=TRUE) %>% as_tibble %>% filter(redcap_repeat_instrument != 'patient_follow_up') %>% mutate(gender=as.factor(gender))
studyData <- as_tibble(merge(survey, registry, by.x='patient_record_id', by.y='record_id'))

currently_using_hu <- studyData %>% filter(using_hu == 1)

currently_using_hu <- currently_using_hu %>%
    mutate(dose_specify1 = as.character(dose_specify1)) %>%
    mutate(dose_specify1 = ifelse(str_detect(dose_specify1, "Can't remember around 500-600"), NA, dose_specify1)) %>%
    mutate(dose_specify1 = ifelse(str_detect(dose_specify1, "Compounded"), NA, dose_specify1)) %>%
    mutate(dose_specify1 = ifelse(str_detect(dose_specify1, "5ml"), NA, dose_specify1)) %>%
    mutate(dose_specify1 = str_replace(dose_specify1, "mg", "")) %>%
    mutate(dose_specify1 = as.numeric(dose_specify1))

currently_using_hu <- currently_using_hu %>%
    mutate(start_hu_dose = ifelse(start_hu_dose==1, 250, start_hu_dose)) %>%
    mutate(start_hu_dose = ifelse(start_hu_dose==2, 500, start_hu_dose)) %>%
    mutate(start_hu_dose = ifelse(start_hu_dose==3, 1000, start_hu_dose)) %>%
    mutate(start_hu_dose = ifelse(start_hu_dose==4, 1500, start_hu_dose)) %>%
    mutate(start_hu_dose = ifelse(start_hu_dose==5, dose_specify1, start_hu_dose))

currently_using_hu <- currently_using_hu %>%
    mutate(dose_specify2 = as.character(dose_specify2)) %>%
    mutate(dose_specify2 = str_replace(dose_specify2, "500mg [+]100mg=600mg", "600mg")) %>%
    mutate(dose_specify2 = ifelse(str_detect(dose_specify2, "250-300mg"), NA, dose_specify2)) %>%
    mutate(dose_specify2 = ifelse(str_detect(dose_specify2, "5ml"), NA, dose_specify2)) %>%
    mutate(dose_specify2 = str_replace(dose_specify2, "mg", "")) %>%
    mutate(dose_specify2 = as.numeric(dose_specify2))

currently_using_hu <- currently_using_hu %>%
    mutate(current_hu_dose = ifelse(current_hu_dose==1, 250, current_hu_dose)) %>%
    mutate(current_hu_dose = ifelse(current_hu_dose==2, 500, current_hu_dose)) %>%
    mutate(current_hu_dose = ifelse(current_hu_dose==3, 1000, current_hu_dose)) %>%
    mutate(current_hu_dose = ifelse(current_hu_dose==4, 1500, current_hu_dose)) %>%
    mutate(current_hu_dose = ifelse(current_hu_dose==5, dose_specify2, current_hu_dose))

currently_using_hu %>% ggplot + geom_point(aes(x=start_hu_dose,y=current_hu_dose)) + geom_abline(intercept = 0, slope = 1)
ggsave('./media/start-current_hudose.png',dpi=300)

currently_using_hu %>% mutate(hu_dose_difference = current_hu_dose - start_hu_dose) %>% ggplot + geom_histogram(aes(hu_dose_difference)) + geom_vline(xintercept = 0)
ggsave('./media/count-differential-hudose.png',dpi=300)

currently_using_hu %>% mutate(headaches = as.factor(headaches)) %>% ggplot + geom_jitter(aes(x=start_hu_dose,y=current_hu_dose,color=headaches)) + geom_abline(intercept = 0, slope = 1)
ggsave('./media/start-current_hudose-headaches.png',dpi=300)

currently_using_hu %>% mutate(hu_not_expensive = as.factor(hu_not_expensive)) %>% ggplot + geom_jitter(aes(x=start_hu_dose,y=current_hu_dose,color=hu_not_expensive)) + geom_abline(intercept = 0, slope = 1)
ggsave('./media/start-current_hudose-not-expensive.png',dpi=300)
