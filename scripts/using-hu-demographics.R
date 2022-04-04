library(tidyverse)

survey <- read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE) %>% as_tibble

registry <- read.csv('./data/SPARCO DATABASE_HU EXTRACT.csv', header=TRUE) %>% as_tibble %>% filter(redcap_repeat_instrument != 'patient_follow_up') %>% mutate(gender=as.factor(gender))

studyData <- as_tibble(merge(survey, registry, by.x='patient_record_id', by.y='record_id'))

studyData %>% ggplot + geom_histogram(aes(x=age,fill=gender)) + facet_wrap(~using_hu)
ggsave('./media/using-hu-age-gender.png',dpi=300)

studyData %>% ggplot + geom_density(aes(x=age,y=..density..,fill=gender,color=gender),alpha=0.5) + facet_wrap(~using_hu)
ggsave('./media/using-hu-age-gender-density.png',dpi=300)
