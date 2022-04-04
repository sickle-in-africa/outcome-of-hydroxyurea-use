get_study_data <- function() {

    read.csv('/home/jack/computer/genemap/hydroxyurea-study/data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE) %>% 
        as_tibble -> survey
    read.csv('/home/jack/computer/genemap/hydroxyurea-study/data/SPARCO DATABASE_HU EXTRACT.csv', header=TRUE) %>% 
        as_tibble %>% 
        filter(redcap_repeat_instrument != 'patient_follow_up') -> registry

    merge(survey, registry, by.x='patient_record_id', by.y='record_id') %>%
        as_tibble %>%
        return()
}