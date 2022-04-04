library(tidyverse)
library(cowplot)
library(randomForest)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./scripts/modules/imputing.R')

main <- function() {

    set.seed(42)

    get_study_data() -> study_data

    study_data %>% select_not_using_hu_and_clean() -> study_data_clean

    study_data_clean %>% impute_missing_values() -> study_data_imputed

    return(study_data_imputed)
}



clean_and_simplify <- function(input_data) {

    input_data %>%
        update_gender_column() %>%
        update_marital_status_column() %>%
        update_using_hu_column() %>%
        filter(using_hu==3L) %>%
        update_hu_stopped_reasons_columns() %>%
        mutate(hu_is_available = as.factor(!as.logical(available_pharmacies))) %>%
        select(
            age,
            gender,
            marital_status,
            hosp_name,
            using_hu,
            side_effect,
            cost,
            requires_test,
            available_pharmacies,
            doctor_stopped_use) %>%
        return()
}

plot_reasons_hu_stop_bar <- function(input_data) {

    study_data_imputed %>% 
        pivot_longer(cols=c(
            hu_is_available,
            hu_is_affordable,
            hu_is_prescribed,
            hu_has_no_side_effects), names_to='reason', values_to='response') %>% 
        select(reason, response) %>% group_by(reason,response) %>% 
        summarise(count=n()) %>% 
        mutate(response=as.factor(response)) %>% 
        ggplot(aes(x=reason,y=count,fill=response)) + 
            geom_bar(position="dodge", stat="identity") + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
}

plot_hosp_name_vs_hu_available_scatter <- function(input_data) {

    study_data_imputed %>% 
        ggplot(aes(x=hosp_name, y=hu_is_available, fill=hosp_name)) + 
            geom_jitter(width=0.15,height=0.15) + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none") %>%
        return()
}

plot_complications_bar <- function(input_data) {

    input_data %>%
        pivot_longer(cols=c(
            bone_pain1,
            chest_pain1,
            leg_ulcer1,
            avn1,
            stroke1,
            priapism1,
            jaundice1,
            abnormal_appetite1)) %>% 
        group_by(name, value) %>% 
        summarise(count=n()) %>% 
        ggplot(aes(x=name, fill=value, y=count)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()
}
