library(tidyverse)
library(cowplot)
library(randomForest)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./scripts/modules/imputing.R')

main <- function() {

    get_study_data() -> study_data

    study_data %>% select_using_hu_and_clean() -> study_data_using_hu

    study_data %>% select_not_using_hu_and_clean() -> study_data_not_using_hu

    study_data_using_hu %>% impute_missing_values() -> study_data_using_hu_imputed

    study_data_not_using_hu %>% impute_missing_values() -> study_data_not_using_hu_imputed

    rbind(study_data_using_hu_imputed, study_data_not_using_hu_imputed) -> study_data_imputed

    return(study_data_imputed)
}

plot_side_effects_stack <- function(input_data) {

    input_data %>% filter(currently_using_hu==TRUE) %>% nrow -> n_currently_using_hu
    input_data %>% filter(currently_using_hu==FALSE) %>% nrow -> n_stopped_using_hu

    input_data %>% 
        group_by(currently_using_hu, hu_has_no_side_effects) %>% 
        summarise(count=n()) %>% 
        mutate(norm_count=ifelse(currently_using_hu==TRUE,count/n_currently_using_hu,count/n_stopped_using_hu)) %>% 
        ggplot(aes(x=currently_using_hu,fill=hu_has_no_side_effects,y=norm_count)) + 
            geom_bar(stat="identity") %>%
        return()
}

plot_start_vs_current_hu_dose_scatter <- function(input_data) {

    input_data %>% 
        ggplot(aes(x=start_hu_dose,y=current_hu_dose)) + 
            geom_point() + 
            geom_abline()
}

plot_number_side_effects_age_scatter <- function(input_data) {

    input_data %>%
        create_number_side_effects_column() %>%
        ggplot(aes(x=age, y=number_side_effects)) +
            geom_point() %>%
        return()
}

create_number_side_effects_column <- function(input_data) {

    input_data %>%
        mutate(number_side_effects = 
            as.logical(headaches) +
            as.logical(dizziness.x) +
            as.logical(nausea) +
            as.logical(vomiting) +
            as.logical(stomach_upset) +
            as.logical(diarrhea) +
            as.logical(constipation) +
            as.logical(skin_changes) +
            as.logical(flu_symptoms) +
            as.logical(low_blood_count)) %>%
        return()
}

main_old <- function() {

    get.study.data() -> study.data

    study.data %>% clean.and.simplify() -> study.data.clean

    study.data.clean %>% impute.missing.values() -> study.data.imputed

    study.data.imputed %>% pivot.around.reported.symptoms() -> study.data.long

    study.data.long %>% 
        group_by(reason,response) %>%
        summarise(count=n()) %>% 
        ggplot(aes(x=reason,fill=response,y=count)) + 
            geom_bar(position="dodge", stat="identity") + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

    n_using_hu <- study.data.imputed %>% filter(currently_using_hu==TRUE) %>% nrow
    n_stopped_hu <- study.data.imputed %>% filter(currently_using_hu==FALSE) %>% nrow

    study.data.long %>% 
        group_by(reason,response, currently_using_hu) %>%
        summarise(count=n()) %>%
        mutate(norm_count = ifelse(currently_using_hu==TRUE,count/n_using_hu,count/n_stopped_hu)) %>%
        ggplot(aes(x=reason,fill=response,y=norm_count)) + 
            geom_bar(position="stack", stat="identity") + 
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

    glm(currently_using_hu ~ ., study.data.imputed, family="binomial") %>% summary

    glm(currently_using_hu ~ headaches+dizziness.x+nausea+vomiting+stomach_upset+diarrhea+constipation+skin_changes+flu_symptoms+low_blood_count, study.data.imputed, family="binomial") %>% summary

}

clean.and.simplify <- function(input.data) {

    input.data %>%
    filter(using_hu != 2) %>%
    filter(scd_test_result_ss_sbthal==1L) %>%
    update_gender_column()  %>% 
    update_hu_side_effects_columns() %>%
    create_currently_using_hu_column() %>%
    select(
        gender,
        age,
        hosp_name,
        headaches,
        dizziness.x,
        nausea,
        vomiting,
        stomach_upset,
        diarrhea,
        constipation,
        skin_changes,
        flu_symptoms,
        low_blood_count,
        currently_using_hu) %>%
    return()
}

impute.missing.values <- function(input.data) {

    rfImpute(gender ~ ., input.data, iter=6) %>% 
        as_tibble %>% 
        return()
}

pivot.around.reported.symptoms <- function(input.data) {

    input.data %>%
        pivot_longer(cols=c(
            headaches,
            dizziness.x,
            nausea,
            vomiting,
            stomach_upset,
            diarrhea,
            constipation,
            skin_changes,
            flu_symptoms,
            low_blood_count), names_to='reason', values_to='response') %>%
        mutate(reason=as.factor(reason)) %>%
        mutate(response=as.factor(response)) %>%
        return()
}