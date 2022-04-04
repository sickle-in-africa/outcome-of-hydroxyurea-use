library(tidyverse)
library(cowplot)
library(randomForest)
library(viridis)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./scripts/modules/imputing.R')
source('./scripts/modules/learning.R')

get_data_and_tidy <- function() {

    get_study_data() -> study_data

    study_data %>% select_using_hu_and_clean() -> study_data_using_hu

    study_data %>% select_not_using_hu_and_clean() -> study_data_not_using_hu

    study_data_using_hu %>% impute_missing_values() -> study_data_using_hu_imputed

    study_data_not_using_hu %>% impute_missing_values() -> study_data_not_using_hu_imputed

    rbind(study_data_using_hu_imputed, study_data_not_using_hu_imputed) -> study_data_imputed

    return(study_data_imputed)
}

plot_age_current_dose_point <- function(input_data) {

    input_data %>%
        filter(currently_using_hu==TRUE) %>%
        filter(age <= 30) %>%
        ggplot(aes(x=age, y=current_hu_dose)) + 
            geom_point() + 
            geom_smooth()
}

plot_high_current_dose_point <- function(input_data) {

    input_data %>%
        filter(age >= 10 & age <=30) %>% 
        mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) %>% 
        ggplot(aes(x=age,y=current_hu_dose, color=high_current_dose)) + 
            geom_point() %>%
        return()
}

plot_high_current_dose_boxplot <- function(input_data) {

    input_data %>%
        filter(age >= 10 & age <=30) %>% 
        mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) %>% 
        ggplot(aes(x=high_current_dose,y=freq_pain_before_hu)) + 
            geom_boxplot() %>%
        return()
}

build_high_current_dose_model <- function(input_data) {

    predictor <- freq_pain_before_hu

    input_data %>% 
        filter(age >= 10 & age <=30) %>% 
        mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) -> 
        high_current_dose

    glm(
        high_current_dose ~ freq_pain_before_hu + age + gender, 
        data=high_current_dose, 
        family="binomial") %>% 
        summary %>%
        return()
}

build_high_current_dose_model_table <- function(input_data) {

    input_data %>%
        select(
            age,
            gender,
            freq_pain_before_hu,
            freq_hosp_adm_before_hu,
            current_hu_dose) %>%
        filter(age >= 10 & age <=30) %>% 
        mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) %>%
        select(-current_hu_dose) -> 
        high_current_dose

    glm(
        high_current_dose ~ ., 
        data=high_current_dose, 
        family="binomial") %>% 
        summary %>%
        return()
}

plot_high_current_dose_stack <- function(input_data) {

    input_data %>% 
        filter(age >= 10 & age <=30) %>% 
        mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) -> 
        high_current_dose

    high_current_dose %>% filter(high_current_dose==TRUE) %>% nrow -> n_high_current_dose
    high_current_dose %>% filter(high_current_dose==FALSE) %>% nrow -> n_low_current_dose

    high_current_dose %>% 
        group_by(high_current_dose, chest_pain1) %>% 
        summarise(count=n()) %>% 
        mutate(norm_count=ifelse(high_current_dose,count/n_high_current_dose,count/n_low_current_dose)) %>% 
        ggplot(aes(x=high_current_dose,fill=chest_pain1,y=norm_count)) + 
            geom_bar(stat="identity") %>%
        return()
}