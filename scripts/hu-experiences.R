library(tidyverse)
library(cowplot)
library(randomForest)
library(viridis)
library(paletteer)
library(ggthemes)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./scripts/modules/imputing.R')

theme_update(text = element_text(size = 20))

lightest_grey <- "#d9d9d9"
light_grey <- "#adadad"
dark_grey <- "#303030"
darkest_grey <- "#212121"

ggplot <- function(...) ggplot2::ggplot(...) + 
    scale_color_paletteer_d("ggsci::nrc_npg") +
    scale_fill_paletteer_d("ggsci::nrc_npg")

set.seed(42)

main <- function() {

    get_study_data() -> study_data

    study_data %>% select_using_hu_and_clean() -> study_data_using_hu

    study_data %>% select_not_using_hu_and_clean() -> study_data_not_using_hu

    study_data_using_hu %>% impute_missing_values() -> study_data_using_hu_imputed

    study_data_not_using_hu %>% impute_missing_values() -> study_data_not_using_hu_imputed

    rbind(study_data_using_hu_imputed, study_data_not_using_hu_imputed) -> study_data_imputed

    return(study_data_imputed)
}

create_n_complications_columns <- function(input_data) {

    input_data %>%
        mutate(n_complications_before = 
            ifelse(bone_pain1=="Yes", 1, 0) +
            ifelse(chest_pain1=="Yes", 1, 0) +
            ifelse(leg_ulcer1=="Yes", 1, 0) +
            ifelse(avn1=="Yes", 1, 0) +
            ifelse(stroke1=="Yes", 1, 0) +
            ifelse(priapism1=="Yes", 1, 0) +
            ifelse(jaundice1=="Yes", 1, 0) +
            ifelse(abnormal_appetite1=="Yes", 1, 0)) %>%
        mutate(n_complications_after = 
            ifelse(bone_pain2=="Yes", 1, 0) +
            ifelse(chest_pain2=="Yes", 1, 0) +
            ifelse(leg_ulcer2=="Yes", 1, 0) +
            ifelse(avn2=="Yes", 1, 0) +
            ifelse(stroke2=="Yes", 1, 0) +
            ifelse(priapism2=="Yes", 1, 0) +
            ifelse(jaundice2=="Yes", 1, 0) +
            ifelse(abnormal_appetite1=="Yes", 1, 0)) %>%
        return()
}

plot_complications_before_and_after <- function(input_data) {

    input_data %>%
        ggplot(aes(x=n_complications_before ,y=n_complications_after)) + 
            geom_jitter(width=0.2,height=0.2, colour=1) + 
            geom_abline(colour=2) +
            xlab("number of complications before hu treatment") +
            ylab("number of complications during hu treatment") %>%
    return()
}

plot_complications_before_and_after_boxplot <- function(input_data) {

    input_data %>%
        pivot_longer(cols=c(
            n_complications_before,
            n_complications_after),
            names_to = "complications_time",
            values_to = "number_complications") %>%
        mutate(complications_time = ifelse(complications_time == 'n_complications_before', 'before', 'during')) %>%
        ggplot(aes(
            x=factor(complications_time, level=c('before','during')),
            fill=factor(complications_time, level=c('before','during')), 
            y=number_complications)) +
        geom_boxplot() +
        theme(legend.position = "none") +
        xlab("before or during hydroxyurea treatment") +
        ylab("number of complications") %>%
    return()
}

pivot_complications_column <- function(input_data) {

    input_data %>%
        pivot_longer(cols=c(
            n_complications_before,
            n_complications_after),
            names_to = "complications_time",
            values_to = "number_complications") %>%
        mutate(complications_time = ifelse(complications_time == 'n_complications_before', 'before', 'during')) %>%
        return()
}

model_complications_before_and_after_boxplot <- function(input_data) {

    lm(
        formula=number_complications ~ complications_time + age + gender, 
        data=pivot_complications_column(input_data)) %>%
    summary %>%
    return()
}