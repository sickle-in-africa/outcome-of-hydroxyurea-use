library(tidyverse)
library(cowplot)
library(randomForest)
library(viridis)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./scripts/modules/imputing.R')

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

plot_reasons_hu_use_bar <- function(input_data) {

    n_using_hu <- input_data %>% filter(currently_using_hu==TRUE) %>% nrow
    n_stopped_hu <- input_data %>% filter(currently_using_hu==FALSE) %>% nrow

    input_data %>% 
        pivot_longer(cols=c(
            hu_is_available,
            hu_is_affordable,
            hu_is_prescribed,
            hu_has_no_side_effects), names_to='reason', values_to='response') %>% 
        group_by(reason, response, currently_using_hu) %>% 
        summarise(count=n()) %>% 
        mutate(response=as.factor(response)) %>%
        mutate(norm_count = ifelse(currently_using_hu==TRUE,count/n_using_hu,count/n_stopped_hu)) %>%
        mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently_using","stopped_using")) %>%
        ggplot(aes(x=reason,y=norm_count,fill=response)) + 
            geom_bar(position="stack", stat="identity") + 
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.6), axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
            ylab('') +
            xlab('')
}

plot_start_vs_current_hu_dose_scatter <- function(input_data) {

    input_data %>%
        ggplot(aes(x=start_hu_dose,y=current_hu_dose, color=hu_is_prescribed)) + 
            geom_point() + 
            geom_abline(slope=1,intercept=0)
}

plot_normalised_dose_difference_violin <- function(input_data) {

    input_data %>%
        mutate(norm_dose_difference = dose_difference/start_hu_dose) %>% 
        ggplot(aes(x=hu_is_affordable,y=norm_dose_difference)) + 
            geom_violin() %>%
        return()
}

build_reasons_hu_use_model <- function(input_data) {

    formula = 
        currently_using_hu ~ 
            hu_is_affordable + 
            hu_is_available + 
            hu_is_prescribed +
            hu_has_no_side_effects +
            age +
            gender +
            marital_status

    glm(
        formula=formula,
        data=input_data,
        family="binomial") %>%
        return()
}

plot_hosp_name_vs_hu_available_scatter <- function(input_data) {

    input_data %>% 
        ggplot(aes(x=hosp_name, y=hu_is_available, fill=hosp_name)) + 
            geom_jitter(width=0.15,height=0.15) + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.7),legend.position = "none") +
            xlab('hospital name') %>%
        return()
}

plot_hosp_name_vs_hu_available_bar <- function(input_data) {

    study_data_imputed %>% 
        group_by(hosp_name, hu_is_available) %>% 
        summarise(count=n()) %>% 
        ggplot(aes(x=hosp_name, y=count, fill=hu_is_available)) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()

}

plot_reported_side_effects_bar <- function(input_data) {

    input_data %>% 
        pivot_around_reported_symptoms() %>%
        group_by(reason,response) %>%
        summarise(count=n()) %>% 
        ggplot(aes(x=reason,fill=response,y=count)) + 
            geom_bar(position="dodge", stat="identity") + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()
}

plot_reported_side_effects_stack <- function(input_data) {

    n_using_hu <- input_data %>% filter(currently_using_hu==TRUE) %>% nrow
    n_stopped_hu <- input_data %>% filter(currently_using_hu==FALSE) %>% nrow

    input_data %>% 
        pivot_around_reported_symptoms() %>%
        group_by(reason,response, currently_using_hu) %>%
        summarise(count=n()) %>%
        mutate(norm_count = ifelse(currently_using_hu==TRUE,count/n_using_hu,count/n_stopped_hu)) %>%
        ggplot(aes(x=reason,fill=response,y=norm_count)) + 
            geom_bar(position="stack", stat="identity") + 
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()
}

build_reported_side_effects_model <- function(input_data) {

    formula <- 
        currently_using_hu ~
            headaches +
            dizziness.x +
            nausea +
            vomiting +
            stomach_upset +
            diarrhea +
            constipation +
            skin_changes +
            flu_symptoms +
            low_blood_count +
            age +
            gender

    glm(
        formula=formula,
        data=input_data,
        family="binomial") %>%
        return()      
}

pivot_around_reported_symptoms <- function(input.data) {

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

plot_unsupervised_mds_scatter <- function(input_data) {

    input_data %>%
        build_unsupervised_random_forest() %>%
        perform_multidimensional_scaling() %>%
        ggplot(aes(x=PC1, y=PC2, label=Sample)) + 
            geom_point()
}

plot_supervised_mds_scatter <- function(input_data) {

    input_data %>%
        #filter(norm_dose_difference <= 8.0) %>%
        data.frame %>%
        as_tibble -> input_data_copy

    input_data_copy$Sample <- rownames(input_data_copy)

    input_data_copy %>%
        build_supervised_random_forest() %>%
        perform_multidimensional_scaling() -> mds_data

    merge(input_data_copy,mds_data, by="Sample") %>%
        as_tibble %>%
        ggplot(aes(x=PC1, y=PC2, color=norm_dose_difference)) + 
            geom_point() +
            scale_color_viridis(option = "A")
}

build_unsupervised_random_forest <- function(input_data) {

    randomForest(x=input_data, data=input_data, proximity=TRUE) %>%
        return()
}

build_supervised_random_forest <- function(input_data) {

    input_data %>%
        select(age,
            gender,
            marital_status,
            hosp_name,
            hu_is_available,
            hu_is_affordable,
            hu_is_prescribed,
            hu_has_no_side_effects,
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
            norm_dose_difference) -> data_subset

    randomForest(norm_dose_difference ~ ., data=data_subset, proximity=TRUE) %>%
        return()
}

perform_multidimensional_scaling <- function(input_random_forest) {

    distance.matrix <- as.dist(1-input_random_forest$proximity)

    mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

    mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

    mds.values <- mds.stuff$points

    data.frame(Sample=rownames(mds.values),
        PC1=mds.values[,1],
        PC2=mds.values[,2]) %>%
        as_tibble %>%
        return()
}

plot_complications_before_bar <- function(input_data) {

    n_using_hu <- input_data %>% filter(currently_using_hu==TRUE) %>% nrow
    n_stopped_hu <- input_data %>% filter(currently_using_hu==FALSE) %>% nrow

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
        group_by(currently_using_hu, name, value) %>% 
        summarise(count=n()) %>%
        mutate(norm_count = ifelse(currently_using_hu==TRUE,count/n_using_hu,count/n_stopped_hu)) %>%
        mutate(name=as.factor(name)) %>%
        ggplot(aes(x=name, fill=value, y=norm_count)) +
            geom_bar(stat="identity") +
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()
}

plot_complications_after_bar <- function(input_data) {

    n_using_hu <- input_data %>% filter(currently_using_hu==TRUE) %>% nrow
    n_stopped_hu <- input_data %>% filter(currently_using_hu==FALSE) %>% nrow

    input_data %>%
        pivot_longer(cols=c(
            bone_pain2,
            chest_pain2,
            leg_ulcer2,
            avn2,
            stroke2,
            priapism2,
            jaundice2,
            abnorm_appetite2)) %>% 
        group_by(currently_using_hu, name, value) %>% 
        summarise(count=n()) %>%
        mutate(norm_count = ifelse(currently_using_hu==TRUE,count/n_using_hu,count/n_stopped_hu)) %>%
        mutate(name=as.factor(name)) %>%
        ggplot(aes(x=name, fill=value, y=norm_count)) +
            geom_bar(stat="identity") +
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()
}

plot_complications_circular_bar <- function(input_data) {

    input_data %>%
        pivot_longer(cols=c(
            bone_pain1,
            chest_pain1,
            leg_ulcer1,
            avn1,
            stroke1,
            priapism1,
            jaundice1,
            abnormal_appetite1,
            bone_pain2,
            chest_pain2,
            leg_ulcer2,
            avn2,
            stroke2,
            priapism2,
            jaundice2,
            abnorm_appetite2)) %>% 
        group_by(name, value) %>% 
        summarise(count=n()) %>% 
        ggplot(aes(x=name, fill=value, y=count)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) %>%
        return()
}

plot_freq_pain_crisis_scatter <- function(input_data) {

    input_data %>%
        ggplot(aes(x=freq_pain_before_hu,y=freq_pain_after_hu,color=currently_using_hu)) + 
            geom_point() + 
            geom_abline(slope=1,intercept=0) %>%
        return()
}

plot_freq_hospitalisation_scatter <- function(input_data) {

    input_data %>%
        ggplot(aes(x=freq_hosp_adm_before_hu,y=freq_hosp_adm_after_hu_2,color=currently_using_hu)) + 
            geom_point() + 
            geom_abline(slope=1,intercept=0) %>%
        return()
}