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

plot_age_histogram <- function(input_data) {

    input_data %>% 
        mutate(gender=as.factor(ifelse(gender==1,"M","F"))) %>% 
        select(age,gender) %>% 
        ggplot + 
            geom_histogram(aes(x=age,fill=gender)) %>%
            add_dark_theme() %>%
        return()
}

plot_using_hu_histogram <- function(input_data) {

    input_data %>%
        mutate(using_hu=as.factor(
            ifelse(using_hu==1,"currently_using",
                ifelse(using_hu==2,"never_used","stopped_using")))) %>%
        ggplot(aes(x=using_hu,fill=using_hu)) +
            geom_bar() +
            theme(legend.position = "none") +
            xlab("using hydroxyurea?") %>%
        return()
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
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.4), axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
            ylab('') +
            xlab('')
}

plot_hosp_name_vs_hu_available_scatter <- function(input_data) {

    input_data %>% 
        mutate(hosp_name=
            ifelse(hosp_name=="Jos University Teaching Hospital","Jos University",
                ifelse(hosp_name=="Aminu Kano Teaching Hospital","Aminu Kano",
                    ifelse(hosp_name=="Irua Specialist Teaching Hospital","Irua Specialist",
                        ifelse(hosp_name=="University of Abuja Teaching Hospital, Gwagwalada ", "Abuja, University","Abuja, National"))))) %>%
        ggplot(aes(x=hosp_name, y=hu_is_available, color=hosp_name)) + 
            geom_jitter(width=0.3,height=0.15) + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.7),legend.position = "none") +
            xlab('hospital name') +
            ylab('hydroxyurea is available')%>%
        return()
}

plot_hu_available_vs_hu_presecibed_scatter <- function(input_data) {

    input_data %>% 
         ggplot(aes(x=hu_is_available, y=hu_is_prescribed)) + 
            geom_jitter(width=0.2,height=0.2, colour="#adadad") + 
            theme(legend.position = "none") +
            xlab('hydroxyurea is available') +
            ylab('hydroxyurea is prescribed') %>%
        return()
}

mutate_hosp_name <- function(input_data) {
    
    input_data %>%
        mutate(hosp_name=
        ifelse(hosp_name=="Jos University Teaching Hospital","Jos University",
            ifelse(hosp_name=="Aminu Kano Teaching Hospital","Aminu Kano",
                ifelse(hosp_name=="Irua Specialist Teaching Hospital","Irua Specialist",
                    ifelse(hosp_name=="University of Abuja Teaching Hospital, Gwagwalada ", "Abuja, University","Abuja, National"))))) %>%
        return()
}

plot_using_hu_facet <- function(input_data) {

    input_data %>%
        mutate_hosp_name() %>%
        plot_using_hu_histogram() + 
            facet_wrap(~hosp_name) +
            theme(axis.text.x = element_blank()) + 
            theme(legend.position = "bottomright") %>%
        return()
}

add_dark_theme <- function(input_ggplot) {

    input_ggplot +  theme(
        text = element_text(
            size = 20,
            color="#adadad"),
        plot.background = element_rect(
            fill = "#212121",
            colour = "#212121"),
        panel.background = element_rect(
            fill = "#303030",
            colour = "#303030",
            size = 0.5,
            linetype = "solid"),
        panel.grid.major = element_line(
            size = 0.5, linetype = 'solid',
            colour = "#adadad"), 
        panel.grid.minor = element_line(
            size = 0.25,
            linetype = 'solid',
            colour = "#adadad"),
        axis.text = element_text(
            color="#adadad"),
        legend.background = element_rect(
            fill = "#212121", 
            colour = "transparent")) %>%
        return()
}