library(tidyverse)
library(cowplot)
library(randomForest)
library(paletteer)
library(ggthemes)
library(viridis)

source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/importing.R')
source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/cleaning.R')
source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/imputing.R')
source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/learning.R')

theme_update(text = element_text(size = 20))

lightest_grey <- "#d9d9d9"
light_grey <- "#adadad"
dark_grey <- "#303030"
darkest_grey <- "#212121"

ggplot <- function(...) ggplot2::ggplot(...) + 
    scale_color_paletteer_d("ggsci::nrc_npg") +
    scale_fill_paletteer_d("ggsci::nrc_npg") +  
    theme(
        text = element_text(
            size = 20,
            color=light_grey),
        plot.background = element_rect(
            fill = darkest_grey,
            colour = darkest_grey),
        panel.background = element_rect(
            fill = dark_grey,
            colour = dark_grey,
            size = 0.5,
            linetype = "solid"),
        panel.grid.major = element_line(
            size = 0.5, linetype = 'solid',
            colour = dark_grey), 
        panel.grid.minor = element_line(
            size = 0.25,
            linetype = 'solid',
            colour = dark_grey),
        axis.text = element_text(
            color=light_grey),
        legend.background = element_rect(
            fill = darkest_grey, 
            colour = "transparent"))

media_directory_path <- '/home/jack/computer/genemap/hydroxyurea-study/media/publication-ready-plots/dark-theme/side-effects/'


get_study_data() -> study_data

study_data %>% select_using_hu_and_clean() -> study_data_using_hu
study_data %>% select_not_using_hu_and_clean() -> study_data_not_using_hu
study_data_using_hu %>% impute_missing_values() -> study_data_using_hu_imputed
study_data_not_using_hu %>% impute_missing_values() -> study_data_not_using_hu_imputed
rbind(study_data_using_hu_imputed, study_data_not_using_hu_imputed) -> study_data_imputed

study_data_imputed %>% filter(currently_using_hu==TRUE) %>% nrow -> n_currently_using_hu
study_data_imputed %>% filter(currently_using_hu==FALSE) %>% nrow -> n_stopped_using_hu

study_data_imputed %>%
    mutate(hu_has_no_side_effects = ifelse(hu_has_no_side_effects==TRUE,"yes","no")) %>%
    ggplot(aes(x=hu_has_no_side_effects,fill=hu_has_no_side_effects,y=age)) + 
    geom_boxplot(colour=lightest_grey) +
    theme(legend.position = "none") +
    xlab("does hu have side effects?") +
    ylab("age (years)")
ggsave(paste(media_directory_path, "side_effects_age.png", sep=""),dpi=300)

create_number_side_effects_column <- function(input_data) {

    input_data %>%
        mutate(number_side_effects = as.integer(
            as.logical(headaches) +
            as.logical(dizziness.x) +
            as.logical(nausea) +
            as.logical(vomiting) +
            as.logical(stomach_upset) +
            as.logical(diarrhea) +
            as.logical(constipation) +
            as.logical(skin_changes) +
            as.logical(flu_symptoms) +
            as.logical(low_blood_count))) %>%
        return()
}

study_data_imputed %>%
    create_number_side_effects_column() %>%
    ggplot(aes(x=age, y=number_side_effects)) +
        geom_point(colour=lightest_grey) +
        xlab('age (years)') +
        ylab('number of different side effects experienced') +
        scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
        theme(
            panel.grid.major = element_line(
                size = 0.5, linetype = 'solid',
                colour = darkest_grey))
ggsave(paste(media_directory_path, "age_number_side_effects.png", sep=""),dpi=300)

study_data_imputed %>% 
    ggplot(aes(x=start_hu_dose,y=current_hu_dose)) + 
        geom_abline(colour=darkest_grey, size=1) + 
        geom_point(colour=lightest_grey) + 
        xlab('start hu dose (mg)') +
        ylab('current hu dose (mg)')
ggsave(paste(media_directory_path, "start_vs_current_dose.png", sep=""),dpi=300)

study_data_imputed %>% 
    filter(age <= 18) %>%
    ggplot(aes(x=start_hu_dose,y=current_hu_dose)) + 
        geom_abline(colour=darkest_grey, size=1) + 
        geom_point(colour=lightest_grey) + 
        xlab('start hu dose (mg)') +
        ylab('current hu dose (mg)')
ggsave(paste(media_directory_path, "start_vs_current_dose_under_18.png", sep=""),dpi=300)


study_data_imputed %>% 
    filter(age <= 18) %>%
    mutate(hu_has_no_side_effects = ifelse(hu_has_no_side_effects==TRUE,"yes","no")) %>%
    ggplot(aes(x=hu_has_no_side_effects,fill=hu_has_no_side_effects,y=dose_difference)) + 
        geom_boxplot(colour=lightest_grey) +
        theme(legend.position = "none") +
        xlab("does hu have side effects?") +
        ylab("current dose - start dose (mg)")
ggsave(paste(media_directory_path, "side_effects_dose_difference.png", sep=""),dpi=300)
