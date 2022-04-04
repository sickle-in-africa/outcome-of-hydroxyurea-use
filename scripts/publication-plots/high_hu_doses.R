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
    scale_fill_paletteer_d("ggsci::nrc_npg")
    # theme(
    #     text = element_text(
    #         size = 20,
    #         color=light_grey),
    #     plot.background = element_rect(
    #         fill = darkest_grey,
    #         colour = darkest_grey),
    #     panel.background = element_rect(
    #         fill = dark_grey,
    #         colour = dark_grey,
    #         size = 0.5,
    #         linetype = "solid"),
    #     panel.grid.major = element_line(
    #         size = 0.5, linetype = 'solid',
    #         colour = dark_grey), 
    #     panel.grid.minor = element_line(
    #         size = 0.25,
    #         linetype = 'solid',
    #         colour = dark_grey),
    #     axis.text = element_text(
    #         color=light_grey),
    #     legend.background = element_rect(
    #         fill = darkest_grey, 
    #         colour = "transparent"))

media_directory_path <- '/home/jack/computer/genemap/hydroxyurea-study/media/publication-ready-plots/light-theme/high-hu-doses/'

update_complications_column <- function(input_data) {

    input_data %>%
        mutate(name = ifelse(
            grepl("bone_pain",name),"bone pain", ifelse(
                grepl("chest_pain",name), "chest pain", ifelse(
                    grepl("leg_ulcer",name), "leg ulcer", ifelse(
                        grepl("avn",name), "osteonecrosis", ifelse(
                            grepl("stroke",name), "stroke", ifelse(
                                grepl("priapism",name), "priapism", ifelse(
                                    grepl("jaundice",name),"jaundice",ifelse(
                                        grepl("appetite",name),"abnormal appetite","NA"))))))))) %>%
        return()
}

get_study_data() -> study_data

study_data %>% select_using_hu_and_clean() -> study_data_using_hu
study_data %>% select_not_using_hu_and_clean() -> study_data_not_using_hu
study_data_using_hu %>% impute_missing_values() -> study_data_using_hu_imputed
study_data_not_using_hu %>% impute_missing_values() -> study_data_not_using_hu_imputed
rbind(study_data_using_hu_imputed, study_data_not_using_hu_imputed) -> study_data_imputed

study_data_imputed %>% 
    filter(age >= 10 & age <=30) %>% 
    mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) -> 
    high_current_dose

study_data_imputed %>% plot_unsupervised_mds_scatter

study_data_imputed %>%
    filter(currently_using_hu==TRUE) %>%
    ggplot(aes(x=age, y=current_hu_dose)) + 
        geom_point(colour=lightest_grey) + 
        geom_smooth() +
        xlab("age (years)") +
        ylab("current hu dose (mg)") +
        theme(
            panel.grid.major = element_line(
                size = 0.5, linetype = 'solid',
                colour = darkest_grey))
ggsave(paste(media_directory_path, "age_current_dose.png", sep=""),dpi=300)

study_data_imputed %>%
    filter(currently_using_hu==TRUE) %>%
    filter(age <= 30) %>%
    ggplot(aes(x=age, y=current_hu_dose)) + 
        geom_point(colour=lightest_grey) + 
        geom_smooth() +
        xlab("age (years)") +
        ylab("current hu dose (mg)") +
        theme(
            panel.grid.major = element_line(
                size = 0.5, linetype = 'solid',
                colour = darkest_grey))
ggsave(paste(media_directory_path, "age_current_dose_under_30.png", sep=""),dpi=300)

study_data_imputed %>%
    filter(currently_using_hu==TRUE) %>%
    filter(age <= 30) %>%
    ggplot(aes(x=age, y=current_hu_dose)) + 
        geom_point(colour=lightest_grey) + 
        geom_smooth() + 
        facet_wrap(~ gender) +
        xlab("age (years)") +
        ylab("current hu dose (mg)") +
        theme(
            panel.grid.major = element_line(
                size = 0.5, linetype = 'solid',
                colour = darkest_grey))
ggsave(paste(media_directory_path, "age_current_dose_gender.png", sep=""),dpi=300)

study_data_imputed %>%
    filter(currently_using_hu==TRUE) %>%
    filter(age >= 10 & age <=30) %>% 
    mutate(high_current_dose=ifelse(current_hu_dose>=1200,TRUE,FALSE)) %>% 
    ggplot(aes(x=age,y=current_hu_dose, color=high_current_dose)) + 
        geom_point(colour=lightest_grey) +
        theme(legend.position = "none") +
        xlab("age (years)") +
        ylab("current hu dose (mg)") +
        theme(
            panel.grid.major = element_line(
                size = 0.5, linetype = 'solid',
                colour = darkest_grey))
ggsave(paste(media_directory_path, "age_current_dose_bet_10_and_30.png", sep=""),dpi=300)

study_data_imputed %>%
    filter(age >= 10 & age <=30) %>% 
    mutate(high_current_dose=ifelse(current_hu_dose>=1200,"> 1200","< 1200")) %>% 
    ggplot(aes(x=high_current_dose,fill=high_current_dose,y=freq_pain_before_hu)) + 
        geom_boxplot(colour=lightest_grey) +
        theme(legend.position = "none") +
        xlab("current hu dose (mg)") +
        ylab("pain crises per year")
ggsave(paste(media_directory_path, "high_current_dose_pain_crises.png", sep=""),dpi=300)

study_data_imputed %>%
    filter(age >= 10 & age <=30) %>% 
    mutate(high_current_dose=ifelse(current_hu_dose>=1200,"> 1200","< 1200")) %>% 
    ggplot(aes(x=high_current_dose,fill=high_current_dose,y=freq_hosp_adm_before_hu)) + 
        geom_boxplot(colour=lightest_grey) +
        theme(legend.position = "none") +
        xlab("current hu dose (mg)") +
        ylab("hospitalisations per year")
ggsave(paste(media_directory_path, "high_current_dose_hospitalisations.png", sep=""),dpi=300)

high_current_dose %>% filter(high_current_dose==TRUE) %>% nrow -> n_high_current_dose
high_current_dose %>% filter(high_current_dose==FALSE) %>% nrow -> n_low_current_dose

high_current_dose %>% 
    group_by(high_current_dose, chest_pain1) %>%
    mutate(chest_pain1= tolower(chest_pain1)) %>%
    summarise(count=n()) %>% 
    mutate(norm_count=ifelse(high_current_dose,count*100/n_high_current_dose,count*100/n_low_current_dose)) %>% 
    mutate(high_current_dose=ifelse(high_current_dose==FALSE,"> 1200","< 1200")) %>%
    ggplot(aes(x=high_current_dose,fill=chest_pain1,y=norm_count)) + 
        geom_bar(stat="identity") +
        theme(
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank()
            ) +
        xlab("current hu dose (mg)") +
        ylab("percentage of participants") +
        guides(fill=guide_legend(title="chest pain?"))
ggsave(paste(media_directory_path, "high_current_dose_chest_pain.png", sep=""),dpi=300)

n_using_hu <- study_data_imputed %>% filter(currently_using_hu==TRUE) %>% nrow
n_stopped_hu <- study_data_imputed %>% filter(currently_using_hu==FALSE) %>% nrow

study_data_imputed %>%
    pivot_longer(cols=c(
        bone_pain1,
        chest_pain1,
        leg_ulcer1,
        avn1,
        stroke1,
        priapism1,
        jaundice1,
        abnormal_appetite1),
        values_to="response") %>% 
    group_by(currently_using_hu, name, response) %>% 
    summarise(count=n()) %>%
    mutate(response= tolower(response)) %>%
    mutate(norm_count = ifelse(currently_using_hu==TRUE,count*100/n_using_hu,count*100/n_stopped_hu)) %>%
    mutate(name=as.factor(name)) %>%
    mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently using hu","stopped using hu")) %>%
    update_complications_column() %>%
    ggplot(aes(x=name, fill=response, y=norm_count)) +
        geom_bar(stat="identity") +
        facet_wrap(~currently_using_hu) +
        theme(
            axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank()) +
            ) +
        xlab("") +
        ylab("percentage of participants") +
        guides(fill=guide_legend(title=element_blank()))
ggsave(paste(media_directory_path, "complications_before_hu.png", sep=""),dpi=300)


study_data_imputed %>%
    pivot_longer(cols=c(
        bone_pain2,
        chest_pain2,
        leg_ulcer2,
        avn2,
        stroke2,
        priapism2,
        jaundice2,
        abnorm_appetite2),
        values_to="response") %>% 
    group_by(currently_using_hu, name, response) %>%
    mutate(response= tolower(response)) %>%
    summarise(count=n()) %>%
    mutate(norm_count = ifelse(currently_using_hu==TRUE,count*100/n_using_hu,count*100/n_stopped_hu)) %>%
    mutate(name=as.factor(name)) %>%
    mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently using hu","stopped using hu")) %>%
    update_complications_column() %>%
    ggplot(aes(x=name, fill=response, y=norm_count)) +
        geom_bar(stat="identity") +
        facet_wrap(~currently_using_hu) +
        theme(
            axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank()) +
            ) +
        xlab("") +
        ylab("percentage of participants") +
        guides(fill=guide_legend(title=element_blank()))
ggsave(paste(media_directory_path, "complications_after_hu.png", sep=""),dpi=300)