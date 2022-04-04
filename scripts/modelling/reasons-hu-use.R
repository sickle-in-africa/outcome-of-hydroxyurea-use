library(tidyverse)
library(cowplot)
library(randomForest)
library(paletteer)
library(ggthemes)
library(viridis)

media_directory_path <- '/home/jack/computer/genemap/hydroxyurea-study/media/publication-ready-plots/dark-theme/reasons-hu-use/'

get_study_data <- function() {

    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/importing.R')
    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/cleaning.R')
    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/imputing.R')
    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/learning.R')

    return(get_study_data())
}

impute_study_data <- function(input_data) {

    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/importing.R')
    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/cleaning.R')
    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/imputing.R')
    source('/home/jack/computer/genemap/hydroxyurea-study/scripts/modules/learning.R')

    input_data %>% select_using_hu_and_clean() -> input_data_using_hu
    input_data %>% select_not_using_hu_and_clean() -> input_data_not_using_hu
    input_data_using_hu %>% impute_missing_values() -> input_data_using_hu_imputed
    input_data_not_using_hu %>% impute_missing_values() -> input_data_not_using_hu_imputed
    rbind(input_data_using_hu_imputed, input_data_not_using_hu_imputed) %>%
        rename_hospitals() -> input_data_imputed

    return(input_data_imputed)
}

update_reasons_column <- function(input_data) {

    input_data %>%
        mutate(reason = ifelse(
            grepl("hu_is_affordable",reason),"is hu affordable?", ifelse(
                grepl("hu_is_available",reason), "is hu easily available?", ifelse(
                    grepl("hu_has_no_side_effects",reason), "does hu have side effects?", ifelse(
                        grepl("hu_is_prescribed",reason), "is hu prescribed?", "NA"))))) %>%
        return()
}

update_reasons_columns <- function(input_data) {

    input_data %>%
        mutate(hu_is_affordable=ifelse(hu_is_affordable==TRUE,"yes","no")) %>%
        mutate(hu_is_available=ifelse(hu_is_available==TRUE,"yes","no")) %>%
        mutate(hu_has_no_side_effects=ifelse(hu_has_no_side_effects==TRUE,"yes","no")) %>%
        mutate(hu_is_prescribed=ifelse(hu_is_prescribed==TRUE,"yes","no")) %>%
        return()
}

rename_hospitals <- function(input_data) {

    input_data %>%
        mutate(hosp_name=as.factor(
            ifelse(hosp_name=="Jos University Teaching Hospital","Jos University",
                ifelse(hosp_name=="Aminu Kano Teaching Hospital","Aminu Kano",
                    ifelse(hosp_name=="Irua Specialist Teaching Hospital","Irua Specialist",
                        ifelse(hosp_name=="University of Abuja Teaching Hospital, Gwagwalada ", "Abuja, University","Abuja, National")))))) %>%
                return()
}


draw_raw_data_plots <- function(raw_input_data) {

    #old <- theme_set(theme_bw())
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

    raw_input_data %>%
        mutate(using_hu=as.factor(
            ifelse(using_hu==1,"currently using",
                ifelse(using_hu==2,"never used","stopped using")))) %>%
        ggplot(aes(x=using_hu,fill=using_hu)) +
            geom_bar() +
            theme(legend.position = "none") +
            xlab("using hydroxyurea?") +
            ylab("number of participants")
    ggsave(paste(media_directory_path, "using_hu.png", sep=""),dpi=300)

    raw_input_data %>%
        rename_hospitals() %>%
        mutate(using_hu=as.factor(
            ifelse(using_hu==1,"currently using",
                ifelse(using_hu==2,"never used","stopped using")))) %>%
        ggplot(aes(x=using_hu,fill=using_hu)) +
            geom_bar() +
            facet_wrap(~hosp_name) +
            theme(
                legend.position = "none",
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()) +
            xlab("using hydroxyurea?") +
            ylab("number of participants")
    ggsave(paste(media_directory_path, "using_hu_hospital.png", sep=""),dpi=300)

    raw_input_data %>% 
        update_gender_column() %>%
        update_using_hu_column() %>%
        mutate(using_hu=as.factor(
            ifelse(using_hu==1,"currently using",
                ifelse(using_hu==2,"never used","stopped using")))) %>%
        group_by(using_hu, gender) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=using_hu, y=count, fill=gender)) + 
            geom_bar(stat="identity") +
            xlab("using hydroxyurea?") +
            ylab("number of participants") +
            guides(fill=guide_legend(title=""))
    ggsave(paste(media_directory_path, "using_hu_gender.png", sep=""),dpi=300)

    raw_input_data %>%
        update_gender_column() %>%
        update_using_hu_column() %>%
        select(age, gender, using_hu, hosp_name) %>%
        impute_missing_values() %>%
        mutate(using_hu=as.factor(
            ifelse(using_hu==1,"currently using",
                ifelse(using_hu==2,"never used","stopped using")))) %>%
        ggplot(aes(x=using_hu, y=age, fill=using_hu)) +
            geom_boxplot(colour=lightest_grey) +
            ylab("age (years)") +
            xlab("using hydroxyurea?") +
            theme(legend.position = "none")
    ggsave(paste(media_directory_path, "using_hu_age.png", sep=""),dpi=300)

}


draw_imputed_data_plots <- function(imputed_input_data) {

    #old <- theme_set(theme_bw())
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

    imputed_input_data %>%
        update_reasons_columns() %>%
        pivot_longer(cols=c(
            hu_is_affordable,
            hu_is_available,
            hu_has_no_side_effects,
            hu_is_prescribed), names_to='reason', values_to='response') %>% 
        group_by(reason,response,currently_using_hu) %>% 
        summarise(count=n()) %>% 
        mutate(response=as.factor(response)) %>%
        mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently using hu","stopped using hu")) %>%
        update_reasons_column() %>%
        ggplot(aes(x=reason,y=count,fill=response)) +
            geom_bar(position="dodge", stat="identity") +
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0)) +
            xlab("") +
            ylab("number of participants") +
            guides(fill=guide_legend(title=""))
    ggsave(paste(media_directory_path, "reason_count_currently_using_hu.png", sep=""),dpi=300)

    imputed_input_data %>% filter(currently_using_hu==TRUE) %>% nrow -> n_using_hu
    imputed_input_data %>% filter(currently_using_hu==FALSE) %>% nrow -> n_stopped_hu
    imputed_input_data %>%
    update_reasons_columns() %>%
        pivot_longer(cols=c(
            hu_is_affordable,
            hu_is_available,
            hu_has_no_side_effects,
            hu_is_prescribed), names_to='reason', values_to='response') %>% 
        group_by(reason,response,currently_using_hu) %>% 
        summarise(count=n()) %>% 
        mutate(response=as.factor(response)) %>% 
        mutate(norm_count = ifelse(currently_using_hu==TRUE, count/n_using_hu, count/n_stopped_hu)) %>%
        mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently using hu","stopped using hu")) %>%
        update_reasons_column() %>%
        ggplot(aes(x=reason,y=norm_count,fill=response)) +
            geom_bar(position="stack", stat="identity") +
            facet_wrap(~currently_using_hu) +
            theme(
                axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) +
            xlab("") +
            ylab("") +
            guides(fill=guide_legend(title=""))
    ggsave(paste(media_directory_path, "reason_count_currently_using_hu_stack.png", sep=""),dpi=300)

    imputed_input_data %>%
        update_reasons_columns() %>%
        group_by(hu_is_available,hosp_name,currently_using_hu) %>% 
        summarise(count=n()) %>% 
        mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently using hu","stopped using hu")) %>%
        rename_hospitals() %>%
        ggplot(aes(x=hosp_name,y=count,fill=hu_is_available)) +
            geom_bar(position="stack", stat="identity") +
            facet_wrap(~currently_using_hu) +
            theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0)) +
            xlab("") +
            ylab("number of participants") +
            guides(fill=guide_legend(title="is hu easily\navailable?"))
    ggsave(paste(media_directory_path, "hosp_name_hu_is_available.png", sep=""),dpi=300)

    imputed_input_data %>%
        update_reasons_columns() %>%
        mutate(currently_using_hu = ifelse(currently_using_hu==TRUE,"currently using hu","stopped using hu")) %>%
        rename_hospitals() %>%
        ggplot(aes(x=hosp_name,y=hu_is_available,color=hosp_name)) +
            geom_jitter(height=0.15,width=0.3, size=1) +
            #facet_wrap(~currently_using_hu) +
            theme(
                axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0),
                plot.margin=unit(c(0.3,2,0.3,0.3),"cm")) +
            theme(legend.position = "none") +
            xlab("") +
            ylab("is hu easily available?")
    ggsave(paste(media_directory_path, "hosp_name_hu_is_available_jitter.png", sep=""),dpi=300)

    imputed_input_data %>%
        update_reasons_columns() %>%
        ggplot(aes(x=hu_is_available,y=hu_is_prescribed,color=hu_is_available)) +
            geom_jitter(height=0.15,width=0.15, size=1) +
            theme(legend.position = "none") +
            xlab("is hu easily available?") +
            ylab("is hu prescribed?")
    ggsave(paste(media_directory_path, "hu_is_available_hu_is_prescribed.png", sep=""),dpi=300)
}





