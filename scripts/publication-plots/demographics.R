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

media_directory_path <- '/home/jack/computer/genemap/hydroxyurea-study/media/publication-ready-plots/dark-theme/demographics/'

get_study_data() -> study_data

study_data %>% 
    ggplot(aes(x=age)) + 
        geom_histogram(binwidth=5, fill=lightest_grey) +
        xlab("age (years)") +
        ylab("number of participants")
ggsave(paste(media_directory_path,"age.png",sep=""),dpi=300)

study_data %>%
    mutate(gender=ifelse(gender==1, 'male', 'female')) %>%
    ggplot(aes(x=age)) + 
        geom_histogram(binwidth=5, fill=lightest_grey) +
        facet_wrap(~gender) +
        xlab("age (years)") +
        ylab("number of participants")
ggsave(paste(media_directory_path,"age_gender.png",sep=""),dpi=300)