library(tidyverse)
source("/home/jack/computer/genemap/hydroxyurea-study/scripts/multiplot.R", chdir = TRUE)

survey <- read.csv(
        '/home/jack/computer/genemap/hydroxyurea-study/data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', 
        header=TRUE) %>%
    as_tibble

registry <- read.csv(
        '/home/jack/computer/genemap/hydroxyurea-study/data/SPARCO DATABASE_HU EXTRACT.csv',
        header=TRUE) %>% 
    as_tibble %>% 
    filter(redcap_repeat_instrument != 'patient_follow_up')

studyData <- merge(survey, registry, by.x='patient_record_id', by.y='record_id') %>%
    as_tibble()

registry %>%
    filter(redcap_repeat_instrument != 'patient_follow_up') %>%
    mutate(gender=as.factor(gender)) %>%
    ggplot + 
        geom_histogram(aes(x=age,fill=gender))

survey %>%
    select(using_hu) %>%
    group_by(using_hu) %>%
    summarise(count=n())

studyData %>% 
    mutate(gender=as.factor(gender)) %>%
    ggplot + 
        geom_histogram(aes(x=age,fill=gender)) + facet_wrap(~using_hu)

studyData %>%
    mutate(gender=as.factor(gender)) %>%
    ggplot +
        geom_density(aes(x=age,y=..density..,fill=gender,color=gender),alpha=0.5) + 
        facet_wrap(~using_hu)

survey %>% 
    filter(using_hu == 1) %>% 
    pivot_longer(cols=c(
        hu_not_expensive,
        available_pharm,
        easy_to_take,
        no_side_effect,
        reduces_pain,
        reduces_transfusion,
        doctor_advice), names_to='reason', values_to='response') %>% 
    group_by(reason,response) %>% 
    summarise(count=n()) %>% 
    mutate(response=as.factor(response)) %>% 
    ggplot(aes(x=reason,y=count,fill=response)) +
        geom_bar(position="dodge", stat="identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

survey %>% 
    filter(using_hu == 3) %>% 
    pivot_longer(cols=c(
        side_effect,
        cost,
        requires_test,
        available_pharmacies,
        doctor_stopped_use), names_to='reason', values_to='response') %>% 
    select(reason, response) %>% group_by(reason,response) %>% 
    summarise(count=n()) %>% 
    mutate(response=as.factor(response)) %>% 
    ggplot(aes(x=reason,y=count,fill=response)) + 
        geom_bar(position="dodge", stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

p1 <- survey %>% 
    filter(using_hu == 1) %>% 
    pivot_longer(cols=c(available_pharm), names_to='reason', values_to='response') %>% 
    group_by(reason,response) %>% 
    summarise(count=n()) %>% 
    mutate(response=as.factor(response)) %>% 
    ggplot(aes(x=response,y=count,fill=response)) + 
        geom_bar(position="dodge", stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),legend.position = "none") + 
        ggtitle("I am using HU because\nit is easily available\nin pharmacies")

p2 <- survey %>% 
    filter(using_hu == 3) %>% 
    pivot_longer(cols=c(available_pharmacies), names_to='reason', values_to='response') %>% 
    select(reason, response) %>% group_by(reason,response) %>% 
    summarise(count=n()) %>% 
    mutate(response=as.factor(response)) %>% 
    ggplot(aes(x=response,y=count,fill=response)) + 
        geom_bar(position="dodge", stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),legend.position = "none") + 
        ggtitle("I stopped using HU because\nit wasn't easily available\nin pharmacies")

multiplot(p1,p2,cols=2)

survey %>% 
    filter(using_hu==1) %>% 
    mutate(available_pharm=as.factor(available_pharm)) %>% 
    ggplot(aes(x=hosp_name, y=available_pharm, fill=hosp_name)) + 
        geom_jitter(width=0.15,height=0.15) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none")

survey %>% 
    filter(using_hu==3) %>% 
    mutate(available_pharmacies=as.factor(available_pharmacies)) %>% 
    ggplot(aes(x=hosp_name, y=available_pharmacies, fill=hosp_name)) + 
        geom_jitter(width=0.15,height=0.15) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none")

ggplot(studyData, aes(x=using_hu)) + geom_histogram(stat="count") + facet_wrap(~hosp_name)

survey %>%
    mutate(doctor_advice=as.factor(doctor_advice)) %>% mutate(available_pharm=as.factor(available_pharm)) %>%
    filter(using_hu==1) %>%
    ggplot(aes(x=available_pharm,y=doctor_advice)) +
        geom_jitter(width=0.15,height=0.15)

swap <- function(x) {
    if (is.na(x)) {return(NA)}
    else if (x==1L) { return(2L) }
    else if (x==2L) { return(1L) }
    else { return(x) }
}

d1 <- survey %>%
    filter(using_hu==1 | using_hu==3) %>%
    mutate(available_pharmacies_swap = map_int(available_pharmacies,swap)) %>%
    mutate(is_hu_available = ifelse(is.na(available_pharm),available_pharmacies_swap, available_pharm)) %>%
    filter(is_hu_available != 3)

d1 %>%
    mutate(is_hu_available=as.factor(is_hu_available)) %>%
    ggplot(aes(x=is_hu_available, y=using_hu, fill=is_hu_available)) + 
        geom_jitter(width=0.15,height=0.15) +
        theme(legend.position = "none")

d2 <- survey %>%
    filter(using_hu==1 | using_hu==3) %>%
    mutate(cost_swap = map_int(cost,swap)) %>%
    mutate(is_hu_affordable = ifelse(is.na(hu_not_expensive),cost_swap, hu_not_expensive)) %>%
    filter(is_hu_affordable != 3)

d2 %>%
    mutate(is_hu_affordable=as.factor(is_hu_affordable)) %>%
    ggplot(aes(x=is_hu_affordable, y=using_hu, fill=is_hu_affordable)) + 
        geom_jitter(width=0.15,height=0.15) +
        theme(legend.position = "none")

d3 <- survey %>%
    filter(using_hu==1 | using_hu==3) %>%
    mutate(side_effect_swap = map_int(side_effect,swap)) %>%
    mutate(hu_has_no_side_effects = ifelse(is.na(no_side_effect),side_effect_swap, no_side_effect)) %>%
    filter(hu_has_no_side_effects != 3)

d3 %>%
    mutate(hu_has_no_side_effects=as.factor(hu_has_no_side_effects)) %>%
    ggplot(aes(x=hu_has_no_side_effects, y=using_hu, fill=hu_has_no_side_effects)) + 
        geom_jitter(width=0.15,height=0.15) +
        theme(legend.position = "none")
