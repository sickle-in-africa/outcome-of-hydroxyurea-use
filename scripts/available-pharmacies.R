library(tidyverse)
source("./scripts/multiplot.R", chdir = TRUE)

survey <- as_tibble(read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE))

p1 <- survey %>% 
    filter(using_hu == 1) %>% 
    pivot_longer(cols=c(available_pharm), names_to='reason', values_to='response') %>% 
    group_by(reason,response) %>% 
    summarise(count=n()) %>% 
    mutate(response=as.factor(response)) %>% 
    ggplot(aes(x=response,y=count,fill=response)) + 
        geom_bar(position="dodge", stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),legend.position = "none") + 
        ggtitle("I am using HU because\nit is easily available\nat pharmacies")

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

png(
  "./media/available_pharmacies.png",
  width     = 6,
  height    = 6,
  units     = "in",
  res       = 300,
  pointsize = 2
)
multiplot(p1,p2,cols=2)
dev.off()

survey %>% 
    filter(using_hu==1) %>% 
    mutate(available_pharm=as.factor(available_pharm)) %>% 
    ggplot(aes(x=hosp_name, y=available_pharm, fill=hosp_name)) + 
        geom_jitter(width=0.15,height=0.15) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none")
ggsave('./media/available_pharmacies-hosp-name-jitter.png',dpi=300)

survey %>% 
    filter(using_hu==1) %>% 
    ggplot(aes(x=hosp_name, y=available_pharm, fill=hosp_name)) + 
        geom_violin() +
        geom_jitter(width=0.15,height=0.15) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none")
ggsave('./media/available_pharmacies-hosp-name-violin.png',dpi=300)

survey %>% 
    filter(using_hu==3) %>% 
    mutate(available_pharmacies=as.factor(available_pharmacies)) %>% 
    ggplot(aes(x=hosp_name, y=available_pharmacies, fill=hosp_name)) + 
        geom_jitter(width=0.15,height=0.15) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none")
ggsave('./media/not_available_pharmacies-hosp-name-jitter.png',dpi=300)

survey %>% 
    filter(using_hu==3) %>% 
    ggplot(aes(x=hosp_name, y=available_pharmacies, fill=hosp_name)) +
        geom_violin() +
        geom_jitter(width=0.15,height=0.15) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1),legend.position = "none")
ggsave('./media/not_available_pharmacies-hosp-name-violin.png',dpi=300)

survey %>%
    mutate(doctor_advice=as.factor(doctor_advice)) %>% mutate(available_pharm=as.factor(available_pharm)) %>%
    filter(using_hu==1) %>%
    ggplot(aes(x=available_pharm,y=doctor_advice)) +
        geom_jitter(width=0.15,height=0.15)
ggsave('./media/available-vs-doctors-advice.png',dpi=300)

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
        geom_violin() +
        geom_jitter(width=0.15,height=0.15) +
        theme(legend.position = "none")

lm(is_hu_available~using_hu, data=d1) %>% summary

d2 <- survey %>%
    filter(using_hu==1 | using_hu==3) %>%
    mutate(cost_swap = map_int(cost,swap)) %>%
    mutate(is_hu_affordable = ifelse(is.na(hu_not_expensive),cost_swap, hu_not_expensive)) %>%
    filter(is_hu_affordable != 3)

d2 %>%
    mutate(is_hu_affordable=as.factor(is_hu_affordable)) %>%
    ggplot(aes(x=is_hu_affordable, y=using_hu, fill=is_hu_affordable)) + 
        geom_violin() +
        geom_jitter(width=0.15,height=0.15) +
        theme(legend.position = "none")

lm(is_hu_affordable~using_hu, data=d2) %>% summary

d3 <- survey %>%
    filter(using_hu==1 | using_hu==3) %>%
    mutate(side_effect_swap = map_int(side_effect,swap)) %>%
    mutate(hu_has_no_side_effects = ifelse(is.na(no_side_effect),side_effect_swap, no_side_effect)) %>%
    filter(hu_has_no_side_effects != 3)

d3 %>%
    mutate(hu_has_no_side_effects=as.factor(hu_has_no_side_effects)) %>%
    ggplot(aes(x=hu_has_no_side_effects, y=using_hu, fill=hu_has_no_side_effects)) + 
        geom_violin() +
        geom_jitter(width=0.15,height=0.15) +
        theme(legend.position = "none")

lm(hu_has_no_side_effects~using_hu, data=d3) %>% summary