library(tidyverse)

survey <- as_tibble(read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE))

stopped_using_hu <- survey %>% filter(using_hu == 3)

stopped_using_hu %>% 
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
ggsave('./media/stoppinghureasons.png',dpi=300)

stopped_using_hu %>% 
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
        geom_bar(position="stack", stat="identity") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
ggsave('./media/stoppinghureasons-stacked.png',dpi=300)