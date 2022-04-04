library(tidyverse)

registry <- read.csv('./data/SPARCO DATABASE_HU EXTRACT.csv', header=TRUE)

registry %>% 
    filter(redcap_repeat_instrument != 'patient_follow_up') %>% 
    mutate(gender=as.factor(ifelse(gender==1,"M","F"))) %>% 
    select(age,gender) %>% 
    ggplot + 
        geom_histogram(aes(x=age,fill=gender))
ggsave('./media/age-gender-count.png',dpi=300)