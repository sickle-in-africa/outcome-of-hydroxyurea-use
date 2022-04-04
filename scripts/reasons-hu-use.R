library(tidyverse)

survey <- as_tibble(read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE))

currently_using_hu <- survey %>% filter(using_hu == 1)

currently_using_hu %>% pivot_longer(cols=c(hu_not_expensive,available_pharm,easy_to_take,no_side_effect,reduces_pain,reduces_transfusion,doctor_advice), names_to='reason', values_to='response') %>% group_by(reason,response) %>% summarise(count=n()) %>% mutate(response=as.factor(response)) %>% ggplot(aes(x=reason,y=count,fill=response)) + geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
ggsave('./media/usinghureasons.png',dpi=300)

currently_using_hu %>% pivot_longer(cols=c(hu_not_expensive,available_pharm,easy_to_take,no_side_effect,reduces_pain,reduces_transfusion,doctor_advice), names_to='reason', values_to='response') %>% group_by(reason,response) %>% summarise(count=n()) %>% mutate(response=as.factor(response)) %>% ggplot(aes(x=reason,y=count,fill=response)) + geom_bar(position="stack", stat="identity") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
ggsave('./media/usinghureasons-stacked.png',dpi=300)