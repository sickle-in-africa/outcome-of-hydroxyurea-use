library(tidyverse)

studyData <- as_tibble(read.csv('./data/study-data.csv', header=TRUE))

studyData$using_hu <- factor(studyData$using_hu)
studyData$hosp_name <- factor(studyData$hosp_name)

ggplot(studyData, aes(x=using_hu)) + geom_histogram(stat="count") + facet_wrap(~hosp_name)
ggsave('./media/using_hu-hospName.png', dpi=300)