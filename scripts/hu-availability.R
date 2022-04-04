library(tidyverse)
library(cowplot)
library(randomForest)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./scripts/modules/imputing.R')

main <- function() {

    set.seed(42)

    get_study_data() -> study_data

    study_data %>% clean_and_simplify() -> study_data_clean

    study_data_clean %>% impute_missing_values() -> study_data_imputed

    study_data_imputed %>% 
        ggplot(aes(x=age)) + 
        geom_histogram() + 
        facet_wrap(~using_hu)

    study_data_imputed %>% 
        ggplot(aes(x=age, fill=gender,color=gender)) + 
        geom_density(alpha=0.5) + 
        facet_wrap(~using_hu)

    study_data_imputed %>%
        ggplot(aes(x=using_hu)) + 
            geom_histogram(stat="count") + 
            facet_wrap(~hosp_name)

    glm(currently_using_hu ~ ., data=study_data_imputed, family="binomial") %>%
        summary

}

clean_and_simplify <- function(input_data) {

    input_data %>%
        update_gender_column() %>%
        update_marital_status_column() %>%
        update_using_hu_column() %>%
        select(
            age,
            gender,
            marital_status,
            hosp_name,
            using_hu) %>%
        return()
}

