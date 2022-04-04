library(tidyverse)
library(cowplot)
library(randomForest)

source('./scripts/modules/importing.R')
source('./scripts/modules/cleaning.R')
source('./imputing.R')

main <- function() {

    set.seed(42)

    get.study.data() -> studyData

    studyData %>% clean.and.simplify() -> studyData.cleaned

    studyData.cleaned %>% impute.missing.values() -> studyData.imputed

    # draw plots

    studyData.imputed %>% 
        ggplot(aes(x=start_hu_dose,y=current_hu_dose)) + 
        geom_point() + 
        geom_abline(intercept=0,slope=1)

    lm(dose_difference ~ hosp_name+age+gender+period_using_hu+hu_doses_per_week, data=studyData.imputed) %>% summary

    studyData.imputed %>% 
        ggplot(aes(x=hosp_name, y=dose_difference)) + 
            geom_violin() + 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

    studyData.imputed %>% ggplot(aes(x=age, y=dose_difference)) + geom_point()
    studyData.imputed %>% ggplot(aes(x=period_using_hu, y=dose_difference)) + geom_point()
    studyData.imputed %>% filter(hosp_name=="Jos University Teaching Hospital") %>% ggplot(aes(x=age, y=dose_difference)) + geom_point()
    studyData.imputed %>% filter(hosp_name=="Jos University Teaching Hospital") %>% ggplot(aes(x=period_using_hu, y=dose_difference)) + geom_point()


}

clean.and.simplify <- function(input.data) {

    input.data %>%
        update_gender_column() %>%
        update_marital_status_column() %>% 
        update_religion_column() %>%
        create_currently_using_hu_column() %>% 
        create_have_ever_used_hu_column() %>%
        update_start_hu_dose_column() %>%
        update_current_hu_dose_column() %>%
        create_hu_doses_per_week_column() %>%
        create_dose_difference_column() %>%
        update_period_using_hu_column() %>%
        update_hu_use_reasons_columns() %>%
        filter(scd_test_result_ss_sbthal==1L) %>%
        filter(currently_using_hu=='TRUE') %>%
        select(
            age,
            gender,
            marital_status,
            religion,
            hosp_name,
            period_using_hu,
            start_hu_dose,
            current_hu_dose,
            dose_difference,
            hu_doses_per_week,
            hu_not_expensive,
            available_pharm,
            easy_to_take,
            no_side_effect,
            reduces_pain) %>%
        return()
}

generate.random.forest <- function(input.data) {

    randomForest(currently_using_hu ~ ., input.data, proximity=TRUE) %>%
    return()
}

get.error.data <- function(input.model) {

    data.frame(
        Trees=rep(1:nrow(input.model$err.rate), times=3),
        Type=rep(c("OOB", FALSE, TRUE), each=nrow(input.model$err.rate)),
        Error=c(input.model$err.rate[,"OOB"], 
        input.model$err.rate[,"FALSE"], 
        input.model$err.rate[,"TRUE"])) %>%
        return()
}

perform.multidimensional.scaling <- function(input.model) {

    distance.matrix <- as.dist(1-input.model$proximity)

    mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

    mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

    mds.values <- mds.stuff$points

    data.frame(Sample=rownames(mds.values),
        X=mds.values[,1],
        Y=mds.values[,2]) %>%
        return()
}

perform.logistic.regression <- function(input.data) {
    
    glm(currently_using_hu ~ age, input.data, family="binomial") %>%
        return()
}


#
#  Run workfow
#
#main()