library(tidyverse)
library(cowplot)
library(randomForest)


main <- function() {

    set.seed(42)

    get.study.data() -> studyData

    studyData %>% clean.and.simplify() -> studyData.cleaned

    studyData.cleaned %>% impute.missing.values() -> studyData.imputed

    studyData.imputed %>% generate.random.forest() -> random.forest.model

    random.forest.model %>% get.error.data() -> oob.error.data

    random.forest.model %>% perform.multidimensional.scaling() -> mds.data

    studyData.imputed %>% perform.logistic.regression() -> regression.results

    # draw plots

    studyData.imputed %>% 
        ggplot() + 
            geom_bar(aes(x=gender, fill=gender)) + 
            facet_wrap(~currently_using_hu)

    studyData.imputed %>% 
        ggplot() + 
            geom_density(aes(x=age, y=..density..), fill='grey') + 
            facet_wrap(~currently_using_hu)

    studyData.imputed %>% 
        ggplot(aes(x=currently_using_hu, y=age)) + geom_violin()

    glm(currently_using_hu ~ age, studyData.imputed, family="binomial") %>% 
        summary

        

    oob.error.data %>%
        ggplot(aes(x=Trees, y=Error)) +
        geom_line(aes(color=Type))

    studyData.imputed %>% 
        ggplot(aes(x=currently_using_hu, y=age)) + geom_violin()

    mds.data %>%
        ggplot(aes(x=X, y=Y, label=Sample)) + 
        geom_text(aes(color=studyData.imputed$currently_using_hu)) +
        theme_bw() +
        ggtitle("MDS plot using (1 - Random Forest Proximities)")
}


get.study.data <- function() {

    read.csv('./data/OutcomeOfHydroxyurea_DATA_2021-09-01_0948.csv', header=TRUE) %>% 
        as_tibble -> survey
    read.csv('./data/SPARCO DATABASE_HU EXTRACT.csv', header=TRUE) %>% 
        as_tibble %>% 
        filter(redcap_repeat_instrument != 'patient_follow_up') -> registry

    merge(survey, registry, by.x='patient_record_id', by.y='record_id') %>%
        as_tibble %>%
        return()
}

clean.and.simplify <- function(input.data) {

    input.data %>%
        mutate(gender=as.factor(ifelse(gender==1,"M",ifelse(gender==2,"F",gender)))) %>%
        mutate(marital_status=as.factor(as.integer(marital_status))) %>% 
        mutate(religion=as.factor(as.integer(religion))) %>%
        mutate(scd_test_result_ss_sbthal=as.factor(
            ifelse(scd_test_result_ss_sbthal==1L,"SS",
                    ifelse(scd_test_result_ss_sbthal==3L,"SC", 
                            scd_test_result_ss_sbthal)))) %>%
        mutate(using_hu=as.factor(using_hu)) %>%
        mutate(currently_using_hu=as.factor(ifelse(using_hu==1,T,F))) %>% 
        mutate(have_ever_used_hu=as.factor(ifelse(using_hu==2,T,F))) %>%
        filter(scd_test_result_ss_sbthal=="SS") %>%
        select(age, gender, marital_status, religion, hosp_name, currently_using_hu) %>%
        return()
}

impute.missing.values <- function(input.data) {

    rfImpute(currently_using_hu ~ ., input.data, iter=6) %>% 
        as_tibble %>% 
        mutate(age=round(age)) %>%
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