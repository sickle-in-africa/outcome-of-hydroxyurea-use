plot_unsupervised_mds_scatter <- function(input_data) {

    input_data %>%
        build_unsupervised_random_forest() %>%
        perform_multidimensional_scaling() %>%
        ggplot(aes(x=PC1, y=PC2, label=Sample)) + 
            geom_point()
}

build_unsupervised_random_forest <- function(input_data) {

    randomForest(x=input_data, data=input_data, proximity=TRUE) %>%
        return()
}

plot_supervised_mds_scatter <- function(input_data) {

    input_data %>%
        #filter(norm_dose_difference <= 8.0) %>%
        data.frame %>%
        as_tibble -> input_data_copy

    input_data_copy$Sample <- rownames(input_data_copy)

    input_data_copy %>%
        build_supervised_random_forest() %>%
        perform_multidimensional_scaling() -> mds_data

    merge(input_data_copy,mds_data, by="Sample") %>%
        as_tibble %>%
        ggplot(aes(x=PC1, y=PC2, color=norm_dose_difference)) + 
            geom_point() +
            scale_color_viridis(option = "A")
}

plot_supervised_high_current_dose_mds_scatter <- function(input_data) {

    input_data %>% 
        filter(age >= 10 & age <=30) %>% 
        mutate(high_current_dose=ifelse(current_hu_dose>=100,TRUE,FALSE)) %>%
        data.frame %>%
        as_tibble -> input_data_copy

    input_data_copy$Sample <- rownames(input_data_copy)

    input_data_copy %>%
        build_supervised_high_current_dose_random_forest() %>%
        perform_multidimensional_scaling() -> mds_data

    merge(input_data_copy,mds_data, by="Sample") %>%
        as_tibble %>%
        ggplot(aes(x=PC1, y=PC2, color=high_current_dose)) + 
            geom_point()
}

build_supervised_high_current_dose_random_forest <- function(input_data) {

    input_data %>%
        select(age,
            gender,
            marital_status,
            hosp_name,
            hu_is_available,
            hu_is_affordable,
            hu_is_prescribed,
            hu_has_no_side_effects,
            headaches,
            dizziness.x,
            nausea,
            vomiting,
            stomach_upset,
            diarrhea,
            constipation,
            skin_changes,
            flu_symptoms,
            low_blood_count,
            high_current_dose) -> data_subset

    randomForest(high_current_dose ~ ., data=data_subset, proximity=TRUE) %>%
        return()
}

perform_multidimensional_scaling <- function(input_random_forest) {

    distance.matrix <- as.dist(1-input_random_forest$proximity)

    mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

    mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

    mds.values <- mds.stuff$points

    data.frame(Sample=rownames(mds.values),
        PC1=mds.values[,1],
        PC2=mds.values[,2]) %>%
        as_tibble %>%
        return()
}