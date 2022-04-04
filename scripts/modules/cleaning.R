select_using_hu_and_clean <- function(input_data) {

    input_data %>%
        filter(using_hu==1L) %>%
        update_hu_use_reasons_columns() %>%
        mutate(hu_is_available = available_pharm) %>%
        mutate(hu_is_affordable = hu_not_expensive) %>%
        mutate(hu_is_prescribed = doctor_advice) %>%
        mutate(hu_has_no_side_effects = no_side_effect) %>%
        update_current_hu_dose_column() %>%
        basic_clean() %>%
        select_clean_columns() %>%
        return()
}

select_not_using_hu_and_clean <- function(input_data) {

    input_data %>%
        filter(using_hu==3L) %>%
        update_hu_stopped_reasons_columns() %>%
        mutate(hu_is_available = as.factor(!as.logical(available_pharmacies))) %>%
        mutate(hu_is_affordable = as.factor(!as.logical(cost))) %>%
        mutate(hu_is_prescribed = as.factor(!as.logical(doctor_stopped_use))) %>%
        mutate(hu_has_no_side_effects = as.factor(!as.logical(side_effect))) %>%
        mutate(current_hu_dose = 0) %>%
        basic_clean() %>%
        select_clean_columns() %>%
        return()
}

basic_clean <- function(input_data) {

    input_data %>%
        update_gender_column() %>%
        update_religion_column() %>%
        update_marital_status_column() %>%
        update_using_hu_column() %>%
        create_currently_using_hu_column() %>%
        update_start_hu_dose_column() %>%
        create_dose_difference_column() %>%
        create_norm_dose_difference_column() %>%
        update_hu_side_effects_columns() %>%
        update_complications_before_hu_columns() %>%
        update_complications_after_hu_columns() %>%
        update_scd_test_result_ss_sbthal_column() %>%
        return()
}

select_clean_columns <- function(input_data) {

    input_data %>%
        select(
            # predictors
            age,
            gender,
            marital_status,
            hosp_name,
            hu_is_available,
            hu_is_affordable,
            hu_is_prescribed,
            hu_has_no_side_effects,
            # -- hu side effects
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
            # -- complications before hu
            bone_pain1,
            chest_pain1,
            leg_ulcer1,
            avn1,
            stroke1,
            priapism1,
            jaundice1,
            abnormal_appetite1,
            freq_pain_before_hu,
            freq_hosp_adm_before_hu,
            # -- complications after hu
            bone_pain2,
            chest_pain2,
            leg_ulcer2,
            avn2,
            stroke2,
            priapism2,
            jaundice2,
            abnorm_appetite2,
            freq_pain_after_hu,
            freq_hosp_adm_after_hu_2,
            # responses
            currently_using_hu,
            start_hu_dose,
            current_hu_dose,
            dose_difference,
            norm_dose_difference) %>%
        return()
}

update_gender_column <- function(input_data) {

    input_data %>%
        mutate(gender=as.factor(ifelse(gender==1,"male",ifelse(gender==2,"female",gender)))) %>%
        return()
}

update_marital_status_column <- function(input_data) {

    input_data %>%    
        mutate(marital_status=as.factor(
            ifelse(marital_status==1L,"single",
                ifelse(marital_status==2L, "married",
                    ifelse(marital_status==3L, "other", 
                        ifelse(marital_status==4L, "cohabiting", 
                            ifelse(marital_status==5L, "divorced", 
                                ifelse(marital_status==6L, "widowed", marital_status)))))))) %>%
        return()
}

update_scd_test_result_ss_sbthal_column <- function(input_data) {

    input_data %>%
        mutate(scd_test_result_ss_sbthal=as.factor(ifelse(
            scd_test_result_ss_sbthal==1L,"SS",ifelse(
                scd_test_result_ss_sbthal==2L,"S/beta thalassemia",ifelse(
                    scd_test_result_ss_sbthal==3L,"SC","CC"))))) %>%
        return()
}

update_religion_column <- function(input_data) {

    input_data %>%
        mutate(religion=as.factor(ifelse(
            religion==1L,"Christianity",ifelse(
                religion==2L,"Islam",ifelse(
                    religion==3L,"African traditional beliefs","other"))))) %>%
        return()
}

update_using_hu_column <- function(input_data) {

    input_data %>%
        mutate(using_hu=as.factor(using_hu)) %>%
        return()
}

create_currently_using_hu_column <- function(input_data) {

    input_data %>%
        mutate(currently_using_hu = 
            as.factor(ifelse(using_hu==1L,TRUE,
                ifelse(using_hu==3L,FALSE,NA)))) %>%
        return()
}

create_have_ever_used_hu_column <- function(input_data) {

    input_data %>%
        mutate(have_ever_used_hu=as.factor(ifelse(using_hu==2L,TRUE,FALSE))) %>%
        return()

}

update_hu_side_effects_columns <- function(input_data) {
    
    input_data %>%
        mutate(headaches=as.factor(
            ifelse(headaches==1L,TRUE,
                ifelse(headaches==2L,FALSE,NA)))) %>%
        mutate(dizziness.x=as.factor(
            ifelse(dizziness.x==1L,TRUE,
                ifelse(dizziness.x==2L,FALSE,NA)))) %>%
        mutate(nausea=as.factor(
            ifelse(nausea==1L,TRUE,
                ifelse(nausea==2L,FALSE,NA)))) %>%
        mutate(vomiting=as.factor(
            ifelse(vomiting==1L,TRUE,
                ifelse(vomiting==2L,FALSE,NA)))) %>%
        mutate(stomach_upset=as.factor(
            ifelse(stomach_upset==1L,TRUE,
                ifelse(stomach_upset==2L,FALSE,NA)))) %>%
        mutate(diarrhea=as.factor(
            ifelse(diarrhea==1L,TRUE,
                ifelse(diarrhea==2L,FALSE,NA)))) %>%
        mutate(constipation=as.factor(
            ifelse(constipation==1L,TRUE,
                ifelse(constipation==2L,FALSE,NA)))) %>%
        mutate(skin_changes=as.factor(
            ifelse(skin_changes==1L,TRUE,
                ifelse(skin_changes==2L,FALSE,NA)))) %>%
        mutate(flu_symptoms=as.factor(
            ifelse(flu_symptoms==1L,TRUE,
                ifelse(flu_symptoms==2L,FALSE,NA)))) %>%
        mutate(low_blood_count=as.factor(
            ifelse(low_blood_count==1L,TRUE,
                ifelse(low_blood_count==2L,FALSE,NA)))) %>%
        return()
}


update_hu_use_reasons_columns <- function(input_data) {

    input_data %>%
        mutate(hu_not_expensive = as.factor(
            ifelse(hu_not_expensive==1L,TRUE,
                ifelse(hu_not_expensive==2L,FALSE,NA)))) %>%
        mutate(available_pharm = as.factor(
            ifelse(available_pharm==1L,TRUE,
                ifelse(available_pharm==2L,FALSE,NA)))) %>%
        mutate(easy_to_take = as.factor(
            ifelse(easy_to_take==1L,TRUE,
                ifelse(easy_to_take==2L,FALSE,NA)))) %>%
        mutate(no_side_effect = as.factor(
            ifelse(no_side_effect==1L,TRUE,
                ifelse(no_side_effect==2L,FALSE,NA)))) %>%
        mutate(reduces_pain = as.factor(
            ifelse(reduces_pain==1L,TRUE,
                ifelse(reduces_pain==2L,FALSE,NA)))) %>%
        mutate(reduces_transfusion = as.factor(
            ifelse(reduces_transfusion==1L,TRUE,
                ifelse(reduces_transfusion==2L,FALSE,NA)))) %>%
        mutate(doctor_advice = as.factor(
            ifelse(doctor_advice==1L,TRUE,
                ifelse(doctor_advice==2L,FALSE,NA)))) %>%
        return()
}

update_hu_stopped_reasons_columns <- function(input_data) {

    input_data %>%
        mutate(side_effect = as.factor(
            ifelse(side_effect==1L,TRUE,
                ifelse(side_effect==2L,FALSE,NA)))) %>%
        mutate(cost = as.factor(
            ifelse(cost==1L,TRUE,
                ifelse(cost==2L,FALSE,NA)))) %>%
        mutate(requires_test = as.factor(
            ifelse(requires_test==1L,TRUE,
                ifelse(requires_test==2L,FALSE,NA)))) %>%
        mutate(available_pharmacies = as.factor(
            ifelse(available_pharmacies==1L,TRUE,
                ifelse(available_pharmacies==2L,FALSE,NA)))) %>%
        mutate(doctor_stopped_use = as.factor(
            ifelse(doctor_stopped_use==1L,TRUE,
                ifelse(doctor_stopped_use==2L,FALSE,NA)))) %>%
        return()
}


update_start_hu_dose_column <- function(input_data) {

    input_data %>% 
        mutate(dose_specify1 = as.character(dose_specify1)) %>%
        mutate(dose_specify1 = ifelse(str_detect(dose_specify1, "Can't remember around 500-600"), NA, dose_specify1)) %>%
        mutate(dose_specify1 = ifelse(str_detect(dose_specify1, "Compounded"), NA, dose_specify1)) %>%
        mutate(dose_specify1 = ifelse(str_detect(dose_specify1, "5ml"), NA, dose_specify1)) %>%
        mutate(dose_specify1 = str_replace(dose_specify1, "mg", "")) %>%
        mutate(dose_specify1 = as.numeric(dose_specify1)) %>%
        mutate(start_hu_dose = ifelse(start_hu_dose==1, 250, start_hu_dose)) %>%
        mutate(start_hu_dose = ifelse(start_hu_dose==2, 500, start_hu_dose)) %>%
        mutate(start_hu_dose = ifelse(start_hu_dose==3, 1000, start_hu_dose)) %>%
        mutate(start_hu_dose = ifelse(start_hu_dose==4, 1500, start_hu_dose)) %>%
        mutate(start_hu_dose = ifelse(start_hu_dose==5, dose_specify1, start_hu_dose)) %>%
        return()
}

update_current_hu_dose_column <- function(input_data) {

    input_data %>%
        mutate(dose_specify2 = as.character(dose_specify2)) %>%
        mutate(dose_specify2 = str_replace(dose_specify2, "500mg [+]100mg=600mg", "600mg")) %>%
        mutate(dose_specify2 = ifelse(str_detect(dose_specify2, "250-300mg"), NA, dose_specify2)) %>%
        mutate(dose_specify2 = ifelse(str_detect(dose_specify2, "5ml"), NA, dose_specify2)) %>%
        mutate(dose_specify2 = str_replace(dose_specify2, "mg", "")) %>%
        mutate(dose_specify2 = as.numeric(dose_specify2)) %>%
        mutate(current_hu_dose = ifelse(current_hu_dose==1, 250, current_hu_dose)) %>%
        mutate(current_hu_dose = ifelse(current_hu_dose==2, 500, current_hu_dose)) %>%
        mutate(current_hu_dose = ifelse(current_hu_dose==3, 1000, current_hu_dose)) %>%
        mutate(current_hu_dose = ifelse(current_hu_dose==4, 1500, current_hu_dose)) %>%
        mutate(current_hu_dose = ifelse(current_hu_dose==5, dose_specify2, current_hu_dose)) %>%
        return()
}

create_hu_doses_per_week_column <- function(input.data) {

    input.data %>%
        mutate(others_specify1 = str_replace(others_specify1, "3 times a week", "3")) %>%
        mutate(others_specify1 = str_replace(others_specify1, "Twice daily", "14")) %>%
        mutate(others_specify1 = str_replace(others_specify1, "Alternate days", "3.5")) %>%
        mutate(others_specify1 = ifelse(str_detect(others_specify1, "When available"), NA, others_specify1)) %>%
        mutate(others_specify1 = ifelse(str_detect(others_specify1, "Once available"), NA, others_specify1)) %>%
        mutate(hu_doses_per_week = 
            ifelse(freq_hu_use==1L, 7,
                ifelse(freq_hu_use==2L, 1, 
                    ifelse(freq_hu_use==3L, others_specify1, freq_hu_use)))) %>%
        mutate(hu_doses_per_week = as.numeric(hu_doses_per_week)) %>%
        return()
}

create_dose_difference_column <- function(input_data) {

    input_data %>%
        mutate(dose_difference = current_hu_dose - start_hu_dose) %>%
        return()
}

create_norm_dose_difference_column <- function(input_data) {

    input_data %>%
        mutate(norm_dose_difference = (current_hu_dose - start_hu_dose)/start_hu_dose) %>%
        return()
}

update_period_using_hu_column <- function(input_data) {

    input_data %>%
        mutate(period_using_hu=as.integer(period_using_hu)) %>%
        return()
}

update_complications_before_hu_columns <- function(input_data) {

    input_data %>%
        mutate(bone_pain1 = as.factor(
            ifelse(bone_pain1==1L,"Yes",
                ifelse(bone_pain1==2L,"No",NA)))) %>%
        mutate(chest_pain1 = as.factor(
            ifelse(chest_pain1==1L,"Yes",
                ifelse(chest_pain1==2L,"No",NA)))) %>%
        mutate(leg_ulcer1 = as.factor(
            ifelse(leg_ulcer1==1L,"Yes",
                ifelse(leg_ulcer1==2L,"No",NA)))) %>%
        mutate(avn1 = as.factor(
            ifelse(avn1==1L,"Yes",
                ifelse(avn1==2L,"No",NA)))) %>%
        mutate(stroke1 = as.factor(
            ifelse(stroke1==1L,"Yes",
                ifelse(stroke1==2L,"No",NA)))) %>%
        mutate(priapism1 = as.factor(
            ifelse(priapism1==1L,"Yes",
                ifelse(priapism1==2L,"No",NA)))) %>%
        mutate(jaundice1 = as.factor(
            ifelse(jaundice1==1L,"Yes",
                ifelse(jaundice1==2L,"No",NA)))) %>%
        mutate(abnormal_appetite1 = as.factor(
            ifelse(abnormal_appetite1==1L,"Yes",
                ifelse(abnormal_appetite1==2L,"No",NA)))) %>%
        return()
}

update_complications_after_hu_columns <- function(input_data) {

    input_data %>%
        mutate(bone_pain2 = as.factor(
            ifelse(bone_pain2==1L,"Yes",
                ifelse(bone_pain1==2L,"No",NA)))) %>%
        mutate(chest_pain2 = as.factor(
            ifelse(chest_pain2==1L,"Yes",
                ifelse(chest_pain2==2L,"No",NA)))) %>%
        mutate(leg_ulcer2 = as.factor(
            ifelse(leg_ulcer2==1L,"Yes",
                ifelse(leg_ulcer2==2L,"No",NA)))) %>%
        mutate(avn2 = as.factor(
            ifelse(avn2==1L,"Yes",
                ifelse(avn2==2L,"No",NA)))) %>%
        mutate(stroke2 = as.factor(
            ifelse(stroke2==1L,"Yes",
                ifelse(stroke2==2L,"No",NA)))) %>%
        mutate(priapism2 = as.factor(
            ifelse(priapism2==1L,"Yes",
                ifelse(priapism2==2L,"No",NA)))) %>%
        mutate(jaundice2 = as.factor(
            ifelse(jaundice2==1L,"Yes",
                ifelse(jaundice2==2L,"No",NA)))) %>%
        mutate(abnorm_appetite2 = as.factor(
            ifelse(abnorm_appetite2==1L,"Yes",
                ifelse(abnorm_appetite2==2L,"No",NA)))) %>%
        return()
}