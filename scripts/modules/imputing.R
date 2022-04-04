impute_missing_values <- function(input_data) {

    rfImpute(gender ~ ., input_data, iter=6) %>% 
        as_tibble %>% 
        return()
}