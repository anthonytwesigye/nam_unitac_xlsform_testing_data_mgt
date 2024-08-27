#' Test data pre cleaning
#' Uses packages like kobold and butteR
#'
#' @param input_df_raw_data Specify the data frame for the tool data
#' @param input_df_survey Specify the data frame for the survey sheet
#' @param input_df_choices Specify the data frame for the choices sheet
#' @param input_df_cleaning_log Specify the data frame for the cleaning/checking log
#'
#' @return Data frame of cleaned data
#' @export
#'
#' @examples
#'
test_data_pre_cleaning <- function(input_df_raw_data,
                                   input_df_survey,
                                   input_df_choices,
                                   input_df_cleaning_log) {
  
  # create kobold object
  
  kbo <- kobold::kobold(survey = input_df_survey %>% filter(name %in% colnames(input_df_raw_data)),
                        choices = input_df_choices,
                        data = input_df_raw_data,
                        cleaning = input_df_cleaning_log)
  
  df_cleaning_so_sm <- input_df_cleaning_log %>% 
    filter(type %in% c("change_response", "add_option"))
  
  if(length(unique(df_cleaning_so_sm$value)) > 1){
    
    # find all new choices to add to choices sheet
    
    # gather choice options based on unique choices list
    df_grouped_choices<- input_df_choices %>%
      group_by(list_name) %>%
      summarise(choice_options = paste(name, collapse = " : "))
    
    # get new name and choice pairs to add to the choices sheet
    new_vars <- df_cleaning_so_sm %>%
      left_join(input_df_survey, by = "name") %>%
      filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>%
      separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>%
      left_join(df_grouped_choices, by = "list_name") %>%
      filter(!str_detect(string = choice_options, pattern = value ) ) %>%
      dplyr::rename(choice = value ) %>%
      select(name, choice) %>%
      distinct() %>% # to make sure there are no duplicates
      arrange(name)
    
    # modified choices for the survey tool
    df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)
    
    # special treat for variables for select_multiple, we need to add the columns to the data itself
    df_survey_sm <- input_df_survey %>%
      mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                                str_detect(string = type, pattern = "select_one|select one") ~ "so",
                                TRUE ~ type)) %>%
      select(name, q_type)
    
    # construct new columns for select multiple
    new_vars_sm <- new_vars %>%
      left_join(df_survey_sm, by = "name") %>%
      filter(q_type == "sm") %>%
      mutate(new_cols = paste0(name,"/",choice))
    
    # add new columns to the raw data
    df_raw_data_modified <- input_df_raw_data %>%
      butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )
    
    # make some cleanup
    kbo_modified <- kobold::kobold(survey = input_df_survey %>% filter(name %in% colnames(df_raw_data_modified)),
                                   choices = df_choises_modified,
                                   data = df_raw_data_modified,
                                   cleaning = input_df_cleaning_log)
    kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)
  }else{
    kbo_cleaned <- kobold::kobold_cleaner(kbo)
  }
  
  df_final_cleaned_data <- kbo_cleaned$data
}