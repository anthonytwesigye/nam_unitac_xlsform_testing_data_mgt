library(tidyverse)
library(butteR)
library(supporteR)


# water -------------------------------------------------------------------

# data
data_path_water <- "inputs/rundu_water_check_support.csv"

df_tool_data_water <- readr::read_csv(data_path_water) %>%  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         today = ifelse(row_number() > 5, as_date(start), as_date(start)-1),
         enumerator_id = "",
         `_uuid` = uuid) %>% 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "location")

# data checks

checks_output_water <- list()

# data collected before 26th

df_testing_data_water <- df_tool_data_water %>% 
  filter(today < as_date("2024-08-28")) %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output_water", input_df_name = "df_testing_data_water")

# combined  checks

df_combined_checks_water <- bind_rows(checks_output_water) 

# output the log
write_csv(x = df_combined_checks_water, file = paste0("inputs/combined_checks_rundu_water.csv"), na = "")

# building -------------------------------------------------------------------

# data
data_path_building <- "inputs/rundu_building_check_support.csv"

df_tool_data_building <- readr::read_csv(data_path_building) %>%  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         today = ifelse(row_number() > 5, as_date(start), as_date(start)-1),
         enumerator_id = "",
         `_uuid` = uuid) %>% 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "location")

# data checks

checks_output_building <- list()

# data collected before 26th

df_testing_data_building <- df_tool_data_building %>% 
  filter(today < as_date("2024-08-28")) %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output_building", input_df_name = "df_testing_data_building")

# combined  checks

df_combined_checks_building <- bind_rows(checks_output_building) 

# output the log
write_csv(x = df_combined_checks_building, file = paste0("inputs/combined_checks_rundu_building.csv"), na = "")

# environmental -------------------------------------------------------------------

# data
data_path_environmental <- "inputs/rundu_environmental_check_support.csv"

df_tool_data_environmental <- readr::read_csv(data_path_environmental) %>%  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         today = ifelse(row_number() > 5, as_date(start), as_date(start)-1),
         enumerator_id = "",
         `_uuid` = uuid) %>% 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "location")

# data checks

checks_output_environmental <- list()

# data collected before 26th

df_testing_data_environmental <- df_tool_data_environmental %>% 
  filter(today < as_date("2024-08-28")) %>% 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output_environmental", input_df_name = "df_testing_data_environmental")

# combined  checks

df_combined_checks_environmental <- bind_rows(checks_output_environmental) 

# output the log
write_csv(x = df_combined_checks_environmental, file = paste0("inputs/combined_checks_rundu_environmental.csv"), na = "")
