# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

source("R/support_functions.R")


# water -------------------------------------------------------------------

# Read data and checking log 

df_cleaning_log_water <- read_csv("inputs/combined_checks_rehoboth_water.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) %>%
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>% 
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         sheet = NA_character_,
         index = NA_character_,
         relevant = NA_character_) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
loc_data_water <- "inputs/rehoboth_water_check_support.csv"

df_raw_data_water <- readr::read_csv(file = loc_data_water, na = c("NA", "N/A", "n/a")) %>% 
  mutate(`_uuid` = uuid,
         `_index` = row_number()) %>% 
  select(-uuid)

# tool
loc_tool_water <- "inputs/rehoboth_water_form_v2.xlsx"

df_survey_water <- readxl::read_excel(loc_tool_water, sheet = "survey")
df_choices_water <- readxl::read_excel(loc_tool_water, sheet = "choices")

# main dataset

df_cleaning_log_main_water <-  df_cleaning_log_water %>% 
  filter(is.na(sheet))

df_cleaning_step_water <- test_data_pre_cleaning(input_df_raw_data = df_raw_data_water,
                                           input_df_survey = df_survey_water,
                                           input_df_choices = df_choices_water,
                                           input_df_cleaning_log = df_cleaning_log_main_water)

# write final datasets out
list_of_clean_datasets_water <- list("cleaned_water" = df_cleaning_step_water
)
openxlsx::write.xlsx(x = list_of_clean_datasets_water,
                     file = paste0("inputs/clean_rehoboth_water.xlsx"),
                     overwrite = TRUE, na.string = "")

# building -------------------------------------------------------------------

# Read data and checking log 

df_cleaning_log_building <- read_csv("inputs/combined_checks_rehoboth_building.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) %>%
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>% 
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         sheet = NA_character_,
         index = NA_character_,
         relevant = NA_character_) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
loc_data_building <- "inputs/rehoboth_building_check_support.csv"

df_raw_data_building <- readr::read_csv(file = loc_data_building, na = c("NA", "N/A", "n/a")) %>% 
  mutate(`_uuid` = uuid,
         `_index` = row_number()) %>% 
  select(-uuid)

# tool
loc_tool_building <- "inputs/rehoboth_building_inspectorate_form.xlsx"

df_survey_building <- readxl::read_excel(loc_tool_building, sheet = "survey")
df_choices_building <- readxl::read_excel(loc_tool_building, sheet = "choices")

# main dataset

df_cleaning_log_main_building <-  df_cleaning_log_building %>% 
  filter(is.na(sheet))

df_cleaning_step_building <- test_data_pre_cleaning(input_df_raw_data = df_raw_data_building,
                                                    input_df_survey = df_survey_building,
                                                    input_df_choices = df_choices_building,
                                                    input_df_cleaning_log = df_cleaning_log_main_building)

# write final datasets out
list_of_clean_datasets_building <- list("cleaned_building" = df_cleaning_step_building
)
openxlsx::write.xlsx(x = list_of_clean_datasets_building,
                     file = paste0("inputs/clean_rehoboth_building.xlsx"),
                     overwrite = TRUE, na.string = "")

# environmental -------------------------------------------------------------------

# Read data and checking log 

df_cleaning_log_environmental <- read_csv("inputs/combined_checks_rehoboth_environmental.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) %>%
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) %>% 
  filter(!is.na(value), !is.na(uuid)) %>%
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         sheet = NA_character_,
         index = NA_character_,
         relevant = NA_character_) %>%
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
loc_data_environmental <- "inputs/rehoboth_environmental_check_support.csv"

df_raw_data_environmental <- readr::read_csv(file = loc_data_environmental, na = c("NA", "N/A", "n/a")) %>% 
  mutate(`_uuid` = uuid,
         `_index` = row_number()) %>% 
  select(-uuid)

# tool
loc_tool_environmental <- "inputs/rehoboth_environmental_health_form.xlsx"

df_survey_environmental <- readxl::read_excel(loc_tool_environmental, sheet = "survey")
df_choices_environmental <- readxl::read_excel(loc_tool_environmental, sheet = "choices")

# main dataset

df_cleaning_log_main_environmental <-  df_cleaning_log_environmental %>% 
  filter(is.na(sheet))

df_cleaning_step_environmental <- test_data_pre_cleaning(input_df_raw_data = df_raw_data_environmental,
                                                         input_df_survey = df_survey_environmental,
                                                         input_df_choices = df_choices_environmental,
                                                         input_df_cleaning_log = df_cleaning_log_main_environmental)

# write final datasets out
list_of_clean_datasets_environmental <- list("cleaned_environmental" = df_cleaning_step_environmental
)
openxlsx::write.xlsx(x = list_of_clean_datasets_environmental,
                     file = paste0("inputs/clean_rehoboth_environmental.xlsx"),
                     overwrite = TRUE, na.string = "")
