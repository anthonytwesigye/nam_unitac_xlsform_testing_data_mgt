library(tidyverse)
library(supporteR)
library(xlsformfill) # https://github.com/impact-initiatives/xlsformfill


df_gps_pts <- readxl::read_excel("inputs/gps_point_samples.xlsx", sheet = "rehoboth")

# generate names
df_gen_names <- tibble(name = randomNames::randomNames(n = 500, ethnicity = 3, name.sep = " ")) %>% 
    mutate(entry_id = row_number(),
           phone_num = paste0("081", sample(3453682:9825300, 500))) %>% 
    select(entry_id, name, phone_num)
# sample(df_gen_names$name, 1)

# water -------------------------------------------------------------------

# tool
loc_tool_water <- "inputs/rehoboth_water_form_v2.xlsx"
df_survey_water <- readxl::read_excel(loc_tool_water, sheet = "survey")
df_choices_water <- readxl::read_excel(loc_tool_water, sheet = "choices")

# generate data
set.seed(12333) 
df_data_water <- xlsformfill::xlsform_fill(df_survey_water, df_choices_water, n = 300) %>% 
    select(-contains("/")) %>% 
    group_by(location) %>% 
    mutate(int.group_rn = row_number()) %>% 
    ungroup() %>% 
    mutate(reference_id = paste0("jcw_", row_number()),
           name = recode(row_number(), !!!setNames(df_gen_names$name, df_gen_names$entry_id)),
           cell = recode(row_number(), !!!setNames(df_gen_names$phone_num, df_gen_names$entry_id)),
           house_no = paste0(location,"_", int.group_rn)) %>% 
    select(-int.group_rn)

write_csv(df_data_water, "inputs/rehoboth_water_check_support.csv")

# building inspection -----------------------------------------------------

# tool
loc_tool_building <- "inputs/rehoboth_building_inspectorate_form.xlsx"
df_survey_building <- readxl::read_excel(loc_tool_building, sheet = "survey")
df_choices_building <- readxl::read_excel(loc_tool_building, sheet = "choices")

# generate data

set.seed(12333) 
df_data_building <- xlsformfill::xlsform_fill(df_survey_building, df_choices_building, n = 275) %>% 
    select(-contains("/")) %>% 
    mutate(application_id = paste0("jcb_", row_number()),
           name = recode(row_number(), !!!setNames(df_gen_names$name, df_gen_names$entry_id)),
           cell = recode(row_number(), !!!setNames(df_gen_names$phone_num, df_gen_names$entry_id)))
# export data
write_csv(df_data_building, "inputs/rehoboth_building_check_support.csv")

# environmental health ----------------------------------------------------

# tool
loc_tool_environmental <- "inputs/rehoboth_environmental_health_form.xlsx"
df_survey_environmental <- readxl::read_excel(loc_tool_environmental, sheet = "survey")
df_choices_environmental <- readxl::read_excel(loc_tool_environmental, sheet = "choices")

set.seed(12333) 
df_data_environmental <- xlsformfill::xlsform_fill(df_survey_environmental, df_choices_environmental, n = 365) %>% 
    select(-contains("/")) %>% 
    mutate(application_id = paste0("jce_", row_number()),
           name = recode(row_number(), !!!setNames(df_gen_names$name, df_gen_names$entry_id)),
           cell = recode(row_number(), !!!setNames(df_gen_names$phone_num, df_gen_names$entry_id)))
# export data
write_csv(df_data_environmental, "inputs/rehoboth_environmental_check_support.csv")
