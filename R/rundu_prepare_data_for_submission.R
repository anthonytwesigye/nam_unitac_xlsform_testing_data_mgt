library(tidyverse)
library(supporteR)


df_gps_pts <- readxl::read_excel("inputs/gps_point_samples.xlsx", sheet = "rundu")

# water -------------------------------------------------------------------

# cleaned
loc_data_water_cleaned <- "inputs/clean_rundu_water.xlsx"
df_data_water_cleaned <- readxl::read_excel(loc_data_water_cleaned)

# extract groups
df_tool_groups_water <- df_survey_water %>% 
    mutate(i.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_)) %>% 
    fill(i.group) %>% 
    mutate(i.group = ifelse(str_detect(string = type, pattern = "group|repeat|^note$"), NA_character_, i.group),
           i.name = ifelse(!is.na(i.group), paste0(i.group,"/",name), name),
           i.name = ifelse(str_detect(string = type, pattern = "group|repeat|^note$"), NA_character_, i.name)) %>%
    filter(!is.na(i.name)) %>% 
    select(name, i.name)    

df_data_water_prepared <- df_data_water_cleaned %>% 
    rename_with(~recode(.x, !!!setNames(df_tool_groups_water$i.name, df_tool_groups_water$name))) %>% 
    mutate(across(.cols = starts_with("grp_review"), .fns = ~ifelse(!is.na(.x), NA_character_, .x))) %>% 
    mutate(`grp_details/gps` = "",
           `grp_details/_gps_latitude` = recode(row_number(), !!!setNames(df_gps_pts$lat, df_gps_pts$pt_num)),
           `grp_details/_gps_longitude` = recode(row_number(), !!!setNames(df_gps_pts$lon, df_gps_pts$pt_num)),
           `grp_details/_gps_altitude` = 0,
           `grp_details/_gps_precision` = 0,
           `grp_details/gps` = paste0(`grp_details/_gps_latitude`, " ", 
                                      `grp_details/_gps_longitude`, " ",
                                      `grp_details/_gps_altitude`, " ",
                                      `grp_details/_gps_precision`)) %>% 
    relocate(any_of(c("grp_details/gps", "grp_details/_gps_latitude", 
                    "grp_details/_gps_longitude", "grp_details/_gps_altitude", "grp_details/_gps_precision")), .after = "grp_details/cell") %>% 
    select(-c("uuid", "index"))

# export data
write_csv(df_data_water_prepared, "outputs/rundu_testing_data_water.csv", na = "")
write_csv(tibble(new_headings = colnames(df_data_water_prepared),
                 old_headings = new_headings), 
          "outputs/columns_rundu_data_water.csv")

# building inspection -----------------------------------------------------

# cleaned
loc_data_building_cleaned <- "inputs/clean_rundu_building.xlsx"
df_data_building_cleaned <- readxl::read_excel(loc_data_building_cleaned)

# extract groups
df_tool_groups_building <- df_survey_building %>% 
    mutate(i.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_)) %>% 
    fill(i.group) %>% 
    mutate(i.group = ifelse(str_detect(string = type, pattern = "group|repeat|^note$"), NA_character_, i.group),
           i.name = ifelse(!is.na(i.group), paste0(i.group,"/",name), name),
           i.name = ifelse(str_detect(string = type, pattern = "group|repeat|^note$"), NA_character_, i.name)) %>%
    filter(!is.na(i.name)) %>% 
    select(name, i.name)

df_data_building_prepared <- df_data_building_cleaned %>% 
    rename_with(~recode(.x, !!!setNames(df_tool_groups_building$i.name, df_tool_groups_building$name))) %>% 
    mutate(across(.cols = starts_with("grp_review"), .fns = ~ifelse(!is.na(.x), NA_character_, .x))) %>% 
    mutate(`grp_details/gps` = "",
           `grp_details/_gps_latitude` = recode(row_number(), !!!setNames(df_gps_pts$lat, df_gps_pts$pt_num)),
           `grp_details/_gps_longitude` = recode(row_number(), !!!setNames(df_gps_pts$lon, df_gps_pts$pt_num)),
           `grp_details/_gps_altitude` = 0,
           `grp_details/_gps_precision` = 0,
           `grp_details/gps` = paste0(`grp_details/_gps_latitude`, " ", 
                                      `grp_details/_gps_longitude`, " ",
                                      `grp_details/_gps_altitude`, " ",
                                      `grp_details/_gps_precision`)) %>% 
    relocate(any_of(c("grp_details/gps", "grp_details/_gps_latitude", 
                      "grp_details/_gps_longitude", "grp_details/_gps_altitude", "grp_details/_gps_precision")), .after = "grp_details/cell") %>% 
    select(-c("uuid", "index"))

# export data
write_csv(df_data_building_prepared, "outputs/rundu_testing_data_building.csv", na = "")
write_csv(tibble(new_headings = colnames(df_data_building_prepared), old_headings = new_headings), "outputs/columns_rundu_data_building.csv")

# environmental health ----------------------------------------------------

# cleaned
loc_data_environmental_cleaned <- "inputs/clean_rundu_environmental.xlsx"
df_data_environmental_cleaned <- readxl::read_excel(loc_data_environmental_cleaned)

# extract groups
df_tool_groups_environmental <- df_survey_environmental %>% 
    mutate(i.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_)) %>% 
    fill(i.group) %>% 
    mutate(i.group = ifelse(str_detect(string = type, pattern = "group|repeat|^note$"), NA_character_, i.group),
           i.name = ifelse(!is.na(i.group), paste0(i.group,"/",name), name),
           i.name = ifelse(str_detect(string = type, pattern = "group|repeat|^note$"), NA_character_, i.name)) %>%
    filter(!is.na(i.name)) %>% 
    select(name, i.name)

# prepare
df_data_environmental_prepared <- df_data_environmental_cleaned %>% 
    rename_with(~recode(.x, !!!setNames(df_tool_groups_environmental$i.name, df_tool_groups_environmental$name))) %>% 
    mutate(across(.cols = starts_with("grp_review"), .fns = ~ifelse(!is.na(.x), NA_character_, .x))) %>% 
    mutate(`grp_details/gps` = "",
           `grp_details/_gps_latitude` = recode(row_number(), !!!setNames(df_gps_pts$lat, df_gps_pts$pt_num)),
           `grp_details/_gps_longitude` = recode(row_number(), !!!setNames(df_gps_pts$lon, df_gps_pts$pt_num)),
           `grp_details/_gps_altitude` = 0,
           `grp_details/_gps_precision` = 0,
           `grp_details/gps` = paste0(`grp_details/_gps_latitude`, " ", 
                                      `grp_details/_gps_longitude`, " ",
                                      `grp_details/_gps_altitude`, " ",
                                      `grp_details/_gps_precision`)) %>% 
    relocate(any_of(c("grp_details/gps", "grp_details/_gps_latitude", 
                      "grp_details/_gps_longitude", "grp_details/_gps_altitude", "grp_details/_gps_precision")), .after = "grp_details/cell") %>% 
    select(-c("uuid", "index"))

# export data
write_csv(df_data_environmental_prepared, "outputs/rundu_testing_data_environmental.csv", na = "")
write_csv(tibble(new_headings = colnames(df_data_environmental_prepared), old_headings = new_headings), "outputs/columns_rundu_data_environmental.csv")

