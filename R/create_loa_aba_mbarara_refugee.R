library(tidyverse)
library(supporteR)

# clean data
clean_data_path_refugee <- "inputs/UGA2402_aba_mbarara_refugee_cleaned_data.xlsx"
clean_data_nms_refugee <- names(readxl::read_excel(path = clean_data_path_refugee, n_max = 2000, sheet = "cleaned_data"))
clean_c_types_refugee <- ifelse(str_detect(string = clean_data_nms_refugee, pattern = "_other$"), "text", "guess")
df_main_clean_data_refugee <- readxl::read_excel(path = clean_data_path_refugee, col_types = clean_c_types_refugee, na = "NA", sheet = "cleaned_data") 

# loops
# roster
clean_data_nms_r_roster_refugee <- names(readxl::read_excel(path = clean_data_path_refugee, n_max = 300, sheet = "cleaned_roster"))
clean_c_types_r_roster_refugee <- ifelse(str_detect(string = clean_data_nms_r_roster_refugee, pattern = "_other$"), "text", "guess")
df_clean_loop_r_roster_refugee <- readxl::read_excel(clean_data_path_refugee, col_types = clean_c_types_r_roster_refugee, sheet = "cleaned_roster") %>% 
    select(-any_of(c("interview_cell", "today", "enumerator_id", "point_number")))
# income
clean_data_nms_r_income_refugee <- names(readxl::read_excel(path = clean_data_path_refugee, n_max = 300, sheet = "cleaned_income_received"))
clean_c_types_r_income_refugee <- ifelse(str_detect(string = clean_data_nms_r_income_refugee, pattern = "_other$"), "text", "guess")
df_clean_loop_r_income_refugee <- readxl::read_excel(clean_data_path_refugee, col_types = clean_c_types_r_income_refugee, sheet = "cleaned_income_received")


# tool
loc_tool_refugee <- "inputs/UGA2402_aba_mbarara_refugee_tool.xlsx"
df_survey_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "survey")
df_choices_refugee <- readxl::read_excel(loc_tool_refugee, sheet = "choices")

# check empty columns in sampled and exclude them from the loa
empty_cols_refugee <- df_main_clean_data_refugee %>% 
    select(-matches("^note_|_note$|^qn_note_|_other$|\\/")) %>% 
    select(where(function(x) all(is.na(x)))) %>%
    colnames()

vars_to_remove_refugee <- c("consent", "enumerator_id", "hoh_equivalent", 
                            "hh_living_cell", "count_hh_number", "fgd_participation",
                            "fcs_cereals", "fcs_pulses", "fcs_dairy", "fcs_protein", "fcs_vegetables", "fcs_fruits", "fcs_oils", "fcs_sugar", "fcs_condiments",
                            "vulnerability_see", "vulnerability_hear", "vulnerability_walk", "vulnerability_concentrate", "vulnerability_self_care", "vulnerability_communicate")

df_loa_with_composites_refugee <- df_survey_refugee %>% 
    filter(str_detect(string = type, pattern = "integer|select_one|select_multiple"),
           !str_detect(string = name, pattern = "_version_"),
           !name %in% vars_to_remove_refugee) %>% 
    separate(col = type, into = c("int.select_type", "int.list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
    mutate(analysis_var = name, analysis_type = case_when(int.select_type %in% c("integer") ~ "mean",
                                                          int.select_type %in% c("select_one") ~ "prop_select_one",
                                                          int.select_type %in% c("select_multiple") ~ "prop_select_multiple"
    )) %>%
    select(analysis_type, analysis_var) %>% 
    bind_rows(tibble::tribble(
        ~analysis_var,    ~analysis_type,
        "i.fcs_cat", "prop_select_one",
        "i.avg_family_size", "mean",
        "i.shelter_crowding_index", "mean",
        "i.months_displaced", "prop_select_one",
        "i.hh_members_age_group", "prop_select_one",
        "i.member_hoh_by_gender", "prop_select_one",
        "i.female_hoh_single_parent", "prop_select_one",
        "i.hoh_education_level", "prop_select_one",
        "i.hh_with_lactating_mother", "prop_select_one",
        "i.num_pregnant_lactating", "mean",
        "i.hh_with_unaccompanied_separated_or_orphan", "prop_select_one",
        "i.disability_prevalence", "prop_select_one",
        "i.hh_with_disabled_member", "prop_select_one",
        "i.hoh_disability", "prop_select_one",
        "i.enough_money_for_food_single_f_hoh", "prop_select_one",
        "i.enough_money_for_educ_and_health_single_f_hoh", "prop_select_one",
        "i.hh_received_aid_past_single_f_hoh", "prop_select_one",
        "i.unmet_needs_single_f_hoh", "prop_select_multiple",
        "i.hh_total_income", "mean",
        "i.hh_income_from_own_business", "mean",
        "i.hh_casual_or_seasonal_labour", "mean",
        "i.hh_un_agencies_or_ngos", "mean",
        "i.hh_employment", "mean",
        "i.hh_remittances", "mean",
        "i.hh_crop_production", "mean",
        "i.hh_support_from_family_and_friends", "mean",
        "i.hh_borrowing_or_credit", "mean",
        "i.hh_other", "mean",
    )) %>% 
    filter(!analysis_var %in% empty_cols_refugee)


# handling loa for specific datasets --------------------------------------

loa_list <- list()

# main dataset

df_loa_disag_refugee_main <- df_loa_with_composites_refugee %>% 
    mutate(subset_1 = "default",
           subset_2 = case_when(analysis_var %in% c("rank_mbarara_decision_impact_on_livelihood") ~ "i.member_hoh_by_gender, hh_time_stayed_in_mbarara",
                                ),
           subset_3 = "i.member_hoh_by_gender"
    ) %>% 
    pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "group_var") %>% 
    filter(!is.na(group_var), !group_var %in% c("NA")) %>% 
    select(-subset_no) %>% 
    mutate(level = 0.95, group_var = ifelse(group_var %in% c("default"), NA_character_, group_var)) %>% 
    filter(!(analysis_var %in% c("respondent_gender") & group_var %in% c("respondent_gender")),
           !(analysis_var %in% c("i.member_hoh_by_gender") & group_var %in% c("i.member_hoh_by_gender"))
           ) %>% 
    filter(analysis_var %in% c("i.months_displaced",
                               "i.avg_family_size", 
                               "i.member_hoh_by_gender",
                               "i.female_hoh_single_parent",
                               "i.hoh_education_level",
                               "i.hh_with_lactating_mother",
                               "i.num_pregnant_lactating",
                               "i.hh_with_unaccompanied_separated_or_orphan",
                               "i.hh_with_disabled_member",
                               "i.hoh_disability",
                               "i.fcs_cat", 
                               "i.shelter_crowding_index",
                               "i.enough_money_for_food_single_f_hoh",
                               "i.enough_money_for_educ_and_health_single_f_hoh",
                               "i.hh_received_aid_past_single_f_hoh",
                               "i.unmet_needs_single_f_hoh",
                               "i.hh_total_income",
                               "i.hh_income_from_own_business",
                               "i.hh_casual_or_seasonal_labour",
                               "i.hh_un_agencies_or_ngos",
                               "i.hh_employment",
                               "i.hh_remittances",
                               "i.hh_crop_production",
                               "i.hh_support_from_family_and_friends",
                               "i.hh_borrowing_or_credit",
                               "i.hh_other",
                               colnames(df_main_clean_data_refugee)) ) %>% 
    mutate(dataset = "main_data")

add_checks_data_to_list("loa_list", "df_loa_disag_refugee_main")

# roster

df_loa_disag_refugee_roster <- df_loa_with_composites_refugee %>% 
    mutate(subset_1 = "default",
           subset_2 = case_when(analysis_var %in% c("i.disability_prevalence") ~ "i.disability_age_group",
                                 analysis_var %in% c("occupation_status") ~ "gender, i.occupation_age_group",
                                 analysis_var %in% c("child_engaged_in_hh_labor") ~ "gender, i.child_labor_age_group",
                                 analysis_var %in% c("child_enrollment_status") ~ "gender, i.child_enrollment_age_group",
                                 analysis_var %in% c("regular_school_attendance") ~ "gender, i.regular_school_attendance_age_group",
                                 analysis_var %in% c("reason_child_not_attending_school") ~ "gender",
                                 analysis_var %in% c("non_formal_educ_activities") ~ "gender, i.non_formal_educ_activities_age_group",
           ),
    ) %>% 
    pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "group_var") %>% 
    filter(!is.na(group_var), !group_var %in% c("NA")) %>% 
    select(-subset_no) %>% 
    mutate(level = 0.95, group_var = ifelse(group_var %in% c("default"), NA_character_, group_var)) %>% 
    filter(!(analysis_var %in% c("gender") & group_var %in% c("gender")) # gender to be changed by actual groupings
           ) %>% 
    filter(analysis_var %in% c("i.hh_members_age_group",
                               "i.disability_prevalence",
                               colnames(df_clean_loop_r_roster_refugee))) %>% 
    mutate(dataset = "roster")

add_checks_data_to_list("loa_list", "df_loa_disag_refugee_roster")

# income received
df_loa_disag_refugee_income <- df_loa_with_composites_refugee %>% 
    mutate(subset_1 = "default",
           subset_2 = "income_post",
           # subset_3 = case_when(analysis_var %in% c("type_health_facility_accessed") ~ " reason_for_health_facility_choice",
           #                      analysis_var %in% c("hh_primary_income_source") ~ " hh_length_stay_adjumani_city"
           #                      )
    ) %>% 
    pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "group_var") %>% 
    filter(!is.na(group_var), !group_var %in% c("NA")) %>% 
    select(-subset_no) %>% 
    mutate(level = 0.95, group_var = ifelse(group_var %in% c("default"), NA_character_, group_var)) %>% 
    filter(!(analysis_var %in% c("income_post") & group_var %in% c("income_post"))
           ) %>% 
    filter(analysis_var %in% colnames(df_clean_loop_r_income_refugee)) %>% 
    mutate(dataset = "income_received")

add_checks_data_to_list("loa_list", "df_loa_disag_refugee_income")


# combine and output the loa ----------------------------------------------


# combine loas

combined_loa <- bind_rows(loa_list)

# output r_loa
write_csv(x = combined_loa, file = paste0("outputs/", butteR::date_file_prefix(), "_r_loa_aba_mbarara_refugee.csv"), na = "NA") 
write_csv(x = combined_loa, file = "inputs/r_loa_aba_mbarara_refugee.csv", na = "NA")

