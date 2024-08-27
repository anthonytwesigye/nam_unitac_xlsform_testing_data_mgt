library(tidyverse)
library(supporteR)
library(xlsformfill) # https://github.com/impact-initiatives/xlsformfill

# tool
# loc_tool <- "inputs/testing_tool.xlsx"
loc_tool <- "inputs/testing_tool_groups.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

set.seed(12333) 
test_data <- xlsformfill::xlsform_fill(df_survey, df_choices, n = 50)

# openxlsx::write.xlsx(test_data, "outputs/test_kobo.xlsx")

write_csv(test_data %>% filter(consent %in% c("yes")), "outputs/testing_tool_groups_data.csv")
