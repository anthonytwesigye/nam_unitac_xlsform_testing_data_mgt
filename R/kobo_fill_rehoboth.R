library(tidyverse)
library(supporteR)
library(xlsformfill) # https://github.com/impact-initiatives/xlsformfill

# tool
loc_tool <- "inputs/rehoboth_building_inspectorate_form.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

set.seed(12333) 
test_data <- xlsformfill::xlsform_fill(df_survey, df_choices, n = 100)

openxlsx::write.xlsx(test_data, "outputs/test_kobo.xlsx")
