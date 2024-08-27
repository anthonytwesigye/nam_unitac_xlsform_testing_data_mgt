library(tidyverse)
library(supporteR)
library(xlsformfill) # https://github.com/impact-initiatives/xlsformfill

# tool
loc_tool <- "inputs/testing_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

set.seed(12333) 
test_data <- xlsformfill::xlsform_fill(df_survey, df_choices, n = 30)

# openxlsx::write.xlsx(test_data, "outputs/test_kobo.xlsx")

write_csv(test_data, "outputs/testing_tool_data.csv")
