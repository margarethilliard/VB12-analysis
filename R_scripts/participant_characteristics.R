
# Source the data 
setwd("/Users/local-margaret/Desktop/VB12-analysis/")
source("scripts/get_data.R")

# Subset of participants with plasma B12 and metagenomes 
df <- metadata_sub

# Load libs 
library(dplyr)
library(gtsummary)
library(flextable)

# Create categorized variables from of the parent study 
df_bal <- df %>%
  dplyr::mutate(age_cat = cut(age,
                              breaks = c(18, 33, 49, 65),
                              labels = c("18-33", "34-49", "50-65"),
                              include.lowest = TRUE),
                bmi_cat = cut(bmi,
                              breaks = c(0, 24.9, 29.9, Inf),
                              labels = c("<25", "25-29.9", "≥30"),
                              include.lowest = TRUE))

# Run goodness-of-fit chi-square tests manually
p_age <- chisq.test(table(df_bal$age_cat))$p.value # tests equal split across 3 age groups
p_bmi <- chisq.test(table(df_bal$bmi_cat))$p.value # tests equal split across 3 BMI groups
p_sex <- chisq.test(table(df_bal$sex))$p.value # tests 50/50 male/female split

# Build the summary table
table1 <- df_bal %>%
  dplyr::select(age_cat, bmi_cat, sex) %>%
  tbl_summary(missing = "no",
              label = list(age_cat ~ "Age group (years)",
                           bmi_cat ~ "BMI category (kg/m²)",
                           sex     ~ "Sex")) %>%
  bold_labels()

# Add p-values manually 
table1_balance <- table1_balance %>%
  modify_table_body(
    ~ .x %>%
      mutate(p.value = case_when(variable == "age_cat" & row_type == "label" ~ p_age,
                                 variable == "bmi_cat" & row_type == "label" ~ p_bmi,
                                 variable == "sex" & row_type == "label" ~ p_sex,
                                 TRUE ~ NA_real_))) %>%
  modify_header(p.value = "**p-value**") %>%
  modify_fmt_fun(p.value ~ function(x) style_pvalue(x, digits = 3)) %>%
  bold_labels()

table1

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "table1.docx")

# Do the same thing, but look for balance in the intake groups 
table2 <- df %>%
  dplyr::select(age, sex, bmi, intake_group) %>%
  tbl_summary(by = intake_group,
              missing = "no",
              label = list(age ~ "Age (years)",
                           sex ~ "Sex",
                           bmi ~ "BMI (kg/m²)"),
    type = list(sex ~ "categorical", # will show n (%) 
                age ~ "continuous", # will show mean (SD)
                bmi ~ "continuous"), # will show mean (SD)
    statistic = list(age ~ "{mean} ({sd})",
                     bmi ~ "{mean} ({sd})")) %>% # No entry needed for sex since the categorical default is n (%) 
  # Be explicit about which tests you want
  add_p(test = list(age  ~ "t.test",
                    bmi  ~ "t.test",
                    sex  ~ "chisq.test")) %>%
  bold_labels() 

table2

table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "table2.docx")

# Do the same thing, but look for balance in the supplement use groups 
table3 <- df %>%
  dplyr::select(age, sex, bmi, supplement_taker) %>%
  tbl_summary(by = supplement_taker,
              missing = "no",
              label = list(age ~ "Age (years)",
                           sex ~ "Sex",
                           bmi ~ "BMI (kg/m²)"),
    type = list(sex ~ "categorical",    # will show n (%) 
                age ~ "continuous",     # will show mean (SD)
                bmi ~ "continuous"),    # will show mean (SD)
    statistic = list(age ~ "{mean} ({sd})",
                     bmi ~ "{mean} ({sd})" )) %>% # No entry needed for sex since the categorical default is n (%)
  # Be explicit about which tests you want
  add_p(test = list(age  ~ "t.test",           
                    bmi  ~ "t.test",            
                    sex  ~ "chisq.test")) %>%   
  add_overall() %>%
  bold_labels()

table3

table3 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "table3.docx")
