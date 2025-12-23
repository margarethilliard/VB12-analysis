
# Credit to Sarah Blecksmith for the code, sourced from here: https://github.com/sblecksmith/CAZyme_project/blob/master/scripts/supplemental_figures.R

# load libraries
library(dplyr)
library(gt)

# source the data 
setwd("/Users/local-margaret/Desktop/VB12-analysis/")
source("scripts/get_data.R")

merged <- metadata_sub

# number of men and women
men = sum(merged$sex == "Male") # men = 143
women = sum(merged$sex == "Female") # women = 144

# age groups women
women_age1 = sum(merged$sex == "Female" & merged$age >= 18 & merged$age <= 33) # 53
women_age2 = sum(merged$sex == "Female" & merged$age >= 34 & merged$age <= 49) # 42
women_age3 = sum(merged$sex == "Female" & merged$age >= 50 & merged$age <= 65) # 48

# age groups men
men_age1 = sum(merged$sex == "Male" & merged$age >= 18 & merged$age <= 33) # 43
men_age2 = sum(merged$sex == "Male" & merged$age >= 34 & merged$age <= 49) # 53
men_age3 = sum(merged$sex == "Male" & merged$age >= 50 & merged$age <= 65) # 47

# bmi groups women
women_age1_bmi1 = sum(merged$sex == "Female" & merged$age >= 18 & merged$age <= 33 & merged$bmi < 25) # 17
women_age1_bmi2 = sum(merged$sex == "Female" & merged$age >= 18 & merged$age <= 33 & merged$bmi >= 25 & merged$bmi <= 29.9) # 19
women_age1_bmi3 = sum(merged$sex == "Female" & merged$age >= 18 & merged$age <= 33 & merged$bmi > 30) # 16

women_age2_bmi1 = sum(merged$sex == "Female" & merged$age >= 34 & merged$age <= 49 & merged$bmi < 25) # 19
women_age2_bmi2 = sum(merged$sex == "Female" & merged$age >= 34 & merged$age <= 49 & merged$bmi >= 25 & merged$bmi <= 29.9) # 10
women_age2_bmi3 = sum(merged$sex == "Female" & merged$age >= 34 & merged$age <= 49 & merged$bmi > 30) # 13

women_age3_bmi1 = sum(merged$sex == "Female" & merged$age >= 50 & merged$age <= 65 & merged$bmi < 25) # 17
women_age3_bmi2 = sum(merged$sex == "Female" & merged$age >= 50 & merged$age <= 65 & merged$bmi >= 25 & merged$bmi <= 29.9) # 22
women_age3_bmi3 = sum(merged$sex == "Female" & merged$age >= 50 & merged$age <= 65 & merged$bmi > 30) # 8

# bmi groups men
men_age1_bmi1 = sum(merged$sex == "Male" & merged$age >= 18 & merged$age <= 33 & merged$bmi < 25) # 19
men_age1_bmi2 = sum(merged$sex == "Male" & merged$age >= 18 & merged$age <= 33 & merged$bmi >= 25 & merged$bmi <= 29.9) # 17
men_age1_bmi3 = sum(merged$sex == "Male" & merged$age >= 18 & merged$age <= 33 & merged$bmi > 30) # 7

men_age2_bmi1 = sum(merged$sex == "Male" & merged$age >= 34 & merged$age <= 49 & merged$bmi < 25) # 21
men_age2_bmi2 = sum(merged$sex == "Male" & merged$age >= 34 & merged$age <= 49 & merged$bmi >= 25 & merged$bmi <= 29.9) # 19
men_age2_bmi3 = sum(merged$sex == "Male" & merged$age >= 34 & merged$age <= 49 & merged$bmi > 30) # 13

men_age3_bmi1 = sum(merged$sex == "Male" & merged$age >= 50 & merged$age <= 65 & merged$bmi < 25) # 17
men_age3_bmi2 = sum(merged$sex == "Male" & merged$age >= 50 & merged$age <= 65 & merged$bmi >= 25 & merged$bmi <= 29.9) # 22
men_age3_bmi3 = sum(merged$sex == "Male" & merged$age >= 50 & merged$age <= 65 & merged$bmi > 30) # 8

# make dataframe for table
Sex <- c(paste0("Female (n=", women, ")"), "-", "-", "-", "-","-", "-", "-", "-",
         paste0("Male (n=", men, ")"), "-", "-", "-", "-","-", "-", "-", "-")

Age <- c(paste0("18-33 (n=", women_age1, ")"), "-", "-", 
         paste0("34-49 (n=", women_age2, ")"),"-", "-",
         paste0("50-65 (n=", women_age3, ")"), "-", "-", 
         paste0("18-33 (n=", men_age1, ")"), "-", "-", 
         paste0("34-49 (n=", men_age2, ")"),"-", "-",
         paste0("50-65 (n=", men_age3, ")"), "-", "-")

BMI <- c(paste0("<25 (n=",women_age1_bmi1, ")"),
         paste0("25-29.9 (n=",women_age1_bmi2, ")"),
         paste0(">30 (n=",women_age1_bmi3, ")"),
         paste0("<25 (n=",women_age2_bmi1, ")"),
         paste0("25-29.9 (n=",women_age2_bmi2, ")"),
         paste0(">30 (n=",women_age2_bmi3, ")"),
         paste0("<25 (n=",women_age3_bmi1, ")"),
         paste0("25-29.9 (n=",women_age3_bmi2, ")"),
         paste0(">30 (n=",women_age3_bmi3, ")"),
         paste0("<25 (n=",men_age1_bmi1, ")"),
         paste0("25-29.9 (n=",men_age1_bmi2, ")"),
         paste0(">30 (n=",men_age1_bmi3, ")"),
         paste0("<25 (n=",men_age2_bmi1, ")"),
         paste0("25-29.9 (n=",men_age2_bmi2, ")"),
         paste0(">30 (n=",men_age2_bmi3, ")"),
         paste0("<25 (n=",men_age3_bmi1, ")"),
         paste0("25-29.9 (n=",men_age3_bmi2, ")"),
         paste0(">30 (n=",men_age3_bmi3, ")"))

age_sex_bmi <- data.frame(Sex, Age, BMI)

tableS1 <-  age_sex_bmi %>% 
  gt() %>%
  text_transform(locations = cells_body(columns = c(Age, Sex)),
                 fn = function(x) {
                   ifelse(x=="-", " ", x)}
  ) %>%
  gtsave("table_S1.html")
