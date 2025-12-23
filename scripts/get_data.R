
# ---- Install and load libraries ----

#install.packages(c("readr", "readxl", "janitor", "dplyr", "tidyr", "tibble", "bestNormalize"))

# Load libraries 
library(readr)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(tibble)
library(bestNormalize)

# ---- Plasma B12 + co-variates ----
# Read in Food Frequency Questionnaire (FFQ) metadata 
FFQ_metadata <- readr::read_csv("/Users/local-margaret/Desktop/VB12-analysis/data/FL100-FFQ.csv") %>%
  as_tibble() %>%
  dplyr::select(c(subject_id, age_ffq, sex_ffq, heightfeet, heightinches, weight,
                  dt_fiber_insol, dt_fiber_sol)) %>% # n = 393 w/ NAs 
  na.exclude # n=359 

# Read in FFQ diet data with plasma B12 measurements
FFQ <- readxl::read_xlsx("/Users/local-margaret/Desktop/VB12-analysis/data/FL100_VitB12_FFQ_plasma.xlsx") %>%
  na.exclude %>% # n = 321 
  #  Merge with FFQ metadata 
  dplyr::left_join(FFQ_metadata, by = "subject_id") %>%
  # Calculate BMI 
  mutate(total_height_inches = (heightfeet*12) + heightinches) %>%
  relocate(total_height_inches, .after = heightinches) %>%
  mutate(BMI = weight / (total_height_inches^2) * 702) %>%
  relocate(BMI, .after = weight) %>%
  dplyr::group_by(subject_id) %>%
  # Calculate total B12 intake 
  mutate(FFQ_B12_sum = dt_vb12 + dt_b12ad + sup_b12) %>%
  relocate(FFQ_B12_sum, .after = sup_b12) %>%
  ungroup() %>%
  # Create binary deficiency variable 
  mutate(B12_status = case_when(vitamin_b12 < 148 ~ "Deficient",
                                vitamin_b12 >= 148 ~ "Replete")) %>%
  # Create binary supplement use variable 
  mutate(supplement_taker = case_when(sup_b12 > 0 ~ "Yes",
                                      sup_b12 == 0 ~ "No"))

# Read in excretion variable data (from Stephanie M.G. Wilson on 12/16/2024) 
metadata <- read.csv("/Users/local-margaret/Desktop/VB12-analysis/data/FL100_eGFR.csv", header = T) %>%
  dplyr::select(c(subject_id, eGFR)) %>%
  # Merge with FFQ/plasma B12 data 
  right_join(FFQ) %>%
  dplyr::rename(age = age_ffq,
         sex = sex_ffq,
         bmi = BMI,
         habitual_dietary_b12 = FFQ_B12_sum,
         daily_supplemented_b12 = sup_b12,
         plasma_b12 = vitamin_b12) %>%
  dplyr::mutate(., sex = ifelse(sex == 1, "Male", "Female")) %>%
  dplyr::mutate(intake_group = case_when(
    # Define high vs. low intake based on the median intake
    habitual_dietary_b12 >= median(habitual_dietary_b12, na.rm = TRUE) ~ "High",
    habitual_dietary_b12 <  median(habitual_dietary_b12, na.rm = TRUE) ~ "Low"))
metadata$subject_id <- as.character(metadata$subject_id)

# ---- Normalize variables ----
# Normalized habitual intake 
habitual_b12_norm <- bestNormalize::bestNormalize(metadata$habitual_dietary_b12)
print(habitual_b12_norm)
metadata$habitual_b12_norm <- habitual_b12_norm$x.t

# ---- Microbiome composition ----
# All analyses use the GTDB taxonomy table 
# Raw copy 
taxonomy_table <- readr::read_table("/Users/local-margaret/Desktop/VB12-analysis/data/modified_merged_metaphlan_v4-0-6_GTDB.txt")

gtdb_taxonomy <- readr::read_table("/Users/local-margaret/Desktop/VB12-analysis/data/modified_merged_metaphlan_v4-0-6_GTDB.txt")
gtdb_taxonomy <- as.data.frame(gtdb_taxonomy)
rownames(gtdb_taxonomy) <- gtdb_taxonomy$clade_name
gtdb_taxonomy$clade_name <- NULL

# Transpose for visualization purposes
gtdb_taxonomy_t <- as.data.frame(t(gtdb_taxonomy))

# Define subjects that are in the metadata and taxonomy data sets
intersecting_ids <- intersect(metadata$subject_id, rownames(gtdb_taxonomy_t))

# Subset
gtdb_taxonomy_sub <- metadata %>% dplyr::filter(subject_id %in% intersecting_ids) 
gtdb_taxonomy_sub <- gtdb_taxonomy_t[intersecting_ids, , drop = FALSE]

# Clean
gtdb_taxonomy_sub$subject_id <- rownames(gtdb_taxonomy_sub)
gtdb_taxonomy_sub$subject_id <- as.character(gtdb_taxonomy_sub$subject_id)

# Join 
gtdb_taxonomy_subset <- inner_join(metadata_sub, gtdb_taxonomy_sub, by = "subject_id")

# ---- Short chain fatty acids ----
# from Andrew Oliver on 10 July 2025
scfa <- read.csv("/Users/local-margaret/Desktop/VB12-analysis/data/fecal_scfas.csv", header = T)
scfa$subject_id <- as.character(scfa$subject_id)
