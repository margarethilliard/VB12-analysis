
#---- Setup ----

#install.packages(c("dplyr", "tidyverse", "readr"))

library(dplyr)
library(tidyverse)
library(readr)

setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

#---- Subset regression metadata based on high/low habitual intake relative to median intake or supplement use ----

regression_full_data <- metadata_sub %>%
  dplyr::select(c("subject_id", "age", "sex", "bmi", "eGFR", "habitual_dietary_b12", "plasma_b12")) 
#write_csv(regression_full_data, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_intake_low <- metadata_sub %>%
  dplyr::select(c("subject_id", "age", "sex", "bmi", "eGFR", "habitual_dietary_b12", "plasma_b12", "intake_group")) %>%
  dplyr::filter(intake_group == "Low") %>%
  dplyr::select(-c("intake_group"))
#readr::write_csv(regression_intake_low, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_intake_high <- metadata_sub %>%
  dplyr::select(c("subject_id", "age", "sex", "bmi", "eGFR", "habitual_dietary_b12", "plasma_b12", "intake_group")) %>%
  dplyr::filter(intake_group == "High") %>%
  dplyr::select(-c("intake_group"))
#readr::write_csv(regression_intake_high, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_supp_no <- metadata_sub %>%
  dplyr::select(c("subject_id", "age", "sex", "bmi", "eGFR", "habitual_dietary_b12", "plasma_b12", "supplement_taker")) %>%
  dplyr::filter(supplement_taker == "No") %>%
  dplyr::select(-c("supplement_taker"))
#readr::write_csv(regression_supp_no, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_supp_yes <- metadata_sub %>%
  dplyr::select(c("subject_id", "age", "sex", "bmi", "eGFR", "habitual_dietary_b12", "plasma_b12", "supplement_taker")) %>%
  dplyr::filter(supplement_taker == "Yes") %>%
  dplyr::select(-c("supplement_taker"))
#readr::write_csv(regression_supp_yes, "/Users/local-margaret/Downloads/dietML_metadata.csv")

#---- Subset merged relative abundance table to include only TaxaHFE-engineered taxa ----

# Full data model
clades_to_keep_FD <- c("d__Bacteria|p__Proteobacteria|c__Alphaproteobacteria|o__RF32",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Monoglobales",
                       "d__Bacteria|p__Firmicutes_C|c__Negativicutes|o__Acidaminococcales|f__Acidaminococcaceae|g__Phascolarctobacterium",
                       "d__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Lactococcus",
                       "d__Bacteria|p__Firmicutes|c__Bacilli|o__Erysipelotrichales|f__Erysipelotrichaceae|g__Holdemania",
                       "d__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Streptococcus|s__Streptococcus_thermophilus",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Oscillospiraceae|g__Dysosmobacter|s__Dysosmobacter_sp001916835",
                       "d__Bacteria|p__Actinobacteriota|c__Coriobacteriia|o__Coriobacteriales|f__Eggerthellaceae|g__Senegalimassilia|s__Senegalimassilia_anaerobia")

# Low intake subset model
clades_to_keep_LI <- c("d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Fusicatenibacter",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__AM51-8",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Scatomonas",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Ruminococcaceae|g__Negativibacillus|s__Negativibacillus_sp000435195",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Oscillospiraceae|g__Evtepia|s__Evtepia_gabavorous",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Blautia_A|s__Blautia_A_hydrogenotrophica",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Mediterraneibacter|s__Mediterraneibacter_lactaris",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__CAG-317",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Oscillospiraceae|g__CAG-170|s__CAG-170_sp002404795")

# High intake subset model
clades_to_keep_HI <- c("d__Bacteria|p__Proteobacteria|c__Alphaproteobacteria",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Oscillospiraceae|g__Dysosmobacter",
                       "d__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Lactococcus",
                       "d__Bacteria|p__Bacteroidota|c__Bacteroidia|o__Bacteroidales|f__Bacteroidaceae|g__Bacteroides|s__Bacteroides_ovatus",
                       "d__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Streptococcus|s__Streptococcus_thermophilus",
                       "d__Bacteria|p__Firmicutes|c__Bacilli|o__RF39",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__TANB77|f__CAG-508|g__CAG-245|s__CAG-245_sp000435175",
                       "d__Bacteria|p__Actinobacteriota|c__Coriobacteriia|o__Coriobacteriales|f__Eggerthellaceae|g__Senegalimassilia|s__Senegalimassilia_anaerobia")

# No supplement subset model
clades_to_keep_NS <- c("d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Eisenbergiella",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__AM51-8",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Ruminococcaceae|g__Faecalibacterium|s__Faecalibacterium_prausnitzii_C",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Eubacterium_F|s__Eubacterium_F_sp003491505",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Blautia_A|s__Blautia_A_faecis",
                       "d__Bacteria|p__Firmicutes_C|c__Negativicutes|o__Veillonellales|f__Dialisteraceae|g__Dialister|s__Dialister_invisus",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Ruminococcaceae|g__Negativibacillus|s__Negativibacillus_sp000435195",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__Mediterraneibacter|s__Mediterraneibacter_lactaris",
                       "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Lachnospirales|f__Lachnospiraceae|g__CAG-317")

# Yes supplement subset model
clades_to_keep_S <- c("d__Bacteria|p__Firmicutes_C",
                      "d__Bacteria|p__Proteobacteria",
                      "d__Bacteria|p__Bacteroidota|c__Bacteroidia|o__Bacteroidales|f__Tannerellaceae",
                      "d__Bacteria|p__Desulfobacterota_I|c__Desulfovibrionia|o__Desulfovibrionales|f__Desulfovibrionaceae|g__Bilophila",
                      "d__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Lactococcus",
                      "d__Bacteria|p__Bacteroidota|c__Bacteroidia|o__Bacteroidales|f__Bacteroidaceae|g__Phocaeicola|s__Phocaeicola_vulgatus",
                      "d__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Streptococcus|s__Streptococcus_thermophilus",
                      "d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Oscillospiraceae|g__Dysosmobacter|s__Dysosmobacter_sp001916835")

# Filter defined taxa from the merged relative abundance taxa table 
taxa_table_filtered_FD <- taxonomy_table %>% dplyr::filter(clade_name %in% clades_to_keep_FD)
taxa_table_filtered_LI <- taxonomy_table %>% dplyr::filter(clade_name %in% clades_to_keep_LI)
taxa_table_filtered_HI <- taxonomy_table %>% dplyr::filter(clade_name %in% clades_to_keep_HI)
taxa_table_filtered_NS <- taxonomy_table %>% dplyr::filter(clade_name %in% clades_to_keep_NS)
taxa_table_filtered_S <- taxonomy_table %>% dplyr::filter(clade_name %in% clades_to_keep_S)

# Rename the first column to "taxa"
colnames(taxa_table_filtered_FD)[1] <- "taxa"
colnames(taxa_table_filtered_LI)[1] <- "taxa"
colnames(taxa_table_filtered_HI)[1] <- "taxa"
colnames(taxa_table_filtered_NS)[1] <- "taxa"
colnames(taxa_table_filtered_S)[1] <- "taxa"

#  Pivot longer (so you have subject, taxa, value)
taxa_table_filtered_long_FD <- taxa_table_filtered_FD %>%
  pivot_longer(cols = -taxa, names_to = "subject_id", values_to = "abundance")

taxa_table_filtered_long_LI <- taxa_table_filtered_HI %>%
  pivot_longer(cols = -taxa, names_to = "subject_id", values_to = "abundance")

taxa_table_filtered_long_HI <- taxa_table_filtered_LI %>%
  pivot_longer(cols = -taxa, names_to = "subject_id", values_to = "abundance")

taxa_table_filtered_long_NS <- taxa_table_filtered_NS %>%
  pivot_longer(cols = -taxa, names_to = "subject_id", values_to = "abundance")

taxa_table_filtered_long_S <- taxa_table_filtered_S %>%
  pivot_longer(cols = -taxa, names_to = "subject_id", values_to = "abundance")

# Pivot wider (so subjects are rows, taxa are columns)
taxa_table_transposed_FD <- taxa_table_filtered_long_FD %>%
  pivot_wider(names_from = taxa, values_from = abundance)
taxa_table_transposed_FD$subject_id <- as.character(taxa_table_transposed_FD$subject_id)

taxa_table_transposed_LI <- taxa_table_filtered_long_LI %>%
  pivot_wider(names_from = taxa, values_from = abundance)
taxa_table_transposed_LI$subject_id <- as.character(taxa_table_transposed_LI$subject_id)

taxa_table_transposed_HI <- taxa_table_filtered_long_HI %>%
  pivot_wider(names_from = taxa, values_from = abundance)
taxa_table_transposed_HI$subject_id <- as.character(taxa_table_transposed_HI$subject_id)

taxa_table_transposed_NS <- taxa_table_filtered_long_NS %>%
  pivot_wider(names_from = taxa, values_from = abundance)
taxa_table_transposed_NS$subject_id <- as.character(taxa_table_transposed_NS$subject_id)

taxa_table_transposed_S <- taxa_table_filtered_long_S %>%
  pivot_wider(names_from = taxa, values_from = abundance)
taxa_table_transposed_S$subject_id <- as.character(taxa_table_transposed_S$subject_id)

# Join respective metadata and TaxaHFE-engineered data sets 
regression_full_data_HFE <- left_join(regression_full_data, taxa_table_transposed_FD, by = "subject_id") %>% drop_na()
#readr::write_csv(regression_full_data_HFE, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_low_intake_HFE <- left_join(regression_intake_low, taxa_table_transposed_LI, by = "subject_id") %>% drop_na()
#readr::write_csv(regression_low_intake_HFE, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_high_intake_HFE <- left_join(regression_intake_high, taxa_table_transposed_HI, by = "subject_id") %>% drop_na()
#readr::write_csv(regression_high_intake_HFE, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_supp_no_HFE <- left_join(regression_supp_no, taxa_table_transposed_NS, by = "subject_id") %>% drop_na()
#readr::write_csv(regression_supp_no_HFE, "/Users/local-margaret/Downloads/dietML_metadata.csv")

regression_supp_yes_HFE <- left_join(regression_supp_yes, taxa_table_transposed_S, by = "subject_id") %>% drop_na()
#readr::write_csv(regression_supp_yes_HFE, "/Users/local-margaret/Downloads/dietML_metadata.csv")
