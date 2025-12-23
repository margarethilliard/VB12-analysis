# ---- Set up ---- 
set.seed(8675309)

#install.packages(c("readr", "dplyr", "tidyr", "ggpubr", "rstatix", "stringr", "ggplot2", "patchwork"))

# Load libraries 
library(readr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(rstatix)
library(stringr)
library(ggplot2)
library(patchwork)

# Set working directory and source the data
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# ---- Read in pathway completion data from anvio-estimate-metabolism ---- 
# Read in the module completness result file 
res <- readr::read_delim("data/B12_biosynth-module_pathwise_completeness-MATRIX.txt") %>%
  select(-c("module", "module_name", "module_class", "module_category", "module_subcategory"))

# Tidy up the data frame and add a subject_id identifier to merge on 
res <- res %>%
  tidyr::pivot_longer(cols = everything(),
                            names_to = "MAG",
                            values_to = "pathwise_completion") %>% 
  mutate(across(c(MAG, pathwise_completion), as.character)) %>%
  mutate(subject_id = stringr::str_extract(MAG, "(?<=_).*?(?=_)")) %>%
  mutate(MAG = sub("^MAG_", "", MAG)) %>%
  mutate(MAG = sub("_", ".", MAG))
  
# Merge pathway completion results with metadata to identify supplement use and intake group by subject_id
data <- left_join(metadata_sub, res, by = "subject_id")

data$pathwise_completion <- as.numeric(data$pathwise_completion)

# ---- Data wrangling ---- 
# Total number of medium-quality draft MAGs per subject ID 
MAG_n <- readr::read_delim("/Users/local-margaret/Downloads/external_genomes_v2.txt") %>% 
  tidyr::separate(name, into = c("MAG", "subject_id", "number"), sep = "_") %>%
  select(subject_id, number) %>%
  group_by(subject_id) %>%
  summarise(n_MAGs = n())

# Total number of MAGs with a complete B12 synthesis pathway per subject ID 
complete_b12 <- data %>%
  dplyr::filter(pathwise_completion >= 0.9) %>%
  #dplyr::filter(pathwise_completion == 1) %>%
  group_by(subject_id) %>%
  mutate(n_complete_b12 = n()) %>%
  select(subject_id, n_complete_b12, intake_group, supplement_taker) %>%
  ungroup() %>%
  distinct()

data_merged <- left_join(complete_b12, MAG_n) %>%
  mutate(prop_complete_b12 = n_complete_b12/n_MAGs)

# ---- Plot 2A: differences in the proportion of MAGs with complete B12 synthesis pathway based on B12 intake group ---- 
stat.test_res <-  rstatix::wilcox_test(prop_complete_b12 ~ intake_group, data = data_merged) %>%
  rstatix::add_significance() 

stat.test_res

plot_A <- data_merged %>% 
  mutate(intake_group = factor(intake_group, levels = c("Low", "High"))) %>%
  group_by(intake_group) %>%
  mutate(avg = mean(prop_complete_b12),
         sd = sd(prop_complete_b12),
         se = sd(prop_complete_b12) / sqrt(n())) %>%
  mutate(sd_low = avg - se,
         sd_high = avg + se) %>%
  ungroup() %>%
  ggplot(aes(x = intake_group, y = avg, fill = intake_group)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = sd_low, ymax = sd_high),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  scale_fill_manual(values = c("#969696", "#e24f4a")) +
  labs(x = expression(B[12] ~ "intake group relative to median"),
       y = expression(atop("Proportion of total MAGs with complete", 
                           B[12] ~ " synthesis pathway"))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + 
  ggpubr::stat_pvalue_manual(stat.test_res, label = "p", y.position = 0.22, vjust = 0.25,
                             tip.length = 0.05, label.size = 4, position = position_identity())  # shortens length of bracket

plot_A

# ---- Plot 3B: differences in the proportion of MAGs with complete B12 synthesis pathway based on supplement use ---- 
stat.test_res <-  rstatix::wilcox_test(prop_complete_b12 ~ supplement_taker, data = data_merged) %>%
  rstatix::add_significance() 

stat.test_res

plot_B <- data_merged %>% 
  group_by(supplement_taker) %>%
  mutate(avg = mean(prop_complete_b12),
         sd = sd(prop_complete_b12),
         se = sd(prop_complete_b12) / sqrt(n())) %>%
  mutate(sd_low = avg - se,
         sd_high = avg + se) %>%
  ungroup() %>%
  ggplot(aes(x = supplement_taker, y = avg, fill = supplement_taker)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = sd_low, ymax = sd_high),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  scale_fill_manual(values = c("#969696", "#e24f4a")) +
  labs(x = expression(B[12] ~ "supplement use"),
       y = expression(atop("Proportion of total MAGs with complete", 
                           B[12] ~ " synthesis pathway"))) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + 
  ggpubr::stat_pvalue_manual(stat.test_res, label = "p.signif", y.position = 0.22, vjust = 0.25,
                             tip.length = 0.05, label.size = 4, position = position_identity())  # shortens length of bracket

plot_B

# ---- Design multi-panel figure ---- 
plot_A + plot_B +
  plot_annotation(tag_levels = "A")

#ggsave("figures/MAGs_Patchwork.pdf", width = 8, height = 5)
