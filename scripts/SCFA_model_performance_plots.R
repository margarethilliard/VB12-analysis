 
# ---- Set up ----

setwd("/Users/local-margaret/Desktop/VB12-analysis/")

# load libraries
library(dplyr)
library(plotrix) # for std err calc
library(ggplot2)
library(FSA) # dunn test
library(multcompView) # significance letter comparisons 
library(ggtext)
library(forcats)
library(patchwork)

# ---- Part 1: Pathway abundances ----

# define the directory where all output folders are located for each data set 

# High intake data sets
wd1 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/high_intake/pathways/acetate/"
wd2 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/high_intake/pathways/propionate/"
wd3 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/high_intake/pathways/butyrate/"

# Low intake data sets 
wd4 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/low_intake/pathways/acetate/"
wd5 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/low_intake/pathways/propionate/"
wd6 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/low_intake/pathways/butyrate/"

# Supplement user data sets 
wd7 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/supp_use/pathways/acetate/"
wd8 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/supp_use/pathways/propionate/"
wd9 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/supp_use/pathways/butyrate/"

# No supplement use data sets 
wd10 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/no_supp_use/pathways/acetate/"
wd11 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/no_supp_use/pathways/propionate/"
wd12 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/no_supp_use/pathways/butyrate/"

# list all ml_results.csv files in sub-directories
file_paths1 <- list.files(path = wd1, pattern = "ml_results.csv", 
                                        recursive = TRUE, full.names = TRUE)
file_paths2 <- list.files(path = wd2, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths3 <- list.files(path = wd3, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths4 <- list.files(path = wd4, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths5 <- list.files(path = wd5, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths6 <- list.files(path = wd6, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths7 <- list.files(path = wd7, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths8 <- list.files(path = wd8, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths9 <- list.files(path = wd9, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths10 <- list.files(path = wd10, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths11 <- list.files(path = wd11, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths12 <- list.files(path = wd11, pattern = "ml_results.csv", 
                           recursive = TRUE, full.names = TRUE)

# combine the files for the respective data set 
combined_results1 <- lapply(file_paths1, read.csv) %>% bind_rows() 
combined_results2 <- lapply(file_paths2, read.csv) %>% bind_rows() 
combined_results3 <- lapply(file_paths3, read.csv) %>% bind_rows() 
combined_results4 <- lapply(file_paths4, read.csv) %>% bind_rows() 
combined_results5 <- lapply(file_paths5, read.csv) %>% bind_rows() 
combined_results6 <- lapply(file_paths6, read.csv) %>% bind_rows() 
combined_results7 <- lapply(file_paths7, read.csv) %>% bind_rows() 
combined_results8 <- lapply(file_paths8, read.csv) %>% bind_rows() 
combined_results9 <- lapply(file_paths9, read.csv) %>% bind_rows() 
combined_results10 <- lapply(file_paths10, read.csv) %>% bind_rows() 
combined_results11 <- lapply(file_paths11, read.csv) %>% bind_rows() 
combined_results12 <- lapply(file_paths12, read.csv) %>% bind_rows() 

# add a column to define the model
combined_results1$model <- "dietML_high_intake_acetate"
combined_results2$model <- "dietML_high_intake_propionate"
combined_results3$model <- "dietML_high_intake_butyrate"

combined_results4$model <- "dietML_low_intake_acetate"
combined_results5$model <- "dietML_low_intake_propionate"
combined_results6$model <- "dietML_low_intake_butyrate"

combined_results7$model <- "dietML_supp_use_acetate"
combined_results8$model <- "dietML_supp_use_propionate"
combined_results9$model <- "dietML_supp_use_butyrate"

combined_results10$model <- "dietML_no_supp_use_acetate"
combined_results11$model <- "dietML_no_supp_use_propionate"
combined_results12$model <- "dietML_no_supp_use_butyrate"

# join the data 
joined_result <- full_join(combined_results1, combined_results2)
joined_result <- full_join(joined_result, combined_results3)
joined_result <- full_join(joined_result, combined_results4)
joined_result <- full_join(joined_result, combined_results5)
joined_result <- full_join(joined_result, combined_results6)
joined_result <- full_join(joined_result, combined_results7)
joined_result <- full_join(joined_result, combined_results8)
joined_result <- full_join(joined_result, combined_results9)
joined_result <- full_join(joined_result, combined_results10)
joined_result <- full_join(joined_result, combined_results11)
joined_result <- full_join(joined_result, combined_results12)

# calculate percent change in MAE compared to null model
combined_results <- joined_result %>% 
  janitor::clean_names() %>%
  dplyr::filter(., metric == "mae") %>%
  dplyr::mutate(., percent_change = ((estimate - null_model_avg) / (null_model_avg)*100)) 

# do a global statistical test 
kruskal.test(percent_change ~ model, data = combined_results)

## do a post-hoc test if the global test p-value < 0.05  
dunn_res <- FSA::dunnTest(percent_change ~ model, data = combined_results, method = "bh")

# Start with Dunn test results
dunn_df <- as.data.frame(dunn_res$res)

# Copy Dunn p-values
pvals <- dunn_df$P.adj

# Fix names: remove spaces around dash
names(pvals) <- gsub(" - ", "-", dunn_df$Comparison)

# Generate letters
letters <- multcompLetters(pvals, threshold = 0.05)

# Restore original model names in letters
letters_df <- data.frame(
  model = names(letters$Letters),
  letter = letters$Letters)

letters_df

# calculate mean percent change for MAE (regression models)
combined_results_mae <- combined_results %>% 
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_percent_change = mean(percent_change),
                   se_percent_change = plotrix::std.error(percent_change),
                   mean_MAE = mean(estimate), 
                   n_observations = n()) %>%
  dplyr::ungroup()

combined_results_mae <- combined_results_mae %>%
  left_join(letters_df, by = "model")

# clean response labels
response_labels <- c("dietML_high_intake_acetate" = "Acetate",
                     "dietML_high_intake_butyrate" = "Butryate",
                     "dietML_high_intake_propionate" = "Propionate",
                     "dietML_low_intake_butyrate" = "Butryate",
                     "dietML_low_intake_propionate" = "Propionate",
                     "dietML_low_intake_acetate" = "Acetate",
                     "dietML_no_supp_use_propionate" = "Propionate", 
                     "dietML_no_supp_use_acetate" = "Acetate",
                     "dietML_no_supp_use_butyrate" = "Butryate",
                     "dietML_supp_use_acetate" = "Acetate",
                     "dietML_supp_use_butyrate"= "Butryate",
                     "dietML_supp_use_propionate" = "Propionate")

# create another facet variable 
combined_results_mae <- combined_results_mae %>%
  mutate(group = case_when(
    grepl("_low_intake_", model) ~ "Low intake",
    grepl("_high_intake_", model) ~ "High intake",
    grepl("_no_", model) ~ "No supplement use",
    TRUE ~ "Supplement use" 
  ))

combined_results_mae$model <- factor(
  combined_results_mae$model,
  levels = c("dietML_low_intake_acetate", 
             "dietML_low_intake_propionate",
             "dietML_low_intake_butyrate",
             "dietML_high_intake_acetate",
             "dietML_high_intake_propionate",
             "dietML_high_intake_butyrate",
             "dietML_supp_use_acetate",
             "dietML_supp_use_propionate",
             "dietML_supp_use_butyrate",
             "dietML_no_supp_use_acetate",
             "dietML_no_supp_use_propionate",
             "dietML_no_supp_use_butyrate"))

# explicit order of sub-plots 
desired_order <- c("Low intake", "High intake", "No supplement use", "Supplement use")

combined_results_mae$group <- factor(
  combined_results_mae$group,
  levels = desired_order)

group_labels <- c("Low intake" = "Low intake (< 8.16 µg/d)",
                  "High intake" = "High intake (> 8.16 µg/d)")

# plot percent change in MAE compared to null model 
plot_1_pwys <- ggplot(combined_results_mae, aes(x = forcats::fct_rev(factor(model)), y = mean_percent_change)) +
  geom_col(aes(fill = group), position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_percent_change - se_percent_change,
                    ymax = mean_percent_change + se_percent_change),
                width = 0.2) +
  scale_fill_manual(values = c("#969696", "#e24f4a",
                               "#969696", "#e24f4a")) +
  geom_text(aes(label = letter, 
                y = 5), # adjust vertical position
            size =5) +  ylab("Mean Percent Change in MAE\nCompared to Null Model") +
  xlab("") +
  theme_bw(base_size = 18) +
  facet_wrap(~group, scales = "free_y", labeller = labeller(group = group_labels)) +
  scale_x_discrete(labels = response_labels) +
  theme(
    axis.text = element_text(colour = "black", size = 18),
    axis.text.y = ggtext::element_markdown(),
    axis.title.x = element_text(size = 20),
    axis.ticks.x = element_blank(),
    legend.position = "none", 
    strip.text = element_text(size = 20),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 20)) +
  scale_y_continuous(breaks = seq(-10, 3, by = 2)) + 
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", linewidth = 1)

plot_1_pwys

# same for r-squared
combined_results_rsq_summary <- joined_result %>%
  janitor::clean_names() %>%
  dplyr::filter(., metric == "rsq") %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_rsq = mean(estimate),
                   se_rsq = plotrix::std.error(estimate),
                   n_observations = n()) %>%
  dplyr::ungroup()

# create another facet variable 
combined_results_rsq_summary <- combined_results_rsq_summary %>%
  mutate(group = case_when(
    grepl("_low_intake_", model) ~ "Low intake",
    grepl("_high_intake_", model) ~ "High intake",
    grepl("_no_", model) ~ "No supplement use",
    TRUE ~ "Supplement use"))

desired_order <- c("Low intake", "High intake", "No supplement use", "Supplement use")

combined_results_rsq_summary$group <- factor(
  combined_results_rsq_summary$group,
  levels = desired_order)

# plot
plot_2_pwy <- ggplot(combined_results_rsq_summary, aes(x = forcats::fct_rev(factor(model)), y = mean_rsq)) +
  geom_col(aes(fill = group), position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_rsq - se_rsq,
                    ymax = mean_rsq + se_rsq),
                width = 0.2) +
  ylab(expression("Average Explained Variance (" * R^2 * ")")) +
  xlab("") +
  theme_bw(base_size = 18) +
  facet_wrap(~group, scales = "free_y", labeller = labeller(group = group_labels)) +
  scale_x_discrete(labels = response_labels) +
  scale_fill_manual(values = c("#969696", "#e24f4a",
                               "#969696", "#e24f4a")) +
  theme(axis.text = element_text(colour = "black", size = 18),
        axis.text.y = ggtext::element_markdown(),
        axis.title.x = element_text(size = 20),
        axis.ticks.x = element_blank(),
        legend.position = "none", 
        strip.text = element_text(size = 20),
        strip.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 20)) +
  scale_y_continuous(breaks = seq(0, 0.20, by = 0.05)) + 
  coord_flip()

plot_2_pwy

# ---- Part 2: Microbiome composition ----

# High intake data sets
wd1 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/high_intake/microbiome/acetate/"
wd2 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/high_intake/microbiome/propionate/"
wd3 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/high_intake/microbiome/butyrate/"

# Low intake data sets 
wd4 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/low_intake/microbiome/acetate/"
wd5 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/low_intake/microbiome/propionate/"
wd6 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/low_intake/microbiome/butyrate/"

# Supplement user data sets 
wd7 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/supp_use/microbiome/acetate/"
wd8 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/supp_use/microbiome/propionate/"
wd9 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/supp_use/microbiome/butyrate/"

# No supplement use data sets 
wd10 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/no_supp_use/microbiome/acetate/"
wd11 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/no_supp_use/microbiome/propionate/"
wd12 <- "/Users/local-margaret/Desktop/VB12-analysis/data/intake_subset_analysis/no_supp_use/microbiome/butyrate/"

# list all ml_results.csv files in sub-directories
file_paths1 <- list.files(path = wd1, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths2 <- list.files(path = wd2, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths3 <- list.files(path = wd3, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths4 <- list.files(path = wd4, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths5 <- list.files(path = wd5, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths6 <- list.files(path = wd6, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths7 <- list.files(path = wd7, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths8 <- list.files(path = wd8, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths9 <- list.files(path = wd9, pattern = "ml_results.csv", 
                          recursive = TRUE, full.names = TRUE)
file_paths10 <- list.files(path = wd10, pattern = "ml_results.csv", 
                           recursive = TRUE, full.names = TRUE)
file_paths11 <- list.files(path = wd11, pattern = "ml_results.csv", 
                           recursive = TRUE, full.names = TRUE)
file_paths12 <- list.files(path = wd11, pattern = "ml_results.csv", 
                           recursive = TRUE, full.names = TRUE)

# combine the files for the respective data set 
combined_results1 <- lapply(file_paths1, read.csv) %>% bind_rows() 
combined_results2 <- lapply(file_paths2, read.csv) %>% bind_rows() 
combined_results3 <- lapply(file_paths3, read.csv) %>% bind_rows() 
combined_results4 <- lapply(file_paths4, read.csv) %>% bind_rows() 
combined_results5 <- lapply(file_paths5, read.csv) %>% bind_rows() 
combined_results6 <- lapply(file_paths6, read.csv) %>% bind_rows() 
combined_results7 <- lapply(file_paths7, read.csv) %>% bind_rows() 
combined_results8 <- lapply(file_paths8, read.csv) %>% bind_rows() 
combined_results9 <- lapply(file_paths9, read.csv) %>% bind_rows() 
combined_results10 <- lapply(file_paths10, read.csv) %>% bind_rows() 
combined_results11 <- lapply(file_paths11, read.csv) %>% bind_rows() 
combined_results12 <- lapply(file_paths12, read.csv) %>% bind_rows() 

# add a column to define the model
combined_results1$model <- "taxaHFE_ML_high_intake_acetate"
combined_results2$model <- "taxaHFE_ML_high_intake_propionate"
combined_results3$model <- "taxaHFE_ML_high_intake_butyrate"

combined_results4$model <- "taxaHFE_ML_low_intake_acetate"
combined_results5$model <- "taxaHFE_ML_low_intake_propionate"
combined_results6$model <- "taxaHFE_ML_low_intake_butyrate"

combined_results7$model <- "taxaHFE_ML_supp_use_acetate"
combined_results8$model <- "taxaHFE_ML_supp_use_propionate"
combined_results9$model <- "taxaHFE_ML_supp_use_butyrate"

combined_results10$model <- "taxaHFE_ML_no_supp_use_acetate"
combined_results11$model <- "taxaHFE_ML_no_supp_use_propionate"
combined_results12$model <- "taxaHFE_ML_no_supp_use_butyrate"

# join the data 
joined_result <- full_join(combined_results1, combined_results2)
joined_result <- full_join(joined_result, combined_results3)
joined_result <- full_join(joined_result, combined_results4)
joined_result <- full_join(joined_result, combined_results5)
joined_result <- full_join(joined_result, combined_results6)
joined_result <- full_join(joined_result, combined_results7)
joined_result <- full_join(joined_result, combined_results8)
joined_result <- full_join(joined_result, combined_results9)
joined_result <- full_join(joined_result, combined_results10)
joined_result <- full_join(joined_result, combined_results11)
joined_result <- full_join(joined_result, combined_results12)

# calculate percent change in MAE compared to null model
combined_results <- joined_result %>% 
  janitor::clean_names() %>%
  dplyr::filter(., metric == "mae") %>%
  dplyr::mutate(., percent_change = ((estimate - null_model_avg) / (null_model_avg)*100)) 

# do a global statistical test 
kruskal.test(percent_change ~ model, data = combined_results)

## do a post-hoc test if the global test p-value < 0.05  
dunn_res <- FSA::dunnTest(percent_change ~ model, data = combined_results, method = "bh")

# Start with Dunn test results
dunn_df <- as.data.frame(dunn_res$res)

# Copy Dunn p-values
pvals <- dunn_df$P.adj

# Fix names: remove spaces around dash
names(pvals) <- gsub(" - ", "-", dunn_df$Comparison)

# Generate letters
letters <- multcompLetters(pvals, threshold = 0.05)

# Restore original model names in letters
letters_df <- data.frame(
  model = names(letters$Letters),
  letter = letters$Letters)

letters_df

# calculate mean percent change for MAE (regression models)
combined_results_mae <- combined_results %>% 
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_percent_change = mean(percent_change),
                   se_percent_change = plotrix::std.error(percent_change),
                   mean_MAE = mean(estimate), 
                   n_observations = n()) %>%
  dplyr::ungroup()

combined_results_mae <- combined_results_mae %>%
  left_join(letters_df, by = "model")

# create another facet variable 
combined_results_mae <- combined_results_mae %>%
  mutate(group = case_when(
    grepl("_low_intake_", model) ~ "Low intake",
    grepl("_high_intake_", model) ~ "High intake",
    grepl("_no_", model) ~ "No supplement use",
    TRUE ~ "Supplement use"))

# clean response labels
response_labels <- c("taxaHFE_ML_high_intake_acetate" = "Acetate",
                     "taxaHFE_ML_low_intake_butyrate" = "Butryate",
                     "taxaHFE_ML_no_supp_use_propionate" = "Propionate",
                     "taxaHFE_ML_high_intake_butyrate" = "Butryate",
                     "taxaHFE_ML_low_intake_propionate" = "Propionate",
                     "taxaHFE_ML_supp_use_acetate" = "Acetate",
                     "taxaHFE_ML_high_intake_propionate" = "Propionate", 
                     "taxaHFE_ML_no_supp_use_acetate" = "Acetate",
                     "taxaHFE_ML_supp_use_butyrate" = "Butryate",
                     "taxaHFE_ML_low_intake_acetate" = "Acetate",
                     "taxaHFE_ML_no_supp_use_butyrate"= "Butryate",
                     "taxaHFE_ML_supp_use_propionate" = "Propionate")

combined_results_mae$model <- factor(
  combined_results_mae$model,
  levels = c("taxaHFE_ML_low_intake_acetate", 
             "taxaHFE_ML_low_intake_propionate",
             "taxaHFE_ML_low_intake_butyrate",
             "taxaHFE_ML_high_intake_acetate",
             "taxaHFE_ML_high_intake_propionate",
             "taxaHFE_ML_high_intake_butyrate",
             "taxaHFE_ML_supp_use_acetate",
             "taxaHFE_ML_supp_use_propionate",
             "taxaHFE_ML_supp_use_butyrate",
             "taxaHFE_ML_no_supp_use_acetate",
             "taxaHFE_ML_no_supp_use_propionate",
             "taxaHFE_ML_no_supp_use_butyrate"))

# explicit order of sub-plots 
desired_order <- c("Low intake", "High intake", "No supplement use", "Supplement use")

combined_results_mae$group <- factor(
  combined_results_mae$group,
  levels = desired_order)

# plot percent change in MAE compared to null model 
plot_1_taxa <- ggplot(combined_results_mae, aes(x = forcats::fct_rev(factor(model)), y = mean_percent_change)) +
  geom_col(aes(fill = group), position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_percent_change - se_percent_change,
                    ymax = mean_percent_change + se_percent_change),
                width = 0.2) +
  geom_text(aes(label = letter, 
                y = 6.75), # adjust vertical position
            size = 5) +  ylab("Mean Percent Change in MAE\nCompared to Null Model") +
  xlab("") +
  theme_bw(base_size = 18) +
  facet_wrap(~group, scales = "free_y", labeller = labeller(group = group_labels)) +
  scale_x_discrete(labels = response_labels) +
  scale_fill_manual(values = c("#969696", "#e24f4a",
                               "#969696", "#e24f4a")) +
  theme(axis.text = element_text(colour = "black", size = 18),
    axis.text.y = ggtext::element_markdown(),
    axis.title.x = element_text(size = 20),
    axis.ticks.x = element_blank(),
    legend.position = "none", 
    strip.text = element_text(size = 20),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 20)) +
  scale_y_continuous(breaks = seq(-10, 3, by = 2)) + 
  scale_y_continuous(breaks = seq(-10, 3, by = 2)) + 
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", linewidth = 1)

plot_1_taxa

# same for r-squared
combined_results_rsq_summary <- joined_result %>%
  janitor::clean_names() %>%
  dplyr::filter(., metric == "rsq") %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_rsq = mean(estimate),
                   se_rsq = plotrix::std.error(estimate),
                   n_observations = n()) %>%
  dplyr::ungroup()

# create another facet variable 
combined_results_rsq_summary <- combined_results_rsq_summary %>%
  mutate(group = case_when(
    grepl("_low_intake_", model) ~ "Low intake",
    grepl("_high_intake_", model) ~ "High intake",
    grepl("_no_", model) ~ "No supplement use",
    TRUE ~ "Supplement use"))

desired_order <- c("Low intake", "High intake", "No supplement use", "Supplement use")

combined_results_rsq_summary$group <- factor(
  combined_results_rsq_summary$group,
  levels = desired_order)

# plot
plot_2_taxa <- ggplot(combined_results_rsq_summary, aes(x = forcats::fct_rev(factor(model)), y = mean_rsq)) +
  geom_col(aes(fill = group), position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_rsq - se_rsq,
                    ymax = mean_rsq + se_rsq),
                width = 0.2) +
  ylab(expression("Average Explained Variance (" * R^2 * ")")) +
  xlab("") +
  theme_bw(base_size = 18) +
  facet_wrap(~group, scales = "free_y", labeller = labeller(group = group_labels)) +
  scale_x_discrete(labels = response_labels) +
  scale_fill_manual(values = c("#969696", "#e24f4a",
                               "#969696", "#e24f4a")) +
  theme(axis.text = element_text(colour = "black", size = 18),
    axis.text.y = ggtext::element_markdown(),
    axis.title.x = element_text(size = 20),
    axis.ticks.x = element_blank(),
    legend.position = "none", 
    strip.text = element_text(size = 20),
    strip.background = element_blank(),
    axis.text.x = element_text(color = "black", size = 20)) +
  scale_y_continuous(breaks = seq(0, 0.20, by = 0.05)) + 
  coord_flip()

plot_2_taxa

# ---- Design multi-panel figure ----

plot_1_pwys # main text figure to be patchwork-ed with other plots 

(plot_1_taxa / plot_2_taxa/ plot_2_pwy)  +
  plot_annotation(tag_levels = 'A')  

#ggsave("figures/intake_subset_SHAP/combined_model_accuracy_patchwork.pdf", width = 18, height = 12)
