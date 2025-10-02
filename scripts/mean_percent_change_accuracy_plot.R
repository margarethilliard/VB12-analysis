
# ---- Set up ---- 

#install.packages(c("dplyr", "plotrix", "ggplot2", "FSA", "multcompView", "ggtext", "janitor", "forcats"))

# Load libraries 
library(dplyr)
library(plotrix) # for std err calc
library(ggplot2)
library(FSA) # dunn test
library(multcompView) # compact letter display
library(ggtext)
library(janitor)
library(forcats)

# Set working directory
setwd("/Users/local-margaret/Desktop/VB12-analysis/")

# ---- Read in model performance data ---- 
## Define directories where all output folders are located for each data set 

# TaxaHFE-engineered data sets
wd1 <- "data/supp_users_best_model_reduced_features/"
wd2 <- "data/no_supp_best_model_reduced_features/"
wd3 <- "data/full_data_best_model_reduced_features/"
wd4 <- "data/low_intake_best_model_reduced_features/"
wd5 <- "data/high_intake_best_model_reduced_features/"

# Diet only data sets
wd6 <- "data/full_data/"
wd7 <- "data/no_supps/"
wd8 <- "data/supp_users/"
wd9 <- "data/high_intake/"
wd10 <- "data/low_intake/"

# Base model: no diet or microbiome 
wd11 <- "data/base_model/"

## List ml_results.csv files in sub-directories
file_paths1 <- list.files(path = wd1, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths2 <- list.files(path = wd2, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths3 <- list.files(path = wd3, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths4 <- list.files(path = wd4, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths5 <- list.files(path = wd5, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths6 <- list.files(path = wd6, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths7 <- list.files(path = wd7, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)
file_paths8 <- list.files(path = wd8, pattern = "ml_results.csv",  recursive = TRUE, full.names = TRUE)
file_paths9 <- list.files(path = wd9, pattern = "ml_results.csv",  recursive = TRUE, full.names = TRUE)
file_paths10 <- list.files(path = wd10, pattern = "ml_results.csv",  recursive = TRUE, full.names = TRUE)
file_paths11 <- list.files(path = wd11, pattern = "ml_results.csv", recursive = TRUE, full.names = TRUE)

## Combine  files for the respective data set 
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

## Add a column to define the model
combined_results1$model <- "taxaHFE_ML_supplement_users"
combined_results2$model <- "taxaHFE_ML_no_supplement_use"
combined_results3$model <- "taxaHFE_ML_full_data"

combined_results4$model <- "taxaHFE_ML_low_intake"
combined_results5$model <- "taxaHFE_ML_high_intake"

combined_results6$model <- "dietML_full_data"
combined_results7$model <- "dietML_no_supplement_use"
combined_results8$model <- "dietML_supplement_users"
combined_results9$model <- "dietML_high_intake"
combined_results10$model <- "dietML_low_intake"

combined_results11$model <- "base_model"

## Join data 
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

## Percent change in MAE compared to null model
combined_results <- joined_result %>% 
  janitor::clean_names() %>%
  dplyr::filter(., metric == "mae") %>%
  dplyr::mutate(., percent_change = ((estimate - null_model_avg) / (null_model_avg)*100)) 

## Global statistical test 
kruskal.test(percent_change ~ model, data = combined_results)

## Post-hoc test if the global test p-value < 0.05  
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
  letter = letters$Letters
)

letters_df

# For stars that indicate significance use this:
#dunn_df <- as.data.frame(dunn_res$res)
#sig_comparisons <- dunn_df %>%
#  filter(P.adj < 0.05) %>%
#  mutate(group1 = sapply(strsplit(Comparison, " - "), `[`, 1),
#         group2 = sapply(strsplit(Comparison, " - "), `[`, 2),
#         label = case_when(
#           P.adj < 0.001 ~ "***",
#           P.adj < 0.01 ~ "**",
#           P.adj < 0.05 ~ "*"
#         ))

## Calculate mean percent change for MAE (regression models)
combined_results_mae <- combined_results %>% 
  dplyr::group_by(model) %>%
  dplyr::summarise(mean_percent_change = mean(percent_change),
                   se_percent_change = plotrix::std.error(percent_change),
                   mean_MAE = mean(estimate), 
                   n_observations = n()) %>%
  dplyr::ungroup()

combined_results_mae <- combined_results_mae %>%
  left_join(letters_df, by = "model")

# Create a variable to facet by
combined_results_mae <- combined_results_mae %>%
  mutate(group = case_when(
    grepl("^dietML", model) ~ "Diet Only",
    grepl("^taxaHFE_ML", model) ~ "Diet + Microbiome",
    model == "base_model" ~ "Base Model"))

combined_results_mae$model <- factor(
  combined_results_mae$model,
  levels = c("base_model", 
             "dietML_full_data",
             "dietML_low_intake",
             "dietML_high_intake",
             "dietML_supplement_users",
             "dietML_no_supplement_use",
             "taxaHFE_ML_full_data",
             "taxaHFE_ML_low_intake",
             "taxaHFE_ML_high_intake",
             "taxaHFE_ML_supplement_users",
             "taxaHFE_ML_no_supplement_use"))

## Plot percent change in MAE compared to null model 
ggplot(combined_results_mae, aes(x = forcats::fct_rev(factor(model)), y = mean_percent_change)) +
  geom_bar(aes(fill = model), stat = "identity", width = 0.75) +
  geom_errorbar(aes(ymin = mean_percent_change - se_percent_change,
                    ymax = mean_percent_change + se_percent_change),
                width = 0.2) +
  geom_text(aes(label = letter, 
                y = 3), # adjust vertical position
            size = 5) +
  scale_x_discrete(labels = c("base_model" = "Base Model<br>(no diet or microbiome)",
                              "dietML_full_data" = "Diet only model",
                              "dietML_low_intake" = "Diet only model,<br>low intake",
                              "dietML_high_intake" = "Diet only model,<br>high intake",
                              "dietML_no_supplement_use" = "Diet only model,<br>no supplement use",
                              "dietML_supplement_users" = "Diet only model,<br>supplement use",
                              "taxaHFE_ML_full_data" = "Diet + Microbiome model",
                              "taxaHFE_ML_low_intake" = "Diet + Microbiome model,<br>low intake",
                              "taxaHFE_ML_high_intake" = "Diet + Microbiome model,<br>high intake",
                              "taxaHFE_ML_no_supplement_use" = "Diet + Microbiome model,<br>no supplement use",
                              "taxaHFE_ML_supplement_users" = "Diet + Microbiome model,<br> supplement use")) +
  scale_fill_manual(values = c("base_model" = "#969696",
                               "dietML_full_data" = "#E65100",
                               "dietML_low_intake" = "#EF6C00",
                               "dietML_high_intake" = "#FB8C00",
                               "dietML_no_supplement_use" = "#FFB74D",
                               "dietML_supplement_users" = "#FFF3E0",
                               "taxaHFE_ML_full_data" = "#C2185B",
                               "taxaHFE_ML_low_intake" = "#EC407A",
                               "taxaHFE_ML_high_intake" = "#F06292",
                               "taxaHFE_ML_supplement_users" = "#F8BBD0",
                               "taxaHFE_ML_no_supplement_use" = "#FCE4EC")) +
  ylab("Mean Percent Change in MAE\nCompared to Null Model") +
  xlab("") +
  theme_bw(base_size = 18) +
  theme(
    axis.text = element_text(colour = "black", size = 18),
    axis.text.y = ggtext::element_markdown(),
    axis.title.x = element_text(size = 20),
    axis.ticks.x = element_blank(),
    legend.position = "none") +
  scale_y_continuous(breaks = seq(-10, 3, by = 2)) + 
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", size = 1)

#ggsave("figures/Mean_change_MAE.pdf", height = , width = 10)
