
# ----- Setup ----- 

#install.packages(c("dplyr", "purrr", "tidyr", "tibble", "DHARMa", "emmeans", "ggpubr", "stringr", "ggplot2", "ggsignif", "patchwork", "performance"))

# Load libraries
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(DHARMa)
library(emmeans)
library(ggpubr)
library(stringr)
library(ggplot2)
library(ggsignif)
library(patchwork)
library(performance) 

# Set working directory and source the data 
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# ----- SCFA GLMs ----- 

metadata_sub$sex <- as.factor(metadata_sub$sex)

data <- left_join(metadata_sub, scfa, by = "subject_id")

# Define response–predictor pairs of interest
model_pairs <- tibble::tibble(
  response = c("acetate", "acetate",
               "propionate", "propionate", 
               "new_butyrate", "new_butyrate"),
  predictor = c("supplement_taker", "intake_group", 
                "supplement_taker", "intake_group", 
                "supplement_taker", "intake_group"))

# Define reference groups for clear interpretation 
data$intake_group <- as.factor(data$intake_group)
data$intake_group <- relevel(data$intake_group, ref = "Low") # low = 1, high = 2 

data$supplement_taker <- as.factor(data$supplement_taker)
data$supplement_taker <- relevel(data$supplement_taker, ref = "No") # No = 1, Yes = 2 

# Function to fit GLM adjusted for co-variates, extract results, check residuals and dispersion of simulated data 
fit_glm_with_dharma <- function(response, predictor, data) {
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol"))
  model <- glm(formula, family = Gamma(link = "log"), data = data)
  
  # DHARMa simulation
  sim_res <- simulateResiduals(model)
  test_res <- testUniformity(sim_res) # tests if residuals are uniform
  test_disp <- testDispersion(sim_res) # tests for over- and under-dispersion
  uniform_flag <- test_res$p.value > 0.05  # TRUE if residuals look okay
  dispersion_flag <- test_disp$p.value > 0.05  # TRUE if dispersion looks okay
  pseudo_r2 <- with(summary(model), 1 - deviance/null.deviance) 
  
  # Extract coefficient table -- note this is in log scale at this point  
  coef_table <- summary(model)$coefficients
  
  # Remove intercept for reporting predictors only
  coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
  if(nrow(coef_table) == 0) return(NULL)
  
  results <- tibble(
    response = response,
    predictor = predictor,
    term = rownames(coef_table),
    estimate = coef_table[, "Estimate"],
    std_error = coef_table[, "Std. Error"],
    z_value = coef_table[, ifelse("z value" %in% colnames(coef_table), "z value", "t value")],
    p_value = coef_table[, ifelse("Pr(>|z|)" %in% colnames(coef_table), "Pr(>|z|)", "Pr(>|t|)")],
    # exponentiation of log estimate to get multiplicative effect 
    multiplicative_effect = exp(coef_table[, "Estimate"]),
    percent_change = (exp(coef_table[, "Estimate"]) - 1) * 100,
    residuals_ok = uniform_flag,
    dispersion_ok = dispersion_flag,
    pseudo_r2 = pseudo_r2
  )
  
  return(results)
}

# Run models for all pairs and combine results
all_results <- pmap_dfr(list(model_pairs$response, model_pairs$predictor), fit_glm_with_dharma, data = data)

# Adjust p-values for multiple comparisons 
all_results <- all_results %>%
  mutate(p_value_adj = p.adjust(p_value, method = "fdr"), .after = p_value)

# View significant results
all_results <- all_results %>%
  filter(p_value_adj < 0.05) %>%
  mutate(
    # log-scale coefficient from GLM to a multiplicative effect confidence interval
    multiplicative_CI_lower = exp(estimate - 1.96 * std_error),
    multiplicative_CI_upper = exp(estimate + 1.96 * std_error),
    # percent change CI
    percent_CI_lower = (multiplicative_CI_lower - 1) * 100,
    percent_CI_upper = (multiplicative_CI_upper - 1) * 100,
    label = paste(response, predictor, sep = " ~ "))

print(all_results)

# Ignore significant associations with co-variates for now 
all_results_main <- all_results %>% filter(term == "intake_groupHigh" | term == "supplement_takerYes")

print(all_results_main)

# ----- Plotting significant results ----- 

ggplot(all_results_main, aes(x = percent_change, y = reorder(label, percent_change))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = percent_CI_lower, xmax = percent_CI_upper), height = 0.2) +
  labs(x = "Percent change in SCFA concentration\ncompared to reference group", y = "") +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(colour = "black", size = 18),
        axis.title.x = element_text(size = 18)) 

# ---- Estimated marginal means (absolute adjusted means) ----

# Define response–predictor pairs of interest

model_pairs <- tibble::tibble(
  response = c("acetate", "acetate",
               "propionate", "propionate", 
               "new_butyrate", "new_butyrate"),
  predictor = c("supplement_taker", "intake_group",
                "supplement_taker", "intake_group",
                "supplement_taker", "intake_group"))

get_emm_df <- function(response, predictor, data) {
  # build models
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol"))
  model <- glm(formula, family = Gamma(link = "log"), data = data)
  
  # get EMMs and back-transform the response variable 
  emm <- emmeans(model, specs = predictor, type = "response")
  
  # EMM results to tidy format
  emm_df <- as.data.frame(emm) %>%
    rename(predicted = response) %>%
    mutate(response_var = response,
           predictor_var = predictor,
           predictor_level = .data[[predictor]]) %>%
    select(response_var, predictor_var, predictor_level,
           predicted, SE, df, lower.CL, upper.CL)
  
  # Pairwise contrasts
  contrast_obj <- contrast(emm, "pairwise")
  contrast_df <- as.data.frame(contrast_obj)
  
  # Add confidence intervals
  ci_df <- as.data.frame(confint(contrast_obj))
  
  # Merge estimates and CIs
  contrast_df <- left_join(contrast_df, ci_df %>% select(contrast, lower.CL, upper.CL),
                           by = "contrast") %>%
    mutate(response_var = response,
           predictor_var = predictor) %>%
    select(response_var, predictor_var, contrast, ratio, SE, df, lower.CL, upper.CL, p.value)
  
  # Return a list with both EMMs and contrasts
  return(list(emm = emm_df, contrasts = contrast_df))
  }

# Loop through all models
emm_all <- pmap(
  list(model_pairs$response, model_pairs$predictor),
  get_emm_df,
  data = data)

# Extract results separately 
emm_results <- bind_rows(map(emm_all, "emm")) 

# Do multiple test correction for contrasts 
contrast_results <- bind_rows(map(emm_all, "contrasts")) %>%
  mutate(p.adj = p.adjust(p.value, method = "fdr")) 
  
sig_labels_supps <- contrast_results %>%
  filter(predictor_var == "supplement_taker") %>%
  mutate(
    # Split the contrast into group1 and group2
    group1 = sapply(stringr::str_split(as.character(contrast), " / "), `[`, 1),
    group2 = sapply(stringr::str_split(as.character(contrast), " / "), `[`, 2),
    # Recode significance stars
    label = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*",
      TRUE ~ "ns")) %>%
  group_by(response_var) %>%
  # Set y.position slightly above the max predicted value for each facet
  mutate(y.position = max(emm_results$predicted[emm_results$response_var == unique(response_var)]) * 1.15) %>%
  ungroup() %>%
  select(response_var, group1, group2, y.position, p.adj, label) %>%
  dplyr::mutate(response_var = recode(response_var, "acetate" = "Acetate",
                                      "propionate" = "Propionate", 
                                      "new_butyrate" =  "Butyrate"))

sig_labels_intake <- contrast_results %>%
  filter(predictor_var == "intake_group") %>%
  # Split the contrast into group1 and group2
  mutate(group1 = sapply(str_split(as.character(contrast), " / "), `[`, 1),
         group2 = sapply(str_split(as.character(contrast), " / "), `[`, 2),
         # Recode significance stars
         label = case_when(p.adj < 0.001 ~ "***",
                           p.adj < 0.01  ~ "**",
                           p.adj < 0.05  ~ "*",
                           TRUE ~ "ns")) %>%
  group_by(response_var) %>%
  # Set y.position slightly above the max predicted value for each facet
  mutate(y.position = max(emm_results$predicted[emm_results$response_var == unique(response_var)]) * 1.15) %>%
  ungroup() %>%
  select(response_var, group1, group2, y.position, p.adj, label) %>%
  dplyr::mutate(response_var = recode(response_var, "acetate" = "Acetate",
                                      "propionate" = "Propionate", 
                                      "new_butyrate" =  "Butyrate"))

# ----- Figure 2A-B: Plotting EMMs -----

plot_1 <- emm_results %>%
  dplyr::filter(predictor_var == "supplement_taker") %>%
  dplyr::mutate(response_var = recode(response_var, "acetate" = "Acetate",
                                      "propionate" = "Propionate", 
                                      "new_butyrate" =  "Butyrate")) %>%
  ggplot(aes(x = predictor_level, 
             y = predicted, 
             fill = predictor_level)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.15, linewidth = 0.6) +
  facet_wrap(~response_var, scales = "free_y") +
  scale_fill_manual(values = c("#969696", "#e24f4a")) +
  labs(x = expression(B[12] ~ "supplement use"),
       y = "Predicted mean SCFA concentration\n(adjusted for covariates)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12), # "strip" = facet wrap label 
        strip.background = element_blank(),
        axis.text.x = element_text(color = "black")) +
  ggpubr::stat_pvalue_manual(sig_labels_supps,
                             label = "label",
                             xmin = "group1",
                             xmax = "group2",
                             y.position = "y.position")

plot_1


plot_2 <- emm_results %>%
  dplyr::filter(predictor_var == "intake_group") %>%
  dplyr::mutate(response_var = recode(response_var, "acetate" = "Acetate",
                                      "propionate" = "Propionate", 
                                      "new_butyrate" =  "Butyrate")) %>%
  ggplot(aes(x = predictor_level, 
             y = predicted,
             fill = predictor_level)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.15, linewidth = 0.6) +
  facet_wrap(~response_var, scales = "free_y") +
  scale_fill_manual(values = c("#969696", "#e24f4a")) +
  labs(x = expression(B[12] ~ "intake group relative to median"),
    y = "Predicted mean SCFA concentration\n(adjusted for covariates)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        axis.text.x = element_text(color = "black")) +
  ggpubr::stat_pvalue_manual(sig_labels_intake,
                             label = "label",
                             xmin = "group1",
                             xmax = "group2",
                             y.position = "y.position")

plot_2

# ---- Patchwork plots together ----

plot_2 / plot_1+
  plot_annotation(tag_levels = 'A')

#ggsave("figures/EMM_SCFA_patchwork.pdf", width = 8, height = 6)
