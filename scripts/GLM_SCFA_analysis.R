
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

data <- left_join(metadata_sub, scfa, by = "subject_id") %>%
  na.exclude()

# Define response–predictor pairs
model_pairs <- tibble::tibble(
  response = c("acetate", "acetate","acetate", "acetate",
               "propionate", "propionate", "propionate", "propionate",
               "new_butyrate", "new_butyrate","new_butyrate", "new_butyrate"),
  predictor = c("habitual_dietary_b12", "supplement_taker", "intake_group", "B12_tertile",
                "habitual_dietary_b12", "supplement_taker", "intake_group", "B12_tertile",
                "habitual_dietary_b12", "supplement_taker", "intake_group", "B12_tertile"))

# Define reference groups for clear interpretation 
data$intake_group <- as.factor(data$intake_group)
data$intake_group <- relevel(data$intake_group, ref = "Low") # low = 1, high = 2 

data$supplement_taker <- as.factor(data$supplement_taker)
data$supplement_taker <- relevel(data$supplement_taker, ref = "No") # No = 1, Yes = 2 

# Function to fit GLM adjusted for proper co-variate, extract results, check residuals and dispersion of simulated data 
fit_glm_with_dharma <- function(response, predictor, data) {
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol + dt_prot_animal"))
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
    pseudo_r2 = pseudo_r2)
  
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

View(all_results_main)

# ----- Plotting significant results ----- 
ggplot(all_results_main, aes(x = percent_change, y = reorder(label, percent_change))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = percent_CI_lower, xmax = percent_CI_upper), height = 0.2) +
  labs(x = "Percent change in SCFA concentration\ncompared to reference group", y = "") +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(colour = "black", size = 18),
        axis.title.x = element_text(size = 18)) 

# ggsave("figures/GLM_SCFA~intake.pdf", height = 5, width = 8)

# ---- Estimated marginal means (EMMs) ----
model_pairs <- tibble::tibble(
  response = c("acetate", "acetate",
               "propionate", "propionate", 
               "new_butyrate", "new_butyrate"),
  predictor = c("supplement_taker", "intake_group",
                "supplement_taker", "intake_group",
                "supplement_taker", "intake_group"))

get_emm_df <- function(response, predictor, data) {
  # build models
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol + dt_prot_animal"))
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
emm_all <- pmap(list(model_pairs$response, model_pairs$predictor),
                get_emm_df,
                data = data)

# Extract results separately 
emm_results <- bind_rows(map(emm_all, "emm")) 

# Do multiple test correction for contrasts 
contrast_results <- bind_rows(map(emm_all, "contrasts")) %>%
  mutate(p.adj = p.adjust(p.value, method = "fdr")) 

sig_labels_supps <- contrast_results %>%
  filter(predictor_var == "supplement_taker") %>%
  # Split the contrast into group1 and group2
  mutate(group1 = sapply(str_split(as.character(contrast), " / "), `[`, 1),
         group2 = sapply(str_split(as.character(contrast), " / "), `[`, 2),
         # Re-code significance stars
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
  dplyr::mutate(response_var = dplyr::recode(response_var, 
                                             "acetate" = "Acetate",
                                             "propionate" = "Propionate", 
                                             "new_butyrate" =  "Butyrate"))

sig_labels_intake <- contrast_results %>%
  filter(predictor_var == "intake_group") %>%
  # Split the contrast into group1 and group2
  mutate(group1 = sapply(str_split(as.character(contrast), " / "), `[`, 1),
         group2 = sapply(str_split(as.character(contrast), " / "), `[`, 2),
         # Re-code significance stars
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
  dplyr::mutate(response_var = dplyr::recode(response_var, "acetate" = "Acetate",
                                             "propionate" = "Propionate", 
                                             "new_butyrate" =  "Butyrate"))

# ----- Plotting EMMs -----
plot_1 <- emm_results %>%
  dplyr::filter(predictor_var == "supplement_taker") %>%
  dplyr::mutate(response_var = dplyr::recode(response_var, "acetate" = "Acetate",
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

#ggsave("figures/EMM_SCFA_supp_use.pdf", width = 8, height = 3)

plot_2 <- emm_results %>%
  dplyr::filter(predictor_var == "intake_group") %>%
  dplyr::mutate(response_var = dplyr::recode(response_var, "acetate" = "Acetate",
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

#ggsave("figures/EMM_SCFA_intake_group.pdf", width = 8, height = 3)

# ---- Design multi-panel plot ----
plot_2 / plot_1+
  plot_annotation(tag_levels = 'A')

#ggsave("figures/EMM_SCFA_patchwork_animal_protein_covariate.pdf", width = 8, height = 6)

# ---- Dose response using estimated marginal slopes ----
metadata_sub$sex <- as.factor(metadata_sub$sex)

data <- left_join(metadata_sub, scfa, by = "subject_id") %>%
  na.exclude()

model_pairs <- tibble::tibble(
  response = c("acetate",
               "propionate",
               "new_butyrate"),
  predictor = c("habitual_b12_norm", 
                "habitual_b12_norm", 
                "habitual_b12_norm"))

data$habitual_b12_norm <- as.numeric(data$habitual_b12_norm)

# Function to get emtrends results for a continuous predictor
get_trend_df <- function(response, predictor, data) {
  
  # Fit GLM with Gamma log-link
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol + dt_prot_animal"))
  model <- glm(formula, family = Gamma(link = "log"), data = data)
  
  # ---- 1. Marginal slope estimation ----
  # emtrends gives slope on the LINK scale (log-scale)
  trends <- emtrends(model, ~ 1, var = predictor)
  
  # Convert to data frame
  trends_df <- as.data.frame(trends) %>%
    rename(slope = habitual_b12_norm.trend,
           slope_SE = SE,
           slope_df = df,
           slope_lower.CL = lower.CL,
           slope_upper.CL = upper.CL) %>%
    mutate(response_var = response,
           predictor_var = predictor,
           # exponentiate for interpretation as multiplicative effect per ΔX:
           ratio_per_unit = exp(slope),
           ratio_lower.CL = exp(slope_lower.CL),
           ratio_upper.CL = exp(slope_upper.CL)) %>%
    select(response_var, predictor_var,
           slope, slope_SE, slope_df, slope_lower.CL, slope_upper.CL,
           ratio_per_unit, ratio_lower.CL, ratio_upper.CL)
  
  return(trends_df)
}

# ---- 2. Loop over all responses ----
trend_results <- pmap_dfr(list(model_pairs$response, model_pairs$predictor),
                          get_trend_df,
                          data = data)

# ---- 3. FDR correction for slopes ----
trend_results <- trend_results %>%
  mutate(p.value = 2 * pnorm(abs(slope / slope_SE), lower.tail = FALSE),
         p.adj = p.adjust(p.value, method = "fdr"))

trend_results

trend_results %>%
  select(response_var, predictor_var,
         slope, slope_SE, slope_lower.CL, slope_upper.CL, p.adj) %>%
  tibble()

# ---- 4. Plot model-based predicted curves ----
data$acetate <- as.numeric(data$acetate)

model_acetate <- glm(acetate ~ habitual_b12_norm + age + sex + bmi + dt_fiber_sol + dt_prot_animal,
                     family = Gamma(link = "log"),
                     data = data)

emmip(model_acetate,
      ~ habitual_b12_norm,
      at = list(habitual_b12_norm = seq(min(data$habitual_b12_norm, na.rm = TRUE),
                                        max(data$habitual_b12_norm, na.rm = TRUE),
                                        length.out = 100)),
      type = "response")

model_propionate <- glm(propionate ~ habitual_b12_norm + age + sex + bmi + dt_fiber_sol + dt_prot_animal,
                        family = Gamma(link = "log"),
                        data = data)

emmip(model_propionate,
      ~ habitual_b12_norm,
      at = list(habitual_b12_norm = seq(min(data$habitual_b12_norm, na.rm = TRUE),
                                        max(data$habitual_b12_norm, na.rm = TRUE),
                                        length.out = 100)),
      type = "response")

model_butyrate <- glm(new_butyrate ~ habitual_b12_norm + age + sex + bmi + dt_fiber_sol + dt_prot_animal,
                      family = Gamma(link = "log"),
                      data = data)

emmip(model_butyrate,
      ~ habitual_b12_norm,
      at = list(habitual_b12_norm = seq(min(data$habitual_b12_norm, na.rm = TRUE),
                                        max(data$habitual_b12_norm, na.rm = TRUE),
                                        length.out = 100)),
      type = "response")

# ---- Plot curves with raw data superimposed and add confidence intervals ----- 
get_emm_curve <- function(model, predictor, data, n_points = 200) {
  
  # Define grid for predictor
  grid_vals <- seq(min(data[[predictor]], na.rm = TRUE),
                   max(data[[predictor]], na.rm = TRUE),
                   length.out = n_points)
  
  # Build 'at=' list dynamically
  at_list <- setNames(list(grid_vals), predictor)
  
  # Get predictions across the grid
  emm_grid <- emmeans(model,
                      specs = predictor,
                      at = at_list,
                      type = "response")
  
  as.data.frame(emm_grid) %>%
    rename(pred = response) %>%
    mutate(predictor_level = .data[[predictor]])
}

# --- Plot dose response curves ----

plot_emm_with_data <- function(model, predictor, response, data) {
  
  # EMM curve points
  curve_df <- get_emm_curve(model, predictor, data)
  
  # Extract relevant raw data
  raw_df <- data %>%
    select(predictor_level = all_of(predictor),
           raw_response    = all_of(response))
  
  # Plot
  ggplot() +
    # raw points
    geom_point(data = raw_df,
               aes(x = predictor_level, y = raw_response),
               alpha = 0.4) +
    # confidence band
    geom_ribbon(data = curve_df,
                aes(x = predictor_level, ymin = lower.CL, ymax = upper.CL),
                alpha = 0.25) +
    # fitted curve
    geom_line(data = curve_df,
              aes(x = predictor_level, y = pred),
              linewidth = 1.2) +
    labs(x = predictor,
         y = response,
         theme_classic(base_size = 14))
}

# Acetate
acetate_model <- glm(as.formula("acetate ~ habitual_b12_norm + age + sex + bmi + dt_fiber_sol + dt_prot_animal"),
                     family = Gamma(link = "log"),
                     data = data)

acetate_plot <- plot_emm_with_data(acetate_model,
                                   predictor = "habitual_b12_norm",
                                   response  = "acetate",
                                   data = data)

acetate_plot <- acetate_plot + 
  labs(x = "") +
  labs(y = "Predicted fecal acetate concentration\n(adjusted for covariates)") + 
  annotate("text", 
           x = 0.8, 
           y = 60, 
           hjust = 0, vjust = 1, 
           label = "Marginal slope\np = 0.01",
           size = 4,
           color = "red")

acetate_plot

# Proionate
propionate_model <- glm(as.formula("propionate ~ habitual_b12_norm + age + sex + bmi + dt_fiber_sol + dt_prot_animal"),
                        family = Gamma(link = "log"),
                        data = data)

propionate_plot <- plot_emm_with_data(propionate_model,
                                      predictor = "habitual_b12_norm",
                                      response  = "propionate",
                                      data = data)

propionate_plot <- propionate_plot + 
  labs(x = "") +
  labs(y = "Predicted fecal propionate concentration\n(adjusted for covariates)") +
  annotate("text", 
           x = 0.8, 
           y = 20, 
           hjust = 0, vjust = 1, 
           label = "Marginal slope\np = 0.02",
           size = 4,
           color = "red")

propionate_plot

# Butyrate
butyrate_model <- glm(
  as.formula("new_butyrate ~ habitual_b12_norm + age + sex + bmi + dt_fiber_sol + dt_prot_animal"),
  family = Gamma(link = "log"),
  data = data)

butyrate_plot <- plot_emm_with_data(propionate_model,
                                    predictor = "habitual_b12_norm",
                                    response  = "new_butyrate",
                                    data = data)

butyrate_plot <- butyrate_plot + 
  labs(x = "") +
  labs(y = "Predicted fecal butyrate concentration\n(adjusted for covariates)") +
  annotate("text", 
           x = 0.8, 
           y = 30, 
           hjust = 0, vjust = 1, 
           label = "Marginal slope\np = 0.17",
           size = 4,
           color = "red")

butyrate_plot

xaxis_label <- ggplot() + 
  theme_void() + 
  labs(x = expression(Habitual~dietary~B[12]~"(normalized) | covariates")) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.title.x = element_text(hjust = 0.5, vjust = 0),  # vjust moves the label closer
        panel.background = element_blank()) +
  theme_classic(base_size = 14)

acetate_plot <- acetate_plot + labs(tag = "A")
butyrate_plot <- butyrate_plot + labs(tag = "B")
propionate_plot <- propionate_plot + labs(tag = "C")
xaxis_label <- xaxis_label + labs(tag = NULL)

# ---- Design multi-panel figure ----
(acetate_plot + butyrate_plot + propionate_plot) / xaxis_label +
  plot_layout(heights = c(1, 0.001))  # smaller relative height for x-axis label
#plot_annotation(tag_levels = 'A')

#ggsave("figures/SCFA~habitualB12.pdf", width = 12, height = 5)