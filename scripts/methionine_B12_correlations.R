
# Partial credit to Andrew Oliver for portions of this code, sourced from here: https://github.com/aoliver44/SCFA-Analysis/blob/main/figure_scripts/Figure1.R

# ---- Set up ----
#install.packages(c("dplyr", "readr", "ggplot2", "bestNormalize", "car", "rstatix", "ggpubr", "wesanderson", "gtsummary", "kableExtra", "png", "patchwork", "cowplot", "ggcorrplot"))

# Load libraries 
library(dplyr)
library(readr)
library(ggplot2)
library(bestNormalize)
library(car)
library(rstatix)
library(ggpubr)
library(wesanderson)
library(gtsummary)
library(kableExtra)
library(png)
library(patchwork)
library(cowplot)
library(ggcorrplot)

# Set working directory and source the data
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

data <- metadata_sub # subset that has microbiome data available 

data <- full_join(data, scfa, by = "subject_id") %>%
  na.exclude()

length(data$subject_id)
# [1] 247

met_norm <- bestNormalize::bestNormalize(data$dietary_methionine)
print(met_norm)
data$met_norm <- met_norm$x.t

corr_dat <- data %>%
  dplyr::select(age, sex, bmi, 
                supplement_taker, intake_group,
                dietary_methionine, habitual_dietary_b12, dt_fiber_sol, 
                habitual_b12_norm, met_norm,
                acetate_norm, propionate_norm, new_butyrate_norm,
                acetate, propionate, new_butyrate) %>%
  na.exclude() %>%
  mutate(sex_binary = if_else(sex == "Male",1,0))

# ---- Partial correlations for in-text reference ----

# dietary methionine and vitamin B12 were correlated with each other (partial Spearman’s rho = 0.27, p < 0.001)
library(ppcor)
result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$dietary_methionine,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol")],
  method = "spearman")

print(result_pcor)

# a relationship which strengthened when analyzing only participants in the no B12 supplement use group (partial Spearman’s rho = 0.58, p < 0.001) 
corr_dat_no_supp  <- corr_dat %>%
  dplyr::filter(supplement_taker == "No")

result_pcor <- ppcor::pcor.test(
  x = corr_dat_no_supp$habitual_dietary_b12,
  y = corr_dat_no_supp$dietary_methionine,
  z = corr_dat_no_supp[,c("age", "sex_binary", "bmi", "dt_fiber_sol")],
  method = "spearman")

print(result_pcor)

# and disappeared when looking at the supplement use group (partial Spearman’s rho = -0.05, p = 0.6).
corr_dat_supp <- corr_dat %>%
  dplyr::filter(supplement_taker == "Yes")

result_pcor <- ppcor::pcor.test(
  x = corr_dat_supp$habitual_dietary_b12,
  y = corr_dat_supp$dietary_methionine,
  z = corr_dat_supp[,c("age", "sex_binary", "bmi", "dt_fiber_sol")],
  method = "spearman")

print(result_pcor)

# To our surprise, we found  that dietary methionine was not correlated with fecal acetate (partial Spearman’s rho = 0.08, p = 0.2)
result_pcor <- ppcor::pcor.test(
  x = corr_dat$dietary_methionine,
  y = corr_dat$acetate,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "habitual_dietary_b12")],
  method = "spearman")

print(result_pcor)

# propionate (partial Spearman’s rho = 0.09, p = 0.2), 
result_pcor <- ppcor::pcor.test(
  x = corr_dat$dietary_methionine,
  y = corr_dat$propionate,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "habitual_dietary_b12")],
  method = "spearman")

print(result_pcor)

# or butyrate (partial Spearman’s rho = 0.05, p = 0.4) after controlling for age, sex, BMI, soluble dietary fiber, and dietary B12. 
result_pcor <- ppcor::pcor.test(
  x = corr_dat$dietary_methionine,
  y = corr_dat$new_butyrate,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "habitual_dietary_b12")],
  method = "spearman")

print(result_pcor)

# In contrast, we found that dietary B12 was negatively correlated with fecal acetate (partial Spearman’s rho = -0.19, p < 0.004)
result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$acetate,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "dietary_methionine")],
  method = "spearman")

print(result_pcor)

# and propionate (partial Spearman’s rho = -0.14, p = 0.03) after controlling for age, sex, BMI, soluble dietary fiber and dietary methionine. 
result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$propionate,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "dietary_methionine")],
  method = "spearman")

print(result_pcor)

# We did not find that dietary B12 was correlated with fecal butyrate (partial Spearman’s rho = -0.1, p = 0.1). 
result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$new_butyrate,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "dietary_methionine")],
  method = "spearman")

print(result_pcor)

# ---- High/low intake boxplots ----

stat.test <- corr_dat %>%
  ungroup() %>%
  rstatix::wilcox_test(data = ., dietary_methionine ~ intake_group)%>%
  rstatix::add_significance() 

wilcox.test(dietary_methionine ~ intake_group, data = corr_dat, exact = FALSE)

# Re-order for consistent plotting
corr_dat$intake_group <- factor(corr_dat$intake_group, levels = c("Low", "High"))

plot_A <- ggplot(corr_dat, aes(x=intake_group, y=dietary_methionine, colour = intake_group)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) +
  scale_colour_manual(values = c("#969696", "#e24f4a")) +
  scale_x_discrete(labels = c("Low"  = "Low (< 8.16 \u00B5g/d)",
                              "High" = "High (> 8.16 \u00B5g/d)")) +
  labs(color='Intake group') +
  theme_bw(base_size = 16) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text = element_text(colour = "black")) +
  labs(x = expression(B[12] ~ "intake group"),
       y = "Dietary methionine, grams/day") +
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", y.position = 5, position = position_identity(),
                             tip.length = 0.005) # shortens length of bracket

# ---- Supplement use/no use boxplots ----

stat.test <- corr_dat %>%
  ungroup() %>%
  rstatix::wilcox_test(data = ., dietary_methionine ~ supplement_taker)%>%
  rstatix::add_significance() 

wilcox.test(dietary_methionine ~ supplement_taker, data = corr_dat, exact = FALSE)

plot_B <- ggplot(corr_dat, aes(x=supplement_taker, y=dietary_methionine, colour = supplement_taker)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) +
  #scale_colour_manual(values = c("black", "red")) +
  scale_colour_manual(values = c("#969696", "#e24f4a")) +
  labs(color='Supplement use') +
  theme_bw(base_size = 16) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text = element_text(colour = "black")) +
  labs(x = expression(B[12] ~ "supplement use"),
       y = "Dietary methionine, grams/day") +
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", y.position = 5, position = position_identity(),
                             tip.length = 0.005) # shortens length of bracket

# ---- High/low intake partial regressions ----

# Extract partial regression data for high intake group
data_high <- data %>%
  dplyr::filter(intake_group == "High")
data_high$normalized_resp <- bestNormalize::bestNormalize(data_high$dietary_methionine, allow_orderNorm = F, k = 10, r = 10)$x.t
data_high$normalized_pred <- bestNormalize::bestNormalize(data_high$habitual_dietary_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
model_high <- lm(normalized_resp ~ normalized_pred + as.numeric(bmi) + as.numeric(age) + as.factor(sex) + as.numeric(dt_fiber_sol), data = data_high)
partial_regression_high <- car::avPlots(model_high, plot = FALSE)

# Extract beta coefficients and p-values
summary_high <- summary(model_high)
beta_high <- round(coef(summary_high)["normalized_pred", "Estimate"], 2)
p_high <- coef(summary_high)["normalized_pred", "Pr(>|t|)"]

# Extract partial regression data for low intake group
data_low <- data %>%
  dplyr::filter(intake_group == "Low")
data_low$normalized_resp <- bestNormalize::bestNormalize(data_low$dietary_methionine, allow_orderNorm = F, k = 10, r = 10)$x.t
data_low$normalized_pred <- bestNormalize::bestNormalize(data_low$habitual_dietary_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
model_low <- lm(normalized_resp ~ normalized_pred + as.numeric(bmi) + as.numeric(age) + as.factor(sex) + as.numeric(dt_fiber_sol), data = data_low)
partial_regression_low <- car::avPlots(model_low, plot = FALSE)

# Extract beta coefficients and p-values
summary_low <- summary(model_low)
beta_low <- round(coef(summary_low)["normalized_pred", "Estimate"], 2)
p_low <- coef(summary_low)["normalized_pred", "Pr(>|t|)"]

# Combine the data
plot_data_high <- as.data.frame(partial_regression_high$normalized_pred) %>%
  mutate(intake_group = "High")
plot_data_low <- as.data.frame(partial_regression_low$normalized_pred) %>%
  mutate(intake_group = "Low")

combined_data <- bind_rows(plot_data_high, plot_data_low)

# Format p-values
p_high_text <- ifelse(p_high < 0.001, "p < 0.001", paste0("p = ", round(p_high, 3)))
p_low_text <- ifelse(p_low < 0.001, "p < 0.001", paste0("p = ", round(p_low, 3)))

combined_data$intake_group <- factor(combined_data$intake_group, levels = c("Low", "High"))

# Create combined plot
plot_C <- ggplot(combined_data) + 
  aes(x = normalized_pred, y = normalized_resp, color = intake_group, fill = intake_group) + 
  geom_point(alpha = 0.6, size = 1.5) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) + 
  scale_color_manual(values = c("Low" = "#969696", "High" = "#e24f4a")) +
  scale_fill_manual(values = c("Low" = "#969696", "High" = "#e24f4a")) +
  theme_bw(base_size = 16) + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"),
        legend.position = "top") +
  labs(x = expression(Dietary~B[12]~"(normalized) | covariates"),
       y = "Dietary methionine (normalized) | covariates",
       color = expression(B[12] ~ "intake group"),
       fill = expression(B[12] ~ "intake group")) + 
  coord_cartesian(ylim = c(-3, 3.25)) +
  annotate("text",
           x = 0.25, y = 3,
           label = paste0("beta coef. = ", beta_low, "\n", p_low_text),
           size = 4,
           color = "#969696",
           hjust = 0) +
  annotate("text",
           x = 1, y = 3,
           label = paste0("beta coef. = ", beta_high, "\n", p_high_text),
           size = 4,
           color = "#e24f4a",
           hjust = 0)

# ---- Supplement use/no use partial regressions ----

# Extract partial regression data for supplement use group
data_supps <- data %>%
  dplyr::filter(supplement_taker == "Yes")
data_supps$normalized_resp <- bestNormalize::bestNormalize(data_supps$dietary_methionine, allow_orderNorm = F, k = 10, r = 10)$x.t
data_supps$normalized_pred <- bestNormalize::bestNormalize(data_supps$habitual_dietary_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
model_supps <- lm(normalized_resp ~ normalized_pred + as.numeric(bmi) + as.numeric(age) + as.factor(sex) + as.numeric(dt_fiber_sol), data = data_supps)
partial_regression_supps <- car::avPlots(model_supps, plot = FALSE)

# Extract beta coefficients and p-values
summary_supps <- summary(model_supps)
beta_supps <- round(coef(summary_supps)["normalized_pred", "Estimate"], 2)
p_supps <- coef(summary_supps)["normalized_pred", "Pr(>|t|)"]

# Extract partial regression data for no supplement use group
data_nosupps <- data %>%
  dplyr::filter(supplement_taker == "No")
data_nosupps$normalized_resp <- bestNormalize::bestNormalize(data_nosupps$dietary_methionine, allow_orderNorm = F, k = 10, r = 10)$x.t
data_nosupps$normalized_pred <- bestNormalize::bestNormalize(data_nosupps$habitual_dietary_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
model_nosupps <- lm(normalized_resp ~ normalized_pred + as.numeric(bmi) + as.numeric(age) + as.factor(sex) + as.numeric(dt_fiber_sol), data = data_nosupps)
partial_regression_nosupps <- car::avPlots(model_nosupps, plot = FALSE)

# Extract beta coefficients and p-values
summary_nosupps <- summary(model_nosupps)
beta_nosupps <- round(coef(summary_nosupps)["normalized_pred", "Estimate"], 2)
p_nosupps <- coef(summary_nosupps)["normalized_pred", "Pr(>|t|)"]

# Combine the data
plot_data_supps <- as.data.frame(partial_regression_supps$normalized_pred) %>%
  mutate(supplement_taker = "Yes")
plot_data_nosupps <- as.data.frame(partial_regression_nosupps$normalized_pred) %>%
  mutate(supplement_taker = "No")

combined_data <- bind_rows(plot_data_supps, plot_data_nosupps)

# Format p-values
p_supps_text <- ifelse(p_supps < 0.001, "p < 0.001", paste0("p = ", round(p_supps, 3)))
p_nosupps_text <- ifelse(p_nosupps < 0.001, "p < 0.001", paste0("p = ", round(p_nosupps, 3)))

combined_data$supplement_taker <- factor(combined_data$supplement_taker, levels = c("No", "Yes"))

# Create combined plot
plot_D <- ggplot(combined_data) + 
  aes(x = normalized_pred, y = normalized_resp, color = supplement_taker, fill = supplement_taker) + 
  geom_point(alpha = 0.6, size = 1.5) + 
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) + 
  scale_color_manual(values = c("No" = "#969696", "Yes" = "#e24f4a")) +
  scale_fill_manual(values = c("No" = "#969696", "Yes" = "#e24f4a")) +
  theme_bw(base_size = 16) + 
  theme(panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"),
        legend.position = "top") +
  labs(x = expression(Dietary~B[12]~"(normalized) | covariates"),
       y = "Dietary methionine (normalized) | covariates",
       color = expression(B[12] ~ "supplement use"),
       fill = expression(B[12] ~ "supplement use")) + 
  coord_cartesian(ylim = c(-3, 3.25)) +
  annotate("text",
           x = 0.5, y = 3,
           label = paste0("beta coef. = ", beta_nosupps, "\n", p_nosupps_text),
           size = 4,
           color = "#969696",
           hjust = 0) +
  annotate("text",
           x = 1.5, y = 3,
           label = paste0("beta coef. = ", beta_supps, "\n", p_supps_text),
           size = 4,
           color = "#e24f4a",
           hjust = 0)

# ---- Final patchwork plot ----
(plot_C + plot_A ) /
  (plot_D + plot_B) + 
  plot_annotation(tag_levels = 'A')
  
#ggsave("figures/methionine_relationship_with_B12_patchwork.pdf", height = 10, width =18)
