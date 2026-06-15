
# ----- Setup ----- 

#install.packages(c("dplyr", "purrr", "tidyr", "tibble", "DHARMa", "emmeans", "ggpubr", "stringr", "ggplot2", "ggsignif", "patchwork", "performance"))

# Load libraries
library(dplyr)
library(ggplot2)
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

metadata_sub$sex <- as.factor(metadata_sub$sex)

data <- left_join(metadata_sub, plasma_scfa, by = "subject_id") %>%
  na.exclude()

# ----- Mann-Whitney U tests ----- 

# Look at the distributions of the three variables of interest 
hist(data$p_acetic_acid_nmol)
hist(data$p_butyric_acid_nmol)
hist(data$p_propionic_acid_nmol)

hist(log(data$p_acetic_acid_nmol)) # approximately normal 
hist(log(data$p_butyric_acid_nmol))
hist(log(data$p_propionic_acid_nmol)) # approximately normal-ish

# Reviewer suggests doing a more rudimentary test of associations before GLM
wilcox.test(p_acetic_acid_nmol ~ intake_group, data=data) 
wilcox.test(p_propionic_acid_nmol ~ intake_group, data=data)
wilcox.test(p_butyric_acid_nmol ~ intake_group, data=data)

wilcox.test(p_acetic_acid_nmol ~ supplement_taker, data=data)
wilcox.test(p_propionic_acid_nmol ~ supplement_taker, data=data)
wilcox.test(p_butyric_acid_nmol ~ supplement_taker, data=data) # no differences ! 

# ---- Partial spearman correlations for peer review and supplementary tables ---- 

library(ppcor)
library(bestNormalize)

data <- left_join(metadata_sub, plasma_scfa, by = "subject_id") %>%
  na.exclude()

data$p_propionic_acid_nmol

corr_dat <- data %>%
  dplyr::select(age, sex, bmi, 
                supplement_taker, intake_group,
                dietary_methionine, habitual_dietary_b12, dt_fiber_sol, 
                habitual_b12_norm, 
                p_acetic_acid_nmol, p_propionic_acid_nmol, p_butyric_acid_nmol) %>%
  na.exclude() %>%
  mutate(sex_binary = if_else(sex == "Male",1,0))

result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$p_acetic_acid_nmol,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "dietary_methionine")],
  method = "spearman")

print(result_pcor)

result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$p_propionic_acid_nmol,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "dietary_methionine")],
  method = "spearman")

print(result_pcor)

result_pcor <- ppcor::pcor.test(
  x = corr_dat$habitual_dietary_b12,
  y = corr_dat$p_butyric_acid_nmol,
  z = corr_dat[,c("age", "sex_binary", "bmi", "dt_fiber_sol", "dietary_methionine")],
  method = "spearman")

print(result_pcor)

# ----- Model diagnostics on GLM with Gamma distribution, log link -----
# Define response–predictor pairs
model_pairs <- tibble::tibble(
  response = c("p_acetic_acid_nmol", "p_acetic_acid_nmol","p_acetic_acid_nmol", 
               "p_propionic_acid_nmol", "p_propionic_acid_nmol", "p_propionic_acid_nmol", 
               "p_butyric_acid_nmol", "p_butyric_acid_nmol","p_butyric_acid_nmol"),
  predictor = c("habitual_dietary_b12", "supplement_taker", "intake_group", 
                "habitual_dietary_b12", "supplement_taker", "intake_group", 
                "habitual_dietary_b12", "supplement_taker", "intake_group"))

# Define reference groups for clear interpretation 
data$intake_group <- as.factor(data$intake_group)
data$intake_group <- relevel(data$intake_group, ref = "Low") # low = 1, high = 2 

data$supplement_taker <- as.factor(data$supplement_taker)
data$supplement_taker <- relevel(data$supplement_taker, ref = "No") # No = 1, Yes = 2 

# Function to fit GLM adjusted for proper co-variates, extract results, check residuals and dispersion of simulated data 
fit_glm_with_dharma <- function(response, predictor, data) {
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol + dietary_methionine"))
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

# Ignore significant associations with co-variates for now 
all_results_main <- all_results %>% 
  filter(term == "intake_groupHigh" | term == "supplement_takerYes" | term == "habitual_dietary_b12")

print(all_results_main)

# Models are not passing diagnostics...

# ---- Check diagnostics on linear models using log response -----

# Add log-transformed columns
data <- data %>%
  mutate(
    log_acetic = log(p_acetic_acid_nmol),
    log_propionate = log(p_propionic_acid_nmol),
    log_butyrate = log(p_butyric_acid_nmol))

fit_lm_with_checks <- function(response, predictor, data) {
  formula <- as.formula(paste(response, "~", predictor,
                              "+ age + sex + bmi + dt_fiber_sol + dietary_methionine"))
  model <- lm(formula, data = data)
  
  # DHARMa checks 
  sim_res <- simulateResiduals(model)
  test_res <- testUniformity(sim_res)
  test_disp <- testDispersion(sim_res)
  
  # Also check with performance for normality of residuals
  check <- performance::check_normality(model)
  
  coef_table <- summary(model)$coefficients
  coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
  
  tibble(
    response = response,
    predictor = predictor,
    term = rownames(coef_table),
    estimate = coef_table[, "Estimate"],
    std_error = coef_table[, "Std. Error"],
    t_value = coef_table[, "t value"],
    p_value = coef_table[, "Pr(>|t|)"],
    r_squared = summary(model)$r.squared,
    residuals_ok = test_res$p.value > 0.05,
    dispersion_ok = test_disp$p.value > 0.05)
}

# Run models for all pairs and combine results
all_results_lm <- pmap_dfr(list(model_pairs$response, model_pairs$predictor), fit_lm_with_checks, data = data)

all_results_lm_main <- all_results_lm %>% 
  filter(term == "intake_groupHigh" | term == "supplement_takerYes" | term == "habitual_dietary_b12")

print(all_results_lm_main)

# Residual normality is not passing.... 

# ---- Try using best normalize funciton to normalize the response vars ----

# Apply bestNormalize to each SCFA and inspect what transformation is chosen
bn_acetic <- bestNormalize(data$p_acetic_acid_nmol, allow_orderNorm = F)
bn_propionate <- bestNormalize(data$p_propionic_acid_nmol, allow_orderNorm = F)
bn_butyrate  <- bestNormalize(data$p_butyric_acid_nmol, allow_orderNorm = F)

# Check which transformation was selected for each
bn_acetic$chosen_transform
bn_propionate$chosen_transform
bn_butyrate$chosen_transform

# Inspect the normalization visually
par(mfrow = c(1, 3))
hist(bn_acetic$x.t,    main = "Acetate normalized",    xlab = "")
hist(bn_propionate$x.t, main = "Propionate normalized", xlab = "")
hist(bn_butyrate$x.t,  main = "Butyrate normalized",   xlab = "")

# Also check QQ plots
par(mfrow = c(1, 3))
qqnorm(bn_acetic$x.t,     main = "Acetate QQ")
qqnorm(bn_propionate$x.t, main = "Propionate QQ")
qqnorm(bn_butyrate$x.t,   main = "Butyrate QQ")

data <- data %>%
  mutate(acetic_norm = bn_acetic$x.t,
         propionate_norm = bn_propionate$x.t,
         butyrate_norm = bn_butyrate$x.t)

# Refit LMs on normalized responses
model_pairs_norm <- tibble::tibble(
  response  = c("acetic_norm", "acetic_norm",
                "propionate_norm", "propionate_norm",
                "butyrate_norm", "butyrate_norm"),
  predictor = c("intake_group", "supplement_taker",
                "intake_group", "supplement_taker",
                "intake_group", "supplement_taker"))

fit_lm_normalized <- function(response, predictor, data) {
  formula <- as.formula(paste(response, "~", predictor, "+ age + sex + bmi + dt_fiber_sol + dietary_methionine"))
  model <- lm(formula, data = data)
  
  sim_res   <- simulateResiduals(model)
  test_res  <- testUniformity(sim_res)
  test_disp <- testDispersion(sim_res)
  
  coef_table <- summary(model)$coefficients
  coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
  
  tibble(
    response      = response,
    predictor     = predictor,
    term          = rownames(coef_table),
    estimate      = coef_table[, "Estimate"],
    std_error     = coef_table[, "Std. Error"],
    t_value       = coef_table[, "t value"],
    p_value       = coef_table[, "Pr(>|t|)"],
    r_squared     = summary(model)$r.squared,
    residuals_ok  = test_res$p.value > 0.05,
    dispersion_ok = test_disp$p.value > 0.05)
}

norm_results <- pmap_dfr(
  list(model_pairs_norm$response, model_pairs_norm$predictor),
  fit_lm_normalized,
  data = data)

# Check which models pass DHARMa
norm_results %>% 
  select(response, predictor, term, residuals_ok, dispersion_ok) %>%
  filter(term == "intake_groupHigh" | term == "supplement_takerYes")

# If I allow best normalize to use the orderNorm transformation then all of the checks pass, 
# but this decision comes at the expense of being able to back transform the data 

# Without allowing the orderNorm transformation, then the checks almost all pass 
# with the exception of butyrate ~ supplement use (residual normality not passing)

# Check how badly the butyrate ~ supplement use model fails 

# Fit the butyrate ~ supplement model and look closely at the DHARMa plot
model_butyrate_supp <- lm(butyrate_norm ~ supplement_taker + age + sex + 
                            bmi + dt_fiber_sol + dietary_methionine, 
                          data = data)

sim_res <- simulateResiduals(model_butyrate_supp)
plot(sim_res)
testOutliers(sim_res)

# ----- Is the left censoring actually an LOD?? ----

min(data$p_acetic_acid_nmol, na.rm = TRUE)
# 0.02036302
min(data$p_propionic_acid_nmol, na.rm = TRUE)
# 0.0001349892
min(data$p_butyric_acid_nmol, na.rm = TRUE)
# 0.0001134945

data %>% 
  arrange(p_butyric_acid_nmol) %>%
  select(p_butyric_acid_nmol) %>%
  head(20)

data %>%
  arrange(p_acetic_acid_nmol) %>%
  select(p_acetic_acid_nmol) %>%
  head(20)

# Butyrate and propionate have a strong censoring signal
# but acetate does not 

# Acetate can probably use an lm/normalized response lm, while butyrate and propionate should use tobit models 

lod_propionate <- min(data$p_propionic_acid_nmol, na.rm = TRUE)
lod_butyrate <- min(data$p_butyric_acid_nmol, na.rm = TRUE)

# ------ Tobit models + getting the covariate-adjusted means using emmeans ------

# Andrew, if you're reading this I 100% agree with your approach to modeling plasma SCFA in your 2024 paper!

library(VGAM)

# Fit Tobit models with VGAM
tobit_prop_intake <- vglm(p_propionic_acid_nmol ~ intake_group + age + sex + 
                            bmi + dt_fiber_sol + dietary_methionine,
                          family = tobit(Lower = lod_propionate),
                          data = data)

tobit_prop_supp <- vglm(p_propionic_acid_nmol ~ supplement_taker + age + sex + 
                          bmi + dt_fiber_sol + dietary_methionine,
                        family = tobit(Lower = lod_propionate),
                        data = data)

tobit_but_intake <- vglm(p_butyric_acid_nmol ~ intake_group + age + sex + 
                           bmi + dt_fiber_sol + dietary_methionine,
                         family = tobit(Lower = lod_butyrate),
                         data = data)

tobit_but_supp <- vglm(p_butyric_acid_nmol ~ supplement_taker + age + sex + 
                         bmi + dt_fiber_sol + dietary_methionine,
                       family = tobit(Lower = lod_butyrate),
                       data = data)

summary(tobit_prop_intake)
summary(tobit_prop_supp)
summary(tobit_but_intake)
summary(tobit_but_supp)
  
# ---- Getting p-values -----

get_tobit_pvals <- function(model, predictor, response) {
  coef_table <- summary(model)@coef3
  
  coef_table <- as.data.frame(coef_table) %>%
    tibble::rownames_to_column("term") %>%
    filter(grepl(predictor, term)) %>%
    mutate(response = response,
           predictor_var = predictor)
  
  return(coef_table)
}

pval_results <- bind_rows(
  get_tobit_pvals(tobit_prop_intake, "intake_group", "propionate"),
  get_tobit_pvals(tobit_prop_supp, "supplement_taker", "propionate"),
  get_tobit_pvals(tobit_but_intake, "intake_group", "butyrate"),
  get_tobit_pvals(tobit_but_supp, "supplement_taker", "butyrate"))

print(pval_results)

# ----- Linear regression to model acetate ---- 

lm_acetate_intake <- lm(acetic_norm ~ intake_group + age + sex + bmi + dt_fiber_sol + dietary_methionine,
                        data = data)

lm_acetate_supp <- lm(acetic_norm ~ supplement_taker + age + sex + bmi + dt_fiber_sol + dietary_methionine,
                      data = data)

summary(lm_acetate_intake)
summary(lm_acetate_supp)

# Check DHARMa still passes
simulateResiduals(lm_acetate_intake) %>% testUniformity()
simulateResiduals(lm_acetate_supp)   %>% testUniformity()

# Extract the p-values 
get_lm_pvals <- function(model, predictor, response) {
  coef_table <- summary(model)$coefficients
  
  as.data.frame(coef_table) %>%
    tibble::rownames_to_column("term") %>%
    filter(grepl(predictor, term)) %>%
    mutate(response = response,
           predictor_var = predictor) %>%
    rename(Estimate = Estimate,
           `Pr(>|z|)` = `Pr(>|t|)`)
}

acetic_pvals <- bind_rows(
  get_lm_pvals(lm_acetate_intake, "intake_group", "acetic_acid"),
  get_lm_pvals(lm_acetate_supp,   "supplement_taker", "acetic_acid"))

print(acetic_pvals)
