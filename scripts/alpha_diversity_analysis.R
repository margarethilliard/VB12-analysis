
# ----- Set up ----- 

#install.packages(c("dplyr", "broom", "vegan", "ggplot2", "bestNormalize"))

library(dplyr)
library(broom)
library(vegan)
library(ggplot2)
library(bestNormalize)

setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# ---- Prepare data ----

# use the transposed abundance table where samples are rows, taxa are columns
metaphlan_sub <- gtdb_taxonomy_sub %>%
  # remove any metadata 
  dplyr::select(-subject_id)
  
# convert to a data frame
metaphlan_df <- as.data.frame(metaphlan_sub)

# convert values to numeric
metaphlan_df[] <- lapply(metaphlan_df, as.numeric)

# ---- Alpha diversity analysis ---- 

# diversity metrics to loop over
alpha_metrics <- c("shannon", "simpson", "invsimpson")

# list to hold results
div_list <- list()

# compute  diversity indices 
for (metric in alpha_metrics) {
  div_list[[metric]] <- vegan::diversity(metaphlan_df, index = metric)
}

# compute richness separately
div_list[["richness"]] <- vegan::specnumber(metaphlan_df)

# combine into data frame
alpha_diversity <- data.frame(subject_id = rownames(metaphlan_df))

for (metric in names(div_list)) {
  alpha_diversity[[metric]] <- div_list[[metric]]
}

# merge with metadata
alpha_diversity <- merge(alpha_diversity, metadata, by = "subject_id")

# compute evenness 
alpha_diversity <- alpha_diversity %>%
  mutate(pielou_evenness = shannon / log(richness)) %>%
  mutate(simpson_evenness = invsimpson / richness) %>%
  mutate(shannon_evenness = shannon / log(richness))

# write.csv(alpha_diversity, "data/alpha_diversity.csv")

# ---- Test for relationships between alpha diversity and categorical intake variables ----

# make sure categorical vars are factors 
alpha_diversity$supplement_taker <- as.factor(alpha_diversity$supplement_taker)
alpha_diversity$intake_group <- as.factor(alpha_diversity$intake_group)
alpha_diversity$B12_tertile <- as.factor(alpha_diversity$B12_tertile)

alpha_metrics <- c("shannon", "invsimpson", "richness", 
             "shannon_evenness", "simpson_evenness", "pielou_evenness")

predictors <- c("supplement_taker", "intake_group")

stats <- list()

# linear models loop 
for (predictor in predictors) {
  for (metric in alpha_metrics) {
    model <- lm(reformulate(predictor, metric), data = alpha_diversity)
    tidy_model <- broom::tidy(model)
    glance_model <- broom::glance(model)
    
    # Remove intercept and get the first (main) p-value
    p_val <- tidy_model$p.value[tidy_model$term != "(Intercept)"][1]
    
    stats[[paste(metric, predictor, sep = "_")]] <- data.frame(
      Metric = metric,
      Predictor = predictor,
      R2 = round(glance_model$r.squared, 3),
      p_val_raw <- p_val, 
      p_value = signif(p_val, 3))
  }
}

stats_df <- do.call(rbind, stats)

# multiple comparison corrections 
stats_df$FDR_p_value <- p.adjust(stats_df$p_val_raw, method = "fdr")

# clean up the table 
stats_df <- stats_df %>%
  select(-c(p_val_raw....p_val))

rownames(stats_df) <- NULL
print(stats_df)

#write.csv(stats_df, "alpha_summary_stats.csv", row.names = FALSE)

# --- Test for relationships between alpha diversity and habitual intake as a continuous predictor ----

# normalize predictor variable first 
habitual_b12_norm <- bestNormalize::bestNormalize(alpha_diversity$habitual_dietary_b12)
alpha_diversity$habitual_b12_norm <- habitual_b12_norm$x.t

lm_stats <- lapply(alpha_metrics, function(metric) {
  model <- lm(reformulate("habitual_b12_norm", metric), data = alpha_diversity)
  tidy_model <- broom::tidy(model)
  glance_model <- broom::glance(model)
  
  data.frame(
    Metric = metric,
    R2 = round(glance_model$r.squared, 3),
    p_value = signif(tidy_model$p.value[2], 3)
  )
})

lm_stats_df <- do.call(rbind, lm_stats)

# Find significant relationships

for (predictor in predictors) {
  for (metric in alpha_metrics) {
    model <- lm(reformulate(predictor, metric), data = alpha_diversity)
    tidy_model <- broom::tidy(model)
    glance_model <- broom::glance(model)
    
    # Remove intercept and get the first (main) p-value
    p_val <- tidy_model$p.value[tidy_model$term != "(Intercept)"][1]
    
    stats[[paste(metric, predictor, sep = "_")]] <- data.frame(
      Metric = metric,
      Predictor = predictor,
      R2 = round(glance_model$r.squared, 3),
      p_val_raw <- p_val, 
      p_value = signif(p_val, 3))
  }
}

stats_df <- do.call(rbind, stats)

# multiple comparison corrections 
stats_df$FDR_p_value <- p.adjust(stats_df$p_val_raw, method = "fdr")

# clean up the table 
stats_df <- stats_df %>%
  select(-c(p_val_raw....p_val))

rownames(stats_df) <- NULL
print(stats_df)

# pivot longer for plotting
plot_data <- alpha_diversity %>%
  select(habitual_b12_norm, all_of(alpha_metrics)) %>%
  pivot_longer(cols = all_of(alpha_metrics), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = as.character(Metric))

# scatter plots 
ggplot(plot_data, aes(x = habitual_b12_norm, y = Value)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ Metric, scales = "free_y") +
  #geom_text(aes(x = Inf, y = Inf,  # top-right of facet
                #label = paste0("R² = ", R2, "\np = ", p_value)),
            #color = "red",
            #hjust = 1.1, vjust = 1.1,
            #inherit.aes = FALSE,
            #size = 4) +
  theme_minimal(base_size = 14) +
  labs(
    x = expression(Normalized~Habitual~B[12]~" intake"),
    y = "Alpha Diversity Metric")

## save plot 
#ggsave("figures/alpha_diversity_vs_intake.pdf", height = 5, width = 8)

## get stats from plotting data 
stats_table <- plot_data %>%
  group_by(Metric) %>%
  do({
    model <- lm(Value ~ habitual_b12_norm, data = .)
    tidied <- broom::tidy(model)
    glanced <- broom::glance(model)
    
    data.frame(
      #Mean = mean_val,
      Estimate = tidied$estimate[tidied$term == "habitual_b12_norm"],
      p_value = tidied$p.value[tidied$term == "habitual_b12_norm"],
      r_squared = glanced$r.squared
    )
  }) %>%
  ungroup()

print(stats_table)
#write.csv(stats_table, "linear_alpha_summary.csv", row.names = FALSE
