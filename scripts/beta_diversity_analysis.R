# ----- Set up ----- 

#install.packages(c("dplyr", "vegan", "bestNormalize", "ape", "ggplot2))

# Load libraries
library(dplyr)
library(vegan)
library(bestNormalize)
library(ape)
library(ggplot2)

# Set working directory and source the data 
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# ---- Prepare data ----

# use the transposed table where samples are rows, taxa are columns
metaphlan_sub <- gtdb_taxonomy_sub %>%
  # remove any metadata 
  dplyr::select(-subject_id)

# convert to a data frame
metaphlan_df <- as.data.frame(metaphlan_sub)

# convert values to numeric
metaphlan_df[] <- lapply(metaphlan_df, as.numeric)

# reading in alpha diversity table that has useful metadata 
##alpha_diversity <- readr::read_delim("data/alpha_diversity.csv") %>%
  #select(-c("...1"))


# ---- Beta diversity analysis ---- 

beta_metrics <- c("bray", "jaccard")
independent_vars <- c("B12_tertile", "supplement_taker", "intake_group", "habitual_b12_norm")

beta_results <- data.frame()

# compute distances 
beta_dists <- lapply(beta_metrics, function(method) {
  vegan::vegdist(metaphlan_df, method = method)
})
names(beta_dists) <- beta_metrics

for (metric in beta_metrics) {
  dist_matrix <- beta_dists[[metric]]
  
  for (var in independent_vars) {
    formula <- as.formula(paste("dist_matrix ~", var))
    
    result <- vegan::adonis2(
      formula,
      data = metadata_sub,
      permutations = 999,
      by = "margin",
      method = attr(dist_matrix, "method")
    )
    
    beta_results <- rbind(beta_results, data.frame(
      Metric = metric,
      Predictor = var,
      R2 = round(result$R2[1], 3),
      p_value = signif(result$`Pr(>F)`[1], 3)
    ))
  }
}
print(beta_results)

# multiple comparison corrections 
beta_results$FDR_p_value <- p.adjust(beta_results$p_value, method = "fdr")

# write table 
#write.csv(beta_results, "beta_summary_stats.csv", row.names = FALSE)

# ---- Visualize ordination ---- 

ordination_data <- lapply(names(beta_dists), function(metric) {
  dist_mat <- beta_dists[[metric]]
  pcoa_res <- pcoa(dist_mat)
  
  coords <- as.data.frame(pcoa_res$vectors[, 1:2])
  coords$subject_id <- rownames(coords)
  coords$Metric <- metric
  return(coords)
})

ordination_df <- bind_rows(ordination_data)

# merge with metadata
ordination_df <- left_join(ordination_df, metadata_sub, by = "subject_id")

# plot
ggplot(ordination_df, aes(Axis.1, Axis.2, color = habitual_b12_norm)) +
  geom_point(size = 2) +
  facet_wrap(~ Metric, scales = "free") +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal(base_size = 14) +
  labs(x = "PCoA Axis 1", y = "PCoA Axis 2",
       color = expression(Normalized~Habitual~B[12]~" intake"),
       title = "Beta Diversity (PCoA) by B12 Intake")

# nmds 
nmds_results <- lapply(beta_dists, function(dist_mat) {
  metaMDS(dist_mat, k = 2, trymax = 100, autotransform = FALSE, trace = FALSE)
})

names(nmds_results) <- names(beta_dists)

# extract nmds scores 
nmds_coords <- lapply(names(nmds_results), function(metric) {
  scores_df <- as.data.frame(scores(nmds_results[[metric]]))
  scores_df$subject_id <- rownames(scores_df)
  scores_df$Metric <- metric
  scores_df
})

nmds_df <- bind_rows(nmds_coords)

# merge 
nmds_df <- left_join(nmds_df, metadata_sub, by = "subject_id")

# plot
ggplot(nmds_df, aes(NMDS1, NMDS2, color = habitual_b12_norm)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ Metric, scales = "free") +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal(base_size = 14) +
  labs(
    x = "NMDS Axis 1", y = "NMDS Axis 2",
    color = expression(Normalized~Habitual~B[12]),
    title = "NMDS of Beta Diversity by Habitual B12 Intake"
  )

# stress scores 
nmds_stress <- sapply(nmds_results, function(x) x$stress)
print(round(nmds_stress, 3))

# Rule of thumb:
# < 0.1 = good
# 0.1–0.2 = usable
# > 0.2 = poor fit (shouldn't trust ordination)

# ---- Check homogeneity of dispersion assumption ----

beta_metrics <- c("bray", "jaccard")
discrete_independent_vars <- c("B12_tertile", "supplement_taker", "intake_group") 

dispersion_results_table <- data.frame()

for (metric in beta_metrics) {
  dist_matrix <- beta_dists[[metric]]
  
  for (var in discrete_independent_vars) {
    
    grouping <- metadata_sub[[var]] 
    
    dispersion_result <- vegan::betadisper(dist_matrix, grouping)
    anova_result <- anova(dispersion_result)
    
    dispersion_results_table <- rbind(dispersion_results_table, data.frame(
      Metric = metric,
      Variable = var,
      p_value = signif(anova_result$`Pr(>F)`[1], digits = 3)
    ))
  }
}

# both distance matrices are not significantly different in terms of their dispersion based on supplement use, B12 tertile, or B12 intake relative to the mean
print(dispersion_results_table)
