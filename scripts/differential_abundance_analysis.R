
# ---- Set up ---- 
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# install.packages(c("ggplot2", "patchwork"))
library(ggplot2)

# load maaslin3 package 
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("biobakery/maaslin3")
library(maaslin3)

##### construct differential abundance models #####

# make sure metadata is a data.frame (not tibble) for Maaslin3
metadata <- as.data.frame(metadata_sub)

# rownames need to be subject id for Maaslin3 
rownames(metadata) <- metadata$subject_id

set.seed(1996)

fit_out <- maaslin3(input_data = gtdb_taxonomy,
                    input_metadata = metadata,
                    output = 'diff_abundance_supp_user+fiber_output_GTDB',
                    formula = '~ supplement_taker + dt_fiber_sol',
                    normalization = 'TSS',
                    transform = 'LOG',
                    min_prevalence = 0.25,
                    min_abundance = 0.001, 
                    cores = 1)

fit_out_high_low <- maaslin3(input_data = gtdb_taxonomy,
                             input_metadata = metadata,
                             output = 'diff_abundance_intake_grp+fiber_output_GTDB',
                        formula = '~ intake_group + dt_fiber_sol',
                        normalization = 'TSS',
                        transform = 'LOG',
                        min_prevalence = 0.25,
                        min_abundance = 0.001,
                        cores = 1)


fit_out_plasma <- maaslin3(input_data = gtdb_taxonomy,
                    input_metadata = metadata,
                    output = 'plasma_group_+fiber_output_GTDB',
                    formula = '~ plasma_group + dt_fiber_sol',
                    normalization = 'TSS',
                    transform = 'LOG',
                    min_prevalence = 0.25,
                    min_abundance = 0.001,
                    cores = 1)

# ---- Box plots for differences in taxa abundance based on intake group ----
transformed <- readr::read_delim("/Users/local-margaret/Desktop/VB12-analysis/diff_abundance_intake_grp+fiber_output_GTDB/features/data_transformed.tsv") %>%
  select(c(`feature`, `d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Acutalibacteraceae|g__UBA1417|s__UBA1417_sp003531055`))

transformed$feature <- as.factor(transformed$feature)

metadata_to_join <- metadata_sub %>%
  rename(feature = subject_id) %>%
  select(c(feature, intake_group))

metadata_to_join$feature <- as.factor(metadata_to_join$feature)

boxplot_data <- left_join(transformed, metadata_to_join, by = "feature") %>%
  na.exclude()

plot <- ggplot(boxplot_data, aes(x=intake_group, 
                                   y=`d__Bacteria|p__Firmicutes_A|c__Clostridia|o__Oscillospirales|f__Acutalibacteraceae|g__UBA1417|s__UBA1417_sp003531055`,
                                   colour = intake_group)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0.15), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) + 
  scale_colour_manual(values = c("#e24f4a", "#969696")) +
  theme_bw(base_size = 16) + 
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "black")) +
  labs(x = expression(
    atop(
      B[12]~"intake group relative to median")),
    y = expression(
      atop(
        paste(italic("Acutalibacteraceae"), " UBA1417 abundance"),
        "(Normalization: TSS, Transformation: Log base 2)")),
    subtitle = "FDR-corrected q = 0.007")

plot

#ggsave("figures/Acutalibacteraceae_diff_ab_supp.pdf", height = 6, width =8)
