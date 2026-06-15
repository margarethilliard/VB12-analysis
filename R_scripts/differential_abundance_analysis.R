
# ---- Set up ---- 
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# install.packages(c("ggplot2"))
library(ggplot2)

# load maaslin3 package 
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("biobakery/maaslin3")
library(maaslin3)

# ---- Build differential abundance models ----

# make sure metadata is a data.frame (not tibble) for Maaslin3
metadata <- as.data.frame(metadata_sub)

# row names need to be subject id for Maaslin3 
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
