
# --- Set up ----
library(shapviz)
library(dplyr)
library(tidyr)
library(purrr)

set.seed(8675309)

# ---- Function to process multiple .RData files at once ----
process_shap_file <- function(file_path) {
  
  # Load into isolated environment
  env <- new.env()
  load(file_path, envir = env)
  sv <- env$sv_full
  
  # ---- extract metadata from file name ----
  file_name <- basename(file_path)
  
  predictor <- stringr::str_extract(file_name, "(?<=~)(pathways|microbiome)")
  response <- sub("^regression_([^~]+)~.*$", "\\1", file_name)
  subgroup <- sub("^.*~[^_]+_(.*)\\.(rds|RData)$", "\\1",file_name)
  
  # ---- build beeswarm table ----
  shap_wide <- as.data.frame(sv$S) %>%
    mutate(row_id = row_number())
  
  feature_wide <- as.data.frame(sv$X) %>%
    mutate(row_id = row_number())
  
  shap_long <- shap_wide %>%
    pivot_longer(cols = -row_id,
                 names_to = "feature",
                 values_to = "shap_value")
  
  feature_long <- feature_wide %>%
    pivot_longer(cols = -row_id,
                 names_to = "feature",
                 values_to = "feature_value")
  
  beeswarm_tbl <- left_join(shap_long,
                            feature_long,
                            by = c("row_id", "feature"))
  
  # ---- compute feature importance ----
  feature_importance <- beeswarm_tbl %>%
    group_by(feature) %>%
    summarise(mean_abs_shap = mean(abs(shap_value)),
              .groups = "drop") %>%
    arrange(desc(mean_abs_shap))
  
  # ---- keep top 10 features ----
  top10_features <- feature_importance %>%
    slice_head(n = 10) %>%
    pull(feature)
  
  beeswarm_tbl_top10 <- beeswarm_tbl %>%
    filter(feature %in% top10_features) %>%
    left_join(feature_importance, by = "feature") %>%
    mutate(predictor = predictor, 
           response = response,
           subgroup = subgroup)
  
  return(beeswarm_tbl_top10)
}

# ---- Batch process files ----
shap_files <- list.files("/Users/local-margaret/Desktop/VB12-analysis/data/SHAP_objects/intake_subset",
                         full.names = TRUE)

shap_files
# will need to add other models, as they performed equally as well 

all_beeswarm_tbl <- bind_rows(lapply(shap_files, process_shap_file))

unique(all_beeswarm_tbl$subgroup)
unique(all_beeswarm_tbl$predictor)
unique(all_beeswarm_tbl$response)

View(all_beeswarm_tbl)

# Write the entire giant file
#readr::write_csv(all_beeswarm_tbl, "data/supplementary_shap_table.csv")

# separate tibbles by metadata
grouped_tables <- all_beeswarm_tbl %>%
  group_by(predictor, response, subgroup) %>%
  group_split()

group_keys <- purrr::map_dfr(
  grouped_tables,
  ~ distinct(.x, predictor, response, subgroup))

setwd("/Users/local-margaret/Desktop/VB12-analysis")
output_dir <- "data/beeswarm_tables"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

all_beeswarm_tbl %>%
  group_by(predictor, response, subgroup) %>%
  group_walk(~ {
    
    tbl_out <- .x %>%
      mutate(
        predictor = .y$predictor,
        response  = .y$response,
        subgroup  = .y$subgroup
      )
    
    file_name <- sprintf(
      "beeswarm_%s_%s_%s.csv",
      .y$predictor,
      .y$response,
      .y$subgroup
    )
    
    readr::write_csv(
      tbl_out,
      file.path(output_dir, file_name)
    )
  })
