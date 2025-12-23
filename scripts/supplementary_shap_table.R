
# --- Set up ----
library(shapviz)
library(dplyr)
library(tidyr)

# ---- Function to process multiple .RData files at once ----
process_shap_file <- function(file_path) {
  
  # Load into isolated environment
  env <- new.env()
  load(file_path, envir = env)
  sv <- env$sv_full
  
  # ---- extract metadata from file name ----
  file_name <- basename(file_path)
  
  response <- sub("^regression_([^~]+)~.*$", "\\1", file_name)
  subgroup <- sub("^.*~microbiome_(.*)\\.RData$", "\\1", file_name)
  
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
    mutate(response = response,
           subgroup = subgroup)
  
  return(beeswarm_tbl_top10)
}

# ---- Batch process files ----
shap_files <- list.files("data/SHAP_objects/intake_subset",
                         full.names = TRUE)

all_beeswarm_tbl <- bind_rows(lapply(shap_files, process_shap_file))

unique(all_beeswarm_tbl$subgroup)

# Fix wonky labels 
all_beeswarm_tbl$subgroup[all_beeswarm_tbl$subgroup == 'regression_propionate~pathways_low_intake.rds'] <- 'low_intake'
all_beeswarm_tbl$subgroup[all_beeswarm_tbl$subgroup == 'regression_butyrate~pathways_no_supp_users.rds'] <- 'no_supp_users'
all_beeswarm_tbl$subgroup[all_beeswarm_tbl$subgroup == 'regression_propionate~pathways_high_intake.rds'] <- 'high_intake'
all_beeswarm_tbl$subgroup[all_beeswarm_tbl$subgroup == 'regression_propionate~pathways_no_supp_users.rds'] <- 'no_supp_users'

View(all_beeswarm_tbl)

# Write file
#readr::write_csv(all_beeswarm_tbl, "data/supplementary_shap_table.csv")
