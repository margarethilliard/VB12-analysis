## taxaHFE hyper parameter random grid search

## script originally written by Andrew Oliver, modified by Margaret Hilliard 

## Define the docker commands and parameters that will not change
docker_cmd="singularity run --pwd /app --no-home --contain --workdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/hyper_param_tune/"
data_path="/quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/hyper_param_tune/"
docker_image="/quobyte/dglemaygrp/aoliver/software/taxahfe_ml_dev.sif"
metadata="taxaHFE_metadata.txt"
microbiome="modified_merged_metaphlan_v4-0-6_GTDB.txt"
fixed_parameters="-s subject_id -l plasma_b12 -t numeric -L 3 -wWD"
ncores = 6

## list the parameters and options you want to test
prevelance=c(0.20,0.25,0.30)
abundance=c(0.01, 0.05, 0.1)
cor_level=c(0.75,0.85,0.95)
nperm=c(10,40,80)
train_split=c(0.7,0.75,0.8)
folds=c(3,5,10)
# regression tasks:
 metric=c("mae", "rmse", "rsq")
# classifier tasks:
# metric=c("bal_accuracy", "kap", "f_meas")
tune_length=c(80,160,240)
tune_time=c(2,5,8)
tune_stop=c(10,15)
superfilter=c("", " -d ")

## Expand the above options into every combination
grid_search <- tidyr::expand_grid(prevelance, abundance, cor_level, 
                   nperm, train_split, folds, metric, 
                   tune_length, tune_time, tune_stop, superfilter)

## make some unique output names
grid_search$outputs <- paste0("output_", seq(1:nrow(grid_search)))

library(dplyr)

## the grid can make A LOT of search parameters. Random search is often
## good enough. Randomly select n number of parameter combinations

random_search <- grid_search %>% dplyr::slice_sample(., n = 500)
               
## Write these combinations to a file:

for (i in seq(1:nrow(random_search))) {
  tmp <- print(paste0("mkdir ",data_path,random_search[["outputs"]][i]," && ",docker_cmd," --bind ",data_path,":/data ",docker_image," ",metadata," ",microbiome," ",random_search[["outputs"]][i], "/out.csv ", fixed_parameters," -n ", ncores, " --metric ", random_search[["metric"]][i] ," --prevalence ", random_search[["prevelance"]][i]," --abundance ", random_search[["abundance"]][i]," -c ", random_search[["cor_level"]][i]," --train_split ", random_search[["train_split"]][i]," --tune_length ", random_search[["tune_length"]][i]," --tune_time ", random_search[["tune_time"]][i]," --tune_stop ", random_search[["tune_stop"]][i]," --folds ", random_search[["folds"]][i], " --nperm ", random_search[["nperm"]][i], " --seed ", sample(1:100000,1), random_search[["superfilter"]][i]), quote = F)
  readr::write_lines(file = "hyper_param_search_commands.txt", x = tmp, append = TRUE)
}
