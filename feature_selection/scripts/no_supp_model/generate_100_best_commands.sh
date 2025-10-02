#!/bin/bash

for seed in $(cat random_seeds.txt) 
do
echo "mkdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/no_supp_best_model/output_${seed} && singularity run --pwd /app --no-home --contain --workdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/no_supp_best_model/ --bind /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/no_supp_best_model/:/data /quobyte/dglemaygrp/aoliver/software/taxahfe_ml_dev.sif taxaHFE_metadata.csv modified_merged_metaphlan_v4-0-6_GTDB.txt -o output_${seed} -s subject_id -l plasma_b12 -t numeric -L 3 -wWD -n 6 --metric rmse --prevalence 0.25 --abundance 0.01 -c 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --nperm 80 --seed ${seed} -d" >> 100_best_commands.txt
done
