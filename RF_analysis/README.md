#!/bin/bash

# TaxaHFE-ML & dietML workflow for FL100 VB12-microbiome project 
# Written by: Margaret A. Hilliard, last updated on October 1, 2025

# PRE-PROCESSING STEPS:

# 1. If you're doing a subset analysis, subset your metadata using "subset_taxaHFE_dataset.R" script, or similar method 

# 2. Generate a merged relative abundnace table (we used Metaphlan4, then converted the taxonomy to GTDB format). The file should look similar to this: 
#clade_name      001    002    005   
#d__Bacteria     99.05847000000004       99.93078        98.18908
#d__Archaea      0.8837499999999999      0.0     1.79209 
#d__Bacteria|p__Firmicutes_A     81.83093000000004       83.96293999999999	81.48342000000001     

# 3. Generate a metadata file. It should contain the subjectID, predictor, response, and co-variates of interest: 
#subject_id	plasma_b12	habitual_dietary_b12	age	sex	bmi
#001	365	9.08	25	Male	20.8
#002	411	10.320929	30	Female	23.6
#005	267	1.4061	27	Female	27.0

# STEP 1: Generate a random seed file & a text file with the list of commands (this is your input to TaxaHFE-ML in parallel on HIVE)

# A note on resource usage: I'm using an interactive HIVE desktop environment where I've requested 30 cores & 64 GB of RAM for ~ 8 hours 
mkdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data && cd /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data

bash generate_100_random_seeds.sh >> random_seeds.txt 

# change the parameters/paths in this file before running 
bash generate_100_commands.sh

# STEP 2: Run TaxaHFE-ML to select most predictive microbial features re: your outcome of interest  

module load apptainer/latest 
module load parallel/20220522

parallel -j 5 -a 100_commands.txt

# STEP 3: Use the watch script to keep track of the best performing models (in a separate terminal tab)

watch -d bash watch_script.sh 

# STEP 4: Re-run the best performing model using the --shap flag and associated seed 

mkdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data/output_shap && singularity run --pwd /app --no-home --contain --workdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/supp_best_model/ --bind /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data/:/data /quobyte/dglemaygrp/aoliver/software/taxahfe_ml_dev.sif taxaHFE_metadata.csv modified_merged_metaphlan_v4-0-6_GTDB.txt -o output_shap -s subject_id -l plasma_b12 -t numeric -L 3 -n 6 --metric rmse --prevalence 0.25 --abundance 0.01 -c 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --nperm 80 --seed 9618 -d --shap

# STEP 5: Subset your data to include all dietary features + important microbial features ONLY (again, I'm using my "subset_taxaHFE_dataset.R" script to do this but you can use another programming language)

# STEP 6: Run DietML using the subset of features (NOT TaxaHFE-ML, becuase we don't need to further select features!)

mkdir /home/hilliard/full_data_reduced_features && cd /home/hilliard/full_data_reduced_features

# use the same set of random seeds! 
cp /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data/random_seeds.txt .

# change the parameters/paths in this file before running 
bash generate_100_commands.sh

module load apptainer/latest 
module load parallel/20220522

parallel -j 5 -a 100_best_commands.txt

# STEP 7: Use the watch script to keep track of the best performing models (in a separate terminal tab)

watch -d bash dietML_watch_script.sh

# STEP 8: Re-run the best performing model using the --shap TRUE argument and associated seed 

mkdir -p /home/hilliard/full_data_reduced_features/output_shap && singularity exec --no-home --bind /home/hilliard/full_data_reduced_features/:/data /quobyte/dglemaygrp/aoliver/software/nutrition_tools.sif dietML --subject_identifier subject_id --label plasma_b12 --type regression --model rf --ncores 6 --metric rmse --cor_level 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --seed 2482 --shap TRUE /data/dietML_metadata.csv /data/output_shap/ml_results/ 

# STEP 10: Quantify model accuracy/performace relative to the null model using the "mean_percent_change_accuracy_plot.R" script 

