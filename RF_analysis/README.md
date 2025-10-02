
## TaxaHFE-ML & dietML workflow on HIVE for FL100 VB12 x microbiome project
- Written by: Margaret A. Hilliard, last updated on October 1, 2025
- A note on resource usage: I'm using an interactive HIVE desktop environment where I've requested 30 cores & 64 GB of RAM for ~ 8 hours 

### PRE-PROCESSING STEPS:

- Generate a merged relative abundnace table (we used Metaphlan4, then converted the taxonomy to GTDB format).

    clade_name      001    002    005   
    d__Bacteria     99.05847000000004       99.93078        98.18908
    d__Archaea      0.8837499999999999      0.0     1.79209 
    d__Bacteria|p__Firmicutes_A     81.83093000000004       83.96293999999999	81.48342000000001

- Generate a metadata file. It should contain the subjectID, predictor, response, and co-variates of interest: 

    subject_id	plasma_b12	habitual_dietary_b12	age	sex	bmi
    001	365	9.08	25	Male	20.8
    002	411	10.320929	30	Female	23.6
    005	267	1.4061	27	Female	27.0
    
- If you're doing a subset analysis, subset your metadata using "subset_taxaHFE_dataset.R" script, or similar method


### STEP 1: Generate a random seed file & a text file with the list of commands (this is your input to TaxaHFE-ML in parallel on HIVE)

    mkdir /home/hilliard/full_data_reduced_features && cd/home/hilliard/full_data_reduced_features
    
    bash generate_100_random_seeds.sh >> random_seeds.txt 
    
    # change the parameters & paths in this file before running 
    bash generate_100_commands.sh

### STEP 2: Run dietML

    module load apptainer/latest 
    module load parallel/20220522
    
    parallel -j 5 -a 100_commands.txt

### STEP 3: Use the watch script to keep track of the best performing models (I use a separate terminal tab)

    watch -d bash watch_script.sh 

# STEP 4: Re-run the best performing model using the --shap flag and associated seed 

    mkdir /home/hilliard/full_data_reduced_features/output_shap && singularity exec --no-home --bind /home/hilliard/full_data_reduced_features/:/data /quobyte/dglemaygrp/aoliver/software/nutrition_tools.sif dietML --subject_identifier subject_id --label plasma_b12 --type regression --model rf --ncores 6 --metric rmse --cor_level 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --seed 2482 --shap TRUE /data/dietML_metadata.csv /data/output_shap/ml_results/

# STEP 10: Quantify model accuracy/performace relative to the null model using the "mean_percent_change_accuracy_plot.R" script 

