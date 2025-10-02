### Feature selection workflow on HIVE for FL100 VB12 x microbiome project 
- Written by: Margaret A. Hilliard, last updated on October 1, 2025
- A note on resource usage: I'm using an interactive HIVE desktop environment where I've requested 30 cores & 64 GB of RAM for ~ 8 hours 

### Pre-processing steps:
- Generate a metadata file. It should contain the subjectID, predictor, response, and co-variates of interest. Example format: 

        subject_id,age,sex,bmi,eGFR,plasma_b12,habitual_dietary_b12
        subject_001,25,Male,20.838589981447125,113.7838304,365,9.08
        subject_002,30,Female,23.613722432613056,117.2782542,411,10.320929
        subject_003,27,Female,27.028095733610822,115.9365355,267,1.4061000000000001

- Generate a merged relative abundance taxa table (we used MetaPhlAn4, then converted the results to the GTDB taxonomy format). Example format: 
       
        clade_name      subject_001    subject_002    subject_003   
        d__Bacteria     99.05847000000004       99.93078        98.18908
        d__Archaea      0.8837499999999999      0.0     1.79209 
        d__Bacteria|p__Firmicutes_A     81.83093000000004       83.96293999999999	81.48342000000001   

- If you're doing a subset analysis, subset your metadata using "subset_taxaHFE_dataset.R" script, or similar method to include only participants with your feature of interest (e.g., supplement users). You don't need to subset your taxa table until after feature selection. :)  

### Step 1: Generate a random seed and command text file (this is your input to parallel on HIVE)

        mkdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data_best_model && cd /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data_best_model
        
        bash generate_100_random_seeds.sh >> random_seeds.txt 
        
        # change the parameters & paths in this file before running 
        bash generate_100_commands.sh

### Step 2: Run TaxaHFE-ML to select the most explanatory microbial features for your response of interest 

        module load apptainer/latest 
        module load parallel/20220522
        
        parallel -j 5 -a 100_best_commands.txt

### Step 3: Use the watch script to keep track of the best performing models (in a separate terminal tab)

        watch -d bash watch_script.sh # to exit watch use: control+c 

### Step 4: Re-run the best performing model using the --shap flag and associated seed 

        mkdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data_best_model/output_shap && singularity run --pwd /app --no-home --contain --workdir /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data_best_model/ --bind /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data_best_model/:/data /quobyte/dglemaygrp/aoliver/software/taxahfe_ml_dev.sif taxaHFE_metadata.csv modified_merged_metaphlan_v4-0-6_GTDB.txt -o output_shap -s subject_id -l plasma_b12 -t numeric -L 3 -n 6 --metric rmse --prevalence 0.25 --abundance 0.01 -c 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --nperm 80 --seed 9618 -d --shap 

### Step 5: Inspect the SHAP plot, then subset your data to include all dietary & person-specific features + important microbial features  by using the "subset_taxaHFE_dataset.R" script

- Note: If you're changing the clades that TaxaHFE-ML selected (that appeared in the SHAP plot), I've done this in a very un-glamorous and manual way by searching the raw merged GTDB taxa table file for "caccae" for example to get the proper format for "Bacteroides caccae". The reason being, the taxa will have slightly different formatting in the SHAP plot than the GTDB taxonomy input file (lowercase characters will be capitalized respectively).

### Step 6: Run dietML using the subset of taxonomic features you've identified (don't run TaxaHFE-ML again becuase we don't need to further select features!).

        mkdir /home/hilliard/full_data_best_model_reduced_features && cd /home/hilliard/full_data_best_model_reduced_features
    
        cp /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/gtdb_taxonomy/regression_models/full_data_best_model/random_seeds.txt .
    
        module load apptainer/latest 
        module load parallel/20220522
        
        # change the parameters & paths in this file before running 
        bash generate_100_commands.sh
        
        parallel -j 5 -a 100_best_commands.txt

### Step 7: Use the watch script to keep track of the best performing models (in a separate terminal tab)

        watch -d bash dietML_watch_script.sh # to exit watch use: control+c

### Step 8: Re-run the best performing model using the --shap TRUE argument and associated seed 

        mkdir -p /home/hilliard/full_data_best_model_reduced_features/output_shap && singularity exec --no-home --bind /home/hilliard/full_data_best_model_reduced_features/:/data /quobyte/dglemaygrp/aoliver/software/nutrition_tools.sif dietML --subject_identifier subject_id --label plasma_b12 --type regression --model rf --ncores 6 --metric rmse --cor_level 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --seed 2482 --shap TRUE /data/dietML_metadata.csv /data/output_shap/ml_results/ 

### Step 9: Quantify model accuracy/performace relative to the null model using the "mean_percent_change_accuracy_plot.R" script

