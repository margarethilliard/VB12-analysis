
## dietML workflow on HIVE for FL100 VB12 x microbiome project
- Written by: Margaret A. Hilliard, last updated on October 1, 2025
- A note on resource usage: I'm using an interactive HIVE desktop environment where I've requested 30 cores & 64 GB of RAM for ~ 8 hours 

### Pre-processing steps:

- Generate a metadata file. It should contain the subjectID, predictor, response, and co-variates of interest. Example format: 

        subject_id,age,sex,bmi,eGFR,plasma_b12,habitual_dietary_b12
        subject_001,25,Male,20.838589981447125,113.7838304,365,9.08
        subject_002,30,Female,23.613722432613056,117.2782542,411,10.320929
        subject_003,27,Female,27.028095733610822,115.9365355,267,1.4061000000000001

    
- If you're doing a subset analysis, subset your metadata using "subset_taxaHFE_dataset.R" script, or similar method to include only participants with your feature of interest (e.g., supplement users). 

### STEP 1: Generate a random seed file & a text file with the list of commands (this is your input to parallel on HIVE)

    mkdir /quobyte/dglemaygrp/mhilliard/FL100/dietML/full_data && cd /quobyte/dglemaygrp/mhilliard/FL100/dietML/full_data
    
    bash generate_100_random_seeds.sh >> random_seeds.txt 
    
    # change the parameters & paths in this file before running 
    bash generate_100_commands.sh

### STEP 2: Run dietML

    module load apptainer/latest 
    module load parallel/20220522
    
    parallel -j 5 -a 100_commands.txt

### STEP 3: Optional, use the watch script to keep track of the best performing models (I use a separate terminal tab)

    watch -d bash watch_script.sh 

### STEP 4: Quantify model accuracy/performace relative to the null model using the "mean_percent_change_accuracy_plot.R" script 
