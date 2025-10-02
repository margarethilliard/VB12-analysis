### Hyper parameter tuning workflow on HIVE for FL100 VB12 x microbiome project 
- Written by: Margaret A. Hilliard, last updated on October 1, 2025
- The original TaxaHFE_random_search.R script was written by Andrew Oliver - thanks, Andrew! 
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

### Step 1: Generate command text file by running the "TaxaHFE_random_search.R" script

- Key output file is "hyper_param_search_commands.txt"

### Step 2: Run TaxaHFE-ML 

        module load apptainer/latest 
        module load parallel/20220522
        
        parallel -j 5 -a hyper_param_search_commands.txt

### Step 3: Use the watch script to keep track of the best performing models (in a separate terminal tab)

        watch -d bash watch_script.sh # to exit watch use: control+c 

### Step 4: Inspect and notate the best performing model's parameters to use on all RF models going forward 

      grep "your_best_seed_here" output_*/ml_analysis/ml_results.csv
      grep "output_number_here" hyper_param_search_commands.txt
