## DietML and TaxaHFE workflow on HIVE for FL100 VB12 x microbiome project
- Written by: Margaret A. Hilliard, last updated on Feb. 20, 2026
- A note on resource usage: For this analysis I used array job scripts that were executed on HIVE, a slurm-based HPC cluster managed by the High Performance Computing Core Facility at the University of California Davis.

 ##  Input file summary

| File name | Path | Description |
|------|------|--------------|
| 1. scfa_metadata.csv | intake_subset_analyses/data/ | contains subject_id,new_butyrate,acetate,propionate,age,sex,bmi,dt_fiber_sol,intake_group,supplement_taker variables |
| 2. pathways_metadata.csv | intake_subset_analyses/data/ | contains HUMAnN 3.0 pathways (columns) and subject_ids (rows) + all covariate variables listed in scfa_metadata.csv |
| 3. modified_merged_metaphlan_v4-0-6_GTDB.txt | intake_subset_analyses/data/ | Shared microbiome input file used for all TaxaHFE-ML models. This version was converted to GTDB taxonomy from the merged MetaPhlAn4 table using [this utility script](https://github.com/biobakery/MetaPhlAn/blob/master/metaphlan/utils/sgb_to_gtdb_profile.py). IMPORTANT NOTE: the GTDB taxa table was further modified by changing the delimiter in the header to a pipe to be compliant with TaxaHFE-ML's expected delimiter! |
| 4. random_seeds.txt | intake_subset_analyses/ | One seed (four digit number) per line, without a header, to generate reproducible random searches. If you need to generate this file, you can use scripts/generate_100_random_seeds.sh, and modify it to make longer/shorter seeds if needed. |

 Workflow overview 
 ------------------

1. Navigate to project root directory and generate nested output directories and subset the input metadata files based on B12 intake group and supplement use. 
```bash
cd /path/to/project_root/intake_subset_analyses/
bash scripts/shared_subset_analysis_setup.sh
```
- Input 1: scfa_metadata.csv Example format:

        subject_id,new_butyrate,acetate,propionate,age,sex,bmi,dt_fiber_sol,intake_group,supplement_taker
        subject_001,9.2,32.6,8.3,25,Male,20.8,6.9,High,No
        subject_002,8.3,36.0,5.5,30,Female,23.6,8.0,High,No
        subject_003,13.4,27.5,6.3,36,Female,25.3,7.8,Low,No

- Input 2: pathways_metadata.csv 
- Output: ~32+ new metadata files that are roughly and finely subsetted based on intake group and metabolite (outcome of interest). 

2. Generate DietML and TaxaHFE-ML commands:
```bash
# optional: bash scripts/generate_100_random_seeds.sh 
bash scripts/generate_taxaHFE_ML_commands.sh # for microbiome data 
bash scripts/generate_dietML_commands.sh # for pathway data 
```
- You can change the parameters to your liking (See "Define metabolite-specific hyperparameters" section) and use your own list of random seeds for reproducibility. 
- Input 1a: data/*_metadata.csv. These are subset- and metabolite-specific metadata files that follow the naming pattern: <subset>_<metabolite>_metadata.csv. If the file says "pathways" it also contains the HUMAnN 3.0 pathways associated with the subset of samples, and will be the sole input to DietML. These files can be generated using scripts/subset_analysis_setup.sh
- Input 1b: modified_merged_metaphlan_v4-0-6_GTDB.txt. Shared microbiome input file used for all microbiome models. This version was converted to GTDB taxonomy from the merged metaphlan4 table using this utility script: https://github.com/biobakery/MetaPhlAn/blob/master/metaphlan/utils/sgb_to_gtdb_profile.py. IMPORTANT NOTE: the GTDB taxa table was further modified by changing the delimiter in the header to '|' to be compliant with taxaHFE-ML's expected delimiter! 
- Input 2: random_seeds.txt. One seed per line to generate reproducible random searches. If you need to generate a list of random seeds, you can use scripts/generate_100_random_seeds.sh. I've also provided the random_seed.txt file I've used. 
- Output:  12 subset- and metabolite-specific command files placed in nested directories (something like: intake_subset_analyses/subset_of_interest/)

2. Generate DietML and TaxaHFE-ML array job scripts for all subsets/metabolite combinations:
```bash
bash scripts/generate_taxaHFE-ML_array_job_scripts.sh # for microbiome data 
bash scripts/generate_dietML_array_job_scripts.sh # for pathway data 
```
- Array jobs are a great way to run a lot of commands in a resource savvy way. These scripts schedule 100 scripts, but only run 5 at a time (to limit my resource usage). This is a customizable feature that can be changed in the SLURM header, or the "ARRAY_JOBS" object in the "generature_*_array_jobs.sh" scripts.

3. Submit array jobs to the SLURM scheduler: 
```bash
# Example:
sbatch high_intake/pathways/propionate/scripts/high_intake_propionate_array_job.sh
squeue --me # check the job status 

```
Required directory structure:
-----------------------
```
# intake_subset_analyses/ 
# ├── data/ 
# │   ├── high_intake_acetate_metadata.csv
# │   ├── high_intake_butyrate_metadata.csv
# │   ├── high_intake_propionate_metadata.csv
# │   ├── low_intake_acetate_metadata.csv
# │   ├── ...                                        # subsetted metadata files generated using shared_subset_analysis_setup.sh go here 
# │   └── modified_merged_metaphlan_v4-0-6_GTDB.txt  # shared microbiome file
# ├── high_intake/
# │   └── microbiome/
# │       ├── acetate/
# │       │   └── commands/                          # scripts from generate_taxaHFE_ML_commands.sh go here
# │       ├── butyrate/
# │       │   └── commands/
# │       └── propionate/
# │           └── commands/
# │   └── pathways/
# │       ├── acetate/
# │       │   └── commands/                          # scripts from generate_dietML_commands.sh go here
# │       ├── butyrate/
# │       │   └── commands/
# │       └── propionate/
# │           └── commands/
# ├── low_intake/
# │   └── microbiome/
# │       └── ...
# │   └── pathways/
# │       └── ...
# ├── no_supp_use/
# │   └── microbiome/
# │       └── ...
# │   └── pathways/
# │       └── ...
# ├── supp_use/
# │   └── microbiome/
# │       └── ...
# │   └── pathways/
# │       └── ...
# └── scripts/
#     └── shared_subset_analysis_setup.sh
#     └── generate_dietML_commands.sh
#     └── generate_taxaHFE_ML_commands.sh
#     └── ...
# ├── random_seeds.txt                               # can be generated from generate_100_random_seeds.sh if needed 
```
