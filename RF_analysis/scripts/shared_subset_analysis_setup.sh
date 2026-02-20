#!/bin/bash

# Purpose: creates nested output directories and subsets of the scfa_metadata.csv and pathways_metadata.csv files based on VB12 intake group and supplement use for use with scripts in intake_subset_analysis/scripts
# Input 1: scfa_metadata.csv containing the following variables: subject_id,new_butyrate,acetate,propionate,age,sex,bmi,dt_fiber_sol,intake_group,supplement_taker
# Input 2: pathways_metadata.csv that contains all humann pathways (columns) and subject_ids (rows) + all variables listed in scfa_metadata.csv
# Usage: bash shared_subset_analysis_setup.sh 
# Output: ~32+ new metadata files that are roughly and finely subsetted based on intake group and metabolite. You'll also have the appropriate nested output directories to run scripts/generate_taxaHFE_ML_commands.sh!

### PART 0 for DIRECTORY SETUP ###

# Define groups for creating unique directories 
intake_groups=("high_intake" "low_intake" "supp_use" "no_supp_use")
data_types=("microbiome" "pathways")
short_chain_fatty_acids=("acetate" "propionate" "butyrate")

# Create output directories for all combinations of data 
for intake in "${intake_groups[@]}"; do
  for type in "${data_types[@]}"; do
    for acid in "${short_chain_fatty_acids[@]}"; do
      dir="intake_subset_analyses/${intake}/${type}/${acid}"
      echo "Creating directory: $dir"
      mkdir -p "$dir"
    done
  done
done

# Load conda to use miller 
module load conda/latest

# Create conda environment and install miller from conda-forge
conda create -n tools -y
conda activate tools
conda install -c conda-forge miller -y

# Verify miller works (similar to dplyr but in bash)
mlr --version

# Roughly subset the input metadata files by intake/supplement use 
cd /quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/intake_subset_analyses/data/

### PART 1 for TAXAHFE-ML ###

# Base input
input="scfa_metadata.csv"

# Subset categories and filters
declare -A subsets=(
  [supp_use]="\$supplement_taker == \"Yes\""
  [no_supp_use]="\$supplement_taker == \"No\""
  [high_intake]="\$intake_group == \"High\""
  [low_intake]="\$intake_group == \"Low\""
)

# Create the 4 major subset files based on intake/supplement use
for name in "${!subsets[@]}"; do
  echo "Creating subset for taxaHFE-ML: $name"
  mlr --csv filter "${subsets[$name]}" then cut -x -f supplement_taker,intake_group "$input" > "${name}_scfa_metadata.csv"
done

# For each subset, create 3 metabolite-specific files
metabolites=(acetate propionate butyrate)

# what columns to drop for each metabolite
declare -A drop_cols=(
  [acetate]="propionate,new_butyrate"
  [propionate]="acetate,new_butyrate"
  [butyrate]="acetate,propionate"
)

for subset in "${!subsets[@]}"; do
  for meta in "${metabolites[@]}"; do
    echo "Creating ${subset}_${meta}_metadata.csv"
    mlr --csv cut -x -f ${drop_cols[$meta]} "${subset}_scfa_metadata.csv" > "${subset}_${meta}_metadata.csv"
  done
done

### PART 2 for DIETML ###

# Metadata input file for dietML effort 
input="pathways_metadata.csv"

# Subset categories and filters are the same as above 
declare -A subsets=(
  [supp_use]="\$supplement_taker == \"Yes\""
  [no_supp_use]="\$supplement_taker == \"No\""
  [high_intake]="\$intake_group == \"High\""
  [low_intake]="\$intake_group == \"Low\""
)

# Create the 4 major subset files based on intake/supplement use 
for name in "${!subsets[@]}"; do
  echo "Creating subset for dietML: $name"
  mlr --csv filter "${subsets[$name]}" then cut -x -f supplement_taker,intake_group "$input" > "${name}_pathway_metadata.csv"
done

# For each subset, create 3 metabolite-specific files
metabolites=(acetate propionate butyrate)

# what columns to drop for each metabolite
declare -A drop_cols=(
  [acetate]="propionate,new_butyrate"
  [propionate]="acetate,new_butyrate"
  [butyrate]="acetate,propionate"
)

for subset in "${!subsets[@]}"; do
  for metabolite in "${metabolites[@]}"; do
    echo "Creating ${subset}_${metabolite}_pathway_metadata.csv"
    mlr --csv cut -x -f ${drop_cols[$metabolite]} "${subset}_pathway_metadata.csv" > "${subset}_${metabolite}_pathway_metadata.csv"
  done
done