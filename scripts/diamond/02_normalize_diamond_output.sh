#!/bin/bash

##########################################################
#
# File name: normalize_diamond_output.sh
#
# Author: Margaret Hilliard on 20250113
#
# Purpose: normalize diamond hits to custom db by gene length then  
# create various collapsed versions of the result file for downstream use
#
# Usage: bash normalize_diamond_output.sh
#
##########################################################

## set directory and file paths
workdir=/share/lemaylab-backedup/mhilliard/FL100/diamond
db=/share/lemaylab-backedup/mhilliard/B12_database/database/final_db.faa
seqkit=/share/lemaylab/mhilliard/software/seqkit

cd $workdir 

## get custom database sequence lengths  
$seqkit fx2tab --length --name $db >> database/B12_db_aa_lengths.txt
## replace the tab delim with space 
cat database/B12_db_aa_lengths.txt | sed 's/\t/ /g' >> database/B12_db_aa_lengths_space_delim.txt

## count occurrences of hits 
mkdir -p gene_counts

source ~/.bashrc
conda activate metagenome

while read subject frags mc; do
python3 custom_db_analysis_counter.py -I diamond_output/${subject}.txt -O gene_counts/${subject}_org_by_gene.csv
done < B12_FL100_paths.txt

## get normalized occurrence count 
mkdir -p normalized_counts

while read subject frags mc; do
python3 make_CAZy_normtab.py \
--mc $mc \
--genelen database/B12_db_aa_lengths_space_delim.txt \
--count gene_counts/${subject}_org_by_gene.csv \
--out normalized_counts/${subject}.csv
done < B12_FL100_paths.txt

## merge the normalized count tables 
python3 make_CAZy_normtab.py \
--mergeout merged_RPKG_diamond_result.csv \
--mergein normalized_counts/*.csv

## collapse normalized count table by enzyme 
python3 collapse_by_enzyme_abundance.py \
merged_RPKG_diamond_result.csv \
merged_RPKG_diamond_result_by_enzyme.csv

conda deactivate 