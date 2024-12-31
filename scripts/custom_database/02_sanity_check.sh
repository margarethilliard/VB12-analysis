#!/bin/bash

##########################################################
#
# File name: sanity_check.sh
#
# Author: Margaret Hilliard on 20240830
#
# Purpose: compare the expected number of enzymes (queried via wget)
# with the number of enzymes writted to custom KEGG database
# (e.g. were their any "HTTP Error 403: Forbidden" errors?)
#
# Usage: bash sanity_check.sh
#
##########################################################

## set directory and file paths
WORKDIR=/share/lemaylab-backedup/mhilliard/B12_database

cd ${WORKDIR}

## most come from "KO_to_geneID.txt"
cat KO_to_geneID.txt | cut -f2  >> expected_geneIDs.txt

## the rest come from "B12_genes_with_KEGG_ID.txt"
cat B12_genes_with_KEGG_ID.txt | cut -f 4 | awk 'NR>1'  >> expected_geneIDs.txt

## count
num_id=$(cat expected_geneIDs.txt | wc -l)
echo "Expect ${num_id} gene IDs in the database..."

## sort the expected geneID file 
cat expected_geneIDs.txt | sort >> sorted_expected_geneIDs.txt

cat database/B12_reference_db_with_eukaryotes.faa | grep ">" | cut -f1 -d ' ' | sed 's/>//g' | sort >> database_geneIDs.txt

## count
num_id_db=$(cat database_geneIDs.txt | wc -l)
echo "There are ${num_id_db} gene IDs in the database..."

## compare outcome
echo "the differences are printed in 'difference_in_geneIDs.txt'"
diff -u database_geneIDs.txt sorted_expected_geneIDs.txt >> difference_in_geneIDs.txt

## made a 'to do' list for appending the differences
cat difference_in_geneIDs.txt | grep "^+" | sed 's/^+//' | awk 'NR>1' | sort | uniq > gene_IDs_to_append.txt

## remove intermediate files
rm expected_geneIDs.txt database_geneIDs.txt sorted_expected_geneIDs.txt difference_in_geneIDs.txt