#!/bin/bash

##########################################################
#
# File name: append_missing_geneIDs.sh
#
# Author: Margaret Hilliard on 20240830
#
# Purpose: append missing geneIDs to custom KEGG db file
# using the output from 02_sanity_check.sh which found
# geneIDs that weren't downloaded via wget in build_custom_db.sh
# likely due to an "HTTP Error 403: Forbidden" error
#
# Usage: bash append_missing_geneIDs.sh
#
##########################################################

workdir=/share/lemaylab-backedup/mhilliard/B12_database
KEGG_ID_gene_file=gene_IDs_to_append.txt

cd ${workdir}

echo "Start getting aaseq from remaining KEGG IDs..."
	KEGG_IDs=$(cat ${KEGG_ID_gene_file})
	for value in $KEGG_IDs
	do
		wget "https://rest.kegg.jp/get/$value/aaseq" -O - >> database/B12_reference_db_with_eukaryotes.faa
	done

echo "Getting aaseq from geneID DONE AT: "; date "+%Y-%m-%d %H:%M:%S"

## compare input / output and expected difference lengths
## basic logic: input length + additional sequences = output length

input_n=$(cat database/01_B12_reference_db_with_eukaryotes_backup.faa | grep ">" | wc -l)
appended_geneID=$(cat gene_IDs_to_append.txt | wc -l)
output_n=$(cat database/B12_reference_db_with_eukaryotes.faa | grep ">" | wc -l)

if [ $((input_n + appended_geneID)) -eq $output_n ]; then
  echo "Input number ($input_n) plus appended number ($appended_geneID) is equal to expected output number ($output_n)"
else
  echo "Input number plus appended number is not equal to expected output number... please investigate."
fi
