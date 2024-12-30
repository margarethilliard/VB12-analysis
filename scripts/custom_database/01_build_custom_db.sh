#!/bin/bash

##########################################################
#
# File name: build_custom_db.sh
#
# Original script: /share/lemaylab/yirui/FL100_metagenomic_analysis/script/custom_bgl_db_KEGG.sh
#
# Author: Yirui Tang on 20240108
#
# Modified by: Margaret Hilliard on 20240919
#
# Purpose: make a custom amino acid database from KEGG
#	  KEGG API user manual: https://www.kegg.jp/kegg/rest/keggapi.html
# Usage: bash build_custom_db.sh
#
##########################################################

## set directory path
WORKDIR=/share/lemaylab-backedup/mhilliard/B12_database

## and file variables
EC_gene_file=${WORKDIR}/B12_genes_with_EC_numbers.txt
KO_gene_file=${WORKDIR}/B12_genes_with_K_numbers.txt
KEGG_ID_gene_file=${WORKDIR}/B12_genes_with_KEGG_ID.txt

cd ${WORKDIR}

# STEP 1: link EC to KO
echo "Begin linking EC to KO..."

if [ -f EC_to_KO.txt ]
then
	echo "EC_to_KO.txt already exists and will not be overwritten..."
else
	echo "Start linking EC to KO..."
	# take fourth column and leave out the header
	ecID=$(cat ${EC_gene_file} | cut -f4 | awk 'NR>1')
	for value in $ecID
	do
		wget "https://rest.kegg.jp/link/ko/$value" -O - >> EC_to_KO.txt
	done
fi

echo "Linking EC to KO DONE AT: "; date "+%Y-%m-%d %H:%M:%S"

# STEP 2: link K number to geneID
echo "Begin linking K number to geneID..."

if [ -f KO_to_geneID.txt ]
then
	echo "KO_to_geneID.txt already exists and will not be overwritten..."
else
	echo "Start linking K number to geneID..."
	# take second field
	ko_IDs=$(awk -F"\\t" '{print $2}' EC_to_KO.txt)
	for value in $ko_IDs
	do
		wget "https://rest.kegg.jp/link/genes/$value" -O - >> KO_to_geneID.txt
	done

	echo "Appending existing K numbers to the KO_to_geneID.txt file..."
	# add 'ko:' preceeding each K number in the fourth field
	more_ko_IDs=$(cat ${KO_gene_file} | cut -f4 | awk 'NR>1' | sed -e 's/^/ko:/')
	for value in $more_ko_IDs
	do
		wget "https://rest.kegg.jp/link/genes/$value" -O - >> KO_to_geneID.txt
	done

fi

echo "Linking KO to geneID DONE AT: "; date "+%Y-%m-%d %H:%M:%S"

# STEP 3: get aaseq from geneID/KEGG ID
echo 'Begin getting aaseq from geneID...'

if [ -f database/01_B12_reference_db_with_eukaryotes.faa ]
then
	echo "01_B12_reference_db_with_eukaryotes.faa already exists and will not be overwritten..."
else
	echo "Start getting aaseq from geneID..."
	geneIDs=$(awk -F"\\t" '{print $2}' KO_to_geneID.txt)
	for value in $geneIDs
	do
		wget "https://rest.kegg.jp/get/$value/aaseq" -O - >> database/01_B12_reference_db_with_eukaryotes.faa
	done

	echo "Start getting aaseq from remaining KEGG IDs..."
	KEGG_IDs=$(cat ${KEGG_ID_gene_file} | cut -f4 | awk 'NR>1')
	for value in $KEGG_IDs
	do
		wget "https://rest.kegg.jp/get/$value/aaseq" -O - >> database/01_B12_reference_db_with_eukaryotes.faa
	done

fi

# remove intermediate files
# rm EC_to_KO.txt KO_to_geneID.txt

echo "Getting aaseq from geneID DONE AT: "; date "+%Y-%m-%d %H:%M:%S"