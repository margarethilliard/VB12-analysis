#!/bin/bash

##########################################################
#
# File name: sanity_check.sh
#
# Author: Margaret Hilliard on 20240830
#
# Purpose: compare the expected number of taxa (queried via wget)
# with the number of taxa writted to custom KEGG database
# (e.g. did bbmap's filter tool hit exact matches only??)
#
# Usage: bash sanity_check.sh
#
##########################################################

## set directory and file paths
WORKDIR=/share/lemaylab-backedup/mhilliard/B12_database

cd ${WORKDIR}

echo 'Getting taxonomic info from KEGG API...'

if [ -f kegg_orgs.txt ]
then
	echo kegg_orgs.txt already exists and will not be overwritten...
else
	wget "https://rest.kegg.jp/list/organism" -O - >> kegg_orgs.txt
fi

if [ -f db_taxa_no_eukaryotes.txt ]
then
	echo db_taxa_no_eukaryotes.txt already exists and will not be overwritten...
else
        echo 'Getting taxonomy for your orgs... '
	orgs=$(grep ">" ${DB} | cut -f 1 -d ":" | sed -e 's/>//g' | uniq)
	for org in $orgs
	do
		cat kegg_orgs.txt | cut -f 2,3,4 | awk -v value="${org}" '$1==value' >> db_taxa_no_eukaryotes.txt

	done
fi

echo "Finished getting taxonomy at: "; date "+%Y-%m-%d %H:%M:%S"

numb_orgs=$(cat db_taxa_no_eukaryotes.txt | wc -l)

## basic logic is:
## db w/ eukaryotes - eukaryotes = db w/out eukaryotes

## get eukaryote N
cat db_taxa_with_eukaryotes.txt | grep "Eukaryotes" | cut -f1 | sed s/$/:/ | sort | uniq >> eukaryotic-taxa.txt
eukaryotes_N=$(cat eukaryotic-taxa.txt | sort | uniq | wc -l)

## get total taxa N
total_N=$(cat db_taxa_with_eukaryotes.txt | cut -f1 | sort | uniq | wc -l)

## get !eukaryote N (prokaryotes + archaea)
not_eukaryotes_N=$(cat db_taxa_no_eukaryotes.txt| cut -f1 | sort | uniq | wc -l)

echo " ${total_N} should = ${eukaryotes_N} + ${not_eukaryotes_N} -- does it?? "

## remove intermediate files
rm eukaryotic-taxa.txt db_taxa_no_eukaryotes.txt db_taxa_with_eukaryotes.txt kegg_orgs.txt