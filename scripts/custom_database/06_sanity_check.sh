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
WORKDIR=/share/lemaylab-backedup/mhilliard/B12_database/
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
        echo 'Getting taxonomy for your prokaryotic orgs... '
	orgs=$(grep ">" database/B12_reference_db_no_eukaryotes.faa | cut -f 1 -d ":" | sed -e 's/>//g' | uniq)
	for org in $orgs
	do
		cat kegg_orgs.txt | cut -f 2,3,4 | awk -v value="${org}" '$1==value' >> db_taxa_no_eukaryotes.txt

	done
fi

if [ -f db_taxa_with_eukaryotes.txt ]
then
	echo db_taxa_with_eukaryotes.txt already exists and will not be overwritten...
else
        echo 'Getting taxonomy for your eukaryotic orgs... '
	orgs=$(grep ">" database/B12_reference_db_with_eukaryotes.faa | cut -f 1 -d ":" | sed -e 's/>//g' | uniq)
	for org in $orgs
	do
		cat kegg_orgs.txt | cut -f 2,3,4 | awk -v value="${org}" '$1==value' >> db_taxa_with_eukaryotes.txt

	done
fi

echo "Finished getting taxonomy at: "; date "+%Y-%m-%d %H:%M:%S"

numb_orgs=$(cat db_taxa_no_eukaryotes.txt | wc -l)

## basic logic is:
## db_taxa_with_eukaryotes.txt - number eukaryotes in db = db_taxa_no_eukaryotes.txt


## get eukaryote N
cat db_taxa_with_eukaryotes.txt | grep "Eukaryotes" | cut -f1 | sed s/$/:/ | sort | uniq >> eukaryotic-taxa.txt
eukaryotes_N=$(cat eukaryotic-taxa.txt | sort | uniq | wc -l)

## get total taxa N
total_N=$(cat db_taxa_with_eukaryotes.txt | cut -f1 | sort | uniq | wc -l)

## get !eukaryote N (prokaryotes + archaea)
not_eukaryotes_N=$(cat db_taxa_no_eukaryotes.txt| cut -f1 | sort | uniq | wc -l)

if [ $((eukaryotes_N + not_eukaryotes_N)) -eq $total_N ]; then
  echo "Number of eukaryotic taxa ($eukaryotes_N) plus prokaryotic taxa ($not_eukaryotes_N) is equal to expected output number ($total_N)"
else
  echo "Number of eukaryotic taxa plus prokaryotic taxa does not equal the expected total number of taxa... please investigate."
fi

## remove intermediate files
# rm eukaryotic-taxa.txt db_taxa_no_eukaryotes.txt db_taxa_with_eukaryotes.txt kegg_orgs.txt