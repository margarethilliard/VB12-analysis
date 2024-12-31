#!/bin/bash

##########################################################
#
# File name: get_taxa_from_db.sh
#
# Author: Margaret Hilliard on 20240830
#
# Purpose: list unique organisms and their higher level taxonomic rank from a custom KEGG database
#		KEGG API user manual: https://www.kegg.jp/kegg/rest/keggapi.html
# 		KEGG Organisms: https://rest.kegg.jp/list/organism
#
# Usage: bash get_taxa_from_db.sh
#
##########################################################

## set directory and file paths
WORKDIR=/share/lemaylab-backedup/mhilliard/B12_database
DB=${WORKDIR}/database/B12_reference_db_with_eukaryotes.faa

cd ${WORKDIR}

echo 'Getting taxonomic info from KEGG API...'

if [ -f kegg_orgs.txt ]
then
	echo kegg_orgs.txt already exists and will not be overwritten...
else
	wget "https://rest.kegg.jp/list/organism" -O - >> kegg_orgs.txt
fi

if [ -f db_taxa_with_eukaryotes.txt ]
then
	echo db_taxa_with_eukaryotes.txt already exists and will not be overwritten...
else
        echo 'Getting taxonomy for your orgs... '
	orgs=$(grep ">" ${DB} | cut -f 1 -d ":" | sed -e 's/>//g' | uniq)
	for org in $orgs
	do
		cat kegg_orgs.txt | cut -f 2,3,4 | awk -v value="${org}" '$1==value' >> db_taxa_with_eukaryotes.txt

	done
fi

echo "Finished getting taxonomy at: "; date "+%Y-%m-%d %H:%M:%S"

numb_orgs=$(cat db_taxa_with_eukaryotes.txt | wc -l)
rm kegg_orgs.txt

echo "Found ${numb_orgs} unique orgs in your custom db!"