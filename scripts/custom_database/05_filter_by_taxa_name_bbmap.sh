#!/bin/bash

##########################################################
#
# File name: filter_by_taxa_name_bbmap.sh
#
# Author: Margaret Hilliard on 20240917
#
# Purpose: remove Eukaryotic organisms from a custom KEGG
# database after defining the taxa codes in the database
#
# Usage: bash filter_by_taxa_name_bbmap.sh
#
##########################################################

# set directory and file vars
WORKDIR=/share/lemaylab-backedup/mhilliard/B12_database/
DB_name=database/B12_reference_db_with_eukaryotes.faa # relative to workdir

cd ${WORKDIR}

# make list of taxa to exclude or keep from 02_get_uniq_orgs_and_taxonomy_from_db.sh output file
# specifically, put a ':' after each taxa code to encorage exact matches using bbmap's filterbyname.sh tool
cat db_taxa_with_eukaryotes.txt | grep "Eukaryotes" | cut -f1 | sed s/$/:/ | sort | uniq >> eukaryotic-taxa.txt

# similarly, add a space after each taxa code in the amino acid database
# use substring=f argument in bbmap to discourage erroneous (substring) matches
cat ${DB_name} | sed 's/:/: /g' > database/B12_reference_db_with_eukaryotes_and_spaced_headers.faa

# load software
module load /software/modules/modulefiles_static/bbmap/38.87

# usage: filterbyname.sh in=reads.fq out=filtered.fq names=names.txt substring=f include=f
filterbyname.sh in=database/B12_reference_db_with_eukaryotes_and_spaced_headers.faa out=database/B12_reference_db_no_eukaryotes.faa names=eukaryotic-taxa.txt substring=f include=f ignorejunk

# remove the intermediate files
rm eukaryotic-taxa.txt database/B12_reference_db_with_eukaryotes_and_spaced_headers.faa
#db_taxa_with_eukaryotes.txt