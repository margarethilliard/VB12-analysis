#!/bin/bash

##########################################################
#
# File name: filter_ag_vg_bbmap.sh
#
# Author: Margaret Hilliard on 20240917
#
# Purpose: remove viral and addendum category
# peptides from a custom KEGG database

# Usage: bash filter_by_name_bbmap.sh
#
##########################################################

# set directory and file vars
workdir=/share/lemaylab-backedup/mhilliard/B12_database
db_name=database/deduped_B12_db_no_eukaryotes_short_headers_filtered.faa # relative to workdir

cd ${workdir}

# make list of taxa to exclude or keep
cat $db_name | grep ">" | cut -f2 -d '-' | sed 's/^/-/g' | sed 's/$/-/g' | grep -e "-vg-" | uniq >> addendum_and_viral.txt
cat $db_name | grep ">" | cut -f2 -d '-' | sed 's/^/-/g' | sed 's/$/-/g' | grep -e "-ag-" | uniq >> addendum_and_viral.txt

# load software
module load /software/modules/modulefiles_static/bbmap/38.87

# usage: filterbyname.sh in=reads.fq out=filtered.fq names=names.txt substring=f include=f
# need to use substring=t since there are no spaces 
filterbyname.sh in=$db_name out=database/final_db.faa substring=t include=f names=addendum_and_viral.txt ignorejunk

# remove the intermediate files
rm addendum_and_viral.txt