#!/bin/bash

##########################################################
#
# File name: dedup_bbmap.sh
#
# Author: Margaret Hilliard on 20240926
#
# Purpose: remove exact sequence duplicates using bbmap's dedup.sh tool
#
# Usage: bash dedup_bbmap.sh
#
# Manual: https://github.com/BioInfoTools/BBMap/blob/master/docs/guides/DedupeGuide.txt
#
##########################################################

## STEP 1: set up

## set directory
workdir=/share/lemaylab-backedup/mhilliard/B12_database
cd $workdir

## load software
module load /software/modules/modulefiles_static/bbmap/38.87

## gzip the intermediate versions of your custom db and associated files
gzip database/B12_reference_db_with_eukaryotes.faa

## STEP 2: use bbmap's dedup.sh tool to remove exact matches in your amino acid sequence database
## usage: dedupe.sh in=X.fa out=Z.fa ac=f
dedupe.sh in=database/B12_reference_db_no_eukaryotes.faa out=database/deduped_B12_db_no_eukaryotes.faa amino ac=f ignorej>

## STEP 3: clean up
module purge
gzip database/B12_reference_db_no_eukaryotes.faa