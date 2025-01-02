#!/bin/bash

##########################################################
#
# File name: seq_length_distribution.sh
#
# Author: Margaret Hilliard on 20241001
#
# Purpose: get the sequence length distribution from your custom db file 
#
# Usage: bash seq_length_distribution.sh
#
##########################################################

## set directory and file vars
workdir=/share/lemaylab-backedup/mhilliard/B12_database
## software path
seqkit=/share/lemaylab/mhilliard/software/seqkit

cd ${workdir}

if [ -f ${workdir}/final_db_seq_lengths.txt ]
then
    echo "$(date) final_db_seq_lengths.txt already exists, not running seqkit's fx2tab..."
else
    echo "$(date) running seqkit's fx2tab..."
    $seqkit fx2tab --length --name --header-line database/final_db.faa >> final_db_seq_lengths.txt
fi

## compress intermediate files
# gzip database/deduped_B12_db_no_eukaryotes.faa
# gzip database/deduped_B12_db_no_eukaryotes_short_headers.faa
# gzip database/deduped_B12_db_no_eukaryotes_short_headers_filtered.faa
# gzip fasta_header_description_lookup.tsv

