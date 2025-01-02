#!/home/mhilliard/.conda/envs/env/bin/python

##########################################################
#
# File name: filter_fasta_by_header.py
#
# Author: Margaret Hilliard on 20250102
#
# Purpose: filters FASTA records based on a regular expression pattern in the header 
# and writes the filtered records to an output file using Biopython's SeqIO module
#
# Usage: python filter_fasta_by_header.py
#
##########################################################

# run once to create initial conda environement:
# module load anaconda3/4.12.0
# conda create --name <my-env>

# run once to install biopython:
# conda install biopython

# run everytime you use the script:
# source ~/.bashrc
# conda activate metagenome

import re
from Bio import SeqIO

def filter_fasta(fasta_file, header_pattern, output_file):
    """
    Filters FASTA records based on a regular expression pattern 
    in the header and writes the filtered records to an output file.

    Args:
        fasta_file: Path to the input FASTA file.
        header_pattern: The regular expression pattern to search for in the header.
        output_file: Path to the output FASTA file.
    """
    filtered_records = []
    with open(fasta_file, "r") as handle, open(output_file, "w") as out_handle:
        for record in SeqIO.parse(handle, "fasta"):
            # if the header does not match the specified regex pattern, keep the record
            if not re.match(header_pattern, record.id):  
                SeqIO.write(record, out_handle, "fasta")

# example usage:
fasta_file = "database/deduped_B12_db_no_eukaryotes_short_headers.faa"  
header_pattern = r"^no-.*"  # pattern to match entries starting with "no-"
output_file = "database/deduped_B12_db_no_eukaryotes_short_headers_filtered.faa"

filter_fasta(fasta_file, header_pattern, output_file)
