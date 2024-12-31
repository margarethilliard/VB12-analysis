#!/home/mhilliard/.conda/envs/env/bin/python

##########################################################
#
# File name: parse_multifasta_short_header.py
#
# Author: Margaret Hilliard on 20240926
#
# Purpose: read from and write to multi-fasta headers for downstream
#          interpretability using Biopython's SeqIO module
#
# Usage: python parse_multifasta_short_header.py
#
##########################################################

# run once to create initial conda environement:
# module load anaconda3/4.12.0
# conda create --name env

# run everytime you use the script:
# source ~/.bashrc
# conda activate env

# run once to install biopython:
# conda install biopython

# navigate to database directory 
# cd /share/lemaylab-backedup/mhilliard/B12_database/database

import re
from Bio import SeqIO

input_file = "deduped_B12_db_no_eukaryotes.faa"
output_file = "deduped_B12_db_no_eukaryotes_modified_short_headers.faa" # desired output file name

with open(output_file, "w") as out_handle: # makes the output file writable
  for record in SeqIO.parse(input_file, "fasta"):
      # define the elements you want in your fasta file header
      org_code = record.description.split()[0] # splits the description into a list based on spaces and takes the first element
      org_code = org_code.replace(":", "") # take the ':' out of the taxa code
      accession = record.description.split()[1] # second element
      k_no = record.description.split()[2] # third element
      new_id = f"{k_no}-{org_code}-{accession}"  # reorder elements
      record.id = new_id # reassign record.id
      # if "no" is in the second field of the record.id (no KO assigned)
      # search the last field of the record description for
      # a string match to "Cb" followed by word characters
      id_parts = record.id.split("-") # three parts of record id
      desc_parts = record.description.split("|") # two parts of record description
      if len(id_parts) >= 2 and id_parts[0] == "no":
        match = re.search(r"Cb\w+", desc_parts[1])
        if match:
          id_parts[0] = match.group(0)
          record.id = "-".join(id_parts)
      # do the same regex search for BtuB/BtuM
      # but make sure it's not case senstive
      if len(id_parts) >= 2 and id_parts[0] == "no":
        match = re.search(r"Btu\w+", desc_parts[1], re.IGNORECASE)
        if match:
          id_parts[0] = match.group(0)
          record.id = "-".join(id_parts)
      record.description = "" # assigning to an empty string to not print in fasta
      SeqIO.write(record, out_handle, "fasta")
