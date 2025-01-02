#!/home/mhilliard/.conda/envs/env/bin/python

##########################################################
#
# File name: make_description_lookup_file.py
#
# Author: Margaret Hilliard on 20240926
#
# Purpose: read from a multi-fasta file, write short and long 
# versions of the fasta headers to tsv file for downstream use 
#
# Usage: python make_description_lookup_file.py
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

input_file = "database/deduped_B12_db_no_eukaryotes.faa" 
output_file = "fasta_header_description_lookup.tsv"

with open(output_file, "w") as outfile:
  for record in SeqIO.parse(input_file, "fasta"):
      # define the elements you want in your fasta file header
      org_code = record.description.split()[0] # splits the description into a list based on spaces and takes the first element
      accession = record.description.split()[1] # second element
      k_no = record.description.split()[2] # third element
      # this is a description field that contains spaces and a
      # variable number of words and symbols after a pipe
      if "|" in record.description:
          after_pipe = record.description.split("|", 1)[1]  # split at the first "|" and take the second part
      # this is another description that starts after the fourth field
      # as defined by a space delimiter and ends at the pipe
      parts = record.description.split() # splits description fields by space delimiter
      if len(parts) >= 4:
          temp = " ".join(parts[3:])  # join everything from the fourth field onwards
          fourth_to_pipe = temp.split("|")[0]  # split at the first "|" and take the first part
      new_description = f"{org_code}{accession}|{fourth_to_pipe}|{after_pipe}"  # reorder elements
      record.description = new_description # assign new description to the record
      record.id = k_no # reassign ko_no to record.id
      # this part gets rid of leading and trailing spaces that are artifacts
      # of the original fasta headers
      parts = record.description.split("|", 3)  # split into 3 parts at most
      if len(parts) > 2:
          parts[1] = parts[1].rstrip() # remove trailing spaces from the second part
          parts[2] = parts[2].strip() # remove leading/trailing spaces from the third part
          record.description = "|".join(parts)  # join back with "|"
      parts = record.description.split("|") # split the description by pipes
      if len(parts) >= 3: # check there are at least three parts
          match = re.search(r"(Cb\w+|Btu\w+)", parts[2], re.IGNORECASE)  # search for "Cb" or "Btu" followed by word characters
          if match:
              string_match = match.group(0)
              record.id = string_match  # reassign record.id
      parts = record.description.split("|")
      if len(parts) >= 2 and parts[1] == "KO assigned":
          parts[1] = "no KO assigned"
          record.description = "|".join(parts)
      # write a look up file that has the short and
      # long description in tab delimited format
      org_code_reformat = org_code.replace(":", "") # take the ':' out of the taxa code
      short_description = f"{record.id}-{org_code_reformat}-{accession}"
      
      print(f"{short_description}\t{record.id} {record.description}", file=outfile)
