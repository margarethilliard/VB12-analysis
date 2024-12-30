# Diet and Microbiome Predictors of Plasma Vitamin B12 in Healthy U.S. Adults
Scripts used in the analysis of vitamin B12 data for the USDA Nutritional Phenotyping Study

## STEP 1: Download required software 
- R 4.2.2 (or newer)
- RStudio 'version' (or newer)
- Python 3.9.13
- bbmap 
- TaxaHFE version 2.0
- dietML

## STEP 2: create the custom prokaryotic vitamin B12 amino acid database (optional)
- Scripts to reproduce this step of the workflow are located in "~/scripts/custom_database/"
- Alternatively, you can download a copy of the database from this repository. 

## STEP 3: use DIAMOND to find your sequences of interest in fecal metagenomes 
- Scripts to reproduce this step of the workflow are located in "~/scripts/diamond/"

## STEP 4: use read2contig.sh to map DIAMOND reads to contigs, then assign taxonomy using sourmash 
- Scripts to reproduce this step of the workflow are located in "~/scripts/read2contig/"

## STEP 5: compute B12 sythesis and utilization scores in R 
- Scripts to reproduce this step of the workflow are located in "~/scripts/VB12_scores/"

# Computing Environments
- Steps 2-4 were run on Spitfire, a slurm-based HPC cluster managed by the UC Davis Genome Center 
- Step 5-6 were run locally in R on a MacBook Pro using a Apple M3 Max chip 