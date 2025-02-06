## Diet and Microbiome Predictors of Plasma Vitamin B12 in Healthy U.S. Adults
Scripts used in the analysis of vitamin B12 data for the USDA Nutritional Phenotyping Study

### Step 1: Download required software
- [R 4.2](https://www.r-project.org/) (or newer)
- [RStudio](https://posit.co/download/rstudio-desktop/) '2024.04.2+764' (or newer)
- [TaxaHFE 2.2](https://github.com/aoliver44/taxaHFE) (currently using the dev branch)
- [dietML](https://github.com/aoliver44/nutrition_tools) 
- [wget](https://www.gnu.org/software/wget/)
- [DIAMOND](https://github.com/bbuchfink/diamond)
- [BBMap](https://sourceforge.net/projects/bbmap/)
- [Python 3.9.13](https://www.anaconda.com/download)
- [seqkit](https://github.com/shenwei356/seqkit) (optional)
- [parallel](https://www.gnu.org/software/parallel/man.html) (optional)

### Step 2: Clean the data, check assumptions, transform variables, explore trends, make comparisons, etc.
- Scripts to reproduce this step are located in "scripts/basic_data_exploration/"

### Step 3: Look for dietary and microbial predictors of vitamin B12 using taxaHFE/dietML
- Scripts to reproduce this step are located in "scripts/machine_learning"

### Step 4: Create the custom prokaryotic vitamin B12 amino acid database (optional)
- Scripts to reproduce this step are located in "scripts/custom_database/"
- Alternatively, a copy of the database is located in "scripts/custom_database/database/final_db.faa.gz"

### Step 5: Use DIAMOND to map your sequences of interest to merged fecal metagenomic reads
- Scripts to reproduce this step are located in "scripts/diamond/"

### Step 6: Use read2contig.sh to map DIAMOND reads to contigs, then assign taxonomy using sourmash 
- Scripts to reproduce this step are located in "scripts/read2contig/"

### Step 7: Compute B12 sythesis and utilization scores in R 
- Scripts to reproduce this step are located in "scripts/VB12_scores/"

### A note on computing environments
- Steps written in Shell or Python were run remotely on Spitfire, a slurm-based HPC cluster managed by the UC Davis Genome Center 
- Steps written in R were run locally on a MacBook Pro (Apple M3 Max chip)