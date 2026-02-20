______________
### **Summary**
___________________

This repository contains scripts for analyses presented in the following study (submission pending):

-   **Hilliard, M.A**, Oliver, A., Wilson, S.M.G., Shahab-Ferdows, S., Hampel, D., Bennett, B.J.,  Allen, L.A., Lemay D.G. (2026). Higher dietary vitamin B12 linked to lower fecal short-chain fatty acids and changes in gut microbial function in healthy United States adults. 

______________
### **Data availibility**
___________________

- Metagenomes are deposited in NCBI Sequence Read Archive (SRA) under the BioProject accession numbers [PRJNA795985](https://www.ncbi.nlm.nih.gov/bioproject/795985) and [PRJNA1090850](https://www.ncbi.nlm.nih.gov/bioproject/?term=PRJNA1090850). 
- Requests for non-metagenomic data from the USDA-ARS WHNRC Nutritional Phenotyping Study used in this analysis should be made via email to the senior author. Requests will be reviewed quarterly by a committee consisting of the study investigators.

____________
### **Tools and software**
_______________

- R v4.4.3 & RStudio v2024.12.1+563
- [TaxaHFE-ML](https://github.com/aoliver44/taxaHFE) & [DietML](https://github.com/aoliver44/nutrition_tools)
- Apptainer v1.4.3
- GNU Parallel v20220522 (optional)
- Conda v25.1.0
- Bowtie2 v2.5.2 
- Samtools v1.19.2
- anvi'o v8
- MetaBAT 2 v2.18
- CheckM2 v1.1.0

____________
### **Computing environments**
_______________

- R analyses and visualizations were run locally on a MacBook Pro (Apple M3 Max chip).
- Machine learning analyses and MAG reconstruction were run on HIVE, a slurm-based HPC cluster managed by the High Performance Computing Core Facility at the University of California Davis.
- Metagenomic sequence pre-processing steps were run previously on Spitfire, a slurm-based HPC cluster managed by the University of California Davis Genome Center. Code for metagenomic sequencing pre-processing can be found [here](https://github.com/dglemay/ARG_metagenome).
