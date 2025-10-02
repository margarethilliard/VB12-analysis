______________
### **Summary**
___________________

This repository contains scripts for analyses presented in the following study (submitted for publication):

-   **Hilliard, M.A**, Oliver, A., Wilson, S.M.G., Shahab-Ferdows, S., Hampel, D., Stephensen C.B., Bennett, B.J.,  Allen, L.A., Lemay D.G. (2025). Diet and Microbiome Predictors of Plasma Vitamin B12 status in Healthy US Adults. 

______________
### **Data Availibility**
___________________

- Metagenomes are deposited in NCBI Sequence Read Archive (SRA) under the [study accession SRP354271](https://dataview.ncbi.nlm.nih.gov/object/PRJNA795985) and [BioProject PRJNA1090850](https://www.ncbi.nlm.nih.gov/bioproject/?term=PRJNA1090850). 
- Requests for non-metagenomic data from the USDA-ARS WHNRC Nutritional Phenotyping Study used in this analysis should be made via email to the senior author. Requests will be reviewed quarterly by a committee consisting of the study investigators.

____________
### **Tools and software**
_______________

- R v4.4.3 & RStudio v2024.12.1+563
- [TaxaHFE-ML](https://github.com/aoliver44/taxaHFE) & [dietML](https://github.com/aoliver44/nutrition_tools)
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
- Machine learning analyses were run on HIVE, a slurm-based HPC cluster managed by the High Performance Computing Core Facility at the University of California Davis.
- Metagenomic sequence pre-processing steps were run previously on Spitfire, a slurm-based HPC cluster managed by the University of California Davis Genome Center. Code for metagenomic sequencing pre-processing can be found [here](https://github.com/dglemay/ARG_metagenome).
