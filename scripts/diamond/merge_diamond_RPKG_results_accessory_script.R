
## load libraries 
library(dplyr)
library(readr) 

## get list of csv files 
file_path <- "/Users/local-margaret/Desktop/R-projects/FL100/data/normalized_diamond_counts"  # directory path
csv_files <- list.files(file_path, pattern = "*.csv", full.names = TRUE) 

## read csv files into list 
data_list <- lapply(csv_files, read_csv) 

## make a data frame for the joined data
df <- full_join(data_list[[1]], data_list[[2]], by = "Gene")

## loop over the rest 
for (i in 3:length(data_list)) {
  df <- full_join(df, data_list[[i]])
}

## save file 
write.csv(df, "../merged-diamond-RPKG-results.csv")
