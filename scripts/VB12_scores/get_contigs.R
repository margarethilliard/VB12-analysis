
# function to get contig info from read2contig results file 
get_contigs <- function(i, species_name) {
  contig_info <- full_taxonomic_results %>%
    filter(subject_id == i, species == species_name) %>%
    select(subject_id, species, contig_name, contig_len)
}
      
# empty list for results 
contig_results <- list()

# subject id list 
subjects <- unique(orphan_contigs$subject)

for (i in subjects) { 
  
  # isolate species in a subject 
  species_groups <- orphan_contigs %>%
    filter(subject == i) %>%
    group_by(species) %>%
    group_split()
 
  # process each species group
  for (s in 1:length(species_groups)) { 
      species_name <- species_groups[[s]]$species[1] # single species name with in a subject 
      
      # find the match in sourmash results file  
      contig_info <- get_contigs(i, species_name)
      
      # append result to list 
      contig_results <- append(contig_results, list(contig_info))
      
  }
  print(paste("Finished processing subject id:", i))
}

# bind results
cumulative_contig_results <- bind_rows(contig_results)

cumulative_contig_results <- unique(cumulative_contig_results)
cumulative_contig_results$contig_name <- as.factor(cumulative_contig_results$contig_name)


cumulative_contig_results %>%
  select(species) %>%
  unique() %>%
  arrange(by = species) %>%
  View()

# write results to a file 
write.table(cumulative_contig_results, file = "/Users/local-margaret/Desktop/contigs.txt", 
            append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)
