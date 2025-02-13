
##### load libraries #####

library(tidyverse) # data manipulation
library(DescTools) # data filtering with %like%
library(ggplot2) # plotting 
library(patchwork) # stitching plots together 

##### read in data ######

metadata <- read.table("/Users/local-margaret/Desktop/R-projects/FL100/taxaHFE_metadata.txt", header = T)

abundance <- read_tsv("/Users/local-margaret/Desktop/R-projects/FL100/data/merged_metaphlan_v4-0-6.txt") %>%
  separate(clade_name, c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'strain'), 
           sep='\\|', extra = "merge") %>%
  # replace empty strings with NA 
  mutate(species = str_replace(species, "s__\\b", "s__NA"),
         genus = str_replace(genus, "g__\\b", "g__NA")) %>%
  # replace NA with strings 
  mutate_at(vars(phylum), ~replace_na(., "p__NA")) %>%
  mutate_at(vars(class), ~replace_na(., "c__NA")) %>%
  mutate_at(vars(order), ~replace_na(., "o__NA")) %>%
  mutate_at(vars(family), ~replace_na(., "f__NA")) %>%
  mutate_at(vars(genus), ~replace_na(., "g__NA")) %>%
  mutate_at(vars(species), ~replace_na(., "s__NA")) %>%
  gather(key = "subject_id", value = "relative_abundance", 9:338) %>%
  select(-strain) %>%
  as_tibble()


##### correlations that don't take taxa into account #####

## all participants 
metadata %>%
  select(age, sex, bmi, recent_dietary_b12_avg, habitual_dietary_b12, daily_supplemented_b12, plasma_b12, eGFR, dt_b12ad, dt_vb12) %>%
  corrr::correlate(method = "kendall") %>% corrr::autoplot() +
  scale_y_discrete(labels=c("BMI", 
                            expression(Plasma~B[12]), 
                            expression(B[12]~from~fortificants),
                            expression(Naturally~occuring~B[12]), 
                            expression(Habitual~dietary~B[12]~(sum)),
                            expression(Supplemented~B[12]), 
                            expression(Recent~B[12]~intake~(average)),
                            "Age")) +
  scale_x_discrete(labels=c(expression(Recent~B[12]~intake~(average)),
                                       expression(Supplemented~B[12]),
                                       expression(Habitual~dietary~B[12]~(sum)), 
                                       expression(Naturally~occuring~B[12]), 
                                       expression(B[12]~from~fortificants),
                                       expression(Plasma~B[12]), 
                                       "BMI",
                                       "eGFR"), position = "top")

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/correlations-without-taxa.pdf", height = 5, width = 8)

## no supplements 
metadata %>%
  filter(supplement_taker == "no") %>%
  select(age, sex, bmi, recent_dietary_b12_avg, habitual_dietary_b12, plasma_b12, eGFR, dt_b12ad, dt_vb12) %>%
  corrr::correlate(method = "kendall") %>% corrr::autoplot() 

## yes supplements 
metadata %>%
  filter(supplement_taker == "yes") %>%
  select(age, sex, bmi, recent_dietary_b12_avg, habitual_dietary_b12, daily_supplemented_b12, plasma_b12, eGFR, dt_b12ad, dt_vb12) %>%
  corrr::correlate(method = "kendall") %>% corrr::autoplot()

##### supplement non-user correlations with taxaHFE-identified taxa ##### 

# only retain taxa that taxaHFE identified as important 
selected_abundance <- merge(metadata, abundance, all.x = T, by = "subject_id") %>%
  filter(species %like% "s__Alistipes_finegoldii" |
           genus %like% "g__Intestinimonas" |
           species %like% "s__Roseburia_sp_AM16_25" |
           species %like% "s__Clostridiaceae_bacterium" |
           species %like% "s__Blautia_massiliensis" | 
           species %like% "s__Streptococcus_SGB14888" |
           genus %like% "g__GGB9062" |
           genus %like% "g__GGB3005" |
           species %like% "s__Bacteroides_finegoldii" |
           class %like% "c__CFGB1507") %>%
  filter(supplement_taker == "no") %>%
  select(subject_id, age, bmi, recent_dietary_b12_avg, habitual_dietary_b12, plasma_b12, daily_supplemented_b12, eGFR, dt_b12ad, dt_vb12, kingdom:species, relative_abundance) %>%
  filter(genus != "g__NA") %>%
  unique() %>%
  filter(relative_abundance > 0)

# list of taxa 
selected_taxa <- unique(selected_abundance$genus)

# list of continuous variables 
continuous_vars <- c("age", "plasma_b12", "bmi", 
                     "recent_dietary_b12_avg", "habitual_dietary_b12",
                     "eGFR", "dt_b12ad", "dt_vb12")

# empty list for results
correlation_results <- list()

# main loop for each var and genus 
for (var in continuous_vars) {
  for (g in selected_taxa) {
    # filter for current genus
    data_genus <- selected_abundance %>% filter(genus == g)
    
    # correlation caluculation
    cor_test <- cor.test(data_genus$relative_abundance, data_genus[[var]], method = "spearman", use = "complete.obs")
    
    n <- sum(!is.na(data_genus$relative_abundance) & !is.na(data_genus[[var]]))
    
    # extract variables 
    cor_value <- cor_test$estimate
    p_value <- cor_test$p.value
    
    # append results to list
    correlation_results <- append(correlation_results, list(tibble(genus = g, variable = var, correlation = cor_value, p_value = p_value, n = n)))
  }
}

# combine results 
correlations_df <- bind_rows(correlation_results)

# save results in a file 
# write.csv(correlations_df, "/Users/local-margaret/Desktop/R-projects/FL100/data/supplement-non-user-correlations.csv")

# only significant correlations 
plot_A <-correlations_df %>%
  filter(n > 10) %>%
  filter(p_value < 0.05) %>%
  ggplot(aes(x=genus, y=variable, fill=correlation, label=round(p_value,2))) + 
  geom_tile() +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1),name="Spearman \ncorrelation coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = ifelse(round(p_value,3) < 0.001, "**", "*")), size = 4) +
  scale_y_discrete(
    # labels start from bottom
    labels=c("Age", "BMI", expression(Daily~fortified~B[12]),
             expression(Daily~natural~B[12]),"eGFR", expression(Habitual~dietary~B[12]~sum),
             expression(Plasma~B[12]), expression(Recent~dietary~B[12]~average)))

plot_A

# ggsave(plot_A, "figures/correlation-plot-supp-non-users.pdf", height = 5, width = 8)

##### supplement user correlations ##### 

selected_abundance <- merge(metadata, abundance, all.x = T, by = "subject_id") %>%
  filter(family %like% "f__Christensenellaceae" |
           family %like% "f__Tannerellaceae" |
           species %like% "s__Ruminococcus_torques" |
           genus %like% "g__Bilophila" |
           genus %like% "g__Ruthenibacterium" |
           species %like% "s__Clostridium_sp_1001270H_150608_G6" |
           phylum %like% "p__Thaumarchaeota" |
           species %like% "s__Roseburia_sp_BX1005" |
           species %like% "s__Bacteroides_caccae") %>%
  filter(supplement_taker == "yes") %>%
  select(subject_id, age, bmi, recent_dietary_b12_avg, habitual_dietary_b12, plasma_b12, daily_supplemented_b12, eGFR, dt_b12ad, dt_vb12, kingdom:species, relative_abundance) %>%
  filter(genus != "g__NA") %>%
  unique() %>%
  filter(relative_abundance > 0)

# list of taxa 
selected_taxa <- unique(selected_abundance$genus)

# list of variables 
continuous_vars <- c("age", "plasma_b12", "bmi", 
                     "recent_dietary_b12_avg", "habitual_dietary_b12",
                     "daily_supplemented_b12", "eGFR", "dt_b12ad", "dt_vb12")

# empty list for results
correlation_results <- list()

# main loop for each var and genus 
for (var in continuous_vars) {
  for (g in selected_taxa) {
    # filter for current genus
    data_genus <- selected_abundance %>% filter(genus == g)
    
    # correlation caluculation
    cor_test <- cor.test(data_genus$relative_abundance, data_genus[[var]], method = "spearman", use = "complete.obs")
    
    n <- sum(!is.na(data_genus$relative_abundance) & !is.na(data_genus[[var]]))
    
    # extract variables 
    cor_value <- cor_test$estimate
    p_value <- cor_test$p.value
    
    # append results to list
    correlation_results <- append(correlation_results, list(tibble(genus = g, variable = var, correlation = cor_value, p_value = p_value, n = n)))
  }
}

# combine results 
correlations_df <- bind_rows(correlation_results)

# save results in a file 
# write.csv(correlations_df, "/Users/local-margaret/Desktop/R-projects/FL100/data/supplement-user-correlations.csv")

# only significant correlations
plot_B <-correlations_df %>%
  filter(n > 10) %>%
  filter(p_value < 0.05) %>%
  ggplot(aes(x=genus, y=variable, fill=correlation, label=round(p_value,2))) + 
  geom_tile() +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1),name="Spearman \ncorrelation coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(aes(label = ifelse(round(p_value,3) < 0.001, "**", "*")), size = 4) +
  scale_y_discrete(
    # labels start from bottom
    labels=c("Age", "BMI", expression(Daily~supplemented~B[12]), expression(Daily~fortified~B[12]), "eGFR", expression(Habitual~dietary~B[12]~sum), expression(Plasma~B[12]), expression(Recent~dietary~B[12]~average))
  )

plot_B

# ggsave(plot_B, "figures/correlation-plot-supp-users.pdf", height = 5, width = 8)

plot_A / plot_B +
  plot_annotation(tag_levels = 'A')

# ggsave("figures/combinded_correlation_figure.pdf", height = 10, width = 8)

View(correlations_df)

# alternative method for correlation plots

test <- merge(metadata, abundance, all.x = T, by = "subject_id") %>%
  filter(species %like% "s__Alistipes_finegoldii" |
           genus %like% "g__Intestinimonas" |
           species %like% "s__Roseburia_sp_AM16_25" |
           species %like% "s__Clostridiaceae_bacterium" |
           species %like% "s__Blautia_massiliensis" | 
           species %like% "s__Streptococcus_SGB14888" |
           genus %like% "g__GGB9062" |
           genus %like% "g__GGB3005" |
           species %like% "s__Bacteroides_finegoldii" |
           class %like% "c__CFGB1507") %>%
  filter(supplement_taker == "no") %>%
  select(subject_id, age, bmi, recent_dietary_b12_avg, habitual_dietary_b12, plasma_b12, daily_supplemented_b12, eGFR, dt_b12ad, dt_vb12, kingdom:species, relative_abundance) %>%
  filter(genus != "g__NA") %>%
  unique() %>%
  filter(relative_abundance > 0)


