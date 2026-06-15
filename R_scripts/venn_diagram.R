
# ---- Set up ---- 

setwd("/Users/local-margaret/Desktop/VB12-analysis/")
source("scripts/get_data.R")

#install.packages(c("dplyr", "ggVennDiagram", "patchwork"))

library(dplyr)
library(ggVennDiagram)
library(patchwork)

# Split the metadata into four subsets based on dietary intake group/ supplement use
high_intake_ids <- metadata_sub %>%
  dplyr::select(subject_id, intake_group) %>%
  filter(intake_group == "High")

supp_user_ids <- metadata_sub %>%
  dplyr::select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "Yes")

low_intake_ids <- metadata_sub %>%
  dplyr::select(subject_id, intake_group) %>%
  filter(intake_group == "Low")

no_supp_ids <- metadata_sub %>%
  dplyr::select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "No")

# ---- Dietary group overlaps ---- 

# intersection in the n = 277 data set (without the fecal SCFA data)
# find overlapping IDs between high intake and supplement use 
intersecting_ids_high <- intersect(high_intake_ids$subject_id, supp_user_ids$subject_id) 
print(intersecting_ids_high) # n = 102

# find overlapping IDs between low intake and non-supplement users 
intersecting_ids_low <- intersect(low_intake_ids$subject_id, no_supp_ids$subject_id) 
print(intersecting_ids_low) # n = 118 

# intersection in the n = 240 data set (with the fecal SCFA data)

high_intake_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>%
  na.exclude() %>%
  dplyr::select(subject_id, intake_group) %>%
  filter(intake_group == "High")

supp_user_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>% 
  na.exclude() %>%
  dplyr::select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "Yes")

low_intake_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>% 
  na.exclude() %>%
  dplyr::select(subject_id, intake_group) %>%
  filter(intake_group == "Low")

no_supp_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>% 
  na.exclude() %>%
  dplyr::select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "No")

intersecting_ids_high1 <- intersect(high_intake_ids1$subject_id, supp_user_ids1$subject_id) 
print(intersecting_ids_high1) # n = 84 

intersecting_ids_low1 <- intersect(low_intake_ids1$subject_id, no_supp_ids1$subject_id) 
print(intersecting_ids_low1) # n = 103 

# ---- Plot Venn Diagrams ---- 

# intersection in the n = 277 data set (without the fecal SCFA data)
df <- list("High intake"= high_intake_ids$subject_id,
           "Adequate intake" = low_intake_ids$subject_id,
           "Supplement use" = supp_user_ids$subject_id,
           "No\nsupplements" = no_supp_ids$subject_id)

ggVennDiagram(df) + ggplot2::scale_fill_gradient(low = "#969696", high = "#e24f4a") +
  coord_cartesian(clip = "off")

# intersection in the n = 240 data set (with the fecal SCFA data)
df1 <- list("High intake"= high_intake_ids1$subject_id,
            "Adequate intake" = low_intake_ids1$subject_id,
            "Supplement use" = supp_user_ids1$subject_id,
            "No\nsupplements" = no_supp_ids1$subject_id)

ggVennDiagram(df1) + ggplot2::scale_fill_gradient(low = "#969696", high = "#e24f4a") +
  coord_cartesian(clip = "off")

# High intake and Supplement user overlaps 

df_high <- list(
  "High\nintake"= high_intake_ids$subject_id,
  "Supplement\nuse" = supp_user_ids$subject_id)

plot_a <- ggVennDiagram(df_high, 
                        label_color = "black", 
                        label_alpha = 0,
                        set_size = 0,
                        label_size = 3) + 
  #scale_fill_distiller(palette = "RdBu") +
  scale_fill_gradient(low = "white", high = "white") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = .4)) +
  annotate("text", x = -7, y = 0, label = "High\nintake", size = 4) +
  annotate("text", x = -7, y = 4, label = "Supplement\nuse", size = 4) +
  theme(legend.position = "none") +
  coord_flip() 

plot_a

df_low <- list(
  "Adequate\nintake" = low_intake_ids$subject_id,
  "No\nsupplement\nuse" = no_supp_ids$subject_id)

# Build your original plot
plot_b <- ggVennDiagram(df_low, 
                        label_color = "black", 
                        label_alpha = 0,
                        set_size = 0,
                        label_size = 3) +
  scale_fill_gradient(low = "white", high = "white") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = .4)) +
  annotate("text", x = -7, y = 0, label = "Adequate\nintake", size = 4) +
  annotate("text", x = -7, y = 4, label = "No\nsupplement\nuse", size = 4) +
  theme(legend.position = "none") +
  coord_flip() 

plot_b

# ---- Multi-panel plot ----

venn_diagrams <- plot_b + plot_a 
venn_diagrams
plot_b + plot_a +
  plot_annotation(tag_levels = 'A')

#ggsave("figures/Venn_Diagram_n=287_intake.pdf", height =6, width = 16)
