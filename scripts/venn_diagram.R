
# ---- Set up ---- 
setwd("/Users/local-margaret/Desktop/VB12-analysis/")
source("scripts/get_data.R")

#install.packages(c("dplyr", "ggVennDiagram", "patchwork"))

library(dplyr)
library(ggVennDiagram)
library(patchwork)

# split the metadata into four based on dietary intake group/ supplement use
high_intake_ids <- metadata_sub %>%
  select(subject_id, intake_group) %>%
  filter(intake_group == "High")

supp_user_ids <- metadata_sub %>%
  select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "Yes")

low_intake_ids <- metadata_sub %>%
  select(subject_id, intake_group) %>%
  filter(intake_group == "Low")

no_supp_ids <- metadata_sub %>%
  select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "No")

# ---- Dietary group overlaps ---- 

## intersection in the n = 287 data set (without the fecal SCFA data)
# find overlapping IDs between high intake and supplement use 
intersecting_ids_high <- intersect(high_intake_ids$subject_id, supp_user_ids$subject_id) 
print(intersecting_ids_high) # n = 104 (146 entries x 121 entries) 

# find overlapping IDs between low intake and non-supplement users 
intersecting_ids_low <- intersect(low_intake_ids$subject_id, no_supp_ids$subject_id) 
print(intersecting_ids_low) # n = 124 (141 entries x 166 entries)

## intersection in the n = 247 data set (with the fecal SCFA data)

high_intake_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>%
  na.exclude() %>%
  select(subject_id, intake_group) %>%
  filter(intake_group == "High")

supp_user_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>% 
  na.exclude() %>%
  select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "Yes")

low_intake_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>% 
  na.exclude() %>%
  select(subject_id, intake_group) %>%
  filter(intake_group == "Low")

no_supp_ids1 <- left_join(metadata_sub, scfa, by = "subject_id") %>% 
  na.exclude() %>%
  select(subject_id, supplement_taker) %>%
  filter(supplement_taker == "No")

intersecting_ids_high1 <- intersect(high_intake_ids1$subject_id, supp_user_ids1$subject_id) 
print(intersecting_ids_high1) # n = 86 (126 entries x 101 entries) 

intersecting_ids_low1 <- intersect(low_intake_ids1$subject_id, no_supp_ids1$subject_id) 
print(intersecting_ids_low1) # n = 106 (121 entries x 146 entries) 

# ---- Venn Diagrams ---- 
## intersection in the n = 287 data set (without the fecal SCFA data)
df <- list("High intake"= high_intake_ids$subject_id,
           "Low intake" = low_intake_ids$subject_id,
           "Supplement use" = supp_user_ids$subject_id,
           "No\nsupplements" = no_supp_ids$subject_id)

ggVennDiagram(df) + ggplot2::scale_fill_gradient(low = "blue", high = "red") +
  coord_cartesian(clip = "off")

# ggsave("figures/Venn_Diagram_n=287.pdf", height = 5, width = 10)

## intersection in the n = 247 data set (with the fecal SCFA data)
df1 <- list("High intake"= high_intake_ids1$subject_id,
            "Low intake" = low_intake_ids1$subject_id,
            "Supplement use" = supp_user_ids1$subject_id,
            "No\nsupplements" = no_supp_ids1$subject_id)

ggVennDiagram(df1) + ggplot2::scale_fill_gradient(low = "blue", high = "red") +
  coord_cartesian(clip = "off")

#ggsave("figures/Venn_Diagram_n=247.pdf", height = 5, width = 10)

# High intake and Supplement user overlaps 

df_high <- list(
  "High\nintake"= high_intake_ids$subject_id,
  "Supplement\nuse" = supp_user_ids$subject_id
)

plot_a <- ggVennDiagram(df_high, label_color = "white", label_alpha = 0) + scale_fill_distiller(palette = "RdBu") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = .4)) 

plot_a
#ggsave("figures/Venn_Diagram_n=287_high_intake_only.pdf", height = 8, width = 8)

df_low <- list(
  "Low\nintake" = low_intake_ids$subject_id,
  "No\nsupplement\nuse" = no_supp_ids$subject_id
)

# Build your original plot
plot_b <- ggVennDiagram(df_low, label_color = "white", label_alpha = 0) +
  scale_fill_distiller(palette = "RdBu") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = .4)) +
  theme(legend.position = "none")

plot_b

#ggsave("figures/Venn_Diagram_n=287_low_intake_only.pdf", height = 8, width = 8)

# ---- Multi-panel plot ----
plot_b + plot_a +
  plot_annotation(tag_levels = 'A')

#ggsave("figures/Venn_Diagram_n=287_intake.pdf", height =6, width = 16)

