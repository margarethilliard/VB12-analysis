
# ---- Set up ---- 
setwd("/Users/local-margaret/Desktop/VB12-analysis/")
library(ggplot2)
library(shapviz)
library(dplyr)
library(tidyr)
library(patchwork)

# ---- Propionate ~ pathways, low intake group ---- 
new_env <- new.env()
load(file = "data/SHAP_objects/intake_subset/regression_propionate~pathways_low_intake.rds", envir = new_env)
new_env$sv_full

shap_mat <- shapviz::get_shap_values(new_env$sv_full)
feat_mat <- get_feature_values(new_env$sv_full)

df_long <- shap_mat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "Feature", values_to = "Svalue") %>%
  left_join(
    feat_mat %>%
      as.data.frame() %>%
      mutate(id = row_number()) %>%
      pivot_longer(-id, names_to = "Feature", values_to = "Value"),
    by = c("id", "Feature"))

up_feats <- c(
  "bmi",
  "unintegrated",
  "pwy_7237_myo_chiro_and_scyllo_inositol_degradation",
  "aspasn_pwy_superpathway_of_l_aspartate_and_l_asparagine_biosynthesis",
  "pwy_7977_l_methionine_biosynthesis_iv",
  "glucose1pmetab_pwy_glucose_and_glucose_1_phosphate_degradation"
)

down_feats <- c("age",
                "pwy_5981_cdp_diacylglycerol_biosynthesis_iii", 
                "pwy_7761_nad_salvage_pathway_ii_pnc_iv_cycle",
                "pwy0_162_superpathway_of_pyrimidine_ribonucleotides_de_novo_biosynthesis")

df_sub_up <- df_long %>% filter(Feature %in% up_feats)
df_sub_down <- df_long %>% filter(Feature %in% down_feats)

df_sub_up <- df_sub_up %>%
  mutate(Feature = as.character(Feature))
df_sub_down <- df_sub_down %>%
  mutate(Feature = as.character(Feature))

df_sub_up <- df_sub_up %>%
  mutate(Tidy_Feature = dplyr::recode(Feature,
                               "bmi" = "BMI",
                               "unintegrated" = "Unintegrated",
                               "pwy_7237_myo_chiro_and_scyllo_inositol_degradation" = "pwy_7237 myo-, chiro- and\nscyllo-inositol degradation",
                               "aspasn_pwy_superpathway_of_l_aspartate_and_l_asparagine_biosynthesis" = "Superpathway of L-aspartate\nand L-asparagine biosynthesis",
                               "pwy_7977_l_methionine_biosynthesis_iv" = "pwy_7977 L-methionine\nbiosynthesis IV",
                               "glucose1pmetab_pwy_glucose_and_glucose_1_phosphate_degradation" = "glucose and glucose-1-phosphate\ndegradation")) %>%
  dplyr::filter(Feature != "bmi") %>%
  dplyr::filter(Feature != "unintegrated")

df_sub_down <- df_sub_down %>%
  mutate(Tidy_Feature = dplyr::recode(Feature,
                               "age" = "Age",
                               "pwy_5981_cdp_diacylglycerol_biosynthesis_iii" = "pwy_5981 CDP-diacylglycerol biosynthesis III",
                               "pwy_7761_nad_salvage_pathway_ii_pnc_iv_cycle" = "pwy_7761 NAD salvage pathway II\n(PNC IV cycle)",
                               "pwy0_162_superpathway_of_pyrimidine_ribonucleotides_de_novo_biosynthesis" = "Superpathway of pyrimidine ribonucleotides\nde novo biosynthesis",)) %>%
  dplyr::filter(Feature != "age")

plot_b <- ggplot(df_sub_up, aes(x = Value, y = Svalue, color = Tidy_Feature)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = F, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  #coord_cartesian(xlim = c(0, 0.5)) +
  #ylim = c(-30,50)) +
  theme_bw(base_size = 14) +
  labs(x="Copies per million",
       y=expression(atop("SHAP value",
                         "(for prediction of fecal propionate)")),
       title= expression("Top features in low "  * B[12] * " intake subset")) +

  scale_color_discrete(type = c("#f5db4c",
                                "#7e2482",
                                "#969696",
                                "#f89540")) +
  theme(legend.position = c(0.7,0.15), 
        #legend.position = "none",
        legend.background = element_rect(color = NA, fill = NA),
        legend.title = element_blank())

plot_b

# ---- Butyrate ~ pathways, no supp use group ---- 

new_env <- new.env()
load(file = "data/SHAP_objects/intake_subset/regression_butyrate~pathways_no_supp_users.rds", envir = new_env)
new_env$sv_full

shap_mat <- shapviz::get_shap_values(new_env$sv_full)
feat_mat <- get_feature_values(new_env$sv_full)

df_long <- shap_mat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "Feature", values_to = "Svalue") %>%
  left_join(
    feat_mat %>%
      as.data.frame() %>%
      mutate(id = row_number()) %>%
      pivot_longer(-id, names_to = "Feature", values_to = "Value"),
    by = c("id", "Feature"))

up_feats <- c(
  "pwy_7315_d_tdp_n_acetylthomosamine_biosynthesis",
  "pwy_6527_stachyose_degradation",
  "thisynara_pwy_superpathway_of_thiamine_diphosphate_biosynthesis_iii_eukaryotes",
  "pwy_6897_thiamine_diphosphate_salvage_ii",
  "pwy_7237_myo_chiro_and_scyllo_inositol_degradation"
)

down_feats <- c("pwy_5981_cdp_diacylglycerol_biosynthesis_iii",
                "pwy_8131_5_deoxyadenosine_degradation_ii",
                "pwy_6969_tca_cycle_v_2_oxoglutarate_synthase",
                "age",
                "pwy0_1477_ethanolamine_utilization")

df_sub_up <- df_long %>% filter(Feature %in% up_feats)
df_sub_down <- df_long %>% filter(Feature %in% down_feats)

df_sub_up <- df_sub_up %>%
  mutate(Feature = as.character(Feature))

df_sub_down <- df_sub_down %>%
  mutate(Feature = as.character(Feature))

df_sub_up <- df_sub_up %>%
  mutate(Tidy_Feature = dplyr::recode(Feature,
                               "pwy_7315_d_tdp_n_acetylthomosamine_biosynthesis" = "pwy_7315 dTDP-N-acetylthomosamine\nbiosynthesis",
                               "pwy_6527_stachyose_degradation"= "pwy_6527 stachyose degradation",
                               "thisynara_pwy_superpathway_of_thiamine_diphosphate_biosynthesis_iii_eukaryotes" = "Superpathway of thiamine diphosphate\nbiosynthesis III\n(eukaryotes)",
                               "pwy_6897_thiamine_diphosphate_salvage_ii" = "pwy_6897 thiamine diphosphate\nsalvage II",
                               "pwy_7237_myo_chiro_and_scyllo_inositol_degradation" = "pwy_7237 myo-, chiro- and\nscyllo-inositol degradation",))

df_sub_down <- df_sub_down %>%
  mutate(Tidy_Feature = dplyr::recode(Feature,
                               "pwy_5981_cdp_diacylglycerol_biosynthesis_iii" = "pwy_5981 CDP-diacylglycerol biosynthesis III",
                               "pwy_8131_5_deoxyadenosine_degradation_ii" = "pwy_8131 5'-deoxyadenosine degradation II",
                               "pwy_6969_tca_cycle_v_2_oxoglutarate_synthase" = "pwy_6969 TCA cycle V (2-oxoglutarate synthase)",
                               "age" = "Age",
                               "pwy0_1477_ethanolamine_utilization" = "pwy0_1477 ethanolamine utilization pathway")) %>%
  dplyr::filter(Feature != "age")

plot_c <- ggplot(df_sub_up, aes(x = Value, y = Svalue, color = Tidy_Feature)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = F, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  #coord_cartesian(xlim = c(0, 0.5)) +
  #ylim = c(-30,50)) +
  theme_bw(base_size = 14) +
  labs(x="Copies per million",
       y=expression(atop("SHAP value",
                         "(for prediction of fecal butyrate)")),
       title= expression("Top features in no "  * B[12] * " supplement use subset")) +
  scale_color_discrete(type = c("#cc4778",
                                "#0d0887",
                                "#7e2482",
                                "peachpuff4",
                                "#90d743")) +
  theme(legend.position = c(0.65,0.8), 
        #legend.position = "none",
        legend.background = element_rect(color = NA, fill = NA),
        legend.title = element_blank())

plot_c

# ---- Propionate ~ pathways, no supp use group ---- 

new_env <- new.env()
load(file = "data/SHAP_objects/intake_subset/regression_propionate~pathways_no_supp_users.rds", envir = new_env)
new_env$sv_full

shap_mat <- shapviz::get_shap_values(new_env$sv_full)
feat_mat <- get_feature_values(new_env$sv_full)

df_long <- shap_mat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "Feature", values_to = "Svalue") %>%
  left_join(
    feat_mat %>%
      as.data.frame() %>%
      mutate(id = row_number()) %>%
      pivot_longer(-id, names_to = "Feature", values_to = "Value"),
    by = c("id", "Feature"))

up_feats <- c(
  "bmi",
  "pwy_6527_stachyose_degradation",
  "x1cmet2_pwy_folate_transformations_iii_e_coli",
  "pwy_7237_myo_chiro_and_scyllo_inositol_degradation",
  "pwy_7977_l_methionine_biosynthesis_iv",
  "unintegrated",
  "pwy_3841_folate_transormations_ii_plants")

down_feats <- c("age",
                "pwy_5981_cdp_diacylglycerol_biosynthesis_iii",
                "pwy_5189_tetrapyrrole_biosynthesis_ii_from_glycine")

df_sub_up <- df_long %>% filter(Feature %in% up_feats)
df_sub_down <- df_long %>% filter(Feature %in% down_feats)

df_sub_up <- df_sub_up %>%
  mutate(Feature = as.character(Feature))

df_sub_down <- df_sub_down %>%
  mutate(Feature = as.character(Feature))

df_sub_up <- df_sub_up %>%
  dplyr::filter(Feature != "bmi") %>%
  dplyr::filter(Feature != "unintegrated") %>%
  mutate(Tidy_Feature = dplyr::recode(Feature,
                               "pwy_6527_stachyose_degradation" = "pwy_6527 stachyose\ndegradation",
                               "x1cmet2_pwy_folate_transformations_iii_e_coli" = "Folate transformations III",
                               "pwy_7237_myo_chiro_and_scyllo_inositol_degradation"= "pwy_7237 myo-, chiro- and\nscyllo-inositol degradation",
                               "pwy_7977_l_methionine_biosynthesis_iv" = "pwy_7977 L-methionine\nbiosynthesis IV", 
                               "pwy_3841_folate_transormations_ii_plants" = "pwy_3841 Folate Transformations II (plants)"))

                               

df_sub_down <- df_sub_down %>%
  mutate(Tidy_Feature = dplyr::recode(Feature,
                               "age" = "Age",
                               "pwy_5981_cdp_diacylglycerol_biosynthesis_iii" = "pwy_5981 CDP-diacylglycerol biosynthesis III",
                               "pwy_5189_tetrapyrrole_biosynthesis_ii_from_glycine" = "pwy_5189 tetrapyrrole biosynthesis II (from glycine)")) %>%
  dplyr::filter(Feature != "age") 

plot_d <- ggplot(df_sub_up, aes(x = Value, y = Svalue, color = Tidy_Feature)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = F, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  #coord_cartesian(xlim = c(0, 0.5)) +
  #ylim = c(-30,50)) +
  theme_bw(base_size = 14) +
  labs(x="Copies per million",
       y=expression(atop("SHAP value",
                         "(for prediction of fecal propionate)")),
       title= expression("Top features in no "  * B[12] * " supplement use subset")) +
  scale_color_discrete(type = c("black",
                                "#cc4778",
                                "#7e2482",
                                "#969696",
                                "#f89540")) +
  theme(legend.position = c(0.77,0.25), 
        #legend.position = "none",
        legend.background = element_rect(color = NA, fill = NA),
        legend.title = element_blank())

plot_d

# ---- Design multi-panel figure ---- 

# Note: plot_1_pwys object is from a different script, called "SCFA_model_performance_plots.R"

plot_1_pwys / (plot_b + plot_d + plot_c ) +
  plot_annotation(tag_levels = 'A')  

#ggsave("figures/fig7.pdf", width = 20, height = 12)
