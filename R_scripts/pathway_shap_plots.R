
# ---- Set up ---- 

setwd("/Users/local-margaret/Desktop/VB12-analysis/")
library(ggplot2)
library(shapviz)
library(dplyr)
library(tidyr)
library(patchwork)

# ---- Propionate ~ pathways, no supp use group ---- 

new_env <- new.env()
load(file = "data/intake_subset_analyses/no_supp_use/pathways/propionate/shap/ml_resultsML_r_workspace.rds", envir = new_env)
new_env$sv_full

p <- shapviz::sv_importance(new_env$sv_full, kind = "bee", show_numbers = TRUE, bee_width = 0.2, max_display = 10)
# modify the geom_text and geom_point layers to change size
p$layers[[3]]$aes_params$size <- 5  # text size 
p$layers[[2]]$aes_params$size <- 3  # point size

propionate_plot_main_text <- p +
  labs(x = expression("predictive of lower propionate < SHAP value > predictive of higher propionate"),
       title = expression("Top features in no "  * B[12] * " supplement use subset")) +
  theme_bw(base_size = 16) +
  scale_y_discrete(labels = rev(c(
    "age" = "Age",
    "pwy_5981_cdp_diacylglycerol_biosynthesis_iii" = "pwy_5981: CDP-diacylglycerol biosynthesis III",
    "bmi" = "BMI",
    "pwy_6292_superpathway_of_l_cysteine_biosynthesis_mammalian" = "Superpathway of mammalian L-cysteine biosynthesis",
    "pwy_5189_tetrapyrrole_biosynthesis_ii_from_glycine" = "pwy_5189: Tetrapyrrole biosynthesis II from glycine",
    "pwy_6527_stachyose_degradation" = "pwy_6527: Stachyose degradation",
    "pwy_3841_folate_transformations_ii_plants" = "pwy_3841: Folate transformations II (plants)",
    "x1cmet2_pwy_folate_transformations_iii_e_coli" = "1CMET2_pwy: Folate transformations III",
    "pwy_6936_seleno_amino_acid_biosynthesis_plants" = "pwy_6936: Seleno-amino acid biosynthesis (plants)", 
    "pwy_7977_l_methionine_biosynthesis_iv" = "pwy_7977: L-Methionine biosynthesis IV"))) +
  theme(
    axis.text = element_text(colour = "black"), 
    axis.title.x = element_text(size = 16),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(linewidth = 2),
    legend.title = element_text(angle = 90, vjust = 1))

propionate_plot_main_text

# ---- Butyrate ~ pathways, no supp use group ---- 

new_env <- new.env()
load(file = "data/intake_subset_analyses/no_supp_use/pathways/butyrate/shap/ML_r_workspace.rds", envir = new_env)
new_env$sv_full

p <- shapviz::sv_importance(new_env$sv_full, kind = "bee", show_numbers = TRUE, bee_width = 0.2, max_display = 10)
# modify the geom_text and geom_point layers to change size
p$layers[[3]]$aes_params$size <- 5  # text size 
p$layers[[2]]$aes_params$size <- 3  # point size

butyrate_plot <- p +
  labs(x = expression("predictive of lower butyrate < SHAP value > predictive of higher butyrate"),
       title = expression("Top features in no "  * B[12] * " supplement use subset")) +
  theme_bw(base_size = 16) +
  scale_y_discrete(labels = rev(c(
    
    "pwy_5981_cdp_diacylglycerol_biosynthesis_iii" = "pwy_5981: CDP-diacylglycerol biosynthesis III",
    "thisynara_pwy_superpathway_of_thiamine_diphosphate_biosynthesis_iii_eukaryotes" = "Superpathway of thiamine diphosphate\nbiosynthesis III (eukaryotes)",
    "pwy0_1061_superpathway_of_l_alanine_biosynthesis" = "pwy0_1061: Superpathway of L-alanine biosynthesis",
    "age" = "Age",
    "pwy0_1477_ethanolamine_utilization" = "pwy0_1477: Ethanolamine utilization pathway",
    "pwy_6292_superpathway_of_l_cysteine_biosynthesis_mammalian" = "Superpathway of mammalian L-cysteine biosynthesis",
    "udpnagsyn_pwy_udp_n_acetyl_d_glucosamine_biosynthesis_i" = "UDP-N-acetyl-D-glucosamine biosynthesis I",
    "pwy_7371_1_4_dihydroxy_6_naphthoate_biosynthesis_ii" = "pwy_7371: 1,4-dihydroxy-6-naphthoate biosynthesis II",
    "pwy_8131_5_deoxyadenosine_degradation_ii" = "pwy_8131: 5'-deoxyadenosine degradation II",
    "pwy_7237_myo_chiro_and_scyllo_inositol_degradation" = "pwy_7237 myo-, chiro- and scyllo-inositol degradation" ))) +

  #scale_colour_gradient(
    #low = "#969696", high = "#e24f4a", breaks = c(0, 1), 
                        #labels = c("Low", "High"), 
                        #name = "Feature value", 
                        #guide = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme(
    axis.text = element_text(colour = "black"), 
    axis.title.x = element_text(size = 16),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(linewidth = 2),
    legend.title = element_text(angle = 90, vjust = 1))

butyrate_plot

# ---- Propionate ~ pathways, low intake ----

new_env <- new.env()
load(file = "data/intake_subset_analyses/low_intake/pathways/propionate/shap/ML_r_workspace.rds", envir = new_env)
new_env$sv_full

p <- shapviz::sv_importance(new_env$sv_full, kind = "bee", show_numbers = TRUE, bee_width = 0.2, max_display = 10)
# modify the geom_text and geom_point layers to change size
p$layers[[3]]$aes_params$size <- 5  # text size 
p$layers[[2]]$aes_params$size <- 3  # point size

propionate_plot <- p +
  labs(x = expression("predictive of lower propionate < SHAP value > predictive of higher propionate"),
       title = expression("Top features in adequate "  * B[12] * " intake subset")) +
  theme_bw(base_size = 16) +
  scale_y_discrete(labels = rev(c(
    "age" = "Age",
    "bmi" = "BMI",
    "pwy_5981_cdp_diacylglycerol_biosynthesis_iii" = "pwy_5981: CDP-diacylglycerol biosynthesis III",
    "pwy0_162_superpathway_of_pyrimidine_ribonucleotides_de_novo_biosynthesis" = "pwy0_162: Superpathway of pyrimidine ribonucleotides\nde novo biosynthesis", 
    "pwy_7761_nad_salvage_pathway_ii_pnc_iv_cycle" ="pwy_7761: NAD salvage pathway II (PNC IV cycle)",
    "pwy_5971_palmitate_biosynthesis_type_ii_fatty_acid_synthase" = "pwy_5971: Palmitate biosynthesis (type II fatty acid synthase)",
    "pwy_5989_stearate_biosynthesis_ii_bacteria_and_plants" = "pwy_5989: Stearate biosynthesis II pathway",
    "pwy_5667_cdp_diacylglycerol_biosynthesis_i" = "pwy_5667: CDP-diacylglycerol biosynthesis I",
    "pwy_7328_superpathway_of_udp_glucose_derived_o_antigen_building_blocks_biosynthesis" = "pwy_7328: Superpathway of UDP-glucose derived\nO-antigen building blocks biosynthesis",
    "unintegrated" = "Unintegrated pathway"))) +
  theme(
    axis.text = element_text(colour = "black"), 
    axis.title.x = element_text(size = 16),
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(linewidth = 2),
    legend.title = element_text(angle = 90, vjust = 1))

propionate_plot

# ---- Design multi-panel figure ---- 

# Note: plot_1_pwys object is from a different script, called "SCFA_model_performance_plots.R"

(plot_1_pwys) / (propionate_plot_main_text)

#ggsave("figures/FIGURE6C_revision_test.pdf", width = 18, height = 12)
