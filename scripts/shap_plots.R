
# ---- Set up ---- 

#install.packages(c("ggplot2", "shapviz", "dplyr", "tidyr", "patchwork", "cowplot"))

library(ggplot2)
library(shapviz)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)

setwd("/Users/local-margaret/Desktop/VB12_analysis/")

# ---- Figure 5: predicting plasma B12 concentration from all participants ---- 
new_env <- new.env()
load(file = "RF_analysis/SHAP_rds_files/regression_plasmaB12~diet+microbiome_FULL_DATA.rds", envir = new_env)

p <- shapviz::sv_importance(new_env$sv_full, kind = "bee", show_numbers = TRUE, bee_width = 0.2, max_display = 10)

# modify the geom_text and geom_point layers to change size
p$layers[[3]]$aes_params$size <- 4  # text size 
p$layers[[2]]$aes_params$size <- 2  # point size

shap_plot <- p +
  labs(x = expression("predictive of low plasma " * B[12] * " < SHAP value > predictive of high plasma " * B[12])) +
  theme_bw(base_size = 15) +
  scale_y_discrete(labels = rev(c(
    "habitual_dietary_b12" = expression("Habitual " * B[12] * " intake, mcg/d"),
    "age" = "Age", 
    "d_bacteria_p_firmicutes_a_c_clostridia_o_oscillospirales_f_oscillospiraceae_g_dysosmobacter_s_dysosmobacter_sp001916835" = expression(italic("Dysosmobacter")~"sp001916835"),
    "d_bacteria_p_proteobacteria_c_alphaproteobacteria_o_rf32" = expression(italic("Alphaproteobacteria")~"RF32"),
    "d_bacteria_p_firmicutes_c_bacilli_o_lactobacillales_f_streptococcaceae_g_streptococcus_s_streptococcus_thermophilus" = expression(italic("Streptococcus thermophilus")),
    "d_bacteria_p_firmicutes_c_c_negativicutes_o_acidaminococcales_f_acidaminococcaceae_g_phascolarctobacterium" = expression(italic("Phascolarctobacterium")~("genus")),
    "d_bacteria_p_actinobacteriota_c_coriobacteriia_o_coriobacteriales_f_eggerthellaceae_g_senegalimassilia_s_senegalimassilia_anaerobia" = expression(italic("Senegalimassilia anaerobia")),
    "d_bacteria_p_firmicutes_c_bacilli_o_lactobacillales_f_streptococcaceae_g_lactococcus" = expression(italic("Lactococcus")~("genus")),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_monoglobales" = expression(italic("Monoglobales")~("order")),
    "bmi" = "BMI"))) + 
  scale_colour_gradient(low = "#969696", high = "#e24f4a", breaks = c(0, 1), 
                        labels = c("Low", "High"), 
                        name = "Feature value", 
                        guide = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme(axis.text = element_text(colour = "black"), 
        axis.title.x = element_text(size = 15),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(linewidth = 2),
        legend.title = element_text(angle = 90, vjust = 1))

shap_plot

# multi-line dependence plot
shap_mat <- shapviz::get_shap_values(new_env$sv_full)
feat_mat <- get_feature_values(new_env$sv_full)

df_long <- shap_mat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "Feature", values_to = "Svalue") %>%
  left_join(feat_mat %>%
              as.data.frame() %>%
              mutate(id = row_number()) %>%
              pivot_longer(-id, names_to = "Feature", values_to = "Value"), by = c("id", "Feature"))

feats <- c("d_bacteria_p_firmicutes_a_c_clostridia_o_oscillospirales_f_oscillospiraceae_g_dysosmobacter_s_dysosmobacter_sp001916835",
           "d_bacteria_p_proteobacteria_c_alphaproteobacteria_o_rf32",
           "d_bacteria_p_firmicutes_c_bacilli_o_lactobacillales_f_streptococcaceae_g_streptococcus_s_streptococcus_thermophilus",
           "d_bacteria_p_firmicutes_c_c_negativicutes_o_acidaminococcales_f_acidaminococcaceae_g_phascolarctobacterium",
           "d_bacteria_p_actinobacteriota_c_coriobacteriia_o_coriobacteriales_f_eggerthellaceae_g_senegalimassilia_s_senegalimassilia_anaerobia",
           "d_bacteria_p_firmicutes_c_bacilli_o_lactobacillales_f_streptococcaceae_g_lactococcus",
           "d_bacteria_p_firmicutes_a_c_clostridia_o_monoglobales")

df_sub <- df_long %>% filter(Feature %in% feats)

df_sub <- df_sub %>%
  mutate(Feature = as.character(Feature))

df_sub <- df_sub %>%
  mutate(Tidy_Feature = recode(Feature,"d_bacteria_p_firmicutes_a_c_clostridia_o_oscillospirales_f_oscillospiraceae_g_dysosmobacter_s_dysosmobacter_sp001916835" = "Dysosmobacter sp001916835",
                               "d_bacteria_p_proteobacteria_c_alphaproteobacteria_o_rf32" = "Alphaproteobacteria RF32",
                               "d_bacteria_p_firmicutes_c_bacilli_o_lactobacillales_f_streptococcaceae_g_streptococcus_s_streptococcus_thermophilus" = "Streptococcus thermophilus",
                               "d_bacteria_p_firmicutes_c_c_negativicutes_o_acidaminococcales_f_acidaminococcaceae_g_phascolarctobacterium" = "Phascolarctobacterium (genus)",
                               "d_bacteria_p_actinobacteriota_c_coriobacteriia_o_coriobacteriales_f_eggerthellaceae_g_senegalimassilia_s_senegalimassilia_anaerobia" = "Senegalimassilia anaerobia",
                               "d_bacteria_p_firmicutes_c_bacilli_o_lactobacillales_f_streptococcaceae_g_lactococcus" = "Lactococcus (genus)",
                               "d_bacteria_p_firmicutes_a_c_clostridia_o_monoglobales" = "Monoglobales (order)"))

dependence_plot_main <- ggplot(df_sub, aes(x = Value, y = Svalue, color = Tidy_Feature)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = F, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw(base_size = 14) +
  xlab("Relative abundance (%)") +
  ylab(expression(atop("SHAP value",
                       "(for prediction of plasma " * B[12] * " status)"))) +
  scale_color_discrete(labels = c("Dysosmobacter sp001916835" = expression(italic("Dysosmobacter")~"sp001916835"),
                                  "Alphaproteobacteria RF32" = expression(italic("Alphaproteobacteria")~"RF32"),
                                  "Streptococcus thermophilus" = expression(italic("Streptococcus thermophilus")),
                                  "Phascolarctobacterium (genus)" = expression(italic("Phascolarctobacterium")~"(genus)"),
                                  "Senegalimassilia anaerobia" = expression(italic("Senegalimassilia anaerobia")),
                                  "Lactococcus (genus)" = expression(italic("Lactococcus")~"(genus)"),
                                  "Monoglobales (order)" = expression(italic("Monoglobales")~"(order)")),
                       type = c("#e95462",
                                "#f5db4c",
                                "#cc4778",
                                "#7e2482",
                                "#969696",
                                "#f89540",
                                "black")) +
  theme(legend.position = c(0.82, 0.8), 
        legend.background = element_rect(color = NA, fill = NA),
        legend.title = element_blank())

dependence_plot_main

# ---- Figure 6A-B: predicting plasma B12 concentration in low intake group ---- 
new_env <- new.env()
load(file = "RF_analysis/SHAP_rds_files/regression_plasmaB12~diet+microbiome+LOW_INTAKE.rds", envir = new_env)

p <- shapviz::sv_importance(new_env$sv_full, kind = "bee", show_numbers = TRUE, bee_width = 0.2, max_display = 5)

# modify the geom_text and geom_point layers to change size
p$layers[[3]]$aes_params$size <- 4  # text size 
p$layers[[2]]$aes_params$size <- 4  # point size

plot_A <- p +
  labs(x = expression("predictive of low plasma " * B[12] * " < SHAP value > predictive of high plasma " * B[12]),
       title = expression("Top features in low "  * B[12] * " intake group subset")) +
  theme_bw(base_size = 15) +
  scale_y_discrete(labels = rev(c(
    "habitual_dietary_b12" = expression("Habitual " * B[12] * " intake, mcg/d"),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_am51_8" = expression(italic("Lachnospiraceae")~"AM51-8"),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_cag_317" = expression(italic("Lachnospiraceae")~"CAG-317"),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_oscillospirales_f_oscillospiraceae_g_evtepia_s_evtepia_gabavorous" = expression(italic("Evtepia gabavorous")),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_oscillospirales_f_ruminococcaceae_g_negativibacillus_s_negativibacillus_sp000435195" = expression(italic("Negativibacillus")~"sp000435195")))) + 
  scale_colour_gradient(low = "#969696", high = "#e24f4a", breaks = c(0, 1), 
                        labels = c("Low", "High"), 
                        name = "Feature value", 
                        guide = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme(axis.text = element_text(colour = "black"), 
        axis.title.x = element_text(size = 15),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(linewidth = 2),
        legend.title = element_text(angle = 90, vjust = 1))

plot_A

# multi-line dependence plot
shap_mat <- shapviz::get_shap_values(new_env$sv_full)
feat_mat <- get_feature_values(new_env$sv_full)

df_long <- shap_mat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "Feature", values_to = "Svalue") %>%
  left_join(feat_mat %>%
              as.data.frame() %>%
              mutate(id = row_number()) %>%
              pivot_longer(-id, names_to = "Feature", values_to = "Value"), by = c("id", "Feature"))

feats <- c("d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_am51_8",
           "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_cag_317")

df_sub <- df_long %>% filter(Feature %in% feats)

df_sub <- df_sub %>%
  mutate(Feature = as.character(Feature))

df_sub <- df_sub %>%
  mutate(Tidy_Feature = recode(Feature,
                               "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_am51_8" = "Lachnospiraceae AM51-8",
                               "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_cag_317" = "Lachnospiraceae CAG-317"))

plot_B  <- ggplot(df_sub, aes(x = Value, y = Svalue, color = Tidy_Feature)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = TRUE, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw(base_size = 14) +
  xlab("Relative abundance (%)") +
  ylab(expression(atop("SHAP value",
                       "(for prediction of plasma " * B[12] * " status)"))) +
  scale_color_manual(values = c("#969696", "black"),
                     labels = c(expression(italic("Lachnospiraceae")~"AM51-8"),
                                expression(italic("Lachnospiraceae")~"CAG-317"))) +
  theme(legend.position = c(0.75, 0.9),
        legend.background = element_rect(color = NA, fill = NA),
        legend.title = element_blank())

plot_B 

# ---- Figure 6C-D: predicting plasma B12 concentration in supp non users ---- 
new_env <- new.env()
load(file = "RF_analysis/SHAP_rds_files/regression_plasmaB12~diet+microbiome+SUPP_NON_USERS.rds", envir = new_env)

p <- shapviz::sv_importance(new_env$sv_full, kind = "bee", show_numbers = TRUE, bee_width = 0.2, max_display = 5)

# modify the geom_text and geom_point layers to change size
p$layers[[3]]$aes_params$size <- 4  # text size 
p$layers[[2]]$aes_params$size <- 4  # point size

plot_C <- p +
  labs(x = expression("predictive of low plasma " * B[12] * " < SHAP value > predictive of high plasma " * B[12]),
       title = expression("Top features in no "  * B[12] * " supplement use subset")) +
  theme_bw(base_size = 15) +
  scale_y_discrete(labels = rev(c(
    "habitual_dietary_b12" = expression("Habitual " * B[12] * " intake, mcg/d"),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_am51_8" = expression(italic("Lachnospiraceae")~"AM51-8"),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_cag_317" = expression(italic("Lachnospiraceae")~"CAG-317"),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_blautia_a_s_blautia_a_faecis" = expression(italic("Blautia A faecis")),
    "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_mediterraneibacter_s_mediterraneibacter_lactaris" = expression(italic("Mediterraneibacter lactaris"))))) + 
  scale_colour_gradient(low = "#969696", high = "#e24f4a", breaks = c(0, 1), 
                        labels = c("Low", "High"), 
                        name = "Feature value", 
                        guide = guide_colorbar(title.position = "left", title.hjust = 0.5)) +
  theme(axis.text = element_text(colour = "black"), 
        axis.title.x = element_text(size = 15),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(linewidth = 2),
        legend.title = element_text(angle = 90, vjust = 1)) 

plot_C

# multi-line dependence plot
shap_mat <- shapviz::get_shap_values(new_env$sv_full)
feat_mat <- get_feature_values(new_env$sv_full)

df_long <- shap_mat %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "Feature", values_to = "Svalue") %>%
  left_join(feat_mat %>%
      as.data.frame() %>%
      mutate(id = row_number()) %>%
      pivot_longer(-id, names_to = "Feature", values_to = "Value"), by = c("id", "Feature"))

feats <- c("d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_blautia_a_s_blautia_a_faecis",
           "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_mediterraneibacter_s_mediterraneibacter_lactaris")

df_sub <- df_long %>% filter(Feature %in% feats)

df_sub <- df_sub %>%
  mutate(Feature = as.character(Feature))

df_sub <- df_sub %>%
  mutate(Tidy_Feature = recode(Feature,
                               "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_blautia_a_s_blautia_a_faecis" = "Blautia A faecis",
                               "d_bacteria_p_firmicutes_a_c_clostridia_o_lachnospirales_f_lachnospiraceae_g_mediterraneibacter_s_mediterraneibacter_lactaris" = "Mediterraneibacter lactaris"))

plot_D <- ggplot(df_sub, aes(x = Value, y = Svalue, color = Tidy_Feature)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(se = TRUE, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw(base_size = 14) +
  xlab("Relative abundance (%)") +
  ylab(expression(atop("SHAP value",
                       "(for prediction of plasma " * B[12] * " status)"))) +
  scale_color_manual(values = c("#969696", "black"),
                     labels = c(expression(italic("Blautia A faecis")),
                                expression(italic("Mediterraneibacter lactaris")))) +
  theme(legend.position = c(0.75, 0.9),
        legend.background = element_rect(color = NA, fill = NA),
        legend.title = element_blank())

plot_D

# ---- Patchwork plots together ----
shap_plot + dependence_plot_main + 
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right") 
  
#ggsave("figures/Shap_patchwork.pdf", width = 15, height = 10)

