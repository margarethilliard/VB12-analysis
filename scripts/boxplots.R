
# ---- Setup ----
set.seed(8675309)

#install.packages(c("dplyr", "ggplot2", "rstatix", "ggpubr", "ggbreak", "patchwork", "scales"))

# Load libraries 
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(ggbreak)
library(patchwork)
library(scales)

# Set working directory and source the data 
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

data <- metadata_sub

# ---- Figure 1A-B: Compare intake and plasma B12 status between supplement users/non-users ----
data_users <- data %>%
  filter(supplement_taker == "Yes") 

data_users %>%
  dplyr::summarise(mean = mean(habitual_dietary_b12))

data_non_users <- data %>%
  filter(supplement_taker == "No")

data_non_users %>%
  dplyr::summarise(mean = mean(habitual_dietary_b12))

## Mann whitney U test 
data$supplement_taker <- as.factor(data$supplement_taker)

stat.test <- data %>%
  ungroup() %>%
  rstatix::wilcox_test(data = ., plasma_b12 ~ supplement_taker)%>%
  rstatix::add_significance() 

wilcox.test(plasma_b12 ~ supplement_taker, data = data, exact = FALSE)

panel_a <- ggplot(data, aes(x=supplement_taker, y=plasma_b12, colour = supplement_taker)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) +
  #scale_colour_manual(values = c("black", "red")) +
  scale_colour_manual(values = c("#969696", "#e24f4a")) +
  labs(color='Supplement use') +
  theme_bw(base_size = 16) + 
  geom_hline(aes(yintercept = 148), colour="black", linetype="dashed") + # replete range 
  geom_hline(aes(yintercept = 300), colour="black", linetype="dashed") + 
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", y.position = 1400, position = position_identity(),
                     tip.length = 0.005) + # shortens length of bracket
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text = element_text(colour = "black")) +
  labs(x = "Supplement use", y = expression(Plasma~vitamin~B[12]~", pmol/L"))

panel_a

## Do the same thing but for habitual intake 
stat.test <- data %>%
  ungroup() %>%
  rstatix::wilcox_test(data = ., habitual_dietary_b12 ~ supplement_taker)%>%
  rstatix::add_significance() 

wilcox.test(habitual_dietary_b12 ~ supplement_taker, data = data, exact = FALSE)

panel_b <- ggplot(data, aes(x=supplement_taker, y=habitual_dietary_b12, colour = supplement_taker)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0.15), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) +
  scale_colour_manual(values = c("#969696", "#e24f4a")) +
  labs(color='Supplement use') +
  theme_bw(base_size = 16) + 
  geom_hline(aes(yintercept = 2.4), colour="black", linetype="dashed") +
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", y.position = 900, vjust = 0.3,
                     tip.length = 0.005, label.size = 4, position = position_identity()) + # shortens length of bracket
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "black")) + 
  labs(x = "Supplement use", 
       y = expression(Habitual~vitamin~B[12]~" intake, " ~mu~g/d)) +
  ggbreak::scale_y_break(
    breaks = c(100, 145, 500, 735),  # Define where breaks happen
    scales = 0.2)  # Controls the size of the breaks (gap size between broken parts)

panel_b

# ---- Figure 1C-D: Compare intake and plasma B12 status between the high/low intake groups ----
data_high <- data %>%
  filter(intake_group == "High") 

data_high %>%
  dplyr::summarise(mean = mean(habitual_dietary_b12))

data_low <- data %>%
  filter(intake_group == "Low")

data_low %>%
  dplyr::summarise(mean = mean(habitual_dietary_b12))

## Mann whitney U test 
data$intake_group <- as.factor(data$intake_group)

## Re-order for consistent plotting
data$intake_group <- factor(data$intake_group, levels = c("Low", "High"))

stat.test <- data %>%
  ungroup() %>%
  rstatix::wilcox_test(data = ., plasma_b12 ~ intake_group)%>%
  rstatix::add_significance() 

wilcox.test(plasma_b12 ~ intake_group, data = data, exact = FALSE)

panel_c <- ggplot(data, aes(x=intake_group, y=plasma_b12, colour = intake_group)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) +
  scale_colour_manual(values = c("#969696", "#e24f4a")) +
  theme_bw(base_size = 16) + 
  geom_hline(aes(yintercept = 148), colour="black", linetype="dashed") + # replete range 
  geom_hline(aes(yintercept = 300), colour="black", linetype="dashed") + 
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", y.position = 1400, position = position_identity(),
                             tip.length = 0.005) + # shortens length of bracket
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text = element_text(colour = "black")) +
  labs(x = expression(B[12] ~ "intake group relative to median"),
       y = expression(Plasma~vitamin~B[12]~", pmol/L"))

panel_c

## Do the same thing but for habitual intake 
stat.test <- data %>%
  ungroup() %>%
  rstatix::wilcox_test(data = ., habitual_dietary_b12 ~ intake_group)%>%
  rstatix::add_significance() 

wilcox.test(habitual_dietary_b12 ~ intake_group, data = data, exact = FALSE)

panel_d <- ggplot(data, aes(x=intake_group, y=habitual_dietary_b12, colour = intake_group)) + 
  geom_boxplot(outliers = FALSE, width = 0.5) +
  geom_jitter(shape = 16, position = position_jitter(width = 0.15, height = 0.15), alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 3, colour = "black", 
               position = position_dodge(width = 0.75)) +
  scale_colour_manual(values = c("#969696", "#e24f4a")) +
  theme_bw(base_size = 16) + 
  geom_hline(aes(yintercept = 2.4), colour="black", linetype="dashed") +
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", y.position = 900, vjust = 0.3,
                             tip.length = 0.005, label.size = 4, position = position_identity()) + # shortens length of bracket
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "black")) + 
  labs(x = expression(B[12] ~ "intake group relative to median"),
       y = expression(Habitual~vitamin~B[12]~" intake, " ~mu~g/d)) +
  ggbreak::scale_y_break(
    breaks = c(100, 145, 500, 735),  # Define where breaks happen
    scales = 0.2)  # Controls the size of the breaks (gap size between broken parts)

panel_d

# ---- Design multi-panel plot ---- 
design <- "
AAABBCC
AAADDEE"

# Note: plot4 object is generated from another script called "partial_and_kendall_correlations.R"
((panel_a + panel_b) / (panel_c + panel_d) | (plot4) ) + 
  patchwork::plot_annotation(tag_levels = 'A')

#ggsave("figures/boxplots.pdf", height = 10, width =18)
