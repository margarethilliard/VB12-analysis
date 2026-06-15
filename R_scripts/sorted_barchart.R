
# Load libraries 
library(dplyr)
library(ggplot2)
library(ggbreak)

# Set working directory and source the data 
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

# B12 sorted bar chart 

# Round concentration to nearest whole number, then count distinct subjects
metadata_sub$conc_rounded <- round(metadata_sub$habitual_dietary_b12)

conc_counts <- aggregate(subject_id ~ conc_rounded, data = metadata_sub, FUN = function(x) length(unique(x)))
colnames(conc_counts) <- c("habitual_dietary_b12", "n_subjects")

conc_counts$habitual_dietary_b12 <- as.numeric(as.character(conc_counts$habitual_dietary_b12))

vline_x <- median(metadata_sub$habitual_dietary_b12)

# Plotting Fig 1A 
sorted_bars <- ggplot(conc_counts, aes(x = habitual_dietary_b12, y = n_subjects)) +
  geom_bar(stat = "identity", fill = "#969696", width = 0.8) +
  labs(x = expression(Dietary~vitamin~B[12]~","~mu~g/d),
       y = "Number of individuals") +
  theme_bw(base_size = 16) +
  theme(panel.border = element_rect(colour = "black"),
        legend.position = "none",
        axis.text.y.right = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
  geom_vline(xintercept = vline_x, colour = "#e24f4a", linetype = "dashed", linewidth = 1.25) +
  # Left arrow
  annotate("segment", x = vline_x - 0.5, xend = vline_x - 7, y = 27.5, yend = 27.5,
           arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.75) +
  # Right arrow
  annotate("segment", x = vline_x + 0.5, xend = vline_x + 7, y = 27.5, yend = 27.5,
           arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 0.75) +
  # Adequate label
  annotate("text", x = vline_x - 3.5, y = 29, label = "Adequate\nintake",
           color = "black", size = 3.5, fontface = "bold") +
  # High label 
  annotate("text", x = vline_x + 3.5, y = 29, label = "High\nintake",
           color = "black", size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 1065)) +
  ggbreak::scale_x_break(
    breaks = c(35, 
               50, 100,
               145, 152,
               285, 310,
              440, 500, 
              738, 744, 
             1050),
   scales = .2,
   ticklabels = seq(0, 1062, by = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

sorted_bars

#ggsave("figures/sorted_bars_ggbreak.pdf", width = 9, height = 5.06)
