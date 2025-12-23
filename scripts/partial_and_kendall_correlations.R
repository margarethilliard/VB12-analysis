
# Credit to Andrew Oliver for the code, sourced from here: https://github.com/aoliver44/SCFA-Analysis/blob/main/figure_scripts/Figure1.R

# ---- Set up ----
#install.packages(c("dplyr", "readr", "ggplot2", "bestNormalize", "car", "rstatix", "ggpubr", "wesanderson", "gtsummary", "kableExtra", "png", "patchwork", "cowplot", "ggcorrplot"))

# Load libraries 
library(dplyr)
library(readr)
library(ggplot2)
library(bestNormalize)
library(car)
library(rstatix)
library(ggpubr)
library(wesanderson)
library(gtsummary)
library(kableExtra)
library(png)
library(patchwork)
library(cowplot)
library(ggcorrplot)

# Set working directory and source the data
setwd("/Users/local-margaret/Desktop/VB12-analysis")
source("scripts/get_data.R")

data <- metadata_sub # subset that has microbiome data available 

# ---- Covariate relationships with response variable ----

# empty df to store results 
tmp_inner <- data.frame()
tmp_outer <- data.frame()
# transformation 
set.seed(123)
print(bestNormalize::bestNormalize(data$plasma_b12, allow_orderNorm = F, k = 10, r = 10)$chosen_transform)
print(bestNormalize::bestNormalize(data$habitual_dietary_b12, allow_orderNorm = F, k = 10, r = 10)$chosen_transform)

data$normalized_resp <- bestNormalize::bestNormalize(data$plasma_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
data$normalized_pred <- bestNormalize::bestNormalize(data$habitual_dietary_b12, allow_orderNorm = F, k = 10, r = 10)$x.t

# multiple linear regression with normalized response, predicting plasma B12 from covariates 
model <- lm(normalized_resp ~ as.numeric(bmi) + as.numeric(age) + as.factor(sex), data = data)

# partial regression plot 
partial_regression <- car::avPlots(model, main = paste(data$plasma_b12, "vs. Anthro")) # the variables don't look particularly explanatory 

car::dfbetaPlots(model = model)

tmp_bmi <- cor.test(partial_regression$`as.numeric(bmi)`[,1], partial_regression$`as.numeric(bmi)`[,2])
tmp_inner_bmi <- data.frame(normalized_pred = partial_regression$`as.numeric(bmi)`[,1],
                            normalized_resp = partial_regression$`as.numeric(bmi)`[,2],
                            response_name = rep("plasma_B12", length(NROW(partial_regression$`as.numeric(bmi)`))),
                            predictor_name = rep("bmi", length(NROW(partial_regression$`as.numeric(bmi)`)))
                            )
tmp_age <- cor.test(partial_regression$`as.numeric(age)`[,1], partial_regression$`as.numeric(age)`[,2])
tmp_inner_age <- data.frame(normalized_pred = partial_regression$`as.numeric(age)`[,1],
                            normalized_resp = partial_regression$`as.numeric(age)`[,2],
                            response_name = rep("plasma_B12", length(NROW(partial_regression$`as.numeric(age)`))),
                            predictor_name = rep("age", length(NROW(partial_regression$`as.numeric(age)`)))
                            )
tmp_sex <- cor.test(partial_regression$`as.factor(sex)Male`[,1], partial_regression$`as.factor(sex)Male`[,2])
tmp_inner_sex <- data.frame(normalized_pred = partial_regression$`as.factor(sex)Male`[,1],
                            normalized_resp = partial_regression$`as.factor(sex)Male`[,2],
                            response_name = rep("plasma_B12", length(NROW(partial_regression$`as.factor(sex)Male`))),
                            predictor_name = rep("sex", length(NROW(partial_regression$`as.factor(sex)Male`)))
                            )
df_anthro <- data.frame(factor = c("BMI", "Age", "Sex"), 
                        corr_estimate = c(tmp_bmi$estimate, tmp_age$estimate, tmp_sex$estimate), 
                        p_value = c(tmp_bmi$p.value, tmp_age$p.value, tmp_sex$p.value))
df_anthro$p_adjust <- p.adjust(df_anthro$p_value, method = "fdr")
tmp_outer <- rbind(tmp_outer, tmp_inner_bmi, tmp_inner_age, tmp_inner_sex)
print(df_anthro)

# ---- Supplementary Figure 1A-C: Plot partial correlations ----
# age panel 
plot1 <- ggplot(subset(tmp_outer, tmp_outer$predictor_name == "age")) + 
  aes(x = normalized_pred, y = normalized_resp, fill = "#969696") + 
  geom_point(aes(fill = response_name), pch=21, colour="black", alpha = 0.5, size=1.5) +
  geom_smooth(aes(fill = response_name), method = "lm", color = "black", linetype = "dashed", se = TRUE) +
  scale_fill_manual(values = "#969696") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = "Age | Sex, BMI") +
  labs(y = expression(Plasma~vitamin~B[12]~"(normalized)")) +
  guides(color="none", fill = "none") +
  coord_cartesian(ylim = c(-3,3.25)) 

plot1

# bmi panel  
plot2 <- ggplot(subset(tmp_outer, tmp_outer$predictor_name == "bmi")) + 
  aes(x = normalized_pred, y = normalized_resp) + 
  geom_point(aes(fill = response_name), pch=21, colour="black", alpha = 0.5, size= 1.5) +
  geom_smooth(aes(fill = response_name), method = "lm", color = "black", linetype = "dashed", se = TRUE) +
  scale_color_manual(values = "#969696") + 
  scale_fill_manual(values = "#969696") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = "BMI | Age, Sex") +
  labs(y = expression(Plasma~vitamin~B[12]~"(normalized)")) +
  guides(color="none", fill = "none") +
  coord_cartesian(ylim = c(-3,3.25)) 

plot2

# customize the dataframe for box plots 
boxplot_dat <- tmp_outer %>% dplyr::filter(., predictor_name == "sex") %>%
  dplyr::mutate(., sex_factor = ifelse(as.numeric(normalized_pred) > 0, "Male", "Female"))
boxplot_dat$sex_factor <- factor(boxplot_dat$sex_factor,
                                 levels = c('Male','Female'), ordered = TRUE)
# look for significant differences in plasma B12 between the sexes 
rstatix::t_test(normalized_resp ~ sex_factor, data = boxplot_dat)

stat.test <- boxplot_dat %>%
  ungroup() %>%
  rstatix::t_test(data = ., normalized_resp ~ sex_factor)%>%
  rstatix::add_significance() %>%
  rstatix::add_xy_position()

# sex panel 
plot3 <- boxplot_dat %>%
  ggplot(aes(x = sex_factor, y = normalized_resp)) + 
  geom_boxplot(aes(fill = sex_factor), outliers = FALSE) + 
  geom_point(aes(fill = sex_factor), pch=21, colour="black", position = position_jitter(0.25), alpha = 0.5, size= 1.5) +
  ggpubr::stat_pvalue_manual(stat.test, label = "p.signif", tip.length = 0.01) +
  scale_fill_manual(values = wesanderson::wes_palette("Moonrise3")) + 
  scale_color_manual(values = wesanderson::wes_palette("Moonrise3")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = "Sex | BMI, Age") +
  labs(y =  expression(Plasma~vitamin~B[12]~"(normalized)")) +
  guides(fill="none", color = "none") +
  coord_cartesian(ylim = c(-3,3.25)) 

plot3

# ---- Figure 1E: Plasma B12 ~ habitual diet + covariates ----

model <- lm(normalized_resp ~ normalized_pred + as.numeric(bmi) + as.numeric(age) + as.factor(sex), data = data)
car::vif(model) # no apparent multi-collinearity
summary(model)

partial_regression <- car::avPlots(model, main = paste("Plasma B12 vs. Habitual Diet"))
tmp_plasma_b12 <- cor.test(partial_regression$normalized_pred[,1], partial_regression$normalized_pred[,2])

car::Anova(model, type = 3)

# gtsummary table from partial regression model 
table <- model %>%
  gtsummary::tbl_regression(include = everything(),
                 label = list(normalized_pred ~ "Habitual dietary B12",
                              `as.numeric(bmi)` ~ "BMI (kg/m2)",  
                              `as.numeric(age)` ~ "Age (years)",
                              `as.factor(sex)` ~ "Sex"))

# extract the p-value for plotting
p_value <- table$table_body$p.value[1]

# table to kable
ktable <- table %>%
  gtsummary::as_kable() %>%
  kableExtra::kable_styling(font_size = 24) %>%
  kableExtra::column_spec(2, latex_column_spec = "input")  # adjust column for superscript

# make stylistic adjustments using HTML
ktable <- gsub("kg/m2", "kg/m<sup>2</sup>", ktable)
ktable <- gsub("Habitual dietary B12", "Habitual dietary B<sub>12</sub> (<span style='font-size:14px;'>&micro;g/d</span>)", ktable)
ktable <- gsub("\\*\\*(.*?)\\*\\*", "\\1", ktable)

# save the table
ktable %>%
  kableExtra::save_kable('fig1_table.png')

## plot the partial regression 
plot4 <- ggplot(as.data.frame(partial_regression$normalized_pred)) + 
  aes(x = normalized_pred, y = normalized_resp, fill = "#969696") + 
  geom_point(pch=21, colour="black", alpha = 0.75, size =1.5) + 
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = TRUE) + 
  scale_fill_manual(values = "#969696") + 
  theme_bw(base_size = 16) + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = expression(Habitual~~B[12]~intake~"(normalized) | covariates")) +
  labs(y = expression(Plasma~vitamin~B[12]~"(normalized) | covariates"))+ 
  guides(color="none", fill = "none") +
  coord_cartesian(ylim = c(-3,3.25)) +
  annotate("text", 
           x = 0.8, 
           y = 3.4, 
           hjust = 0, vjust = 1, 
           label = "Partial correlation\nPearson rho = 0.42\np < 0.001",
           size = 4,
           color = "red")

plot4 # FYI: this object ends up in the multi-panel Figure 1

# --- Design multi-panel figure ----

pw_plot <- plot1 + plot2 + plot3
pw_plot

# ---- Kendall correlation ----
corr_dat <- data %>%
  dplyr::select(plasma_b12,
                eGFR,
                #b12_tnfs_avg,
                habitual_dietary_b12, dt_vb12, dt_b12ad, daily_supplemented_b12,
                age, sex, bmi)

# check structure to ensure all variables are numeric 
str(corr_dat)

corr_dat <- corr_dat %>%
  mutate(sex = ifelse(sex == "Male", 1, 0),# convert male/female to 1/0
         age = as.numeric(age)) # convert age from integer to numeric

# coefficients 
coef <- round(cor(corr_dat, method = "kendall"), 1)

# p-values 
pvals <- ggcorrplot::cor_pmat(corr_dat, method = "kendall")
pvals_adj <- apply(pvals, 2, function(x) p.adjust(x, method = "fdr"))

# plotting 
corr_plot <- ggcorrplot::ggcorrplot(
    legend.title = "Kendall\ncorrelation\ncoefficient",
    coef,
    p.mat = pvals_adj,
    hc.order = TRUE,
    type = "lower",
    insig = "blank",
    lab = TRUE,
    outline.col = "black") + 
  theme(axis.text.y=element_text(size=12),
        axis.text.x = element_text(size=12, angle=45)) +
  # y labels start from bottom
  scale_y_discrete(labels = c("Age",
                              "Sex",
                              expression(B[12]~from~foods),
                              expression(Fortified~B[12]),
                              "eGFR",
                              "BMI",
                              expression(Plasma~B[12]),
                              expression(Habitual~B[12]~intake))) +
  # x labs start from left 
  scale_x_discrete(labels = c("Sex",
                              expression(B[12]~from~foods),
                              expression(Fortified~B[12]),
                              "eGFR",
                              "BMI",
                              expression(Plasma~B[12]),
                              expression(Habitual~B[12]~intake),
                              expression(B[12]~from~supplements)))

corr_plot
# ggsave("figures/kendall-correlation-plot-with-signif.png", width = 8, height = 5)
