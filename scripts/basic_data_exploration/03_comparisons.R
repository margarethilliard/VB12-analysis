##### load libraries #####

# install.packages(c("tidyverse", "plotrix", "rstatix", "ggpubr", "ggbreak", "ppcor"))
library(tidyverse) # data tidying 
library(plotrix) # for calculating SEM 
library(rstatix) # boxplot p-vals 
library(ggpubr) # plotting p-vals 
library(ggbreak) # ggplot axis breaks
library(ppcor) # partial correlations 

##### read in data #####

data <- read_csv("/Users/local-margaret/Desktop/R-projects/FL100/data/cleaned-data/UPDATED_B12_with_BMI.csv")
data <-rename(data, age = age_ffq)
data <-rename(data, sex = sex_ffq)
data <-rename(data, bmi = BMI)
data <-rename(data, recent_dietary_b12_avg = b12_tnfs_avg)
data <-rename(data, habitual_dietary_b12 = FFQ_B12_sum)
data <-rename(data, daily_supplemented_b12 = sup_b12)
data <-rename(data, plasma_b12 = vitamin_b12)


##### filter data for taxaHFE metadata file ##### 

taxaHFE_metadata <- data %>%
  dplyr::select(subject_id, vitamin_b12,
                eGFR,
                b12_tnfs_avg,
                FFQ_B12_sum, dt_vb12, dt_b12ad, sup_b12,
                age_ffq, sex_ffq, BMI) %>%
  dplyr::mutate(., sex_ffq = ifelse(sex_ffq == 1, "Male", "Female"))

taxaHFE_metadata <-rename(taxaHFE_metadata, plasma_b12 = vitamin_b12)
taxaHFE_metadata <-rename(taxaHFE_metadata, age = age_ffq)
taxaHFE_metadata <-rename(taxaHFE_metadata, sex = sex_ffq)
taxaHFE_metadata <-rename(taxaHFE_metadata, bmi = BMI)
taxaHFE_metadata <-rename(taxaHFE_metadata, recent_dietary_b12_avg = b12_tnfs_avg)
taxaHFE_metadata <-rename(taxaHFE_metadata, habitual_dietary_b12 = FFQ_B12_sum)
taxaHFE_metadata <-rename(taxaHFE_metadata, daily_supplemented_b12 = sup_b12)

## save the file 
# write_csv(taxaHFE_metadata, "/Users/local-margaret/Desktop/R-projects/FL100/taxaHFE_metadata.csv")

# quick fix on the metaphlan abundnace table for ease of use 
# read in metaphlan relative abundance 
abundance <- read_tsv("/Users/local-margaret/Desktop/R-projects/FL100/data/merged_metaphlan_v4-0-6_GTDB.txt")

# remove suffix from column names 
abundance <- abundance %>%
  rename_with(~str_remove(., '.mpa4_GTDB'))
## save the file 
# write_delim(abundance, "/Users/local-margaret/Desktop/R-projects/FL100/merged_metaphlan_v4-0-6_GTDB.txt", delim = "\t")

##### basic statistics for table 1 #####

## n clinically deficient 
data %>%
  filter(vitamin_b12 < 148) %>%
  View() # n = 12 

# plasma B12 avg, SE - all participants 
mean(data$vitamin_b12)
std.error(data$vitamin_b12)
## FFQ habitual intake 
mean(data$FFQ_B12_sum) ## this is very high!
std.error(data$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(data$dt_vb12) 
std.error(data$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(data$dt_b12ad) 
std.error(data$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(data$sup_b12) 
std.error(data$sup_b12) 

# age group 1 
test <- data %>%
  filter(age_ffq <= 35) 
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 


# age group 2 
test <- data %>%
  filter(age_ffq >= 36 & age_ffq <= 60)  
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 

# age group 3 
test <- data %>%
  filter(age_ffq >= 51) 
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 

# men 
test <- data %>%
  filter(sex_ffq == 1) 
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 

# women 
test <- data %>%
  filter(sex_ffq == 2) 
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 

# based on BMI range 
test <- data %>%
  filter(BMI >= 30) 
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 

# supplement use 
test <- data %>%
  filter(supplement_taker == "no") 
mean(test$vitamin_b12)
std.error(test$vitamin_b12)
## FFQ habitual intake 
mean(test$FFQ_B12_sum) ## this is very high!
std.error(test$FFQ_B12_sum) 
## FFQ habitual intake - from foods 
mean(test$dt_vb12) 
std.error(test$dt_vb12) 
## FFQ habitual intake - from fortificants 
mean(test$dt_b12ad) 
std.error(test$dt_b12ad) 
## FFQ habitual intake - from supplements
mean(test$sup_b12) 
std.error(test$sup_b12) 

## ASA24 recent intake 
mean(data$b12_tnfs_avg)
std.error(data$b12_tnfs_avg)

mean(data$dt_vb12)   
mean(data$dt_b12ad) 
mean(data$sup_b12) ## high FFQ B12 is likely driven by supplement users 

# distribution of supplemented B12 
ggplot(data, aes(x=sup_b12)) + 
  geom_histogram(color="black", fill="white", bins = 50) 

hist(data$b12_tnfs_avg) # ASA24 
hist(data$sup_b12) 
hist(data$FFQ_B12_sum)

data_users <- data %>%
  filter(supplement_taker == "yes")
data_non_users <- data %>%
  filter(supplement_taker == "no")

mean(data_users$FFQ_B12_sum)
median(data_users$FFQ_B12_sum)
std.error(data_users$FFQ_B12_sum) ## really high average with a lot of variance 

mean(data_non_users$FFQ_B12_sum)
median(data_non_users$FFQ_B12_sum)
std.error(data_non_users$FFQ_B12_sum)

##### supplement use impacts intake and status ##### 

## quick check to see if supplement users have 
## higher plasma B12 than non-users 

## mann whitney U test 
data$supplement_taker <- as.factor(data$supplement_taker)

stat.test <- data %>%
  ungroup() %>%
  wilcox_test(data = ., vitamin_b12 ~ supplement_taker)%>%
  add_significance() %>%
  add_xy_position()

wilcox.test(vitamin_b12 ~ supplement_taker, data = data, exact = FALSE)

ggplot(data, aes(x=supplement_taker, y=vitamin_b12, colour = supplement_taker)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  scale_colour_manual(values = c("black", "red")) +
  labs(color='Supplement use') +
  theme_bw() + 
  geom_hline(aes(yintercept = 148), colour="blue", linetype="dashed") + 
  geom_hline(aes(yintercept = 300), colour="blue", linetype="dashed") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  stat_pvalue_manual(stat.test, label = "Wilcoxon, p = {p}", y.position = 1600) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black")) +
  labs(x = "Supplement use", y = expression(Plasma~vitamin~B[12]~", pmol/L"))

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/difference-in-mean-plasma_B12~supp_use.pdf", height = 5, width = 8)

## do the same thing but for habitual intake 
stat.test <- data %>%
  ungroup() %>%
  wilcox_test(data = ., FFQ_B12_sum ~ supplement_taker)%>%
  add_significance() %>%
  add_xy_position()

wilcox.test(FFQ_B12_sum ~ supplement_taker, data = data, exact = FALSE)

ggplot(data, aes(x=supplement_taker, y=FFQ_B12_sum, colour = supplement_taker)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  scale_colour_manual(values = c("black", "red")) +
  labs(color='Supplement use') +
  theme_bw() + 
  geom_hline(aes(yintercept = 2.4), colour="blue", linetype="dashed") + 
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  stat_pvalue_manual(stat.test, label = "Wilcoxon, p = {p}", y.position = 125,
                     tip.length = 0.005) + # Shortens the length of the bracket
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black")) +
  labs(x = "Supplement use", 
       y = expression(Habitual~vitamin~B[12]~" intake, " ~mu~g/d)) +
  scale_y_break(c(150, 290), scales= 0.25) # check data ranges manually 

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/difference-in-habitual-B12-intake~supp_use.pdf", height = 5, width = 8)


##### pearson correlation figure #####

taxaHFE_metadata %>%
  #filter(supplement_taker == "no") %>%
  select(age, sex, bmi, recent_dietary_b12_avg, habitual_dietary_b12, daily_supplemented_b12, plasma_b12, eGFR, dt_b12ad, dt_vb12) %>%
  corrr::correlate(method = "pearson") %>% corrr::autoplot() 

corr_dat <- data %>%
  dplyr::select(vitamin_b12,
                eGFR,
                b12_tnfs_avg,
                FFQ_B12_sum, dt_vb12, dt_b12ad, sup_b12,
                age_ffq, sex_ffq, BMI)
# check structure 
str(corr_dat)

## coefficients 
coef <- round(cor(corr_dat, method = "kendall"), 1)

# p-values method 1 
# pvals <- Hmisc::rcorr(cor(corr_dat)) %>% `[[`('P') %>% corrr::as_cordf() 

# p-values method 2 
pvals <- cor_pmat(corr_dat, method = "kendall", sig.level = 0.001)


pallet <- wesanderson::wes_palette("Royal1", 2, type = "continuous")


plot4 <- ggcorrplot::ggcorrplot(
  legend.title = "Kendall\ncorrelation\ncoefficient",
  coef,
  p.mat = pvals,
  hc.order = TRUE,
  type = "lower",
  insig = "blank",
  lab = TRUE,
  outline.col = "black") + 
  theme(axis.text.y=element_text(size=12),
        axis.text.x = element_text(size=12, angle=45)) +
  # labels start from bottom
  scale_y_discrete(labels = c("BMI",
                              "eGFR",
                              "Sex",
                              "Age",
                              expression(B[12]~from~foods),
                              expression(B[12]~from~fortificants),
                              expression(Plasma~B[12]),
                              expression(Recent~dietary~B[12]),
                              expression(Habitual~dietary~B[12])
                              )
                   ) +
  # labs start from left 
  scale_x_discrete(labels = c("eGFR",
                              "Sex",
                              "Age",
                              expression(B[12]~from~foods),
                              expression(B[12]~from~fortificants),
                              expression(Plasma~B[12]),
                              expression(Recent~dietary~B[12]),
                              expression(Habitual~dietary~B[12]),
                              expression(Supplemented~B[12])
                              )
                   )


ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/correlation-plot-with-signif.pdf", height = 5, width = 8)

# correlation and p-val matrices 
M = cor(corr_dat)
testRes = cor.mtest(corr_dat, conf.level = 0.95)

corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey', order = 'AOE')


corrplot(M, p.mat = testRes$p, sig.level = 0.001, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='white', number.cex = 0.7, order = 'AOE', diag=FALSE) 

##### partial correlations #####

partialCor <- pcor.test(x=data$FFQ_B12_sum, y=data$vitamin_b12,
                              z=c(data$age_ffq, data$sex_ffq, data$BMI),
                              method = "spearman")
partialCor$estimate
partialCor$p.value
partialCor$statistic
partialCor$n


# model 

test <- data %>%
  dplyr::filter(supplement_taker == "no")
model <- lm(vitamin_b12_ORQ ~ FFQ_B12_sum + BMI + age_ffq + as.factor(sex_ffq), data = test)

# residuals 
olsrr::ols_plot_resid_qq(model)
olsrr::ols_test_normality(model)
olsrr::ols_test_correlation(model)
olsrr::ols_plot_resid_fit(model)
olsrr::ols_plot_resid_hist(model)

# regression 
partial_regression <- car::avPlots(model, main = paste("Plasma B12 vs. Habitual Diet"))

tmp_corr <- cor.test(partial_regression$FFQ_B12_sum[,1], partial_regression$FFQ_B12_sum[,2])

ggplot(as.data.frame(partial_regression$FFQ_B12_sum)) + 
  aes(x = as.numeric(FFQ_B12_sum), y = vitamin_b12_ORQ) +
  geom_point() +
  geom_smooth(method = "lm", color ="black", se = TRUE, linetype = "dashed") + 
              #color = "cornflowerblue", se = TRUE, linetype = "dashed") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = expression(Habitual~vitamin~B[12]~"intake, " ~mu~g/d~ " | covariates")) +
  labs(y = expression(Plasma~vitamin~B[12]~"(normalized) | covariates"))

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/partial-corr-no-supps.pdf", height = 5, width = 8)

data$FFQ_B12_sum
data$plasma_b12
## habitual diet 


##### covariate relationships with response variable #####

data <- data %>%
  dplyr::mutate(., sex = ifelse(sex == 1, "Male", "Female"))

data <-rename(data, age = age_ffq)
data <-rename(data, sex = sex_ffq)
data <-rename(data, bmi = BMI)

tmp_inner <- data.frame()
tmp_outer <- data.frame()

data$normalized <- bestNormalize::bestNormalize(data$vitamin_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
print(bestNormalize::bestNormalize(data$vitamin_b12, allow_orderNorm = F, k = 10, r = 10)$chosen_transform)
  
model <- lm(normalized ~ as.numeric(bmi) + as.numeric(age) + as.factor(sex), data = data)
partial_regression <- car::avPlots(model, main = paste(data$vitamin_b12, "vs. Anthro"))
  
tmp_bmi <- cor.test(partial_regression$`as.numeric(bmi)`[,1], partial_regression$`as.numeric(bmi)`[,2])
tmp_inner_age <- data.frame(normalized_resp = partial_regression$`as.numeric(bmi)`[,2],
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
df_anthro$p_adjust <- p.adjust(df_anthro$p_value)
tmp_outer <- rbind(tmp_outer, tmp_inner_bmi, tmp_inner_age, tmp_inner_sex)
  
print(df_anthro)

##### plotting the partial correlations ##### 

# panel A 
plot1 <- ggplot(subset(tmp_outer, tmp_outer$predictor_name == "age")) + 
  aes(x = normalized_pred, y = normalized_resp, fill = "Royal1") + 
  geom_point(aes(fill = response_name), pch=21, colour="black", alpha = 0.75) +
  geom_smooth(aes(fill = response_name), method = "lm", color = "black", linetype = "dashed", se = TRUE) +
  scale_fill_manual(values = wesanderson::wes_palette("Royal1")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = "Age | Sex, BMI") +
  labs(y = expression(Plasma~vitamin~B[12]~"(normalized)")) +
  guides(color="none", fill = "none")


# panel B 
plot2 <- ggplot(subset(tmp_outer, tmp_outer$predictor_name == "bmi")) + 
  aes(x = normalized_pred, y = normalized_resp) + 
  geom_point(aes(fill = response_name), pch=21, colour="black", alpha = 0.75) +
  geom_smooth(aes(fill = response_name), method = "lm", color = "black", linetype = "dashed", se = TRUE) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1")) + 
  scale_fill_manual(values = wesanderson::wes_palette("Royal1")) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = "BMI | Age, Sex") +
  labs(y = expression(Plasma~vitamin~B[12]~"(normalized)")) +
  guides(color="none", fill = "none")
  
set.seed(123)
# transformation 
data$normalized_resp <- bestNormalize::bestNormalize(data$plasma_b12, allow_orderNorm = F, k = 10, r = 10)$x.t
data$normalized_pred <- bestNormalize::bestNormalize(data$FFQ_B12_sum, allow_orderNorm = F, k = 10, r = 10)$x.t
  
model <- lm(normalized_resp ~ normalized_pred + as.numeric(bmi) + as.numeric(age) + as.factor(sex), data = data)
partial_regression <- car::avPlots(model, main = paste("Plasma B12 vs. Habitual Diet"))
tmp_plasma_b12 <- cor.test(partial_regression$normalized_pred[,1], partial_regression$normalized_pred[,2])
  
plot5 <- ggplot(as.data.frame(partial_regression$normalized_pred)) + 
    aes(x = normalized_pred, y = normalized_resp, fill = pal) + 
    geom_point(pch=21, colour="black", alpha = 0.9) + 
    geom_smooth(method = "lm", color = "black", linetype = "dashed", se = TRUE) + 
    scale_fill_manual(values = wesanderson::wes_palette("Royal1")) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
    labs(x = expression(Habitual~dietary~B[12]~"(normalized) | covariates")) +
    labs(y = expression(Plasma~vitamin~B[12]~"(normalized) | covariates"))+ 
    guides(color="none", fill = "none") 
  
  
  

# customize the box plots 
boxplot_dat <- tmp_outer %>% dplyr::filter(., predictor_name == "sex") %>%
  dplyr::mutate(., sex_factor = ifelse(as.numeric(normalized_pred) > 0, "Male", "Female"))

boxplot_dat$sex_factor <- factor(boxplot_dat$sex_factor,
                                 levels = c('Male','Female'), ordered = TRUE)
  
# look for significant differences 
t_test(normalized_resp ~ sex_factor, data = boxplot_dat)

stat.test <- boxplot_dat %>%
    ungroup() %>%
    t_test(data = ., normalized_resp ~ sex_factor)%>%
    add_significance() %>%
    add_xy_position()

# panel C
plot3 <- boxplot_dat %>%
  ggplot(aes(x = sex_factor, y = normalized_resp)) + 
  geom_boxplot(aes(fill = sex_factor),outliers = FALSE) + 
  geom_point(aes(fill = sex_factor), pch=21, colour="black", position = position_jitter(0.25), alpha = 0.75) +
  stat_pvalue_manual(stat.test, label = "p.signif", tip.length = 0.01) +
  scale_fill_manual(values = wesanderson::wes_palette("Moonrise3")) + 
  scale_color_manual(values = wesanderson::wes_palette("Moonrise3")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = "black")) +
  labs(x = "Sex | BMI, Age") +
  labs(y =  expression(Plasma~vitamin~B[12]~"(normalized)")) +
  guides(fill="none", color = "none")
  


((pLeft / pLeft) &  
    theme(plot.tag.position  = c(.935, .96))) -
  
  ((pRight / pRight) & 
     theme(plot.tag.position  = c(.785, .96))) +
  
  plot_annotation(tag_levels = "A") 



# stitch all the plots together 

patchwork_plot = (free(plot1 | plot2 | plot3)) / plot4 | free(plot5) 

patchwork_plot & plot_annotation(tag_levels = "A")




theme(plot.tag.position = c(0, 1))

patchwork_plot + plot_annotation(tag_levels = 'A')
 
ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/figure1.pdf", height = 10, width = 16)




## n = 88 supplement users 
users <- data %>%  #taxaHFE_metadata %>%
  dplyr::select(subject_id, B12_boxcox, B12..pmol.L., age_ffq, sex_ffq, BMI, b12_tnfs_avg, FFQ_B12_sum, supplement_taker, sup_b12, dt_vb12, dt_b12ad, eGFR) %>%
  #dplyr::mutate(., sex_ffq = ifelse(sex_ffq == 1, "Male", "Female")) %>%
  filter(supplement_taker == "yes") %>%
  dplyr::select(-supplement_taker)

# n = 98 non-users 
nonusers <- taxaHFE_metadata %>%
  filter(supplement_taker == "no") %>%
  dplyr::select(-supplement_taker) %>%
  dplyr::select(-daily_supplemented_b12)

# write_delim(users, "/Users/local-margaret/Desktop/R-projects/FL100/data/taxaHFE_metadata_supplement_users.txt", delim = "\t",)
# write_delim(nonusers, "/Users/local-margaret/Desktop/R-projects/FL100/data/taxaHFE_metadata_supplement_non_users.txt", delim = "\t",)

## relationship of plasma B12 with age 
model1 <- lm(B12_boxcox ~ age_ffq + sex_ffq + BMI, data) 
model2 <- lm(B12_boxcox ~ age_ffq, data) 
summary(model1) ## not significant 
summary(model2) ## not significant 

## test normality of residuals
resids <- residuals(model1)
shapiro.test(resids) ## not normal 

resids <- residuals(model2)
shapiro.test(resids) ## normal residuals 

ggplot(data, aes(x = age_ffq, y = B12_boxcox)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() 

## table 1 statistic values 

quantile(data$age_ffq)
# 0%  25%  50%  75% 100% 
# 18   29   43   52   65 


ggplot(data, aes(x=age_ffq)) + 
  geom_histogram(color="black", fill="white", bins = 50) 

data %>%
  filter(supplement_taker == "no") %>%
  View()

# male = 1 
# female = 2 

library(scales)
ggplot(data, aes(x=B12..pmol.L.)) + 
  geom_histogram(color="black", fill="white", bins = 65) +
  geom_vline(aes(xintercept = 148), colour="blue", linetype="dashed") +
  geom_vline(aes(xintercept = 300), colour="blue", linetype="dashed") + 
  xlab(expression(Plasma~vitamin~B[12]~", pmol/L")) +
  scale_x_continuous(breaks = breaks_width(100)) + 
  theme_minimal()

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/plasma-b12-histogram.pdf", height = 5, width = 8)

ggplot(data, aes(x=FFQ_B12_sum)) + 
  geom_histogram(color="black", fill="white", bins = 175) +
  geom_vline(aes(xintercept = 2.4), colour="blue", linetype="dashed") +
  xlab(expression(Habitual~vitamin~B[12]~" intake, " ~mu~g/d)) +
  scale_x_continuous(breaks = breaks_width(100)) + 
  theme_minimal()

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/b12-intake-histogram.pdf", height = 5, width = 8)




