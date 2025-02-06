##### load libraries #####

# install.packages(c("tidyverse", "plotrix", "rstatix", "ggpubr", "ggbreak", "ppcor"))
library(tidyverse) # data tidying 
library(plotrix) # for calculating SEM 
library(rstatix) # boxplot p-vals 
library(ggpubr) # plotting p-vals 
library(ggbreak) # ggplot axis breaks
library(ppcor) # partial correlations 

##### read in data #####

vb12 <- read.csv("/Users/local-margaret/Desktop/R-projects/FL100/data/cleaned-data/B12_with_BMI.csv")
excretion <- read.csv("/Users/local-margaret/Desktop/R-projects/FL100/data/FL100_eGFR.csv", header = T) %>%
  dplyr::select(c(subject_id, eGFR))

## merge these
data <- left_join(vb12, excretion, by = "subject_id") 

##### data tidying #####

## average tnfs from ASA24 data 
data <- data %>%
  group_by(subject_id) %>%
  mutate(b12_tnfs_avg = (b12_add_tnfs2 + b12_add_tnfs3 + b12_add_tnfs4)/3) %>%
  # if the average is na then mutate again and take
  # the average of the first two observations only
  mutate(b12_tnfs_avg = case_when(
    is.na(b12_tnfs_avg) ~ ((b12_add_tnfs2 + b12_add_tnfs3)/2),
    b12_tnfs_avg != "NA" ~ b12_tnfs_avg)) %>%
  # add the dt_vb1 (naturally occuring),
  # sup_b12 (from supplements), and 
  # dt_b12ad (fortified foods) in FFQ to get 
  # an equivalent of ASA24 tnfs_avg variable 
  mutate(FFQ_B12_sum = dt_vb12 + dt_b12ad + sup_b12)

## define categorical variables of interest
data <- data %>%
  mutate(B12_status = case_when(B12..pmol.L. < 148 ~ "deficient",
                                B12..pmol.L. >= 148 ~ "replete")) %>%
  mutate(supplement_taker = case_when(sup_b12 > 0 ~ "yes",
                                      sup_b12 == 0 ~ "no"))

##### basic statistics and visualization #####

## n clinically deficient 
data %>%
  filter(B12..pmol.L. < 148) %>%
  View()

## ASA24 recent intake 
mean(data$b12_tnfs_avg)
std.error(data$b12_tnfs_avg)

## FFQ habitual intake 
mean(data$FFQ_B12_sum) ## this is very high!
std.error(data$FFQ_B12_sum) 

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

## plasma B12 
mean(data$B12..pmol.L.)
std.error(data$B12..pmol.L.)

##### supplement use impacts intake and status ##### 

## quick check to see if supplement users have 
## higher plasma B12 than non-users 

## mann whitney U test 
data$supplement_taker <- as.factor(data$supplement_taker)

stat.test <- data %>%
  ungroup() %>%
  wilcox_test(data = ., B12..pmol.L. ~ supplement_taker)%>%
  add_significance() %>%
  add_xy_position()

wilcox.test(B12..pmol.L. ~ supplement_taker, data = data, exact = FALSE)

ggplot(data, aes(x=supplement_taker, y=B12..pmol.L., colour = supplement_taker)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.75) +
  scale_colour_manual(values = c("black", "red")) +
  labs(color='Supplement use') +
  theme_bw() + 
  geom_hline(aes(yintercept = 148), colour="blue", linetype="dashed") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  stat_pvalue_manual(stat.test, label = "Wilcoxon, p = {p}", y.position = 1600) +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.position = "top",
        legend.box.background = element_rect(colour = "black")) +
  labs(x = "Supplement use", y = expression(Plasma~vitamin~B[12]~", pmol/L"))

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/difference-in-mean-plasma_B12~supp_use2.pdf", height = 5, width = 8)

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
  scale_y_break(c(148, 289), scales= 0.25, ticklabels=c(300, 400, 500)) +
  scale_y_break(c(500, 1060), scales= 0.25, ticklabels=c(1060, 1065, 1070)) 

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/difference-in-habitual-B12-intake~supp_use.pdf", height = 5, width = 8)

##### partial correlations #####

partialCor <- pcor.test(x=data$FFQ_B12_sum, y=data$B12..pmol.L.,
                              z=c(data$age_ffq, data$sex_ffq, data$BMI),
                              method = "spearman")
partialCor$estimate
partialCor$p.value
partialCor$statistic
partialCor$n

##### filter data for taxaHFE metadata file ##### 

taxaHFE_metadata <- data %>%
  dplyr::select(subject_id, B12..pmol.L., age_ffq, sex_ffq, BMI, B12_status, b12_tnfs_avg, FFQ_B12_sum, supplement_taker, sup_b12, dt_vb12, dt_b12ad, eGFR) %>%
  dplyr::mutate(., sex_ffq = ifelse(sex_ffq == 1, "Male", "Female"))

taxaHFE_metadata <-rename(taxaHFE_metadata, plasma_b12 = B12..pmol.L.)
taxaHFE_metadata <-rename(taxaHFE_metadata, age = age_ffq)
taxaHFE_metadata <-rename(taxaHFE_metadata, sex = sex_ffq)
taxaHFE_metadata <-rename(taxaHFE_metadata, bmi = BMI)
taxaHFE_metadata <-rename(taxaHFE_metadata, b12_status = B12_status)
taxaHFE_metadata <-rename(taxaHFE_metadata, recent_dietary_b12_avg = b12_tnfs_avg)
taxaHFE_metadata <-rename(taxaHFE_metadata, habitual_dietary_b12 = FFQ_B12_sum)
taxaHFE_metadata <-rename(taxaHFE_metadata, daily_supplemented_b12 = sup_b12)

# write_delim(taxaHFE_metadata, "/Users/local-margaret/Desktop/R-projects/FL100/taxaHFE_metadata.txt", delim = "\t")

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
