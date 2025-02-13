
##### load libraries ######

#install.packages(c("tidyverse", "readxl", "data.table", "bestNormalize", "moments", "car", "nortest")
library(tidyverse) # data manipulation, mostly pipes 
library(readxl) # reading in excel formatted data 
library(data.table) # cleaning data 
library(bestNormalize) # data transformations 
library(moments) # skewness 
library(car) # Levines's test 
library(nortest) # Anderson-Darling test

##### read in data #####

## FFQ metadata 
FFQ_metadata <- read.csv("/Users/local-margaret/Desktop/R-projects/FL100/data/cleaned-data/clean_FL100_FFQ.csv") %>%
  as_tibble() %>%
  dplyr::select(c(subject_id, age_ffq, sex_ffq, pregnant, heightfeet, heightinches, weight)) %>% # n = 393 w/ NAs 
  na.exclude # n = 359 
  
## FFQ with plasma B12
FFQ <- readxl::read_xlsx("/Users/local-margaret/Desktop/R-projects/FL100/data/raw data/FL100_VitB12_FFQ_plasma.xlsx") %>%
  na.exclude %>% # n = 321 
  dplyr::left_join(FFQ_metadata, by = "subject_id") %>%
  # calculate BMI 
  mutate(total_height_inches = (heightfeet*12) + heightinches) %>%
  relocate(total_height_inches, .after = heightinches) %>%
  mutate(BMI = weight / (total_height_inches^2) * 702) %>%
  relocate(BMI, .after = weight) %>%
  dplyr::group_by(subject_id) %>%
  mutate(FFQ_B12_sum = dt_vb12 + dt_b12ad + sup_b12) %>%
  relocate(FFQ_B12_sum, .after = sup_b12) %>%
  ungroup() %>%
  mutate(B12_status = case_when(vitamin_b12 < 148 ~ "deficient",
                                vitamin_b12 >= 148 ~ "replete")) %>%
  mutate(supplement_taker = case_when(sup_b12 > 0 ~ "yes",
                                      sup_b12 == 0 ~ "no"))

# ASA24 vars of interest 
vitb12vars <- c("subject_id", "b12_add_tnfs2", "b12_add_tnfs3", "b12_add_tnfs4") 
  
## ASA24
ASA24 <- read.delim("/Users/local-margaret/Desktop/R-projects/FL100/data/ASA24/diet_data_AtLeast2atHomeRecallsQC.txt", sep = "\t") %>%
  as_tibble() %>%
  # select vars of interest
  dplyr::select(all_of(vitb12vars)) %>%
  # caluculate avgs for tnfs variables 
  group_by(subject_id) %>%
  mutate(b12_tnfs_avg = (b12_add_tnfs2 + b12_add_tnfs3 + b12_add_tnfs4)/3) %>%
  # if the average is na then mutate again and take
  # the average of the first two observations only
  mutate(b12_tnfs_avg = case_when(
    is.na(b12_tnfs_avg) ~ ((b12_add_tnfs2 + b12_add_tnfs3)/2),
    b12_tnfs_avg != "NA" ~ b12_tnfs_avg)) %>%
  right_join(FFQ) %>%
  na.exclude
  
## eGFR excretion variable (from Stephanie M.G. Wilson on 12/16/2024)  
data <- read.csv("/Users/local-margaret/Desktop/R-projects/FL100/data/FL100_eGFR.csv", header = T) %>%
  dplyr::select(c(subject_id, eGFR)) %>%
  right_join(ASA24) 
 
## old plasma B12 data - archive 
# B12 <- read_xlsx("/Users/local-margaret/Desktop/R-projects/FL100/data/B12-plasma/2024-08-16-FL100 B12 and folate results and Stnds and Utak.xlsx")
# setnames(B12, "FL100 ID", "subject_id") # rename column "FL100 ID" to "subject_id"

##### visualize distributions and normality of data ##### 

## scatter plot 
ggplot(data, aes(x=as.factor(subject_id), y=vitamin_b12)) + 
  geom_point()

## histogram 
ggplot(data, aes(x=vitamin_b12)) + 
  geom_histogram(color="black", fill="white", bins = 75) + 
  geom_vline(aes(xintercept= 150), colour="blue", linetype="dashed") + 
  geom_vline(aes(xintercept= 300), colour="blue", linetype="dashed") + 
  theme(axis.text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1550)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22))

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/plasma-B12-histogram.pdf")

## Q-Q plot 
qqnorm(data$vitamin_b12)
qqline(data$vitamin_b12, col = 2)

## Shapiro-Wilk test
shapiro.test(data$vitamin_b12) # if p < 0.05, reject the null hypothesis that the distribution of these data are normal
# W = 0.81282, p-value < 2.2e-16

## Anderson-Darling test
ad.test(data$vitamin_b12)
# A = 10.181, p-value < 2.2e-16
# the serum B12 data are not normal, but that's fine. options are to:
# a) use non-parametric statistical methods or 
# b) transform data to conform to normality assumption

##### transforming data with bestNormalize #####

## response transformation
set.seed(0127)
bestNormalize(data$vitamin_b12) # suggests Box-Cox
data$vitamin_b12_ORQ <- (bestNormalize::orderNorm(data$vitamin_b12))$x.t 
data <- data %>%
  relocate(vitamin_b12_ORQ, .after = vitamin_b12)

## repeat normalization checks with transformed serum B12 variable 

## scatter plot 
ggplot(data, aes(x=as.factor(subject_id), y=vitamin_b12_ORQ)) + geom_point()

## histogram 
hist(data$vitamin_b12_ORQ)

## Q-Q plot 
qqnorm(data$vitamin_b12_ORQ)
qqline(data$vitamin_b12_ORQ, col = 2)

## Shapiro-Wilk test
shapiro.test(data$vitamin_b12_ORQ) # if p < 0.05, reject the null hypothesis that the distribution of these data are normal
# W = 0.9993, p-value = 1

## Anderson-Darling test from "nortest" package
ad.test(data$vitamin_b12_ORQ)
# A = 0.0084271, p-value = 1; check skewness and kurtosis before moving on

## skewness ; theoretically = 0 in normal distribution ; measures asymmetry 
skewness(data$vitamin_b12_ORQ)
# [1] -0.00605622
## kurtosis ; theoretically = 3 in normal distribution ; measures degree of extreme outliers 
kurtosis(data$vitamin_b12_ORQ)
# [1] 2.897994

## Levene's Test for Homogeneity of Variance

leveneTest(vitamin_b12 ~ as.factor(supplement_taker), data = data) # p = 0.003696 **

leveneTest(vitamin_b12_ORQ ~ as.factor(supplement_taker), data = data) # p = 0.9449

##### save the data #####

## save this file 
# write_csv(data, "/Users/local-margaret/Desktop/R-projects/FL100/data/cleaned-data/UPDATED_B12_with_BMI.csv")


