
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

## FFQ
FFQ <- read.csv("/Users/local-margaret/Desktop/R-projects/FL100/data/cleaned-data/clean_FL100_FFQ.csv") %>%
  as_tibble()
head(FFQ)
typeof(FFQ$subject_id)
FFQ$subject_id <- as.factor(FFQ$subject_id) # set subject_id data type as factor 

## ASA24
ASA24 <- read.delim("/Users/local-margaret/Desktop/R-projects/FL100/data/ASA24/diet_data_AtLeast2atHomeRecallsQC.txt", sep = "\t") %>%
  as_tibble()
head(ASA24)
typeof(ASA24$subject_id)
ASA24$subject_id <- as.factor(ASA24$subject_id) # set subject_id data type as factor 

## plasma B12
B12 <- read_xlsx("/Users/local-margaret/Desktop/R-projects/FL100/data/B12-plasma/2024-08-16-FL100 B12 and folate results and Stnds and Utak.xlsx")
head(B12)

setnames(B12, "FL100 ID", "subject_id") # rename column "FL100 ID" to "subject_id"
typeof(B12$subject_id)
B12$subject_id <- as.factor(B12$subject_id) # set subject_id data type as factor 

##### subset data #####

## retain FFQ data entry if subject ID is in the B12 data  
B12 # A tibble: 191 × 4 ; observation 191 is a control, so we are looking to retain 190 FFQ observations 
subset_FFQ_test <-FFQ %>% 
  filter(subject_id %in% unique(B12$subject_id)) # A tibble: 190 × 1,519 ; good to go
subset_FFQ <- FFQ %>% # A tibble: 190 × 1,522
  inner_join(B12, by="subject_id") %>%  # an inner_join() only keeps observations from x that have a matching key in y
  arrange(subject_id)  

## retain ASA24 data entry if subject ID is in the B12 data 
ASA24 # A tibble: 378 × 879
sample_ids_ASA24 <- ASA24 %>% 
  filter(subject_id %in% unique(B12$subject_id)) %>% 
  select(subject_id) %>%
  arrange(subject_id) # A tibble: 187 × 1 ; there are only 187 subject ids that match the B12/FFQ dataset 

sample_ids_ASA24$subject_id <- as.factor(sample_ids_ASA24$subject_id)

## since n observations was reduced to 187, investigate which sample IDs are not in the ASA24 data
sample_ids_FFQ <- FFQ %>% 
  filter(subject_id %in% unique(B12$subject_id)) %>% 
  select(subject_id) %>%
  arrange(subject_id) # A tibble: 190 × 1,519
difference = setdiff(sample_ids_FFQ$subject_id, sample_ids_ASA24$subject_id) 

print(difference)
#[1] "7108" "8088" "9059"

subset_FFQ_ASA24 <- subset_FFQ %>% 
  inner_join(ASA24, by="subject_id") %>%  # an inner_join() only keeps observations from x that have a matching key in y
  arrange(subject_id) # A tibble: 187 × 2,400

##### identify variables of interest for vitamin B12 status ##### 

vitb12vars <- c("subject_id", "B12 (pmol/L)", "Folate (nmol/L)", #B12
                "sex_ffq", "pregnant", "age_ffq", "weight", "heightfeet", "heightinches", # some co-variates
                "vitaminb12amount.factor", "vitaminb12years.factor","dt_vb12", "dt_b12ad", "usevitsregularly.factor", "prenatalvitsamount", "prenatalvitsyears", "sup_b12", "bcomplextypevitsamount.factor", "bcomplextypevitsyears", "block_ffq_complete.factor", # FFQ
                "b12_add_tnfs2", "b12_add_tnfs3", "b12_add_tnfs4") #ASA24 
tab <- subset_FFQ_ASA24[,vitb12vars]
nrow(tab)
#[1] 187

## exclude observations without 'Complete' block FFQ variable 
vitb12.complete <- tab[tab$block_ffq_complete.factor == "Complete",]
nrow(vitb12.complete)
#[1] 186
# write_csv(vitb12.complete, "data/output/VitB12_FL100_complete.csv")

## write a list of sample_ids that match the criteria a) data for B12, FFQ, ASA24 (at least two visits) b) block FFQ = complete c) no n/a values in B12 measurement 
sample_list <- vitb12.complete %>%
  select(subject_id) 
print(sample_list) # should be n = 186 
colnames(sample_list) <- c("#subject_id")

# write_delim(sample_list, "data/output/subject_id_list.txt")

## remove any  observations with NA values in B12 quantification column, if applicable 
vitb12.complete <- vitb12.complete %>% drop_na(`B12 (pmol/L)`)
nrow(vitb12.complete) # in this case there were none 
#[1] 186 

##### visualize distributions and normality of data ##### 

## scatter plot 
ggplot(vitb12.complete, aes(x=subject_id, y=`B12 (pmol/L)`)) + 
  geom_point()

## histogram 
ggplot(vitb12.complete, aes(x=`B12 (pmol/L)`)) + 
  geom_histogram(color="black", fill="white", bins = 75) + 
  geom_vline(aes(xintercept= 150), colour="blue", linetype="dashed") + 
  geom_vline(aes(xintercept= 300), colour="blue", linetype="dashed") + 
  theme(axis.text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1550)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20))

# ggsave("/Users/local-margaret/Desktop/R-projects/FL100/figures/plasma-B12-histogram.pdf")

## Q-Q plot 
qqnorm(vitb12.complete$`B12 (pmol/L)`)
qqline(vitb12.complete$`B12 (pmol/L)`, col = 2)

## Shapiro-Wilk test
shapiro.test(vitb12.complete$`B12 (pmol/L)`) # if p < 0.05, reject the null hypothesis that the distribution of these data are normal
# W = 0.80423, p-value = 1.589e-14

## Anderson-Darling test
ad.test(vitb12.complete$`B12 (pmol/L)`)
# A = 7.735, p-value < 2.2e-16
# the serum B12 data are not normal, but that's fine. options are to:
# a) use non-parametric statistical methods or 
# b) transform data to conform to normality assumption

##### transforming data with bestNormalize #####

## response transformation
set.seed(0127)
bestNormalize(vitb12.complete$`B12 (pmol/L)`) # suggests Box-Cox
vitb12.complete$B12_boxcox <- (bestNormalize::boxcox(vitb12.complete$`B12 (pmol/L)`))$x.t 
vitb12.complete <- vitb12.complete %>%
  relocate(B12_boxcox, .after = `B12 (pmol/L)`)

## repeat normalization checks with transformed serum B12 variable 

## scatter plot 
ggplot(vitb12.complete, aes(x=subject_id, y=B12_boxcox)) + geom_point()

## histogram 
hist(vitb12.complete$B12_boxcox)

## Q-Q plot 
qqnorm(vitb12.complete$B12_boxcox)
qqline(vitb12.complete$B12_boxcox, col = 2)

## Shapiro-Wilk test
shapiro.test(vitb12.complete$B12_boxcox) # if p < 0.05, reject the null hypothesis that the distribution of these data are normal
# W = 0.99066, p-value = 0.2692

## Anderson-Darling test from "nortest" package
ad.test(vitb12.complete$B12_boxcox)
# A = 0.43674, p-value = 0.294 ; check skewness and kurtosis before moving on

## skewness ; theoretically = 0 in normal distribution ; measures asymmetry 
skewness(vitb12.complete$B12_boxcox)
# [1] -0.01279851
## kurtosis ; theoretically = 3 in normal distribution ; measures degree of extreme outliers 
kurtosis(vitb12.complete$B12_boxcox)
# [1] 3.489463

## homogeneity of variance 
## box/dot plot (visual check)
ggplot2::ggplot(vitb12.complete, aes(x=vitaminb12amount.factor, y=B12_boxcox)) + 
  geom_boxplot() +
  geom_point(shape = 16, colour = "red", size = 2.5, alpha = 0.3) + 
  xlab("Frequency of Supplemental Vitamin B12") +
  ylab("Box Cox Transformed Serum B12") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = .8))

## Info from the geom_boxplot() documentation about the hinges and whiskers: 
## The lower and upper hinges correspond to the 25th and 75th percentiles.
## The upper and lower whisker extends from the hinge to the largest or smallest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles).
## Data beyond the end of the whiskers are called "outlying" points and are plotted individually.

## Levene's Test for Homogeneity of Variance
leveneTest(vitb12.complete$`B12 (pmol/L)`, group = vitb12.complete$vitaminb12amount.factor, data = vitb12.complete)
leveneTest(vitb12.complete$B12_boxcox, group = vitb12.complete$vitaminb12amount.factor, data = vitb12.complete)

##### BMI calculation #####
## since BMI might be a covariate, calculate BMI based on height and weight
vitb12.complete_withBMI <- vitb12.complete %>%
  mutate(total_height_inches = (heightfeet*12) + heightinches) %>%
  relocate(total_height_inches, .after = heightinches) %>%
  mutate(BMI = weight / (total_height_inches^2) * 702) %>%
  relocate(BMI, .after = weight)

## function that calculates BMI classification/category from: https://stackoverflow.com/questions/72706511/create-function-to-categorize-bmi-in-multiple-dataframes-in-r
bmi_categories <- function(bmi, age) {
  category = factor(rep(NA,length(bmi)), levels = c("Underweight","Normal Weight","Overweight","Obese")) # NA as default value, you could set "" as default, but then you should also add "" to the vector of levels
  
  category[bmi<18.5 & age>6] <- "Underweight"
  category[18.5<=bmi & bmi<25 & age>6] <- "Normal Weight"
  category[25<=bmi & bmi<30 & age>6] <- "Overweight"
  category[30<=bmi & age>6] <- "Obese"
  
  return(category)
}

## define the BMI and age variables in your data frame to use the BMI category function
vitb12.complete_withBMI$BMI_class <- bmi_categories(vitb12.complete_withBMI$BMI, vitb12.complete_withBMI$age_ffq)

vitb12.complete_withBMI <- vitb12.complete_withBMI %>%
  relocate(BMI_class, .after = BMI) %>% # puts this new variable in a place that makes sense 
  print()

## look at BMI category distribution with a bar chart (counts)
ggplot(vitb12.complete_withBMI, aes(x=BMI_class)) +
  geom_bar()

## look at age distribution with a bar chart (counts)
ggplot(vitb12.complete_withBMI, aes(x=age_ffq)) +
  geom_bar()

## save this file 
# write.csv(vitb12.complete_withBMI, "/Users/local-margaret/Desktop/R-projects/FL100/data/cleaned-data/B12_with_BMI.csv")