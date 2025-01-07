library(readr)
library(dplyr)
library(ggplot2)
library(broom)
data <- read_csv("kidney_stone_data.csv")
head(data)
str(data)

#calculate the number and frequency of success and faulure of each treatment 
treatment_success <- data %>% group_by(treatment, success) %>% 
  summarise(N = n()) %>% 
  mutate(Freq = round(N/sum(N), 3))
treatment_success

#calculate the number and frequency of success and faulture by stone size for
#each treatment 
sum_data <- data %>% group_by(treatment, stone_size, success) %>% 
  summarize(N = n()) %>% 
  mutate( Freq = round(N/sum(N), 3))
sum_data

#Run a chi-square test 
trt_ss <- chisq.test(data$treatment, data$stone_size)
tidy(trt_ss)

#Run a multiple logistic regression 
logistic_model <- glm(data = data, 
                      success ~ stone_size + treatment, 
                      family = "binomial")
tidy(logistic_model)
tidy_m <- tidy(logistic_model)

#is small stone more likely to be a success after controlling for 
#treatment option effect? 
small_high_success <- "Yes"

#Is treatment A significantly better than B? 
A_B_sig <- "No"

