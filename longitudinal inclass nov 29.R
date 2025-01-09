library(tidyverse)
library(haven)
df_wide <- read_sav("2006 GSS Panel Data.sav")
head(df_wide)
dim(df_wide)
df_long <- pivot_longer(df_wide, cols = c("satjob_1", "satjob_2", "satjob_3"),
                  names_to = "wave", values_to = "job_satis")
dim(df_long)

library(car)
df_long$wave <- factor(df_long$wave)
leveneTest(y = df_long$job_satis, group =df_long$wave)
#p = 0.73 i.e. not significant, which means groups have homogeneity of variance. 

mean(is.na(df_long$job_satis)) 
# 41.11 % of observations have missing values on this variable! :( 
library(heplots)
class(df_long$job_satis)
df_long$job_satis <- as.numeric(df_long$job_satis)
boxM(Y = df_long$job_satis, group = df_long$wave)
