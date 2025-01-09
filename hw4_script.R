library(tidyverse)
survey <- read_csv("survey.csv")
head(survey)
df <- survey %>% select(lifsat1:lifsat5, pss1:pss10)
head(df)

library(psych)
library(GPArotation)
efa <- psych::fa(df, nfactors = 2, rotate = "oblimin", fm = "pa" )
efa
efa$e.values
#we can see that eigenvalues are markedly above 1 only for the first two factors. 

efa$communality
efa$loadings
efa$Phi

efa3 <- psych::fa(df, nfactors = 3, rotate = "oblimin", fm = "pa" )
efa3
efa3$loadings
