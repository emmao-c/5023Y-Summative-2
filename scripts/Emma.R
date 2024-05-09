#_____Packages--------
library(tidyverse)
library(here)
library(janitor)
library(kableExtra)
library(emmeans)
library (GGally)
library(performance)
library(see)
library(car)

#___Importing data---
probiotic <- read_csv(here::here( "data", "probiotic.csv"))

#___Check the structure of the data--- 
glimpse(probiotic)

#___Check data is in a tidy format---
head(probiotic)

#___Check for duplicated rows ---
probiotic %>% 
  duplicated() %>% 
  sum()

#___Find number of rows---
nrow(probiotic)

#___Checking column names---
colnames(probiotic)

#___Check for typos by looking at distinct characters/values---
probiotic %>% 
  distinct(gender)

probiotic %>% 
  distinct(group)

probiotic %>% 
  distinct(sample)


#___Checking for missing values and typos---                
probiotic %>% 
  is.na() %>% 
  sum()

unique(probiotic$group)
unique(probiotic$sample)
unique(probiotic$subject)
unique(probiotic$gender)

#___Changing gender,group and time to a factor--- 
probiotic$gender <- as.factor(probiotic$gender)
probiotic$group <- as.factor(probiotic$group)
probiotic$time <- as.factor(probiotic$time)

#___Piping probiotic dataset into new set seperating the values for number bacteria present into abundance before and after--- 
prob_sort <- probiotic %>%
  group_by(subject,gender,group)%>%
  summarise(abundance_before =ruminococcus_gnavus_abund[time ==1],
            abundance_after =ruminococcus_gnavus_abund[time==2])

#___Piping probiotic dataset into new set adding a column showing the difference in abundance 
prob_diff <- prob_sort %>%
  select(subject, gender, group,abundance_after,abundance_before)%>%
  mutate(abund_diff = abundance_after - abundance_before )
glimpse(prob_diff)

#____Univariate analysis-----  
#___Histogram showing the distribution of the abundance before the two treatments---  

hist(prob_diff$abundance_before)

#___Histogram showing the distribution of the abundance after the two treatments---  
hist(prob_diff$abundance_after)

#___Histogram showing distribution of the difference in abundance---  
hist(prob_diff$abund_diff)



      

#___Constructing models for analysis of data---   
      
model1<- lm(abund_diff ~ group + gender, data= prob_diff)
summary(model1)
plot(model1)
performance::check_model(model1, detrend = F)



model2<- lm(abund_diff ~  gender, data= prob_diff)
summary(model2)

model3 <- lm(abund_diff ~  gender, data= prob_diff)
summary(model3)











