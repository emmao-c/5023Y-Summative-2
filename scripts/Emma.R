#_____Packages--------
library(tidyverse)
library(here)
library(janitor)
library(kableExtra)
library(emmeans)
library (GGally)
probiotic <- read_csv(here::here( "data", "probiotic.csv"))

#___check the structure of the data--- 
glimpse(probiotic)

#____check data is in a tidy format----- 
head(probiotic)

#____ Check for duplcaited rows ---- 
probiotic %>% 
  duplicated() %>% 
  sum()

# check for typos by looking at distinct characters/values
probiotic %>% 
  distinct(gender)

probiotic %>% 
  distinct(group)

probiotic %>% 
  distinct(sample)


# checking for missing values
probiotic %>% 
  is.na() %>% 
  sum()

#Changing gender to a factor 
probiotic$gender <- as.factor(probiotic$gender)
probiotic$group <- as.factor(probiotic$group)
probiotic$time <- as.factor(probiotic$time)


prob_sort <- probiotic %>%
  group_by(subject,gender,group)%>%
  summarise(abundance_before =ruminococcus_gnavus_abund[time ==1],
            abundance_after =ruminococcus_gnavus_abund[time==2])

prob_diff <- prob_sort %>%
  select(subject, gender, group,abundance_after,abundance_before) %>%
  mutate(abund_diff = abundance_after - abundance_before )

#Monovariate analysis 


ggplot(data = prob_diff, aes(x = group, y = abundance_after)) +
  geom_boxplot(aes(fill = gender),
               alpha = 0.7, 
               width = 0.5, # change width of boxplot
               show.legend = FALSE)

ggplot(data = prob_diff, aes(x = group, y = abundance_before)) +
  geom_boxplot(aes(fill = gender),
               alpha = 0.7, 
               width = 0.5, # change width of boxplot
               show.legend = FALSE)


















