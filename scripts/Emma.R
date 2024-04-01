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
