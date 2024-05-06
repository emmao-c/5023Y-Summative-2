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

#Find number of rows
nrow(prob_diff)

# check for typos by looking at distinct characters/values
probiotic %>% 
  distinct(gender)

probiotic %>% 
  distinct(group)

probiotic %>% 
  distinct(sample)


#___Checking for missing values---                
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
  select(subject, gender, group,abundance_after,abundance_before)%>%
  mutate(abund_diff = abundance_after - abundance_before )

#____Mono variate analysis-----  

#Boxplot of the groups against the abundance after   
ggplot(data = prob_diff, aes(x = group, y = abundance_after)) +
  geom_boxplot(aes(fill = gender),
               alpha = 0.7, 
               width = 0.5, # change width of boxplot
               show.legend = TRUE)

# OUTPUT FIGURE TO FILE

ggsave("figures/boxplot_of_groups_abundance_after.png", dpi=300)

#Boxplot of the groups against the abundance before   

        ggplot(data = prob_diff, aes(x = group, y = abundance_before)) +
  geom_boxplot(aes(fill = gender),
               alpha = 0.7, 
               width = 0.5, # change width of boxplot
               show.legend = TRUE)
# OUTPUT FIGURE TO FILE
        
ggsave("figures/boxplot_of_groups_abundance_before.png", dpi=300)

#Boxplot of the groups against the the difference in abundance    
        
      ggplot(data = prob_diff, aes(x = group, y = abund_diff)) +
  geom_boxplot(aes(fill = gender),
                       alpha = 0.7, 
                       width = 0.5, # change width of boxplot
                       show.legend = TRUE) 
# OUTPUT FIGURE TO FILE
      
ggsave("figures/boxplot_of_groups_abundance_before.png", dpi=300)
# Histogram showing the group values and the difference       
      prob_diff %>% 
        ggplot(aes(x=abundance_before))+
        geom_histogram(bins=30, 
                       aes(y=..density..,
                           fill=group), 
                       position = "identity",
                       colour="black")
      
      prob_diff %>% 
        ggplot(aes(x=abundance_after))+
        geom_histogram(bins=30, 
                       aes(y=..density..,
                           fill=group), 
                       position = "identity",
                       colour="black")
      
      
lsmodel0 <- lm(formula = abund_diff  ~ 1, data = prob_diff)
summary(lsmodel0)

lsmodel1<- lm(abund_diff ~ group, data= prob_diff)
summary(lsmodel1)
anova (lsmodel1)

GGally::ggpairs(prob_diff,
                aes(colour = group))


prob_no_sub <- prob_diff %>%
  group_by(gender,group,abundance_after,abundance_before,)%>%
  summarise(abundance_before =ruminococcus_gnavus_abund[time ==1],
            abundance_after =ruminococcus_gnavus_abund[time==2])






