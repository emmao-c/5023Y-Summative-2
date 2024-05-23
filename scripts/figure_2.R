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
library(rstatix)
library(corrplot)
library(lmtest)

#___Importing data------
probiotic <- read_csv(here::here( "data", "probiotic.csv"))
#___Changing gender,group and time to a factor------
probiotic$gender <- as.factor(probiotic$gender)
probiotic$group <- as.factor(probiotic$group)
probiotic$time <- as.factor(probiotic$time)

probiotic1 <- probiotic [-41,]
probiotic3 <- probiotic1 [-41,]
#___Piping probiotic dataset into new set seperating the values for number bacteria present into abundance before and after----- 
prob_sort <- probiotic %>%
  group_by(subject,gender,group)%>%
  summarise(abundance_before =ruminococcus_gnavus_abund[time ==1],
            abundance_after =ruminococcus_gnavus_abund[time==2])

#___Piping probiotic dataset into new set adding a column showing the difference in abundance---- 
prob_diff <- prob_sort %>%
  select(subject, gender, group,abundance_after,abundance_before)%>%
  mutate(abund_diff = abundance_after - abundance_before )
glimpse(prob_diff)


pal <- c( "#A034F0", "#159700")

fig2 <- probiotic3 %>% 
  ggplot(aes(x = time,
             y = ruminococcus_gnavus_abund,
             fill = group,
             colour = group))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "",
    y = "Ruminococcus Abundance",
    title = "The difference in the reduction of pathogenic bacteria ",
    subtitle = "Box and violin plot of the differnce in R gnavus population according to treatment")+
  facet_wrap(~group)

print(fig2)
