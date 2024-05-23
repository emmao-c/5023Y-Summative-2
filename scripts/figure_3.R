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

#___Piping probiotic dataset into new set seperating the values for number bacteria present into abundance before and after----- 
prob_sort <- probiotic %>%
  group_by(subject,gender,group)%>%
  summarise(abundance_before =ruminococcus_gnavus_abund[time ==1],
            abundance_after =ruminococcus_gnavus_abund[time==2])

total_title <- expression(paste("The difference in ", italic("Ruminococcus ganvus"), " abundance after treatment with either LGG or a placebo group"))
total_subtitle <- expression(paste("Count of ", italic("R. ganvus"), " from Stool Samples of 21 Subjects"))
y <- expression(paste(italic("R. ganvus"), "Abundance"))

probiotic1 <- probiotic [-41,]
probiotic3 <- probiotic1 [-41,]

pal <- c( "#A034F0", "#159700")

fig3 <- probiotic3 %>% 
  ggplot(aes(x = time,
             y = ruminococcus_gnavus_abund,
             fill = group,
             colour = group))+  
  geom_jitter(position = position_jitter(seed = 1, width = 0.2)) +
  
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = pal)+
  scale_colour_manual(values = pal)+ 
  labs(y = y,
       x = "Sampling Time",
       title= total_title, 
       subtitle = total_subtitle)+
  theme_classic()+
  theme(legend.position = "none")+
  facet_grid(gender~group)
print(fig3)


