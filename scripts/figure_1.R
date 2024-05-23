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

#___Piping probiotic dataset into new set adding a column showing the difference in abundance---- 
prob_diff <- prob_sort %>%
  select(subject, gender, group,abundance_after,abundance_before)%>%
  mutate(abund_diff = abundance_after - abundance_before )
glimpse(prob_diff)

#___ Checking whether the gender and group are equally weighted---- 

prob_diff [-14,] %>% 
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  mutate(prob_diff = n/sum(n))

prob_diff [-14,] %>% 
  ggplot(aes(x=group, fill=gender))+
  geom_bar(position=position_dodge2(preserve="single"))+ 
  #keeps bars to appropriate widths
  coord_flip()

group_gender_summary <- prob_diff [-14,] %>% 
  group_by(group, gender) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(subject)) %>% 
  ungroup() %>% # needed to remove group calculations
  mutate(freq=n/sum(n)) # then calculates percentage of each group across WHOLE dataset

group_gender_summary
fig1<-prob_diff [-14,] %>% 
  ggplot(aes(x=group, fill=gender))+
  geom_bar(position=position_dodge2(preserve="single"))+ 
  #keeps bars to appropriate widths
  labs(x="Treatment group",
       y = "Abundance of pathogenic bacteria")+
  geom_text(data = group_gender_summary, # use the data from the summarise object
            aes(x=group,
                y= n+1, # offset text to be slightly to the right of bar
                group=gender, # need species group to separate text
                label=scales::percent(freq) # automatically add %
            ),
            position=position_dodge2(width=0.8))+ # set width of dodge
  scale_fill_manual(values=c("green4",
                             "purple"
  ),labels = c("Female", "Male"), name = c("Gender"))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom",text = element_text(size = 9),element_line(size =1))+
  ggtitle(label = "Distribution of each gender within each treatment group",
          subtitle = "From a total of 21 stool samples, in order to investigate a possible sampling bias") # put legend at the bottom of the graph
# OUTPUT FIGURE TO FILE

print(fig1)