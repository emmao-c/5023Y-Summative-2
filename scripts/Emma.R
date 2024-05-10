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

#___ Checking whether the groups are equally weighted --- 
prob_diff %>%
  group_by(group) %>%
  summarise(n = n())

#___ Checking whether the gender and group are equally weighted --- 

prob_diff %>% 
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  mutate(prob_diff = n/sum(n))

prob_diff%>% 
  ggplot(aes(x=group, fill=gender))+
  geom_bar(position=position_dodge2(preserve="single"))+ 
  #keeps bars to appropriate widths
  coord_flip()

group_gender_summary <- prob_diff %>% 
  group_by(group, gender) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(subject)) %>% 
  ungroup() %>% # needed to remove group calculations
  mutate(freq=n/sum(n)) # then calculates percentage of each group across WHOLE dataset

group_gender_summary

group_gender_summary_graph <- prob_diff %>% 
  ggplot(aes(x=group, fill=gender))+
  geom_bar(position=position_dodge2(preserve="single"))+ 
  #keeps bars to appropriate widths
  labs(x="Group",
       y = "Abundance of pathogenic bacteria")+
  geom_text(data = group_gender_summary, # use the data from the summarise object
            aes(x=group,
                y= n+10, # offset text to be slightly to the right of bar
                group=gender, # need species group to separate text
                label=scales::percent(freq) # automatically add %
            ),
            position=position_dodge2(width=0.8))+ # set width of dodge
  scale_fill_manual(values=c("cyan",
                             "purple"
  ))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom") # put legend at the bottom of the graph


#____Univariate analysis-----  
#___Histogram showing the distribution of the abundance before the two treatments---  

hist(prob_diff$abundance_before)

#___Histogram showing the distribution of the abundance after the two treatments---  
hist(prob_diff$abundance_after)

#___Histogram showing distribution of the difference in abundance---  
hist(prob_diff$abund_diff)

#___Scatter plot to show the abundance difference seperated by group and gender ---
ggplot(prob_diff, aes(x = group,
                            y = abund_diff))+
  geom_point(alpha = 0.4,
             aes(colour = gender))+
  scale_color_brewer(palette = "Dark2")+
  theme_light()
#___Scatter plot to show the abundance before seperated by group and gender ---

ggplot(prob_diff, aes(x = group,
                      y = abundance_before))+
  geom_point(alpha = 0.4,
             aes(colour = gender))+
  scale_color_brewer(palette = "Dark2")+
  theme_light()
#___Scatter plot to show the abundance after seperated by group and gender ---

ggplot(prob_diff, aes(x = group,
                      y = abundance_after))+
  geom_point(alpha = 0.4,
             aes(colour = gender))+
  scale_color_brewer(palette = "Dark2")+
  theme_light()



#___Constructing models for analysis of data---   
      
model1<- lm(abund_diff ~ group + gender, data= prob_diff)
summary(model1)
plot(model1)
performance::check_model(model1, detrend = F)


model2<- lm(abundance_before ~  group + gender, data= prob_diff)
summary(model2)

model3 <- lm(abundance_after ~  group + gender, data= prob_diff)
summary(model3)

#___ Using Cooks distance model to identify the outlier effects---
plot(model1, which = c(4,4))
plot(model2, which = c(4,4))
plot(model3, which = c(4,4))

#___ Seeing if there is significant leverage from the outlier found in the Cooks test ---
prob_diff [14,]

#___ maybe run a frop variable --- 
model4 <- lm(abund_diff ~ group + gender, data= prob_diff[-14,])
summary(model4)
performance::check_model(model4, detrend = F)


#___Breusch Pagan test --- 
lmtest::bptest(model4)

#___ Shapiro wilks test, the residuals do not significantly deviate --- 
shapiro.test(residuals(model4))

car::qqPlot(model4) # adds a confidence interval check


car::boxCox(model2)
car::boxCox(model3)



## Summarise model===

model_sum <- emmeans::emmeans(model_2, specs = ~jun_mean + sex,
                              at =list(jun_mean = c(11.8, 16.4))) %>% 
  as_tibble()


