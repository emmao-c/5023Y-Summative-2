# Assessing the efficacy of the probiotic Lactobacillus rhamnosus in its ability to moderate the number of the pathogenic bacteria Ruminococcus gnavus found in the small intestine. 

### Packages Used:

| Package     | Purpose                                                                                                             |
|--------------|----------------------------------------------------------|
| tidyverse   | For importing, tidying, presenting and manipulating data                                                            |
| ggplot2     | Used for making complex plots from data frames                                                                      |
| janitor     | simple tools for cleaning and examining data                                                                        |
| dplyr       | Used for filtering, selecting columns, sorting data and adding and deleting columns                                 |
| emmeans     | used to estimate marginal means                                                                                     |
| GGally      | allows for added functions to ggplot2 to reduce the complexity of combining geometric objects with transformed data |
| performance | Allows for the assessment of regression models                                                                      |
| see         | Provides utilities for further data analysis                                                                        |
| car         | provides functions for furthering regression analysis                                                               |

## Description 
An investigation into the efficacy of the probiotic Lactobacillus rhamnosus in regards to treating the gastrointestinal pathogenic bacteria Ruminococcus gnavus. Completed using a placebo group as a control and steps taken to investigate the effects of gender. 

## Data
The abundance counts of *Ruminococcus gnavus* in 21 different stool samples from both male and female subjects

##Folder structure 
|Name | Usage|
|data| Stores the data csv file that is used in this analysis |
|figures| containing figures used to understand the data 
|scripts| Containing scripts and RMD file used for final HTML file |

### Variables
| Variable| Definition|
|----|----|
|Subject|subject ID|
|gender| sample gender, Male or Female|
|treatment|Usage of placebo "Placebo" or Lactobacillus rhamnosus probiotic "LGG"|
|ruminococcus_gnavus_abund|Read count abundance of Ruminococcus gnavus |
|abundance_before|Count abundance of Ruminococcus gnavus at timepoint 1, before|
|abundance_after|Count abundance of Ruminococcus gnavus at timepoint 2, after|
|abund_diff|Difference in abundance of Ruminococcus gnavus from timepoint 1, to timepoint 2|
|time|timepoint 1 = before, 2 = after|
|sample|sample number, 2 per patient|
