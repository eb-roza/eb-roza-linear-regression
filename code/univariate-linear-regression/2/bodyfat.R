######################################################################

# TITLE: Bodyfat (2)
# DATE MODIFIED: 10/31/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)
library(janitor)


########## BODYFAT % #####


fat <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\bodyfat.txt",
                  header = TRUE,
                  sep = ",") %>% 
  clean_names()

# a. Create a plot visualizing the joint distribution of (Percent 
# Body Fat, Abdomen).  Does the joint distribution appear to be 
# approximately bivariate normal?   Justify your answer. (3 pts.)


# body fat is approximely normal, while abdoment is skewed.

fat %>% 
  ggplot(aes(x=abdomen, y = pctbf)) +
  geom_point()

qqnorm(fat$pctbf)
qqline(fat$pctbf)

qqnorm(fat$abdomen)
qqline(fat$abdomen)

# d. What is the  for the regression of percent body fat (Y) on 
# abdominal circumference (X).  Interpret this quantity.  (2 pts.)

# 66%; this is the percentage of variation in body fat explained
# by variation in abdomen measurement.

lm.fat <- lm(pctbf ~ abdomen, data = fat)
summary(lm.fat)
