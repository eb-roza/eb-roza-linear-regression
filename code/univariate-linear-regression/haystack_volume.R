######################################################################

# TITLE: Haystack Volume (2)
# DATE MODIFIED: 10/31/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)
library(janitor)


########## HAYSTACK VOLUME #####


hay <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\haystack_volume.txt",
                          header = TRUE,
                          sep = ",") %>% 
  clean_names()


# 1. Answer the following using whatever software you’d like.

# Marginal Distributions
# a. Obtain estimates of the following quantities for Volume (ft3). (3 pts)
### Mean = 3017.789
### Variance (SD) = 914.698
### Total Variation (SYY) = (corrected sum of squares)

mean(hay$volume)
sd(hay$volume)

hist(hay$volume)
boxplot(hay$volume)


# b.  b.Obtain estimates of the following quantities for Circumference (ft.) and Over (ft.)  (3 pts.)
# Circumference
### Mean = 69.09
### Variance (SD) =5.12
### Total Variation (SXX) = (corrected sum of squares)

mean(hay$circum)
sd(hay$circum)

hist(hay$circum)
boxplot(hay$circum)

# Over
### Mean = 36.1195
### Variance (SD) = 3,841
### Total Variation (SXX) = (corrected sum of squares)

mean(hay$over)
sd(hay$over)

hist(hay$over)
boxplot(hay$over)

#Joint Distributions of (Circumference,Volume) and (Over,Volume)


# C.  Create a plot visualizing the joint distribution of 
# ...Volume (Y) vs. Circumference (X).  Give a brief statement 
# (one or two sentences) about the general patterns you see in 
# this plot. Would you characterize the joint distribution as 
# BVN? Why or why not? (4 pts.)

hay %>% 
  ggplot(aes(x=circum, y=volume)) +
  geom_point()

# Both variables do not have a bell-shaped curve or appear to be
# approximately normally distributed, though they do appear to have
# a linear pattern.  Would not consider this distribution to be 
# bivariate normal.


# d. Create a plot visualizing the joint distribution of 
# Volume (Y) vs. Over (X).  Give a brief statement (one or two 
# sentences) about the general patterns you see in this plot. Would
# you characterize the joint distribution as BVN? Why or why not? (4 pts.)

hay %>% 
  ggplot(aes(x=over, y=volume)) +
  geom_point()

# Both variables do  have a bell-shaped curve and appear to be
# approximately normally distributed, though they do appear to have
# a linear pattern.  Would consider this distribution to be 
# bivariate normal.


# e. Which potential predictor, circumference or over, appears to 
# have the strongest relationship with the volume of the haystack? 
# Explain.  (2 pts.) 

# Over; there is less variation both visually and in the summary
# statistics as well as a very strong linear pattern obeserved.


# f. Show the math as to why it is reasonable to estimate the 
# volume of a haystack using the the relationship below. (3 pts)

# hay stacks are half-spheres, so the volume of a sphere is 
# 4*pi*r, and the volume of a hemi-sphere is 2/3*pi*r^3; 
# this would just be showing the equality using these two 
# relationships assuming they are hemispherical.


# g. Use the function above as an estimate of the mean function for 
# Volume | Circumference.  You can use the JMP Calculator to create
# a new column containing the mean function values.  Plot this 
# estimated mean function on a scatterplot of Volume vs. 
# Circumference.  This is can done by first forming a new column 
# containing the formula above and then using Graph > Overlay Plots 
# in JMP to plot both Volume and Volume Formula vs. Circumference.   (3 pts)

hay %<>%
  mutate(est_volume = (circum^3)/(12*pi^2))

hay %>% 
  ggplot(aes(x=circum, y= volume)) +
  geom_point() +
  stat_summary(aes(y = est_volume, group=1), 
               fun.y=mean, colour="red", geom="line",group=1)

# h. Obtain Residual2  value for each point in your dataset.  
# Sum up these values to obtain the total unexplained variation f
# or the mean function given above. (3 pts)

hay %<>%
  mutate(resid = volume - est_volume)

hay %>% 
  summarise(RSS = sum(resid)) %>% 
  ungroup() %>% 
  mutate(RSS_square = RSS^2)
