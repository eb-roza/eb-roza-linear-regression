######################################################################

# TITLE: Twin Cities Homes (2)
# DATE MODIFIED: 11/1/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)
library(janitor)


########## READ FILE #####


tc_homes <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\twin_cities_homes.txt",
                  header = TRUE,
                  sep = ",") %>% 
  clean_names()

# a.How would you characterize the marginal distributions of List 
# Price (Y) and SQFT (X), the finished living area (ft2)? (2 pts.)

# List price has a large amount of variation, but is approximately
# log-normal.  Square footage has less variation, and is also
# approximately log-normal.

Hmisc::describe(tc_homes$list_price)
qqnorm(log(tc_homes$list_price))
qqline(log(tc_homes$list_price))

Hmisc::describe(tc_homes$sqft)
qqnorm(log(tc_homes$sqft))
qqline(log(tc_homes$sqft))

# b. Use Tukey’s Ladder of Powers to find power transformations of 
# ListPrice and SQFT to improve approximate normality.  
# Include plots of your transformed variables.  (4 pts.)

hist(log(tc_homes$list_price))
hist(log(tc_homes$sqft))


# d. Use the Bulging Rule to find a transformation of List Price 
# ( call it ) and SQFT (call it ) such that  and .  You may not be 
# able to achieve both of these goals, but try.  Include a 
#scatterplot of .  You can do this in JMP or using trendscatter in 
# R.  (5 pts.)

trendscatter(log(tc_homes$sqft), log(tc_homes$list_price), f = 0.5)
