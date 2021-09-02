######################################################################

# TITLE: Descriptive Summary Statistics
# DATE MODIFIED: 6/10/2021
# AUTHOR: EDB

######################################################################

# Notes: 

######################################################################

##### Load Libraries #####

library(tidyverse)
library(magrittr)
library(Hmisc)

##### Read Walleye Data #####

df_bass <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\wb-lake.txt",
                         header = TRUE,
                         sep = ",")


##### Describe Walleye Data #####

summary(df_bass)
describe(df_bass)

hist(df_bass$Length,main="Length of Smallmouth Bass",xlab="Length (mm)")
boxplot(df_bass$Length)

hist(Bass$Length,prob=T,ylim=c(0,.006),main="Lengths of Smallmouth Bass")
lines(density(Bass$Length))
rug(Bass$Length)
