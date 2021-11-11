######################################################################

# TITLE: Descriptive Summary Statistics
# DATE MODIFIED: 11/10/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)
library(s20x)

########## WALLEYES #####

# 1. Use histograms, kernel density estimates, and summary statistics to characterize the 
# distribution of weight, length, and mercury concentration for the walleyes in Island 
# Lake.

walleyes <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\walleyes-island-lake.txt",
                       header = TRUE,
                       sep = ",") %>% 
  rename(length = LGTHIN,
         weight = WTLB,
         mercury = HGPPM)

# a. Examine a scatterplot of HGPPM vs. LGTHIN and add marginal distribution estimates 
# to the plot.  Would you characterize the joint distribution of (LGTHIN,HGPPM) as 
# bivariate normal?  Explain.  (3 pts.)

walleyes %>% 
ggplot(aes(x = length, y = mercury)) +
  geom_point()

hist(walleyes$length)
hist(walleyes$mercury)

# Would not characterize this data as BVN, mercury distribution has right skew,
# length is closer to normal with a smaller skew.  Plot has slight linear
# trend with increasing variability as lengths increase.


# b. Take the natural log of HGPPM and use Analyze > Distribution to examine the 
# distribution of both HGPPM and log(HGPPM).  Find the sample means of the mercury 
# levels in the log scale () and in the original scale ().  Convert  back to the 
# original scale and compare it to .  How do they compare? (3 pts.)


walleyes %<>%
  mutate(log_mercury = log(mercury))

Hmisc::describe(walleyes$mercury)
summary(walleyes$mercury)

Hmisc::describe(walleyes$log_mercury)
summary(walleyes$log_mercury)

mean(walleyes$mercury)
exp(mean(walleyes$log_mercury))

# c. Repeat part (b), but this time consider the sample medians instead.  
# What do you find?  (3 pts.)

median(walleyes$mercury)
exp(median(walleyes$log_mercury))

# d) Fit the model using length. Examine residual plots and comment 
# on the model assumptions.   (3 pts.) 

lm_merc_length <- lm(mercury ~ length, data = walleyes)

summary(lm_merc_length)
plot(lm_merc_length)

# Residuals have an increasing cone pattern that is problematic for
# assumptions of constant variance.  Stanardized residuals appear to have
# an increasing trend.

# e. Construct a nonconstant variance plot for the model fit in part (d)
# and discuss what this model suggests regarding the model assumptions.  (3 pts.)

# f. Despite the fact this model is clearly deficient, interpret the 
# both parameters estimates  in words using proper units.  (2 pts.)

# g)Now fit the model: log(mercury) ~. Examine residual plots 
# and comment on the model assumptions.  (3 pts.)

lm_log_merc_length <- lm(log_mercury ~ length, data = walleyes)

summary(lm_log_merc_length)
plot(lm_log_merc_length)

# there is still some curvature in the residuals, though it is improved.

# h. Construct a nonconstant variance plot for the model fit in 
# part (f), , and discuss what this model suggests regarding 
# the model assumptions.  (3 pts.)

# s)Would you recommend using this model to predict the mercury levels 
# and develop consumption advisories for walleyes in the Mississippi River?
#Explain. (1 pt.)

# No, the distributions and assumptions may not hold for a different population.
# A separate model should be built using that data, or built with generallization
# in mind.


# t)The Island Lake walleye data also contains the weight (lbs.) for 
# each of the fish sampled.  Do you think using weight as opposed to length 
# to establish consumption advisories is a good idea?  Justify your answer 
# by fitting models for mercury or log mercury level using X = WTLB as the
# predictor and contrasting the results with those above.  (4 pts.)

walleyes %>% 
  ggplot(aes(x = weight, y = mercury)) +
  geom_point()

# similarly nonconstant variance in weight

lm_merc_weight <- lm(mercury ~ weight, data = walleyes)
summary(lm_merc_weight)
plot(lm_merc_weight)

lm_log_merc_weight <- lm(log_mercury ~ weight, data = walleyes)
summary(lm_log_merc_weight)
plot(lm_log_merc_weight)

# These models are less accurate and also appear to have more leverage
# points, while having similar issues regarding nonconstant variance.


# u) Another possible model to consider is both logs.   Fit this 
# model, examine residual plots, and comment on the adequacy of this model.  (5 pts.)

walleyes %<>% 
  mutate(log_length = log(length))

lm_log_merc_log_length <- lm(log_mercury ~ log_length, data = walleyes)
summary(lm_log_merc_log_length)
plot(lm_log_merc_log_length)

# Improved residual plots and leverage plots without lost accuracy.

# v) Use the estimated slope  from the model in part (t) to interpret 
# the change in the response in the original scale associated with a 1 unit 
# increase in the . (3 pts.)

exp(1.6698)

# for every 1 unit increase in length, mercury increases by 5.3 PPU

# z)Use R to fit the model  and obtain a model summary.  Include the output 
# from R below.  To retain the appearance of R output using Courier 
# New (10 pt) as the font.  (BONUS 5 pts.)

trendscatter(walleyes$log_length, walleyes$log_mercury)

lm_log_merc_log_length <- lm(log_mercury ~ log_length, data = walleyes)
summary(lm_log_merc_log_length)
plot(lm_log_merc_log_length)

