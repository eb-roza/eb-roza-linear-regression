######################################################################

# TITLE: Descriptive Summary Statistics
# DATE MODIFIED: 6/20/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)

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

# SUMMARY:
# all variables are right skewed, but to different degrees
# scales are all different, so comparison will need to be mindful.
# weight is most variable, then mercury, then length

sum_length <- walleyes %>% 
  summarise(mean = mean(length),
            sd = sd(length),
            skew = moments::skewness(length),
            kurt = moments::kurtosis(length),
            cv = sd/mean) %>% 
  mutate(label = "length") 

sum_weight <- walleyes %>% 
  summarise(mean = mean(weight),
            sd = sd(weight),
            skew = moments::skewness(weight),
            kurt = moments::kurtosis(weight),
            cv = sd/mean) %>% 
  mutate(label = "weight") 

sum_mercury <- walleyes %>% 
  summarise(mean = mean(mercury),
            sd = sd(mercury),
            skew = moments::skewness(mercury),
            kurt = moments::kurtosis(mercury),
            cv = sd/mean) %>% 
  mutate(label = "mercury") 

sum_walleye <- bind_rows(sum_length, sum_weight, sum_mercury)


# WEIGHT
walleyes %>% 
  ggplot(aes(x = weight)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density()

qqnorm(walleyes$weight)
qqline(walleyes$weight)

# LENGTH
walleyes %>% 
  ggplot(aes(x = length)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density()

qqnorm(walleyes$length)
qqline(walleyes$length)

# MERCURY
walleyes %>% 
  ggplot(aes(x = mercury)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density()

qqnorm(walleyes$mercury)
qqline(walleyes$mercury)


# b. Is there evidence that the mean length of walleyes in Island Lake is over 15 inches?
#  Conduct an appropriate test to determine this, checking necessary assumptions.  
#Summarize your findings. 

qqnorm((walleyes$length))
qqline((walleyes$length))

hist(walleyes$length)

t.test(walleyes$length, mu = 15, alternative = "less", data = walleyes)


# c. Give a 95% confidence interval for the mean length of walleyes in Island Lake.  
#     Interpret this interval.

t.test(walleyes$length, conf.level = 0.95)$conf.int
#    With 95% confidence, the population average length for walleyes is between 15.5 and 17.9 inches


# d. Give a 95% prediction interval for the length of randomly “selected” walleye from Island Lake. 


# e. Construct a scatterplot of weight (lbs.) vs. length (in.).  Comment on the mean and variance functions, 
#    E(Weight|Length) and Var(Weight|Length)

walleyes %>% 
  ggplot(aes(x = length, y = weight)) + 
  geom_point()
# There is more variation among weights as the mean length increases



########## MN WALLEYES #####

# 1. Use histograms, kernel density estimates, and summary statistics to characterize the 
# distribution of weight, length, and mercury concentration for the walleyes in Island 
# Lake.

mn_walleyes <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\mn-walleyes.txt",
                       header = TRUE,
                       sep = ",")  %>% 
  rename(in_length = LGTHIN,
         cm_length = LGTCM,
         mercury = HGPPM)

# a.  Create a binned version of length (in.) using 2.5 in width bins.  Then examine the conditional distribution 
#     of HGPPM|Length using the binned length variable.  Construct a table in JMP giving  and  and discuss the results.  
min_length <- min(mn_walleyes$in_length)
by_2.5 = seq(min_length,50, by = 2.5)

mn_walleyes %<>% 
  mutate(grp_in_length = cut(in_length, breaks = by_2.5, include.lowest = TRUE))

mn_walleyes %>% 
  ggplot(aes(x = grp_in_length, y = mercury)) +
  geom_point()

summary(mn_walleyes)

# b)  What percent of the variation in the response (HGPPM) is explained by the  regression on binned length?  
#     Interpret this value.

lm_mn_walleyes <- lm(mercury ~ grp_in_length, data = mn_walleyes)

summary(lm_mn_walleyes)
