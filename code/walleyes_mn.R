######################################################################

# TITLE: Descriptive Summary Statistics
# DATE MODIFIED: 10/28/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)


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

# 28% of the variation in the amount of mercury amount in 
# walleyes is explained by the grouped length.

lm_mn_walleyes <- lm(mercury ~ grp_in_length, data = mn_walleyes)

summary(lm_mn_walleyes)

# e)  Construct a scatterplot of HGPPM vs. LGTHIN and add 
# lowess smooth based estimates of  &  to the  plot.  Discuss.  (6 pts.

# This plot suggests the linear pattern may not be the best fit
# for this data, with mercury content growing more rapidly as 
# length grows for some walleye.

plot(mn_walleyes$in_length, mn_walleyes$mercury)
lines(lowess(mn_walleyes$in_length, mn_walleyes$mercury), col='red')

#f)  Repeat part (e) using the logarithm of HGPPM as the response.   Discuss.  (6 pts.)

# This fit is much more appropriate.

plot(mn_walleyes$in_length, log(mn_walleyes$mercury))
lines(lowess(mn_walleyes$in_length, log(mn_walleyes$mercury)), col='red')
