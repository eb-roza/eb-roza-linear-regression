######################################################################

# TITLE: Linear Regression
# DATE MODIFIED: 11/14/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)

#####----- Read Data: Abalone -----#####

abalone <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\abalone.txt",
                   header = TRUE,
                   sep = ",") %>% 
  janitor::clean_names()

#####----- Clean Data: Abalone -----#####


#####----- Analyze Data: Abalone -----#####

PerformanceAnalytics::chart.Correlation(abalone, 
                                        histogram = TRUE,
                                        cex.labels=10)
# Rings has a moderate correlation with all available x's, 
# however, these x's are also highly correlated with each other, 
# with all of the pairwise correlations among x's being around
# or above 0.90.  With the exception of length, these variables
# all exhibit a right skew (length is left-skewed).
# When compared to rings, the strongest linear relationships
# are seen between length and diameter, with variance increasing
# more drastically across the remaining variables.

ppcor::pcor(abalone)
# examining partial correlations, hieght and whole_weight
# emerge with the strongest correlations to the number of rings
# when observed, with shucked_weight having the strongest inverse
# relationship (though these are all strongly related to each
# other).

#####----- Fit Model -----#####

lm.full <- lm(rings ~ .,
              data = abalone)

summary(lm.full)
par(mfrow=c(2,2))
plot(lm.full)
par(mfrow=c(1,1))

# Based on the plots, this model appears deficient; there is curvature
# and non-normality present in the residuals.

car::invResPlot(lm.full)

#####----- Fit Model (Transformed) -----#####

lm.full.2 <- lm(log(rings) ~ .,
              data = abalone)

summary(lm.full.2)
par(mfrow=c(2,2))
plot(lm.full.2)
par(mfrow=c(1,1))

# Tukey's test for Non-addititvity
car::residualPlots(lm.full.2) 

# There still remains curvature to address
car::crPlots(lm.full.2)

car::ceresPlots(lm.full.2)
# multi-collinearity can be confusing this model    

#####----- Fit Final Model (Transformed) -----#####

lm.final = lm(log(rings)
              ~ poly(length,2) + 
                poly(diam,2) + 
                poly(height,2) +
                whole_weight + 
                shucked_weight + 
                visc_weight + 
                poly(shell_weight,2),
              data = abalone)

summary(lm.final)

par(mfrow=c(2,2))
plot(lm.final)
par(mfrow=c(1,1))

# review for outliers
car::outlierTest(lm.final)

# Refit without outliers
lm.final = lm(log(rings)
              ~ poly(length,2) + 
                poly(diam,2) + 
                poly(height,2) +
                whole_weight + 
                shucked_weight + 
                visc_weight + 
                poly(shell_weight,2),
              data = abalone,subset=-c(237, 2182))

summary(lm.final)

car::vif(lm.final)


par(mfrow=c(2,2))
plot(lm.final)
par(mfrow=c(1,1))
