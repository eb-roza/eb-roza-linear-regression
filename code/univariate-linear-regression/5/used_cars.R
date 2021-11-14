######################################################################

# TITLE: Linear Regression
# DATE MODIFIED: 11/12/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)
library(magrittr)
library(s20x)


##### Read Data: Used Cars #####

cars <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\used_cars.txt",
                       header = TRUE,
                       sep = ",") %>% 
  janitor::clean_names()

##### Analyze Data: Used Cars #####

# Find the correlation matrix and comment on the use of the 
# correlation as a measure of linear association between the
# response and the individual.  

cln_cars <- cars %>% 
  mutate(make = as.factor(make),
         model = as.factor(model),
         make_model = as.factor(make_model))

num_cars <- cln_cars %>% 
  select_if(is.numeric)

PerformanceAnalytics::chart.Correlation(num_cars, histogram = TRUE)

# There is a strong relatiopnship between year made and asking price, 
# although this relationship is likely not linear, and both distributions
# have skew present.

# A weak relationship exists between options and mileage, which matches
# the weak linear trend observed.

#  The new price, loan valuation, and average retail also have a 
# relationship with the asking price.  These relationships are more
# linear, with skew present, but in the same direction of asking price.


# Examine a scatterplot matrix of Asking.Price (Y) and the numeric 
#predictors .   Comment on anything interesting you find by
#examining this plot.  You should comment/address the following:   
#  marginal/univariate distributions of  Y vs X
#  relationships between Y vs. X
#  relationships between  X's
#  any unusual cases

# see above for relationships and distributions of y v. x

# loan value and average retail, as expected have a perfect correlationn
# and linear relationship.
# additional strong relationships between average retail and new price, 
# average retail and year made, as well as year made and loan value.
# the remaining relationships are moderate to weak.


# Explain why using both  and  together in a multiple regression
# model makes no sense.

# These two variables are perfectly correlated, so they would
# be explaining the same variation.


# Examine the partial correlations between the response and each 
# of the X's.  Which predictor has the strongest adjusted 
# relationship with the response.  The weakest adjusted relationship?

ppcor::pcor(num_cars)

# Mileage has the strongest negative relationship
# New price has the strongest positive relationship; although weak
# Number of options has the weakest relationship.


# Write out the full mean function for the model using all of the 
# numeric predictors in their original scale as the terms in the 
# model; give the R-square and interpret.  

set.seed(1359)

lm.1 <- lm(asking_price ~ 
             year_made + 
             num_options + 
             mileage + 
             price_new +
             avg_retail, 
           data = num_cars)

summary(lm.1)

# the R-square is 90.9, meaning these variables explain 
# 90.9% of the variation in used car price.


# Comment on the adequacy of this model from an assumption 
# standpoint by examining plots 

plot(lm.1)
# there is some skew present in this model, as well as a significant leverage point (7)


lm.1_fit <- predict(lm.1)
hist(lm.1_fit)

num_cars %<>%
  bind_cols(lm.1_fit) %>% 
  rename(pred = 8) %>% 
  mutate(resid = asking_price - pred)

num_cars %>% 
ggplot(aes(x = resid)) + 
  geom_histogram(color = "black")

num_cars %>% 
  ggplot(aes(x = pred, y = resid)) + 
  geom_point()


# Are the any unusual cases that stand out in your residual plots?  
# If so, what makes and models do they represent.

cln_cars_pred <- cln_cars %>% 
  left_join(num_cars, by = c("asking_price",
                             "year_made",
                             "num_options",
                             "mileage",
                             "price_new",
                             "loan_val",
                             "avg_retail"))


cln_cars_pred %>% 
  ggplot(aes(x = pred, y = resid, color = make)) + 
  geom_point()

lev_resid <- cln_cars_pred %>% 
  filter((resid < -2000 &
           pred > 10000))

# NCV Plot

num_cars <- num_cars %>% 
  mutate(abs_resid = abs(resid)) %>% 
  mutate(root_abs_resid = sqrt(abs_resid))

num_cars %>% 
  ggplot(aes(x = pred, y = root_abs_resid)) +
  geom_point() +
  geom_smooth(method = "lm")
# there is an increasing trend so the variance isn't constant


#Which predictors/terms in model are statistically significant
# at the 0.55 level?  

# Mileage, new price, average retail


# Conduct a Big F-test for removing the insignificant nonconstant 
# terms (i.e. non-intercept terms) from the full model.   
# Completely specify the  for this test, conduct the test, and 
# state your conclusion.  Carefully show how you calculated 
# the F-statistic citing all necessary quantities needed to 
# find it

# Year Made
full.model <- lm(asking_price ~
                   year_made +
                   num_options +
                   mileage +
                   price_new +
                   avg_retail, 
                 data = num_cars)
red.model <- lm(asking_price ~
                #  year_made +
                  num_options +
                  mileage +
                  price_new +
                  avg_retail, 
                data = num_cars)
anova(full.model, red.model)
# fail to reject the null hypothesis that full model is better

# Num Options
full.model <- lm(asking_price ~
                   year_made +
                   num_options +
                   mileage +
                   price_new +
                   avg_retail, 
                 data = num_cars)
red.model <- lm(asking_price ~
                  year_made +
                 # num_options +
                  mileage +
                  price_new +
                  avg_retail, 
                data = num_cars)
anova(full.model, red.model)
# fail to reject the null hypothesis that full model is better


# Mileage
full.model <- lm(asking_price ~
                   year_made +
                   num_options +
                   mileage +
                   price_new +
                   avg_retail, 
                 data = num_cars)
red.model <- lm(asking_price ~
                  year_made +
                  num_options +
                 # mileage +
                  price_new +
                  avg_retail, 
                data = num_cars)
anova(full.model, red.model)
#  reject the null hypothesis that full model is better


# New Price
full.model <- lm(asking_price ~
                   year_made +
                   num_options +
                   mileage +
                   price_new +
                   avg_retail, 
                 data = num_cars)
red.model <- lm(asking_price ~
                  year_made +
                  num_options +
                  mileage +
                  #price_new +
                  avg_retail, 
                data = num_cars)
anova(full.model, red.model)
#  reject the null hypothesis that full model is better


# Avg Retail
full.model <- lm(asking_price ~
                   year_made +
                   num_options +
                   mileage +
                   price_new +
                   avg_retail, 
                 data = num_cars)
red.model <- lm(asking_price ~
                  year_made +
                  num_options +
                  mileage +
                  price_new ,
                  # avg_retail, 
                data = num_cars)
anova(full.model, red.model)
#  reject the null hypothesis that full model is better


# Perform Backward Elimination using Stepwise regression using 
# P-value Threshold for the Stopping Rule.   How does the model 
# chosen compare to the NH model from part (i)?  Include the 
# output from the Stepwise Fit. 

step <- MASS::stepAIC(lm.1, direction = "backward", trace = FALSE)
summary(step)

# Interpret each of the coefficients in the final model using 
# proper units and a suitable increment (i.e. a 1-unit increase
# might be too small to consider for some of the terms in your 
# model).  Also find and discuss a 95% CI for each.


# How does the  for the reduced model compare to the full model? 

# There is no lost accuracy as measured by R-square for the 
# reduced model.


# Construct a bar graph for the Make of the car.  Explain why 
# using Make as factor variable in our model is probably a bad 
# idea.  Hint: How many dummy variable terms would be added to
# the model if we used Make  in our model?

cln_cars %>% 
  ggplot(aes(x = make, y = ..count..)) +
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# There are so many makes, it would be hard to account for all
# of them in this mode, particularly given the variation
# in observed samples.
        