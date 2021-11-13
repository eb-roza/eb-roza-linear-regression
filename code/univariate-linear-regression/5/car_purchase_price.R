######################################################################

# TITLE: Linear Regression
# DATE MODIFIED: 11/13/2021
# AUTHOR: EDB

######################################################################

library(openxlsx)
library(tidyverse)

##### Read Data: Used Cars #####

car_price <- read.delim("C:\\Users\\edbro\\Documents\\Code\\eb-roza-linear-regression\\data\\car_purchase.txt",
                   header = TRUE,
                   sep = ",") %>% 
  janitor::clean_names()

######----- CLEAN DATA -----#####

Hmisc::describe(car_price)

cln_car_price <- car_price %>% 
  mutate(male_yn = if_else(gender == "M", 1, 0),
         married_yn = if_else(marital == "M", 1, 0),
         college_yn = if_else(college_deg == "Yes", 1, 0)) %>% 
  select(-gender,
         -marital,
         -college_deg)

######----- BUILD MODEL - FULL -----#####
# Use all available X's to create the full model.

lm.full <- lm(price_paid ~ 
              income +
              age +
              num_kids +
              male_yn +
              married_yn +
              college_yn, 
              data = cln_car_price)

summary(lm.full)

######----- BUILD MODEL - REDUCED -----#####
# Use backward elimination to simplify your model.  Fit the
# reduced model and interpret each of the estimated coefficients.

lm.back <- MASS::stepAIC(lm.full, direction = "backward", trace = FALSE)

summary(lm.back)

######----- EXAMINE MODEL - REDUCED -----#####

plot(lm.back)

# Plot diagnoistics of the model look good
