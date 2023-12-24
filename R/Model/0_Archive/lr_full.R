rm(list = ls())

# Load libraries & utility function
library(ggplot2)
source('src/Recommender Systems Assimilation Effects/R/Util/util.R')

# Import data set
df <- get_df('20220410')

# Model Logistic Regression
model <- glm(click ~ 
               treatment*visual_sim +
               mobile +
               treatment*category_sim +
               treatment*price_sim,
             
             family=binomial(link='logit'), data=df)
summary(model)
