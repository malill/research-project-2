# Clear work space
rm(list = ls())

# Load libraries & utility function
library(dplyr)
library(survival)
library(fixest) # Recommended by Lucas
source('src/Recommender Systems Assimilation Effects/R/Util/util.R')

df <- get_df('20220410')

# Keep only countries that occur at least 2 times
df <- df[df$user_uid %in% names(which(table(df$user_uid)>1)),]

# Fixed Effects Models
model_fe <- clogit(click ~ 
                     category_sim + price_sim + visual_sim + 
                     strata(user_uid),
                   data=df)
m <- feglm(click ~ treatment*category_sim + treatment*price_sim + treatment*visual_sim | user_uid, data = df)
summary(model_fe)
summary(m)
