# Clear work space
rm(list = ls())

# Load libraries & utility function
library(plm)
source('src/Recommender Systems Assimilation Effects/R/Util/util.R')

# Import data set
df <- get_df('20220410')

# Linear Probability Model using fixed effects
# same result if running lm(click ~ .. + factor(user_uid))
model <- plm(click ~ category_sim + price_sim + visual_sim,
             data=df, index=c('user_uid'), model="within")
summary(model)
