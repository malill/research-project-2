# Clear work space
#rm(list = ls())

# Load libraries & utility function
library(dplyr)
library(lme4)
source('src/Recommender Systems Assimilation Effects/R/Util/util.R')

df <- get_df('20220410')

# Random Effects Model
model_re <- glmer(click ~ 
                    treatment + 
                    mobile + 
                    category_sim + price_sim + visual_sim + 
                    (1|user_uid), 
                  data=df, family=binomial(link = 'logit'))
summary(model_re)