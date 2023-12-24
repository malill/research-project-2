rm(list = ls())

# Load libraries
library(ggplot2)

# Import functions
source('search_behavior/r/fun.R')

# Import data set
df <- read.csv(file = 'data/paper1/click_behavior_processed.csv', sep = ',', header = TRUE)

# Prepare data set
df <- prepare_df(df)

# Model Logistic Regression
model <- glm(click ~ 
               group +
               f_r_img,
               family=binomial(link='logit'), data=df)
summary(model)

# Prediction 
plot_lr_img(model)
