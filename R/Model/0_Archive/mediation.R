rm(list = ls())

# Load libraries
library(ggplot2)

# Import functions
source('search_behavior/r/fun.R')

# Import data set
df <- read.csv(file = 'data/paper1/click_behavior_processed.csv', sep = ',', header = TRUE)

# Prepare data set
df <- prepare_df(df)

########################
## Mediation Analysis ##
########################

# Step 1
model <- glm(click ~ 
               treatment + mobile,
             family=binomial(link='logit'), data=df)
summary(model)

