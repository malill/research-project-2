# ICIS 22: Reviewer 3 claims that on average a user has 3 interactions

# Load libraries and remove environment data
library(dplyr)
rm(list = ls())

# Load data set
df <- read.csv(file = "data/03_processed/click_behavior.csv", sep = ',', header = TRUE)
interactions <- df %>% count(user_uid)

hist(interactions$n, breaks=max(interactions$n), xlim = c(0,25))
