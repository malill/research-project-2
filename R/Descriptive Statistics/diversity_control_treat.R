# Based on the feedback received on doctoral colloquium in Barcelona as well as 
# a comment by reviewer R1, we want to check the level of diversity between 
# treatment (collaborative filtering) and control group (random recommendations)

# Load libraries and remove environment data
library(dplyr)
rm(list = ls())

# Load data set
df <- read.csv(file = "data/03_processed/click_behavior.csv", sep = ',', header = TRUE)
df <- df[, c("treatment", "price_sim", "category_sim", "visual_sim", "mobile")]

# Analyze Similarity Measures 
# Group df by treatment assignment and compare the similarity levels
res <- df %>% group_by(treatment) %>% summarise(
  price_sim = mean(price_sim),
  category_sim = mean(category_sim),
  visual_sim = mean(visual_sim)
)
res
