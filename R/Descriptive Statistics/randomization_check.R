# Randomization Check: double-check whether mobile is distributed the same 
# among control and treatment

# Load libraries and remove environment data
library(dplyr)
rm(list = ls())

# Load data set
df <- read.csv(file = "data/03_processed/click_behavior.csv", sep = ',', header = TRUE)

# Analyze mobile distribution among control/treat
# Group df by user_uid, then group by treatment
res <- df %>% group_by(user_uid) %>% summarise(
  treatment = mean(treatment),
  mobile = mean(mobile)
)

res %>% group_by(treatment) %>% tally(mobile)
