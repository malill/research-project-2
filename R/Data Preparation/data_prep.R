rm(list = ls())

source('src/Recommender Systems Assimilation Effects/R/Util/util.R')

# Import data set
df <- get_df('20220428')

## DOUBLE CHECK IF NEW ITEMS ARE REMOVED ##
new_items <- c(1293, 1338, 1332, 1362, 1363, 1364, 1323, 1324, 1321, 1298, 1297, 1282, 1281)
filter(df, focal_item %in% new_items)

## CHECK NUMBER OF DIFFERENT ITEMS DISCOVER BY TREATMENT ##
# could be an indicator of data misspecification
length(unique(df[df$treatment==0,]$focal_item))
length(unique(df[df$treatment==1,]$focal_item))