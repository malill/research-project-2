rm(list = ls())

# Load libraries & utility function
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
library(corrplot)
library(expss)
library(dplyr)
source('papers/Recommender Systems Assimilation Effects/R/Util/util.R')


# Import data set
df <- get_df('20220428')
df_items <- get_df_item('20220327')

# get_df modifies data, store this in the final file to provide for paper replication
final_data_filename <- paste('click_behavior_final_', '20220428', '.csv', sep='')
final_data_file_path = paste('papers/Recommender Systems Assimilation Effects/Data/', final_data_filename, sep='')
write.csv(df, final_data_file_path, row.names=FALSE)

############################
## Descriptive Statistics ##
############################

# Time span
time_min <- min(df$timestamp)
time_max <- max(df$timestamp)
time_min <- strptime(time_min, "%Y-%m-%d T %H:%M:%OS")
time_max <- strptime(time_max, "%Y-%m-%d T %H:%M:%OS")
cat(time_max - time_min)

# Total observations
cat(nrow(df))

# Total users
cat(length(unique(df$user_uid)))

# Total treatment users
length(unique(df[df$treatment==1, 'user_uid']))

# Total control users
length(unique(df[df$treatment==0, 'user_uid']))

# Total items (comes from database)
cat(length(unique(df_items$PRODUCT_CODE)))


var <- c("click", "mobile", "visual_sim", "price_sim", "category_sim")

descr_stat <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("name", "mean", "sd", "min", "max", "n"))
for (v in var) {
  feat <- sym(v)
  r <- df %>% summarise(
    name=v,
    mean = mean(!!feat), 
    sd = sd(!!feat),
    min = min(!!feat),
    max = max(!!feat),
    n = n())
  descr_stat <- rbind(descr_stat, r)
}
descr_stat %>% mutate(across(where(is.numeric), round, 3))

# Number of users with mobile device
mob <- sum(aggregate(mobile ~ user_uid + mobile, df, max)$mobile)
cat(mob)
cat(mob/length(unique(df$user_uid)))

################
## Click Rate ##
################

# Frequency Table
cc <- cross_cases(df, treatment, click)
cc
# Overall Click rate in Control
sum(cc[cc$row_labels=='treatment|0', c('click|1')])/
  sum(cc[cc$row_labels=='treatment|0', c('click|0','click|1')])
# Overall Click rate in Treatment
sum(cc[cc$row_labels=='treatment|1', c('click|1')])/
  sum(cc[cc$row_labels=='treatment|1', c('click|0','click|1')])

# Chi2-test for differences in click rates (TODO: for all groups)
model <- chisq.test(df$treatment, df$click)
model
model <- wilcox.test(click ~ treatment, df)
model


# Overall CTR (view-click)
agg_click_mob_treat = aggregate(click ~ mobile + treatment, data = df, FUN = mean)
colnames(agg_click_mob_treat)[3] <- 'click.mean'
agg_click_mob_treat$click.count <- aggregate(click ~ mobile + treatment, data = df, FUN = sum)$click
agg_click_mob_treat$click.total <- aggregate(click ~ mobile + treatment, data = df, FUN = length)$click
agg_click_mob_treat$click.sd <- aggregate(click ~ mobile + treatment, data = df, FUN = sd)$click
agg_click_mob_treat$click.CI <- 2*qnorm(.975)*sqrt((agg_click_mob_treat$click.mean*(1-agg_click_mob_treat$click.mean))/agg_click_mob_treat$click.total) # CIs under normal assumption
agg_click_mob_treat

plot_ctr <- function(d) {
  # TODO: Plot sd (generally bad idea for binary? sd=p*(1-p)) or confidence intervals for bars
  
  p <- ggplot(d, aes(x=mobile, y=click.mean, fill=factor(treatment))) + 
    geom_bar(stat="identity", position=position_dodge()) + 
    geom_errorbar(aes(ymin=click.mean-click.CI, ymax=click.mean+click.CI), width=.2,
                  position=position_dodge(.9)) +
    labs(fill = "Group", x="Device", y="Click Rate")
  
  p + 
    scale_x_discrete(limits=c(0, 1), labels = c("0"="Desktop", "1"="Mobile")) + 
    scale_fill_discrete(limits=c(0, 1), labels = c("0"="Control", "1"="Treatment")) +
    theme_bw() +
    theme(axis.line = element_line(colour = 'black'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
}
png(filename="papers/Recommender Systems Assimilation Effects/Output - Assets/clickRate.jpg", width = 800, height = 400)
plot_ctr(agg_click_mob_treat)
dev.off()

# User Click Rate (user-click) NOT REALLY... THE USER CLICK RATE IS NOT A BINARY VARIABLE 
agg_u <- aggregate(click ~ user_uid, data = df, FUN = mean)
colnames(agg_u)[2] <- 'click.mean'
agg_u <- merge(agg_u, aggregate(treatment ~ user_uid, data = df, FUN = max), by='user_uid')
agg_u <- merge(agg_u, aggregate(mobile ~ user_uid, data = df, FUN = max), by='user_uid')
agg <- aggregate(click.mean ~ mobile + treatment, data = agg_u, FUN = mean)
agg$click.sd <- aggregate(click.mean ~ mobile + treatment, data = agg_u, FUN = sd)$click
agg$user.count <- aggregate(user_uid ~ mobile + treatment, data=agg_u, FUN=length)$user_uid
agg
plot_ctr(agg)

###########################
## Panel Data Statistics ##
###########################
# User
N <- length(unique(df$user_uid))
cat('Number of unique users N in sample:', N)

agg = aggregate(treatment ~ user_uid, data = df, mean)
paste('Number of treatment users: ', sum(agg$treatment == 1), 
      ' (share:', round(sum(agg$treatment == 1)/nrow(agg), digits = 3), ')', sep='')
paste('Number of control users: ', sum(agg$treatment == 0), 
      ' (share:', round(sum(agg$treatment == 0)/nrow(agg), digits = 3), ')', sep='')

# Time steps (length)
t <- as.numeric(table(df$user_uid))
summary(t)
qplot(t, geom="histogram", binwidth=1) 

########################
## Correlation Matrix ##
########################
c <- cor(df[c("price_sim", "category_sim", "visual_sim")])
corrplot(c, method='number', tl.col = 'black', tl.srt = 45, cl.pos = 'n', number.digits = 3 )

cat('Correlation between Visual Similarity and Categorical Similarity', cor(df$category_sim, df$visual_sim))

#####################################
## Visual / Categorical Similarity ##
#####################################


# Store .xlsx
store_df('output', df)



### FINAL OUTPUT ###

# Time span
cat(time_max - time_min)

# Total observations
cat(nrow(df))

# Total users
cat(length(unique(df$user_uid)))

# Total treatment users
length(unique(df[df$treatment==1, 'user_uid']))

# Total control users
length(unique(df[df$treatment==0, 'user_uid']))

# Total items (comes from database)
cat(length(unique(df_items$PRODUCT_CODE)))

# Mobile users
length(unique(df[df$mobile==1, 'user_uid']))
length(unique(df[df$mobile==1, 'user_uid']))/length(unique(df$user_uid))

# Table1
descr_stat %>% mutate(across(where(is.numeric), round, 3))

# Correlations
round(cor(df$visual_sim, df$category_sim),3)
round(cor(df$price_sim, df$category_sim),3)
round(cor(df$price_sim, df$visual_sim),3)

# Click by Mobile/Treatment
round(agg_click_mob_treat, 4)

# Click by Treatment
round(sum(cc[cc$row_labels=='treatment|0', c('click|1')])/
  +   sum(cc[cc$row_labels=='treatment|0', c('click|0','click|1')]),4)
round(sum(cc[cc$row_labels=='treatment|1', c('click|1')])/
  +   sum(cc[cc$row_labels=='treatment|1', c('click|0','click|1')]),4)

# Chi2-test for differences in click rates (TODO: for all groups)
model <- chisq.test(df$treatment, df$click)
model
