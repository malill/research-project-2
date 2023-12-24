# https://www.r-bloggers.com/2016/07/performing-principal-components-regression-pcr-in-r/

# Install & load packages
# install.packages('pls')
library(ggplot2)
require(pls)


# Import functions
source('search_behavior/r/fun.R')

# Import data set
df <- read.csv(file = 'data/paper1/click_behavior_processed.csv', sep = ',', header = TRUE)

# Prepare data set
df <- prepare_df(df)

# Calculate PCs
df.pca <- prcomp(df[c('category_sim', 'visual_sim')], center = TRUE,scale. = TRUE)
summary(df.pca)

df$pca1 <- df.pca$x[,1]
df$pca2 <- df.pca$x[,2]

p <- df$pca1
df$pca1 <- ((p-min(p))/(max(p)-min(p)))

model <- glm(click ~ 
               treatment + 
               mobile +
               pca1,
             family=binomial(link='logit'), data=df)
summary(model)

# Plot predictions
for (i in 0:2) {
  plotting_dfm <- expand.grid(pca1 = seq(from=min(df$pca1), to = max(df$pca1), by=0.1),
                              treatment = (0:1), mobile=i)
  plotting_dfm$preds <- plogis( predict(model , newdata=plotting_dfm))
  
  device <- c('Desktop', 'Mobile')[i]
  title <- sprintf("Click Rate by Similarity and Group (%s only)", device)
  
  pl <- ggplot(plotting_dfm, aes(x=pca1, y =preds, color=as.factor(treatment)))
  print(pl + 
    geom_line(lwd=1) +
    ggtitle(title) + 
    ggplot2::ylim(0, 1) +
    ggplot2::xlab("Similarity") +
    ggplot2::ylab("Predicted Click Rate") + 
    ggplot2::labs(col="Group"))
}

# Step 2
model <- lm(click ~ 
               pca1 + mobile, data=df)
summary(model)

# hist(df$pca1, breaks=50)