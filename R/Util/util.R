# Helper functions for 'Recommender Systems Assimilation Effects'
library("writexl")

get_df <- function(version) {
  filename = paste('click_behavior_processed_', version, '.csv', sep='')
  file_path = paste('papers/Recommender Systems Assimilation Effects/Data/', filename, sep='')
  df <- read.csv(file = file_path, sep = ',', header = TRUE)
  
  # Filter bots
  bots <- c('62384c29754b95fa05ac9d3c', '6222950081ece9c26d2c4931', '6233daf067052435d3a8cb34')
  df <- df[!(df$user_uid %in% bots), ]
  
  # Rename columns
  names(df)[names(df) == "group"] <- "treatment"
  names(df)[names(df) == "is_mobile"] <- "mobile"
  names(df)[names(df) == "f_r_price"] <- "price_sim"
  names(df)[names(df) == "f_r_cat"] <- "category_sim"
  names(df)[names(df) == "f_r_img"] <- "visual_sim"
  
  # Normalize categorical similarity
  df$category_sim <- df$category_sim/max(df$category_sim)
  # df[df$category_sim>0, 'category_sim'] <- 1
  
  # Normalize price similarity
  p <- df$price_sim
  p <- abs(p)
  p[p<1] <- 1
  p <- 1/p
  df$price_sim <- p
  
  # Encode Treatment 
  df$treatment <- ifelse(df$treatment=="treatment",1,0)
  
  # Keep only necessary columns
  df <- df[c("user_uid", "click", "timestamp", "treatment", "mobile", "focal_item", "price_sim", "category_sim", "visual_sim")]
  
  return(df)
}

get_df_item <- function(version) {
  filename = paste('items_', version, '.csv', sep='')
  file_path = paste('papers/Recommender Systems Assimilation Effects/Data/', filename, sep='')
  df <- read.csv(file = file_path, sep = ',', header = TRUE)
}

store_df <- function(name, df) {
  filename = paste(name, '.xlsx', sep='')
  filepath = paste('output/', filename, sep='')
  write_xlsx(df, path=filepath)
}

min_max_scale <- function(x) {
  return(x-min(x))/(max(x)-min(x))
}

plot_lr_cat <- function(model) {
  plotting_dfm <- expand.grid(f_r_cat = seq(from=0, to = 3, by=1),
                              group = (0:1))
  plotting_dfm$preds <- plogis( predict(model , newdata=plotting_dfm))
  
  pl <- ggplot(plotting_dfm, aes(x=f_r_cat, y =preds, color=as.factor(group)))
  pl + 
    geom_line(lwd=1) +
    ggtitle("Predicted Click Rate by Categorical Similarity and Treatment") + 
    ggplot2::xlab("Number Shared Categories") +
    ggplot2::ylab("Predicted Click Rate")
}

plot_lr_img <- function(model) {
  plotting_dfm <- expand.grid(f_r_img = seq(from=0, to = 1, by=0.1),
                              group = (0:1))
  plotting_dfm$preds <- plogis( predict(model , newdata=plotting_dfm))
  
  pl <- ggplot(plotting_dfm, aes(x=f_r_img, y =preds, color=as.factor(group)))
  pl + 
    geom_line(lwd=1) +
    ggtitle("Predicted Click Rate by Visual Similarity and Treatment") + 
    ggplot2::xlab("Image Similarity") +
    ggplot2::ylab("Predicted Click Rate")
}