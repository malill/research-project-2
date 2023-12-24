rm(list = ls())

# Load libraries & utility function
library(fixest)
library(lme4)
library(plm)
source('src/Recommender Systems Assimilation Effects/R/Util/util.R')

# Import data set
df <- get_df('20220410')


# Model 1: Logistic Regression (Cross-Sectional, Treatment + Control only)
model1 <- glm(click ~ 
               treatment +
               mobile,
             family=binomial(link='logit'), data=df)
summary(model1)
print(logLik(model1))
print(AIC(model1))


# Model 2: Logistic Regression (Cross-Sectional, Full)
model2 <- glm(click ~ 
               treatment +
               mobile +
               price_sim + category_sim + visual_sim,
             family=binomial(link='logit'), data=df)
summary(model2)
print(logLik(model2))
print(AIC(model2))

# Model 3a: Fixed Effects
model3a <- feglm(click ~ price_sim + category_sim + visual_sim | user_uid, 
               data = df, family=binomial(link = "logit"))
summary(model3a)
print(logLik(model3a))
print(AIC(model3a))

# Model 3b: Random Effects
model3b <- glmer(click ~ 
                    treatment + 
                    mobile + 
                    category_sim + price_sim + visual_sim + 
                    (1|user_uid), 
                  data=df, family=binomial(link = 'logit'))
summary(model3b)
print(logLik(model3b))
print(AIC(model3b))

# Model 3c: Linear Probability Model (cross sectional, full)
# same result if running lm(click ~ .. + factor(user_uid))
model3c <- lm(click ~ treatment + mobile + category_sim + price_sim + visual_sim,
             data=df)
summary(model3c)

# Model 3d: Linear Probability Model (FE)
# same result if running lm(click ~ .. + factor(user_uid))
model3d <- plm(click ~ category_sim + price_sim + visual_sim,
             data=df, index=c('user_uid'), model="within")
summary(model3d)

# Model 3e: Linear Probability Model (RE)
# same result if running lm(click ~ .. + factor(user_uid))
model3e <- plm(click ~ treatment + mobile + category_sim + price_sim + visual_sim,
             data=df, index=c('user_uid'), model="random")
summary(model3e)


# Hausman test

# TODO: should perform exogeneity check (x, e not related) first

# Logit FE vs. RE (remove time invariant predictors)
# PLM FE vs. RE
phtest(model3d, model3e) # library(plm) function out of the box
# -> RE is inconsistent 
