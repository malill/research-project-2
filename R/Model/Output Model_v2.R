rm(list = ls())
cat("\014")

# Load libraries & utility function
library(fixest)
library(lme4)
library(plm)
source('papers/Recommender Systems Assimilation Effects/R/Util/util.R')

# Import data set
df <- get_df('20220428')

# Model 1: Random Effects 
model1 <- glmer(click ~ 
                   treatment + 
                   mobile + 
                   (1|user_uid), 
                 data=df, family=binomial(link = 'logit'))
summary(model1)
print(logLik(model1))
print(AIC(model1))

# Model 2: Random Effects (wo. cat)
model2 <- glmer(click ~ 
                  treatment + 
                  mobile + 
                  visual_sim + price_sim +
                  (1|user_uid), 
                data=df, family=binomial(link = 'logit'))
summary(model2)
print(logLik(model2))
print(AIC(model2))

# Model 3: (full) Random Effects
model3 <- glmer(click ~ 
                    treatment + 
                    mobile + 
                    visual_sim + price_sim + category_sim +
                    (1|user_uid), 
                  data=df, family=binomial(link = 'logit'))
summary(model3)
print(logLik(model3))
print(AIC(model3))


# Fixed Effects (mentioned in text)
modelFE <- feglm(click ~ price_sim + category_sim + visual_sim | user_uid, 
                 data = df, family=binomial(link = "logit"))
summary(modelFE)
print(logLik(modelFE))
print(AIC(modelFE))

###### FINAL OUTPUT ######
# Model1
m1sum <- summary(model1)
m1est <- m1sum$coefficients
m1res <- cbind(round(m1est[, "Estimate"], 3), round(m1est[, "Std. Error"], 3))
colnames(m1res) <- c("Coef.", "SE")
m1res
# Model2
m2sum <- summary(model2)
m2est <- m2sum$coefficients
m2res <- cbind(round(m2est[, "Estimate"], 3), round(m2est[, "Std. Error"], 3))
colnames(m2res) <- c("Coef.", "SE")
m2res
# Model3
m3sum <- summary(model3)
m3est <- m3sum$coefficients
m3res <- cbind(round(m3est[, "Estimate"], 3), round(m3est[, "Std. Error"], 3))
colnames(m3res) <- c("Coef.", "SE")
m3res
# Model3a
mFEasum <- summary(modelFE)
mFEasum
