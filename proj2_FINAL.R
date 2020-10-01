##STATS 6021 - PROJECT 2
##Hemani Choksi(hc8nd), Oretha Domfeh (od9cw), Paul Hicks (pdh2d), Sudhanshu Luthra (sl3zs)

library(tidyverse)
library(leaps)
library(faraway)
df_red <- read.csv("winequality-red.csv", header=TRUE, sep=";")
options(max.print = 999999)
df_red
names(df_red)
summary(lm(quality~alcohol))
summary(lm(quality~fixed.acidity)) #.05 on the money
summary(lm(quality~chlorides))
summary(lm(quality~pH))
# Call:
#   lm(formula = quality ~ pH)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6337 -0.6305  0.3653  0.3705  2.3764 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.57539    0.59827   9.319   <2e-16 ***
#   pH           0.01675    0.18064   0.093    0.926    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8094 on 797 degrees of freedom
# Multiple R-squared:  1.078e-05,	Adjusted R-squared:  -0.001244 
# F-statistic: 0.008595 on 1 and 797 DF,  p-value: 0.9262
summary(lm(quality~volatile.acidity))
summary(lm(quality~free.sulfur.dioxide))
# Call:
#   lm(formula = quality ~ free.sulfur.dioxide)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6875 -0.6425  0.3215  0.3889  2.4248 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          5.700954   0.050600  112.67   <2e-16 ***
#   free.sulfur.dioxide -0.004493   0.002673   -1.68   0.0933 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8079 on 797 degrees of freedom
# Multiple R-squared:  0.003531,	Adjusted R-squared:  0.00228 
# F-statistic: 2.824 on 1 and 797 DF,  p-value: 0.09326

summary(lm(quality~sulphates))
summary(lm(quality~citric.acid))
summary(lm(quality~total.sulfur.dioxide))
summary(lm(df_red$quality~df_red$residual.sugar)) #not signifcant , 0.583 
# Call:
#   lm(formula = df_red$quality ~ df_red$residual.sugar)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6609 -0.6334  0.3580  0.3690  2.3729 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           5.616055   0.041616 134.950   <2e-16 ***
#   df_red$residual.sugar 0.007865   0.014331   0.549    0.583    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8077 on 1597 degrees of freedom
# Multiple R-squared:  0.0001886,	Adjusted R-squared:  -0.0004375 
# F-statistic: 0.3012 on 1 and 1597 DF,  p-value: 0.5832
# summary(lm(quality~density))

###############################
######## RED WINE #############
###############################


#create scatterplot of all predictors 
pairs(df_red, lower.panel = NULL, main="Red Wine Scatterplot of Quantitative Variables", col="red")

##correlation matrix
round(cor(df_red), 3)

#VIF
vif(df_red)

# perform best model fitting for red
allreg_red <- regsubsets(df_red$quality ~., data=df_red, nbest=5)
summary(allreg_red)

##create a "data frame" that stores the predictors in the various models considered as well as their various criteria
best_red <- as.data.frame(summary(allreg_red)$outmat)
best_red$p <- as.numeric(substr(rownames(best_red),1,1))+1
best_red$p
best_red$r2 <- summary(allreg_red)$rsq
best_red$adjr2 <- summary(allreg_red)$adjr2
best_red$mse <- (summary(allreg_red)$rss)/(dim(df_red)[1]-best_red$p)
best_red$cp <- summary(allreg_red)$cp
best_red$bic <- summary(allreg_red)$bic
best_red

# order them
##sort by various criteria
best_red[order(best_red$r2),]
best_red[order(best_red$adjr2),]
best_red[order(best_red$mse),]
best_red[order(best_red$cp),]
best_red[order(best_red$bic),]

##intercept only model
regnull_red <- lm(df_red$quality~1, data=df_red)
##model with all predictors
regfull_red <- lm(df_red$quality~., data=df_red)

##forward selection, backward elimination, and stepwise regression
step(regnull_red, scope=list(lower=regnull_red, upper=regfull_red), direction="forward")
step(regfull_red, scope=list(lower=regnull_red, upper=regfull_red), direction="backward")
step(regnull_red, scope=list(lower=regnull_red, upper=regfull_red), direction="both")

summary(lm(formula = df_red$quality ~ alcohol + volatile.acidity + sulphates + 
     total.sulfur.dioxide + chlorides + pH + free.sulfur.dioxide, 
   data = df_red)) 

################ Box Plots informed by the automatic model search ###############

# par(mfrow=c(1, 1))
boxplot(df_red$alcohol~df_red$quality, main="Boxplot of Red Alcohol Content vs. Quality", col="red")
# Clear that Alcohol Content is a major contributor to get to Quality Ratings of 7 and 8
boxplot(df_red$sulphates~df_red$quality, main="Boxplot of Red Quality and Sulphates", col="red")
# Sulphate also appears to contribute linearly to quality ratings
boxplot(df_red$chlorides~df_red$quality, main="Boxplot of Red Quality and Chlorides", col="blue")
# Quality rating does not appear as sensitive to Chlorides
boxplot(df_red$pH~df_red$quality, main="Boxplot of Red Quality and pH", col="blue")
# Quality rating does not appear as sensitive to pH
boxplot(df_red$free.sulfur.dioxide~df_red$quality, main="Boxplot of Red Quality and Free Sulfur", col="red")
# of the plot suggesting less sensitivity (no real pattern) of quality rating to free sulfur dioxide
boxplot(df_red$total.sulfur.dioxide~df_red$quality, main="Boxplot of Red Quality and Total Sulfur Dioxide", col="blue")
# Quality rating appears less sensitive to total sulfur dioxides (appears horizontal) 
boxplot(df_red$volatile.acidity~df_red$quality, main="Boxplot of Red Quality and Vol. Acid", col="red")
# Clear strong linear relationship between quality rating and Vol. Acidity, unimodal curve

# Boxplots suggest the following predictors are in: alcohol, volatile acidity, and sulphate
# Boxplots suggest the following predictors are out: pH, total sulfur dioxide,
# chlorides, free sulfur dioxide, fixed acidity

# Let's run a partial F-test with a full model and then a reduced model that drops the potential predictors as
# suggested by the boxplot

new_red_model_full <- lm(df_red$quality~df_red$alcohol+df_red$volatile.acidity+df_red$sulphates+
                           df_red$total.sulfur.dioxide+ df_red$chlorides+ df_red$pH+df_red$free.sulfur.dioxide)
summary(new_red_model_full)

# Call:
#   lm(formula = df_red$quality ~ df_red$alcohol + df_red$volatile.acidity + 
#        df_red$sulphates + df_red$total.sulfur.dioxide + df_red$pH + 
#        df_red$free.sulfur.dioxide + df_red$chlorides)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.68918 -0.36757 -0.04653  0.46081  2.02954 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  4.4300987  0.4029168  10.995  < 2e-16 ***
#   df_red$alcohol               0.2893028  0.0167958  17.225  < 2e-16 ***
#   df_red$volatile.acidity     -1.0127527  0.1008429 -10.043  < 2e-16 ***
#   df_red$sulphates             0.8826651  0.1099084   8.031 1.86e-15 ***
#   df_red$total.sulfur.dioxide -0.0034822  0.0006868  -5.070 4.43e-07 ***
#   df_red$pH                   -0.4826614  0.1175581  -4.106 4.23e-05 ***
#   df_red$free.sulfur.dioxide   0.0050774  0.0021255   2.389    0.017 *  
#   df_red$chlorides            -2.0178138  0.3975417  -5.076 4.31e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6477 on 1591 degrees of freedom
# Multiple R-squared:  0.3595,	Adjusted R-squared:  0.3567 
# F-statistic: 127.6 on 7 and 1591 DF,  p-value: < 2.2e-16

# attempt to drastically simplify the model to only 3 predictors based on box plots fails
new_red_model_small <- lm(df_red$quality~df_red$alcohol+df_red$volatile.acidity+df_red$sulphates)
summary(new_red_model_small)

anova(new_red_model_small, new_red_model_full)
# Analysis of Variance Table
# 
# Model 1: df_red$quality ~ df_red$alcohol + df_red$volatile.acidity + df_red$sulphates
# Model 2: df_red$quality ~ df_red$alcohol + df_red$volatile.acidity + df_red$sulphates + 
#   df_red$total.sulfur.dioxide + df_red$pH + df_red$free.sulfur.dioxide + 
#   df_red$chlorides
# Res.Df    RSS   Df Sum of Sq      F    Pr(>F)    
# 1   1595 692.10                                  
# 2   1591 667.54  4    24.568 14.639 9.538e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Since our p-value is < 0.05, we reject the null and
# go with the larger model.
summary(new_red_model_full)
# Adjusted R2 is 0.3567 indicating our model is not explaining the response for quality
# very well.

# attempt to drop "free sulfur dioxides" due to strong correlation with total sulfur dioxides
# in the corr matrix and to try and improve the model's ability to explain our response
# as the adjusted R^2 is still less than 0.40

new_red_model_small2 <- lm(df_red$quality~df_red$alcohol+df_red$volatile.acidity+df_red$sulphates+
                             df_red$total.sulfur.dioxide+df_red$pH+df_red$chlorides)
summary(new_red_model_small2)
# attempt to drop free sulfur dioxide fails
anova(new_red_model_small2, new_red_model_full)

# Analysis of Variance Table
# 
# Model 1: df_red$quality ~ df_red$alcohol + df_red$volatile.acidity + df_red$sulphates + 
#   df_red$total.sulfur.dioxide + df_red$pH + df_red$chlorides
# Model 2: df_red$quality ~ df_red$alcohol + df_red$volatile.acidity + df_red$sulphates + 
#   df_red$total.sulfur.dioxide + df_red$pH + df_red$free.sulfur.dioxide + 
#   df_red$chlorides
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)  
# 1   1592 669.93                              
# 2   1591 667.54  1    2.3941 5.7061  0.01702 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# After running two partial-f tests using the anova table
# in an attempt to simplify the model using information from box plots
# and correlation matrix, we are unable to simplify the model.

# Take a look at the linear assumptions
plot(new_red_model_full$fitted.values,new_red_model_full$residuals, main="Residual Plot Red Wine", col="red")
abline(h=0,col="red")
##ACF plot of residuals
acf(new_red_model_full$residuals, main="ACF of Residuals")
# Certainly appears to have some issues with timing with several points showing significance 
# along the x-axis

##Normal probability or QQ plot of residuals
qqnorm(new_red_model_full$residuals)
qqline(new_red_model_full$residuals, col="red")
#boxcox function found in MASS package. Need to install MASS package first
library(MASS)

boxcox(new_red_model_full)
boxcox(new_red_model_full, lambda = seq(0, 2, 0.5)) #you can adjust the range and interval of the boxcox graph

summary(new_red_model_full) 
####################################################################################
################ Based on poor Adj. R^2 and discrete response values we will #######
################ look to use a logistic regression model to see if we can improve ##
####################################################################################

hist(df_red$quality, col="red")

# suggests that most are in quality rating of 5 or 6 and that splitting into two groups
# will have a balanced set of observations

# create a new categorical variable for "low" and "high" quality red wine
df_red$qual_level[df_red$quality<=5] <- 'low'
df_red$qual_level[df_red$quality>5] <- 'high'
df_red$qual_level <- as.factor(df_red$qual_level)
df_red$qual_level <- relevel(df_red$qual_level, ref = "low")
str(df_red) # check structure of the data frame
contrasts(df_red$qual_level)

#       high
# low     0
# high    1

# split into training and testing data
set.seed(131)

##split data into two equal parts

sample<-sample.int(nrow(df_red), floor(.50*nrow(df_red)), replace = F)
train_red<-df_red[sample, ]
test_red<-df_red[-sample, ]
contrasts(train_red$qual_level)
contrasts(test_red$qual_level)
summary(train_red$qual_level)
# low high 
# 370  429 
summary(test_red$qual_level)
# low high 
# 374  426 

# pretty balanced between levels when comparing both the training and validation sets 
# in terms of # of "low" and "high" observations.

#fit model using training data

model_train<-glm(train_red$qual_level ~ train_red$alcohol + train_red$sulphates + 
                  train_red$volatile.acidity + train_red$chlorides + train_red$total.sulfur.dioxide +
                  train_red$free.sulfur.dioxide + train_red$pH, family="binomial", data = train_red)
summary(model_train)

# drop pH based on Wald Test

model_train2 <- glm(train_red$qual_level ~ train_red$alcohol + train_red$sulphates + 
                      train_red$volatile.acidity + train_red$chlorides + train_red$total.sulfur.dioxide +
                      train_red$free.sulfur.dioxide, family="binomial", data = train_red)
summary(model_train2)
# Call:
#   glm(formula = train_red$qual_level ~ train_red$alcohol + train_red$sulphates + 
#         train_red$volatile.acidity + train_red$chlorides + train_red$total.sulfur.dioxide + 
#         train_red$free.sulfur.dioxide, family = "binomial", data = train_red)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.8895  -0.8648   0.3480   0.8581   2.2494  
# 
# Coefficients:
#                                 Estimate    Std. Error z value Pr(>|z|)    
# (Intercept)                      -6.782591   1.087976  -6.234 4.54e-10 ***
#   train_red$alcohol               0.681481   0.093467   7.291 3.07e-13 ***
#   train_red$sulphates             3.365727   0.640675   5.253 1.49e-07 ***
#   train_red$volatile.acidity     -2.923317   0.516878  -5.656 1.55e-08 ***
#   train_red$chlorides            -5.851908   1.890489  -3.095  0.00197 ** 
#   train_red$total.sulfur.dioxide -0.014561   0.003635  -4.006 6.18e-05 ***
#   train_red$free.sulfur.dioxide   0.027654   0.010713   2.581  0.00984 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1104.88  on 798  degrees of freedom
# Residual deviance:  858.19  on 792  degrees of freedom
# AIC: 872.19
# 
# Number of Fisher Scoring iterations: 4

attach(train_red)
model_train2<-glm(qual_level ~ alcohol + volatile.acidity + sulphates + chlorides +
                    total.sulfur.dioxide + free.sulfur.dioxide, family="binomial", data=train_red)

summary(model_train2)

deltaG2 <- model_train2$null.deviance-model_train2$deviance
1-pchisq(deltaG2, 3)

# Goodness of Fit works out
# [1] 0
boxplot(alcohol~qual_level, main="Boxplot of Red Alcohol Content vs. Quality", col="red")
# Clear that Alcohol Content is a major contributor to get to Quality Ratings of 7 and 8
boxplot(sulphates~qual_level, main="Boxplot of Red Quality and Sulphates", col="red")
# Sulphate also appears to contribute linearly to quality ratings.. relationship between qaultiy and predictor
boxplot(chlorides~qual_level, main="Boxplot of Red Quality and Chlorides", col="blue")
# Quality rating does not appear as sensitive to Chlorides
boxplot(pH~qual_level, main="Boxplot of Red Quality and pH", col="blue")
# Quality rating slighly senesitive to pH than it was before
boxplot(free.sulfur.dioxide~qual_level, main="Boxplot of Red Quality and Free Sulfur", col="red")
# of the plot suggesting less sensitivity (no real pattern) of quality rating to free sulfur dioxide
boxplot(total.sulfur.dioxide~qual_level, main="Boxplot of Red Quality and Total Sulfur Dioxide", col="blue")
# Quality rating appears slighlt sensitive to total sulfur dioxides (appears horizontal) compared to before
boxplot(volatile.acidity~qual_level, main="Boxplot of Red Quality and Vol. Acid", col="red")
# Clear strong linear relationship between quality rating and Vol. Acidity, unimodal curve


#####################
### DEV ROC CURVE ###
#####################
library(ROCR)
preds<-predict(model_train2,newdata=test_red, type="response")
rates<-prediction(preds, test_red$qual_level, label.ordering <- c("low", "high"))
# label.ordering <- c("low", "high") # not used

roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Red Wine Data Set", col='blue')
lines(x = c(0,1), y = c(0,1), col="red")
# ROC Curve is good
auc<-performance(rates, measure = "auc")
auc@y.values
#0.8254

##cutoff of 0.5
table(test_red$qual_level, preds>0.5)

#       FALSE TRUE
# low    262  106
# high    96  336

# False Positive Rating is our main concern due to financial implications
106/(106+262)
#28.8
# False Negative Rating
96/(96+336)
#22.2

table(test_red$qual_level, preds>0.7)
#       FALSE TRUE
# low    332  36
# high   197  235

# New False Positive Rate:
36/(36+332)
#0.0978

# New False Negative Rate:
197/(197+235)
# 0.456
summary(model_train2)

# Calculate the odds increases
### major potential odds increases
# little potential odds increases

### alcohol
exp(0.681481)
### 1.97

# volatile acidity
exp(-2.923317)
#0.05375509

### sulphates
exp(3.365727)
### 28.95454

# chlorides
exp(-5.851908)
# 0.00287441

# total sulfur dioxide
exp(-0.014561)
# 0.9855445

#free sulfur dioxide
exp(0.027654)
# 1.02804
