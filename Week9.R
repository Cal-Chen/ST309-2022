# Week 9
rm(list=ls())

# load the data
library(ISLR)
str(Hitters)
?Hitters

# number of rows that contains NA
sum(is.na(Hitters))

# remove those NA rows
is.na(Hitters) 
Hitters1 <- na.omit(Hitters)
row.names(Hitters)

# install package
# install.packages('leaps')
library(leaps)

# we want to find out a best linear model that describe the relationship between
# salary and other predictors
?regsubsets

# by default the maximum number of predictors included used in 'regsubsets' is 8
subset.Hitters <- regsubsets(Salary~., data = Hitters1)
summary(subset.Hitters)

# we can check the fitted model e.g best linear model with 3 predictors
subset3.Hitters <- lm(Salary~ Hits + CRBI + PutOuts, data = Hitters1)
summary(subset3.Hitters)

#check the components of the subset object
names(summary(subset.Hitters))

# regression coefficients R^2 (The larger the better but does not consider No. of predictors)
summary(subset.Hitters)$rsq
plot(summary(subset.Hitters)$rsq)

# BIC for all for 8 selected models (The smaller the better)
summary(subset.Hitters)$bic
plot(summary(subset.Hitters)$bic)
# BIC select the best subset regression with 6 regressors

# Cp values of 8 selected models (The smaller the better)
summary(subset.Hitters)$cp
plot(summary(subset.Hitters)$cp)
# it selects the one with 8 regressors

# visualize the selected model with different scale
par(mfrow = c(1,3))
plot(subset.Hitters,scale = 'r2', col=1)
plot(subset.Hitters,scale = 'bic', col=2)
plot(subset.Hitters,scale = 'Cp', col=3)

# ----  cross validation ----- #
# apply 10 folds cross validation to select the optimal number of predictors

# First randomly divide the original 263 observations into 10 folds
folds = rep(1:10,27)
folds <- folds[1:263] # cut it at length 263
folds

# randomize the order
folds <- sample(folds,263,replace = FALSE)
folds

# regsubsets does not have a built-in prediction function
# download the load code 'predict.regsubsets.r'
source('predict.regsubsets.r')
cv.errors = matrix(nrow =10,ncol=19)
for(j in 1:10){
  best.fit <- regsubsets(Salary~.,data = Hitters1[folds!=j,],nvmax = 19)
  for(i in 1:19){
    pred <- predict.regsubsets(best.fit,Hitters1[folds==j,],id= i)
    cv.errors[j,i] <- mean((Hitters1$Salary[folds==j] - pred)^2)
  }
}

cvError <- apply(cv.errors,2,mean)
par(mfrow = c(1,1))
plot(cvError,type= 'b')
which.min(cvError)

# NOTE THAT, repeating the above procedure may lead to different model
# because the division of 10 folds are random

# --- Cross validation for selecting tree models ---- #
library(tree)
attach(Carseats)
High=as.factor(ifelse(Sales<=8, "No", "Yes"))
Carseats2=data.frame(Carseats, High)
tree.carseats=tree(High~.-Sales, Carseats2)
summary(tree.carseats)

# apply CV method to determine the tree size
?cv.tree
?prune.misclass
# by default, it runs a 10-folds CV
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)

rbind(cv.carseats$size,cv.carseats$dev)

par(mfrow=c(1,1))
plot(cv.carseats$size,cv.carseats$dev,type ='b')

prune.carseats=prune.misclass(tree.carseats, best=14)
plot(prune.carseats)
text(prune.carseats, pretty=0)



