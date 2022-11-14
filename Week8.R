# week 8

rm(list = ls())
library(MASS)

?Boston
attach(Boston)
str(Boston)

?lm

lm1.Boston=lm(medv~lstat)
summary(lm1.Boston)

# check the component of this lm object
names(lm1.Boston)
lm1.Boston$rank

# confidence interval(95% confidence by default)
confint(lm1.Boston)

# To access the fitted values
# y = beta0 + beta1 * lstat  + epsilon
fitted1 <- lm1.Boston$fitted.values
fitted2 <- predict(lm1.Boston)

head(fitted1)
head(fitted2)

# residual
head(medv - fitted1)
head(lm1.Boston$residuals)

# Prediction on expected value of y 
# confidence interval for fitted value (95% by default)
# y  = beta0 + beta1 x 
predict(lm1.Boston, data.frame(lstat=c(5,10,15)), interval="confidence")

# prediction on individual value of y
# confidence interval for predicted value (larger interval)
# y = beta0 + beta1 x + epsilon
predict(lm1.Boston, data.frame(lstat=c(5,10,15)), interval="prediction")

# draw a fitted line across the data
plot(lstat, medv)
abline(lm1.Boston,lty=2,col=2)

# draw diagnostics plots
par(mfrow=c(2,2))

plot(lm1.Boston)
# residual plots are clearly not patternless!

# H matrix
# Y = X Beta + Epsilon
# Beta_hat = (X^T X)^(-1) X^T Y
# Y_hat = X Beta_hat = X (X^T X)^(-1) X^T Y =  H Y
X <- cbind(rep(1,nrow(Boston)),lstat)
Y <- as.matrix(medv)
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
H <- X %*% solve(t(X) %*% X) %*% t(X)
diag(H)
sum(diag(H)) # which is p + 1 = 2, as we have only one predictor so p=1 

# standardized residual
res <- lm1.Boston$residuals
s_res <- (res - mean(res))/sd(res)
#leverage plot
plot(diag(H),s_res,main='Residual vs Leverage',
     xlab = 'Leverage',ylab = 'Standardized Residuals')

# based on the leverage plot
# 215 413 and 375 are bad leverage point, we need to remove it from the analysis
Boston1=Boston[-c(215,413,375),] # remove 3 bad leverage points
dim(Boston1)

lm11=lm(medv~lstat, data =Boston1) # Not lm(medv~lstat) now!
summary(lm11)
plot(lm11)

# try including all the predictors
lm2.Boston=lm(medv~., Boston) # include all the predictors
summary(lm2.Boston)

# remove the one with largest p values, age
lm3.Boston = lm(medv~.-age, Boston)
summary(lm3.Boston)

#Similarly, remove indus
lm4.Boston = lm(medv~.-age-indus, Boston)
summary(lm4.Boston)

plot(lm4.Boston)
# linear model may not be a good choice in this situation


# ---- step wise regression with 'step' ---- #
lm0.Boston = lm(medv~1)
mean(medv)
summary(lm0.Boston)

# The selected model will be 'between' ls0.Boston and ls2.Boston.
step.Boston = step(lm0.Boston, scope=list(upper=lm2.Boston))
summary(step.Boston) # it selects the the same predictors as in lm4.Boston

# actually step Boston is the same as lm4.Boston
sort(names(step.Boston$coefficients))
sort(names(lm4.Boston$coefficients))

detach(Boston)

# --- Regression with qualitative Predictors --- #
data(Carseats,package = 'ISLR')
str(Carseats)
?ISLR::Carseats

# interaction terms Income:Advertising and Price:Age in the model
# Income x Advertising and Price x Age

lm.Sales=lm(Sales~. + Income:Advertising + Price:Age, data=Carseats)
summary(lm.Sales)

attach(Carseats)
# encoding of dummy variables
# sales = beta0 + .... + beta_k Good + beta_{k+1} Medium + .... + epsilon
contrasts(ShelveLoc)

# --- Cross validation for selecting tree models ---- #
library(tree)
High=as.factor(ifelse(Sales<=8, "No", "Yes"))
Carseats2=data.frame(Carseats, High)
tree.carseats=tree(High~.-Sales, Carseats2)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

# apply CV method to determine the tree size
?cv.tree
?prune.misclass
# by default, it runs a 10-folds CV

# Suppose the sample size is 20 
# 10 folds: 
# 11 | 22 | 33 | 44 | 55 | 66 | 77 | 88 | 99 | 10 10 |
# Each model would go through 10 times for fitting and computing MSE on validation set

set.seed(1000)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
rbind(cv.carseats$size,cv.carseats$dev)

par(mfrow=c(1,1))
plot(cv.carseats$size,cv.carseats$dev,type ='b')

prune.carseats=prune.misclass(tree.carseats, best=12)
plot(prune.carseats)
text(prune.carseats, pretty=0)
