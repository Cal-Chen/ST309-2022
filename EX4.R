
rm(list = ls())
library(tree)

# Q1
# (a)
college <- read.table('college.txt')
str(college)

#convert character string to factor
college$Elite <- as.factor(college$Elite)
college$Private <- as.factor(college$Private)

tree1 <- tree(Elite~. -Top10perc,data=college)
summary(tree1)
plot(tree1)
text(tree1,pretty = 1)

#(b)
set.seed(100)
train <- sample(1:nrow(college),500)
tree2 <- tree(Elite~. -Top10perc,data=college[train,])
summary(tree2)
plot(tree2)
text(tree2,pretty = 1)

#check mis-classification rate of this model
tree.predict=predict(tree2, college[-train,], type="class") 
table(tree.predict, college$Elite[-train], deparse.level = 2)

# mis-classification rate
(6+9)/(nrow(college) - 500)

# fit a logistic regression model
# include all the variables used in the tree model (tree1 and tree2)
logistic1 <- glm(Elite~ Top25perc + S.F.Ratio  +Expend+
                        P.Undergrad  +  perc.alumni + Room.Board +
                        Outstate + Personal + Apps,
                data = college[train,],family = 'binomial')
summary(logistic1)

# remove the one with largest p-value S.F.Ratio
logistic2 <- glm(Elite~ Top25perc + Expend +
                   P.Undergrad  +  perc.alumni + Room.Board +
                   Outstate + Personal + Apps,
                 data = college[train,],family = 'binomial')
summary(logistic2)

# remove Apps
logistic3 <- glm(Elite~ Top25perc + Expend +
                   P.Undergrad  +  perc.alumni + Room.Board +
                   Outstate + Personal,
                 data = college[train,],family = 'binomial')
summary(logistic3)

# remove personal
logistic4 <- glm(Elite~ Top25perc + Expend +
                   P.Undergrad  +  perc.alumni + Room.Board +
                   Outstate,
                 data = college[train,],family = 'binomial')
summary(logistic4)

# remove Perc.alumni
logistic5 <- glm(Elite~ Top25perc + Expend +
                   P.Undergrad + Room.Board +
                   Outstate,
                 data = college[train,],family = 'binomial')
summary(logistic5)

#remove Room.Board
logistic6 <- glm(Elite~ Top25perc + Expend +
                   P.Undergrad +
                   Outstate,
                 data = college[train,],family = 'binomial')
summary(logistic6)

# calculate the prediction error
logistic.predict=predict(logistic6, college[-train,], type="response")
Elite.pred=rep("No",length(college$Elite[-train]))
Elite.pred[logistic.predict>0.5]="Yes"
table(Elite.pred, college$Elite[-train],deparse.level = 2)

# mis-classification rate
(6+5)/(244+22+11)


# Q2
# (a)
library(ISLR)
str(Auto)
auto <- Auto[,-9]

pairs(auto)

# (b)
cor(auto)

#(c)
lm1 <- lm(mpg~.,data = auto)
summary(lm1)

#(d)
par(mfrow=c(2,2))
plot(lm1) 

#(e)
pairs(data.frame(log(Auto$mpg),Auto[,-c(1,9)]))


#Q3
#(a)
set.seed(3)
x1=runif(150) # 150 U(0,1) random numbers
x2=0.5*runif(150)+rnorm(150)/5 # rnorm(150) returns 150 N(0,1) random numbers
y=2+2*x1+x2+rnorm(150)

#(i)
# beta0 = 2, beta1 = 2, beta2 = 1, standard normal 
true_beta <- c(2,2,1)

# Typical things to check after fitting a linear model
# 1) the significance of coefficients and its confidence interval
# 2) the estimated standard error (the 'size' of random noise)
# 3) the Multiple R-square (the the proportion explained by the predictors)
# 4) Diagnostic plots of residuals (appropriateness of linear model)

#(ii)
ytrain = y[1:100];ytest <- y[101:150]
x = data.frame(x1,x2)
xtrain <- x[1:100,];xtest <- x[101:150,]
m1 <- lm(ytrain~ x1 + x2, data=xtrain)
summary(m1) # y = 1.9065 +2.0503x1 + 1.1604x2
cbind(confint(m1),true_beta)

#(iii)
pred.lm <- predict(m1, xtest)
m1.rmspe <- sqrt(mean((ytest - pred.lm)^2))
m1.rmspe


# (b)
set.seed(3)
x1=runif(150)
x2=0.5*x1+rnorm(150)/5
y=2+2*x1+x2+rnorm(150)
#(i)
cor(x1,x2)

#(ii)
ytrain = y[1:100];ytest <- y[101:150]
x = data.frame(x1,x2)
xtrain <- x[1:100,];xtest <- x[101:150,]
m2 <- lm(ytrain~ x1 + x2, data=xtrain)
summary(m2) # y = 2.3265 + 1.8250x1 + 0.4190x2
cbind(confint(m2),true_beta)


#(iii)
pred.lm <- predict(m2,newdata = xtest)
m2.rmspe <- sqrt(mean((ytest - pred.lm)^2))
par(mfrow = c(2,2))
plot(m2)


#(iv)
m3 <- lm(ytrain~ x1, data=xtrain)
summary(m3)

pred.lm <- predict(m3,newdata = xtest)
MSPE <- mean((ytest - pred.lm)^2)
rMSPE <- sqrt(MSPE)
MSPE;rMSPE
plot(m3)


#(c)
set.seed(3)
x1=runif(150)
epsilon=rnorm(150)
x2=0.5*runif(150)+epsilon/5
y=2+2*x1+x2+epsilon

#(i)
cor(epsilon,x2)

#(ii)
ytrain = y[1:100];ytest <- y[101:150]
x = data.frame(x1,x2)
xtrain <- x[1:100,];xtest <- x[101:150,]

# lm(ytrain~ 2 + 2*x1 + x2, data=xtrain)

m4 <- lm(ytrain~ x1 + x2, data=xtrain)
summary(m4)
cbind(confint(m4),true_beta)

pred.lm <- predict(m4,newdata = xtest)
m4.rmspe <- sqrt(mean((ytest - pred.lm)^2))
m4.rmspe
plot(m4)

