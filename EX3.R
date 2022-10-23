
rm(list=ls())

# slide Chap3 page 25
library(tree)
library(ISLR)

attach(Carseats)
High=factor(ifelse(Sales<=8, "No", "Yes"))
Carseats2=data.frame(Carseats, High)
str(Carseats2)

tree.carseat <- tree(High~. - Sales, Carseats2)
summary(tree.carseat)
detach()

# EX 3
# tree model
x <- c(1,0,1,
       0,0,0,
       1,1,1,
       1,1,1,
       1,1,1,
       0,1,0,
       1,1,1,
       1,0,1,
       1,0,1,
       1,1,1,
       1,0,1,
       0,0,1)
X <- matrix(x,ncol=3,byrow = T)
YX <- data.frame(X)
names(YX) <- c('Head_Shape','Body_Shape','Body_Color')
YX$Churn <- c(0,1,1,1,1,0,1,0,0,1,0,1) # 1 indicates Churn

Churn_tree <- tree(Churn~., data = YX,
                   control = tree.control(12,mincut = 1,minsize = 2))
summary(Churn_tree)
plot(Churn_tree, col="red", lwd=2) # lwd defines the width of lines
text(Churn_tree)

