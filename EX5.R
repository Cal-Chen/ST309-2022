# week 11
rm(list=ls())

# Q1
library(ISLR)
#check the structure
str(OJ)

set.seed(3)
train <- sample(1:nrow(OJ),750) #training sample size is 750

# (a)
library(tree)
attach(OJ)

OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]
OJ.tree <- tree(Purchase~. ,data = OJ.train)
summary(OJ.tree)

plot(OJ.tree)
text(OJ.tree,pretty = 0)

#(b) check the performance on testing data
OJ.treePredict <- predict(OJ.tree,newdata = OJ.test,type= 'class')
table(OJ.treePredict,OJ.test$Purchase)

#misclassification rate
(23+43)/nrow(OJ.test)

#(c)
# install.packages("randomForest")
library(randomForest)
?randomForest
OJ.bag <- randomForest(Purchase~.,data=OJ.train,mtry=17,importance = T)
OJ.bag

# check the performance on testing data
OJ.bagPredict <- predict(OJ.bag,newdata = OJ.test,type='class')
table(OJ.bagPredict,OJ.test$Purchase)

# miclassification rate
(20+37)/nrow(OJ.test)

importance(OJ.bag)
#visualize the importance of variables
varImpPlot(OJ.bag, col=c("blue","red"))

# (d)
# sqrt(17)
OJ.RF <- randomForest(Purchase~.,data=OJ.train,mtry=5,importance = T)
OJ.RF

OJ.RFPredict <- predict(OJ.RF,newdata = OJ.test,type='class')
table(OJ.RFPredict,OJ.test$Purchase)

# miclassification rate
(30+24)/nrow(OJ.test)

detach(OJ)


#Q4
customers=read.csv("wholesaleCustomers.csv")
attach(customers)
str(customers)
customers6=customers[,3:8]
km5=kmeans(customers6,5,nstart=20)
table(Channel, km5$cluster)

# the cluster No.s are slightly different fromt the exercise
C1=rep("No",440)
C1[km5$cluster==1]="Yes"
C1 <- factor(C1)
customerC1=data.frame(customers6, C1)
treeC1=tree(C1~.,customerC1)
summary(treeC1)

plot(treeC1,col='blue',lwd=3,type='uniform')
text(treeC1)


#(b) repeat the analysis for cluster 2 and 3
C2 <- rep('No',440)
C3 <- rep('No',440)
C2[km5$cluster == 2] <- 'Yes'
C3[km5$cluster == 3] <- 'Yes'
C2 <- factor(C2)
C3 <- factor(C3)

customerC2 <- data.frame(customers6,C2)
customerC3 <- data.frame(customers6,C3)

treeC2 <- tree(C2~.,customerC2)
treeC3 <- tree(C3~.,customerC3)

plot(treeC2,col='blue',lwd=3,type='uniform')
text(treeC2)

plot(treeC3,col='blue',lwd=3,type='uniform')
text(treeC3)

detach(customers)

#Q5
#(a)
f2000=read.csv("Forbes2000In2017.csv", skip=6, header=T)
f2000n=na.omit(f2000)

#(b)
hc.f2000=hclust(dist(f2000n[,4:7]), method="complete") 
C20=cutree(hc.f2000, 20) 
table(C20)

#(c)
attach(f2000n) 
# complete linkage
# Complete-linkage (farthest neighbor) is where distance is measured 
# between the farthest pair of observations in two clusters.
hc.f30c=hclust(dist(f2000n[1:30,4:7]), method="complete") 
plot(hc.f30c, label=Company.Name[1:30], main="Cluster Dendrogram with Complete Linkage",
     col="turquoise4", xlab="", cex.main=2)

# single linkage
# Single-linkage (nearest neighbor) is the shortest distance 
# between a pair of observations in two clusters
hc.f30s=hclust(dist(f2000n[1:30,4:7]), method="single")
plot(hc.f30s, label=Company.Name[1:30], 
     main="Cluster Dendrogram with Single Linkage", col="darkred", xlab="", cex.main=2)
