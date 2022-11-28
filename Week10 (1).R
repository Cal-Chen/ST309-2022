# Week 10 
rm(list=ls())

# Classification decision:
# Given a feature X, the model will give a probability P(Yes|X)
# If P(Yes|X) > t, classify as 'Yes'. Otherwise, classify it as 'No'.
# e.g. logistic regression, tree 

# confusion matrix
#        Positive         Negative
#  Yes  True Positive  False Positive
#  No   False negative  True negative

# ROC graph plot True positive (y) against False positive (x)

# True Positive rate = P( Yes | Positive) 
# False positive rate = P( Yes | Negative) 

# install package
# install.packages('ROCR')

spamData = read.table('spam.txt')
spamNames <- read.table('spamNames.txt')
names(spamData) <- spamNames[,1]
names(spamData)


attach(spamData)
spam <- rep('No',4601); spam[LABEL == 1] = 'Yes'
# make it into factor
spam <- as.factor(spam)

#By default, column names will be checked to ensure syntactically valid variable names
spamData1 <- data.frame(spamData,spam)
names(spamData)
names(spamData1)
# or we can correct those invalid syntax manually
make.names(names(spamData),unique = TRUE)

# split the data set into training and testing 
set.seed(3)
train <- sample(1:4601,3065,replace = FALSE)
spamTest <- spamData1[-train,]

# fit a regression tree
library(tree)
tree1 = tree(spam~.-LABEL,data = spamData1,subset = train)
summary(tree1)

# select the optimal number of terminal nodes. Cross Validation
tree2 <- cv.tree(tree1,FUN = prune.misclass)
rbind(tree2$size,tree2$dev)

# prediction
?predict.tree
#use predict() not predict.tree
predT <- predict(tree1,spamTest,type='vector')
#predicted probabilities for 'Yes' (Spam) class
head(predT)
predTest.tree <- predT[,2]

# predicted value(fitted value) for training data
predT <- predict(tree1,spamData1[train,],type='vector')
predTrain.tree <- predT[,2]

#ROC curve from pROC package
library(pROC)
roc.info <- roc(LABEL[train], predTrain.tree, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
                xlab="False Positive Rate", ylab="True Positive Rate", col="#377eb8", lwd=4)

roc.df <- data.frame(tpp=roc.info$sensitivities, fpp=100-roc.info$specificities, thresholds=roc.info$thresholds)
roc.df
predLab <- ifelse((predTrain.tree >= 0.289166667),'Yes','No')

confusion <- table(predLab,spam[train],deparse.level = 2)

#tpp
confusion[2,2]/sum(confusion[,2])
#fpp
confusion[2,1]/sum(confusion[,1])

#plot the ROC plots from ROCR package
library(ROCR)
?`plot-methods`
par(mfrow = c(1,2))

?prediction
?performance
prediction.treeTest <- prediction(predTest.tree,LABEL[-train])
prediction.treeTrain <- prediction(predTrain.tree,LABEL[train])
rocTest <- performance(prediction.treeTest,measure = 'tpr',x.measure = 'fpr')
rocTrain <- performance(prediction.treeTrain,measure= 'tpr',x.measure = 'fpr')
# colorize = T adds the color code to the ROC curve according to the cut-off probability for 'Yes'
plot(rocTest,lwd=2,colorize = T,colorkey.width=0.5, main='ROC curve on testing data')
abline(0,1)
plot(rocTrain,lwd=2,colorize = T, main='ROC curve on testing data')
abline(0,1)

# increase the threshold
# confusion matrix
#        Positive         Negative
#  Yes  True Positive ++ False Positive ++
#  No   False negative --  True negative --

# In practice, we need to choose a cut-off probability. Convenient choice is 0.5
# However, we need to consider cost-benefits for different scenarios.

predLab <- ifelse((predTrain.tree >= 0.5),'Yes','No')
confusion <- table(predLab,spam[train],deparse.level = 2)
confusion
confusion/sum(confusion)

# create the cost benefit matrix
CB <- matrix(c(0,-1,-4,0),nrow = 2,byrow = T)
# we assign larger cost to false positive than false negative
# Usually, it is more annoying that important emails are classified as in junk
benchmark <- sum(CB*confusion)/sum(confusion)
benchmark

# To reduce the false positive, we can increase the cut-off probability
alpha=seq(0.5, 0.95, 0.01)
eBenifit=vector(length=length(alpha))

for(i in 1:length(alpha)) {
  predLab=ifelse((predTrain.tree>=alpha[i]), "Yes","No")
  confusion=table(predLab, LABEL[train],deparse.level=2)
  eBenifit[i]=sum(CB*confusion)/sum(confusion)
}
# visualize it 
plot(alpha, eBenifit, type="l", lwd=3, col="darkred", xlab="Cut-off Probability",
     ylab="Expected benifit")
# find out the maximum one
alpha[eBenifit == max(eBenifit)]

# compare the cost with benchmark
eBenifit[alpha == 0.50]
eBenifit[alpha == 0.77]

# ---- comparison of model by comparing the area under ROC curve ----- #

# To calculate the area under the ROC curve
performance(prediction.treeTrain,measure = 'auc')@y.values
performance(prediction.treeTest,measure = 'auc')@y.values

# fit a knn model
# We only use the variables used in fitting a tree model
varNames <- as.character(summary(tree1)$used)
X <- spamData1[,varNames]

# split into train and test data
Xtrain <- X[train,]
Xtest <- X[-train,]

# Since the data is sparse(it contains many 0), we use the correlation based distance
# 1- Corr
D = 1 - cor(t(Xtest),t(Xtrain)) 
dim(D)
# cor(X1,X2) returns correlation between columns of two matrix
inDex=matrix(nrow=1536, ncol=5)
for(i in 1:1536) inDex[i,]=sort.int(D[i,], index.return = T)$ix[1:5]
# contains the row indices of the 5 NN of Xtest[i,] among

# predict based on 3NN and 5NN
predKNN=matrix(nrow=1536, ncol=2)
Y=LABEL[train]
for(i in 1:1536) predKNN[i,]=c(mean(Y[inDex[i,1:3]]), mean(Y[inDex[i,]]))
summary(predKNN)

# Draw ROC curve
pred3=prediction(data.frame(predKNN, predTest.tree), data.frame(LABEL[-train],
                  LABEL[-train],LABEL[-train]))
roc3=performance(pred3, measure ="tpr", x.measure ="fpr")

plot(roc3, col=as.list(c("red","blue","green")), 
     main="ROC curves of 3 classifiers for spam emails on testing data")
legend(0.8, 0.4, c("3-NN","5-NN", "tree"), col=c("red","blue","green"), lty=c(1,1,1))
abline(0,1)

# From the ROC curve, the tree model performs the best
performance(pred3, measure ="auc")@y.values

# To produce over all accuracy rate (1 - misclassification rate)
acc3=performance(pred3, measure="acc")
plot(acc3, lwd=2, col=as.list(c("red","blue","green")),
     main="Accurancy curves of 3 classifiers for spam emails on testing data")
legend(0.8, 0.55, c("3-NN","5-NN", "tree"), col=c("red","blue","green"),
       lty=c(1,1,1))
detach(spamData)
