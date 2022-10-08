# Week 3 R session
rm(list=ls())

# --- PACKAGES --- #

# check whether the package is installed in the current system
require(ISLR)
# run the command to install 
# or under the Packages panel(bottom right window),click install and enter the name
install.packages(ISLR)
# load the package
library(ISLR)
# Lists of data sets in all loaded packages
data()
# list of data sets for a specific packages
data(package = "ISLR")
# Load one of them 
data(mtcars)

#--- formatting text output ---#

str3 <- "ST309 Elementary Data Analytics"
cat(substring(str3,1,5), substring(str3,18,), "\n")
# \n is the new line character
cat(substring(str3,1,5),"\n", substring(str3,18,))

roots.of.two <- 2^(1/1:10)
format(roots.of.two, digits=3)
cat("Roots of two: ", format(roots.of.two, digits=3),"\n")


# --- Arrays and matrix --- #

m1 <- array(1:15,dim = c(3,5))
# or equivalently use "matrix" function
m2 <- matrix(1:15,nrow=3,ncol=5)
# The data in filled in column direction

# For two matrix will the same dimension, the arithmetic operator
# will be applied in a element-wise way
m1 + m2
m1/m2
m1*m2

# for matrix multiplication use '%*%' operator
# transpose 't()' function
# inverse  'solve()' function
# eigen value,vectors 'eigen()' function
# singular value decomposition 'svd()' function
m1 %*% t(m2)
m3 <- matrix(c(2,0.5,0.5,1),2,2)
solve(m3)
eigen(m3)
svd(m3)

# --- writing your own functions --- #

# name <- function(arg1,arg2,...) expr1
# formal format
logistic <- function(r,x){
  r*x*(1-x)
}
# simplify format
logistic <- function(r,x) r*x*(1-x)

# evaluate the logistic function for different values of r
# and x (including vector values).
logistic(3,0.4)
logistic(3.5, seq(0,1,length=6))
# if both input is vector of length greater than 1
# make sure that they are of the same length
logistic(seq(1,3,0.5), seq(0,1,length=6))

# default value
logistic <- function(r,x=0.6) r*x*(1-x)
logistic(3)
logistic(10)

# if the function contains more than 1 line, use formal format
lmap1 <- function(r){
  temp <- logistic(r, 0.6)
  logistic(r, temp)
}

# when you want to run multiple commands in one line
# separate them by ;
lmap1(1);lmap1(0.6);lmap1(3)

# make use of the side effect 
# You don't have to make the function return something in the end
logisticplot <- function(r,lower,upper,npoints){ 
  x <- seq(lower,upper,length=npoints)
  plot(x,logistic(r,x), type = "l")
}
logisticplot(3,0,1,10)
logisticplot(3,0,1,100)

# --- Flow Control --- #
# for Loops
# A for loop often provides the most obvious implementation
# for (loop variable in sequence ) expr1
for (i in 1:5) print(i)

# To calculate the sum 1+2+...+100
totalValue <- 0 # initial 
for(i in 1:100){
  totalValue <- totalValue + i
}
totalValue

# calculate and print the total marks for each students
mk2nd <- read.table("marks2.txt", header=TRUE,row.names = 1)
mk2nd
attach(mk2nd)
# or, equivalently, for (i in 1:length(exam1))
for(i in 1:55) {
  ans <- exam1[i] + exam2[i] + exam3[i]
  cat(row.names(mk2nd)[i], " total marks: ", ans, "\n")
}

# while and repeat loop
# while (condition) expr
# while loop will repeatly run the "expr" inside until the 'condition' is met
j <- 1
while(j <= 6){
  print(j)
  j <- j+1
}
j

# or equivalently use 'repeat' together with 'break'
# For repeat loop, the 'break' is the only way to terminate the loop
j <- 1
repeat{
  print(j)
  j <- j + 1
  if(j > 6) break
}
j

# --- Conditional statement 'if else' --- #

# check whether the elements in x are positive or 
x = c(1,-1,2,-3)
y <- 0
for(i in 1:length(x)){
  if(x[i] >=0)
    y[i] <- 'non-negative'
  else
    y[i] <- 'negative'
}
y

# An example
# If there are multiple expressions after if else condition,
# enclose them by braces {}
for(i in 1:10){
  cat(i, ': ')
  if(i < 3){
    cat('small ')
    cat('numeric ') 
  }
  else if(i >= 3 & i < 7)
    cat('medium ')
  else
    cat('big ')
  cat('number \n')
}

# check whether the students pass or not
for(i in 1:length(exam1)){
  ans <- exam1[i] + exam2[i] + exam3[i]
  cat(row.names(mk2nd)[i], ': ')
  if(ans > 60) 
    cat('PASS \n')
  else 
    cat('FALL \n')
}

# A while loop can be useful when we have a condition for termination of the loop 
# rather than an exact point
npass <- 0 
j <- 1
tot <- exam1 + exam2 + exam3  # total marks for all candidates
while( npass < 10 & j <= length(exam1) ){
  if(tot[j] > 60){
    npass <- npass + 1
    cat(row.names(mk2nd)[j],": ", tot[j],'\n')
  }
  j <- j+1
}
j

# --- Vectorization and avoiding loops --- #
# loops are very inefficiently implemented in R
# It is now important to learn how to avoid them where ever possible
# many functions in R are already vectorized
# It means that you can input a vector or a matrix
ans <- exam1 + exam2 + exam3
cat(paste(row.names(mk2nd), "total:", ans),fill=TRUE) # fill the window
# string is of length 15-16
cat(paste(row.names(mk2nd), "total:", ans),fill= 15) # each line prints out at most 1 string
cat(paste(row.names(mk2nd), "total:", ans),fill= 32) # each line prints out at most 2 string
# fill: maximum of the characters(including space) that printed out in one line

pf <- ifelse(ans>=60, "PASS", "FAIL")
cat(paste(row.names(mk2nd), ":", pf),fill=15)


# download and read the data
nas <- scan("NDoct2003.dat")
naslr <- c(NA,-diff(log(nas)))
nasmat <- array(naslr, c(39,23))
nasmat <- nasmat[-1,] # throw away the first row

# apply: apply a function over an array
# apply(matrix,margin,fun), margin: 1 -> row; 2-> column
timesd <- apply(nasmat,1,sd)
length(timesd)
plot(timesd, type="l")


# --- Binary operator --- #
# you can create your own operator
# A plot-making operator
"%p%" <- function(y,x) plot(x, y, type="l", col=2)
x <- seq(0.1, 20, length=400)
log(x) %p% x
(0.3*cos(x) + 0.7*sin(2*x)) %p% x

# linear regression operator
"%r%" <- function(y,x){ 
  lmstore <- lm(y~x)
  lmstore$coefficients
}
exam2 %r% exam1

detach()


