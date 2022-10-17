# EX2
rm(list=ls())

# install and load the package
library('ISLR')
data("College")

#(a)
summary(College)

#(b)
pairs(College[,1:10])

pairs(College[1:100,10])


#(c)
attach(College)
plot(Private, Outstate, col=c("red","blue"), xlab="Private", 
     ylab="Outstate Tuition", main="Boxplots of out-of-state tuition of US colleges",
     cex.main=2, cex.label=2, cex.axis=1.5)


#(d)
Elite <- ifelse(Top10perc>50, "Yes","No")
Elite <- as.factor(Elite)
# As we don't want to overwrite the College,
# create a new data frame using the identifier 'college'
college <- data.frame(College,Elite)
# or equivalently
college <-cbind(College,Elite)
write.table(college,'college.txt')

# make the boxplot
plot(Elite,Outstate,col=c("red","blue"), xlab="Elite", 
     ylab="Outstate Tuition", main="Boxplots of out-of-state tuition of US colleges",
     cex.main=2, cex.label=2, cex.axis=1.5)

#(e)
par(mfrow=c(2,3))
hist(Apps, nclass=25)
hist(Accept, nclass=25)
hist(Enroll, nclass=25)
hist(Top10perc, nclass=25)
hist(F.Undergrad, nclass=25)
hist(Room.Board, nclass=25)


#(f)
par(mfrow=c(1,1))
#(i) Is the student/faculty ratio a determined factor for the graduation rate
plot(S.F.Ratio, Grad.Rate)

#(ii) Is a private college more likely to be elite?
table(Private,Elite)
13/(199+13);65/(500+65) # yes

#(iii) Are the alumni from private colleges more likely to donate?
plot(Private,perc.alumni,col=c("red","blue"), xlab="Private", 
     ylab="Percentage of alumni who donate", 
     cex.main=2, cex.label=2, cex.axis=1.5)

#(iv) Is the percentage of faculty with PhD degrees important for a college?
pairs(data.frame(PhD,Grad.Rate,perc.alumni))
cor(data.frame(PhD,Grad.Rate,perc.alumni))

# Q5
#(a) 
# -------a=====b -------- 30+a====30+b ----#
# (b-a) times
#(b) write a function to simulate the process
Johnexp <- function(a,b){
  arrival_time <- runif(1,0,60)
  cond1 <- arrival_time > a & arrival_time < b
  cond2 <- arrival_time > 30 + a & arrival_time < 30 + b
  if(cond1 | cond2){
    cat('John will vist his mother')
  }else{
    cat('John will visit his girlfriend')
  }
}

# we can utility the function to check the expectations
Johnexp2 <- function(a,b){
  arrival_time <- runif(30,0,60)
  cond1 <- arrival_time > a & arrival_time < b
  cond2 <- arrival_time > 30 + a & arrival_time < 30 + b
  y <- ifelse(cond1|cond2,1,0)
  return(y)
}

# set up the parameters
a <- 1 ; b<- 25
repition <- 10000
visit_mother <- 0
for(i in 1:repition){
  visit_mother[i] <- sum(Johnexp2(a,b))
}
# check the numerical expectation and theoretical expectations
mean(visit_mother)
b - a

# Q6
# The way using loops
random_triangle <- function(repition){
  y <- 0
  for(i in 1:repition){
    sides <- runif(3,0,1)
    # check the condition
    cond1 <- sides[1] + sides[2] > sides[3]
    cond2 <- sides[1] + sides[3] > sides[2]
    cond3 <- sides[2] + sides[3] > sides[1]
    y[i] <- ifelse(cond1 & cond2 & cond3,1,0)
    # or equivalently 
    # sides <- sort(sides)
    # y[i] <- ifelse(sides[1] + sides[2] > sides[3],1,0)
  }
  prob  <- mean(y)
  cat('The probability of generating a triangle is:',prob)
}
# The probability is around 50%
random_triangle(100000)

# The way without loop (but more computational intensive because the creation of large matrix)
random_triangle2 <- function(repition){
  sides <- matrix(runif(3*repition),nrow=repition,ncol=3)
  # treat each row as one simulation of three sides
  # and sort them in ascending order
  for(i in 1:repition) sides[i,] <- sort(sides[i,])
  y <- (sides[,1] + sides[,2] > sides[,3])*1
  prob <- mean(y)
  cat('The probability of generating a triangle is:',prob)
}

# the correct way for vectorization 
random_triangle3 <- function(repition){
  side1 <- runif(repition)
  side2 <- runif(repition)
  side3 <- runif(repition)
  y <- side1 + side2 > side3 & side1 + side3 > side2 & side2 + side3 > side1
  prob <- mean(y)
  cat('The probability of generating a triangle is:',prob)
}

# check the running time for two version
repition <- 1e5

system.time(random_triangle(repition))
system.time(random_triangle2(repition))
system.time(random_triangle3(repition))
