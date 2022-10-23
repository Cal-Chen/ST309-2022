# Week 5 

# R session 4: Distribution and simulation
rm(list=ls())

# Discrete random variable
# Binomial, Poisson, Geometric,Negative binomial

# Continuous random variables
# uniform,normal,student's t, exponential, gamma, chi-squared

# prefix for random variable function in R
# runif() == r + unif
# r - randomly generate sample
# d - probability mass function/ density function 
# p - cumulative probability function P(X <= c) (cdf) 
# q - quantile function P(X <= q) >= p

# Examples: normal random variables

# Generating 10 random samples from N(10,2^2)
rnorm(10,mean = 10,sd = 2)

# density function f(x) evaluated at x = 10 
dnorm(10,10,2)

# CDF P(X <= 10) for X ~ N(10,2^2)
pnorm(10,10,2) # it should be 0.5

# find out k that satisfies P(X <= k) >= 0.5 
qnorm(0.5,10,2) # it should be 10


# Exercise

# ---------- CDF -----------#
# Suppose X ~ Bin(85, 0.6) and Y ~ N(12, 9) and that X and Y are independent 

# P(X <= 60)
pbinom(60,85,0.6)

# P(X >= 60) = 1 - P(X < 60) = 1 - P(X <= 59)
1- pbinom(59,85,0.6)
pbinom(59,85,0.6,lower.tail = FALSE)

# P(Y <= 15)
pnorm(15,12,3)

# P(X < 60, Y > 15) = P(X <= 59) (1 - P(Y <= 15))
pbinom(59,85,0.6)*(1 - pnorm(15,12,3))

# ----------- quantiles --------------#
# P(X <= q) >= 0.8
qbinom(0.8,85,0.6)
pbinom(55,85,0.6)

# P(Y < q) = 0.25
qnorm(0.25)

# P(X >= q) < 0.6 == P(X < q) > 0.4 == P(X <= q - 1) > 0.4
qbinom(0.4,85,0.6) + 1

# P(Y <= q) = 0.25
qnorm(0.25,12,3)

# ------------ density ------------- #

# t distribution with different degree of freedom
x <- seq(-5,5,length=200)
plot(x, dnorm(x), type="l")
lines(x, dt(x,df=16), col=2)
lines(x, dt(x,df=8), col=3)
lines(x, dt(x,df=4), col=4)
lines(x, dt(x,df=2), col=5)
lines(x, dt(x,df=1), col=6)

# shape and scale of gamma function
x <- seq(0,10,length=200)
plot(x, dgamma(x,shape=1,scale=1), type="l")
lines(x, dgamma(x,shape=1,scale=2), col=2)
lines(x, dgamma(x,shape=1,scale=4), col=3)

plot(x, dgamma(x,shape=2,scale=1), type="l")
lines(x, dgamma(x,shape=2,scale=2), col=2)
lines(x, dgamma(x,shape=2,scale=4), col=3)

# fit an empirical density
# calculate and print the total marks for each students
marks2 <- read.table("marks2.txt", header=TRUE,row.names = 1)
allmarks <- marks2[marks2<41]

x <- seq(5,45,length=200)
hist(allmarks, breaks=seq(5,40,length=14), probability = TRUE)
lines(x,dnorm(x, mean=mean(allmarks), sd = sd(allmarks)),col=2)
# fit an empirical cdf
plot(ecdf(allmarks))
lines(x,pnorm(x, mean=mean(allmarks), sd = sd(allmarks)),col=2)

# -------- generate random samples -------- #
poissamp <- rpois(400, lambda=2)
hist(poissamp, breaks=0:10, probability=TRUE)

normsamp <- rnorm(250, mean=10, sd=5)
hist(normsamp, breaks=seq(-10,30,length=15), probability=TRUE)

x <- seq(-10,30,length=200)
lines(x, dnorm(x, mean=10, sd=5), col=2)

# The command set.seed(...) allows us to determine the starting point 
# of the iterative process and thus ensure identical output 
# from the random number generator.

set.seed(1)
rnorm(5)

set.seed(1)
rnorm(5)

# The functions sample(...) can be used to generate random permutations
nvec <- 10:19
sample(nvec)
sample(nvec,5)

# generate discrete uniform distribution 
# rolling a dice
sample(1:6,20, replace=TRUE)

#------ simulation experiment ----- #
# Explore distribution of sample mean, where random samples are generated from Poisson
poisSampMean1 <- function(n,lambda,r){ 
  meanvec <- c()
  for (j in 1:r){ 
    sampvals <- rpois(n, lambda)
    meanvec[j] <-  mean(sampvals)
  }
  meanvec
}


# check how the sample size n would affect the distribution of sample mean
lambda <- 2
r <- 100
n <- seq(100,600,100)
mean1 <- sapply(n,function(k) poisSampMean1(k,lambda,r))
apply(mean1,2,function(x) c(mean(x),var(x)))

# the distribution are becoming more centralized
par(mfrow=c(2,3))
for(i in 1:6){
  hist(mean1[,i],breaks=seq(1.5,2.5,length=15),main = paste0('n=',n[i]))
}

# check how the number of replications r would affect the distribution of sample mean
lambda <- 2
r <- c(10,50,100,200)
n <- 100
mean2 <- sapply(r,function(k) poisSampMean1(n,2,k))
lapply(mean2,function(x) c(mean(x),var(x)))

# the distribution are almost the same, since 
par(mfrow=c(2,2))
for(i in 1:4){
  hist(mean2[[i]],breaks=seq(1.4,2.7,length=15),main = paste0('r=',r[i]))
}

# draw a histogram and plot a normal distribution with the same mean and standard deviation
histNorm <- function(data, nbins=21){ 
  hist(data, breaks=seq(min(data), max(data), length=nbins),
       probability=TRUE, col=5)
  x <- seq(min(data), max(data), length=200)
  lines(x, dnorm(x, mean=mean(data), sd=sd(data)), col=2)
}
histNorm(poisSampMean1(8,1,1000))
histNorm(poisSampMean1(50,1,1000))
histNorm(poisSampMean1(100,1,1000))
histNorm(poisSampMean1(200,1,1000))

# normality test function 
shapiro.p <- function(data) shapiro.test(data)$p.value
?shapiro.test

# 
poisSW <- function(nvec, lambdavec, r){ 
  numn <- length(nvec); ntot <- sum(nvec)
  numlam <- length(lambdavec)
  
  simvals <- rpois(ntot*numlam*r, lambdavec)
  
  lamvals <- factor(rep(lambdavec, times=ntot*r))
  samps <- factor(rep(1:(numn*r), times=rep((nvec*numlam), each=r) ))
  
  simres <- as.vector(tapply(simvals, list(lamvals, samps), mean))
  
  lamvals2 <- factor(rep(lambdavec, times=numn*r))
  sims <- factor(rep(nvec, each=numlam*r))
  tapply(simres, list(lamvals2, sims), shapiro.p)
}

# large lambda or large sample size
nvec <- c(5,10,50)
lambdavec <- c(1,3,10)
r <- 10

rpois(10,c(1,10))

#############################################
# X1Y1Z1 X2Y2Z2 X3Y3Z3 ..... X(n_t*r) Y(n_t*r) Z(n_t*r)
# 1 3 10 1 3 10 1 3 10 .....     1       3        10
# 1 1  1 1 1  1 1 1  1          30       30       30

# Ux1 Uy1 Uz1 Ux2 Uy2 Uz2 ...... Uxr Uyr Uzr 
#  1   3   10  1   3  10          1   3   10
#  5   5   5   5   5   5 .......  50  50  50

poisSW(nvec, lambdavec, r)

rpois(5,lambda = c(1,2))

