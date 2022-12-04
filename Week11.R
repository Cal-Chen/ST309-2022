# week 11
rm(list=ls())

# ggplot
library(ggplot2)

# check the mpg data set from ggplot package
?mpg

str(mpg)
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

 # First argument of ggplot is the dataset
# First 2 arguments of aes() are always mapped to x and y.
# geom: layers of plot, it can be point, line, histogram, etc
# also try 
ggplot(mpg, aes(cty, hwy)) + geom_point()
ggplot(diamonds, aes(carat, price)) + geom_point()
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(mpg, aes(cty)) + geom_histogram()

# additional aesthetics
attach(mpg)
# check the No of categories in drv
unique(drv)
ggplot(mpg,aes(displ,cty,shape=drv)) + geom_point()

unique(class)
ggplot(mpg,aes(displ,cty,colour=class)) + geom_point()

unique(cyl)
ggplot(mpg,aes(displ,cty,size=cyl)) + geom_point()

# Faceting:
# creates tables of graphics by splitting the data into subsets and 
# displaying the same graph for each subsets
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
unique(mpg$class)

# For a noisy scatter plot, add smoothed or straight line to reveal the dominant pattern.
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()

# fit a linear model
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method="lm")


# install.packages("gridExtra")
library(gridExtra)
plot1=ggplot(mpg, aes(displ, colour=drv)) + geom_freqpoly(binwidth=0.5)
plot2=ggplot(mpg, aes(displ, fill=drv)) + geom_histogram(binwidth=0.5) +
  facet_wrap(~drv, ncol = 1)
grid.arrange(plot1, plot2, ncol=2)

# ----  Comparison with standard R plots --- 
X=read.table("FTSE100-30.txt", header=T, row.names=1)
Y=read.table("FTSE100.txt", header=T, row.names=1)
attach(X)
attach(Y)

# --- time series plot for two stocks --- #
Date=strptime(row.names(Y), "%Y-%m-%d") # convert date characters

# R plot 
plot(Date, HSBA, ylim=c(370,640), type='o', ylab='Stock Price', pch='*',
     col="blue", main="Daily stock prices of BP and HSBC in 2012")
lines(Date, BP, type='o', col='red', lty=2, pch='*')
legend("topleft", c("HSBA", "BP"), col=c("blue", "red"), lty=c(1,2), pch=c("*","*"))

#ggplot
XY=data.frame(X, Y, Date)
ggplot(XY, aes(x=Date, y=HSBA, col='HSBA')) +
  geom_point() + geom_line() +
  geom_point(aes(x=Date, y=BP, col='BP')) +
  geom_line(aes(x=Date, y=BP, col='BP')) +
  ggtitle("Daily stock prices of BP and HSBC in 2012") +
  ylab("Stock Price")

# ---  histogram of daily return --- #
Ry=diff(log(FTSE100))*100

#Rplot
hist(Ry, xlab='log return', probability=T, nclass=30, col="blue",
     main="Daily returns (in percentage) of FTSE100 in 2012")

x = seq(min(Ry), max(Ry), (max(Ry)-min(Ry))/200)
lines(x, dnorm(x, mean(Ry), sd(Ry)), col="red", lwd=2)


#ggplot
t=data.frame(Ry)
ggplot(t, aes(x=Ry)) +
  geom_histogram(aes(y = ..density..), col="blue") +
  ggtitle("Daily returns (in percentage) of FTSE100 in 2012") +
  xlab("log return") +
  stat_function(fun = dnorm, args = list(mean = mean(Ry), sd = sd(Ry)), lwd=1.5, col="red")

# --- matrix scatter plots --- #

# R plot #
pairs(X[,1:10])

#-- gg version -- #
# install.packages("GGally")
library("GGally")
ggpairs(X,columns=1:2,
        upper = list(continuous = wrap('cor',colour='blue')),
        diag = list(continous = wrap('densityDiag',col=2)),
        lower=list(discrete = wrap('points',fill= 'red')))


data(flea)
ggpairs(flea, columns = 2:4)
ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
flea$species |> unique()

# ---- regression plots ---- #
# need to install another package 'quantreg'
library(quantreg)

T1= ggplot(XY, aes(FTSE100, BP)) + geom_point()
# quantile regression
plot1=T1+geom_quantile(quantiles=c(0.1, 0.3, 0.5, 0.7, 0.9),
                       size = 1, alpha = 0.5)
plot2=T1+geom_smooth()
grid.arrange(plot1, plot2, ncol=2)

