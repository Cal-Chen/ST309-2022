# R session week 2

rm(list=ls())
# set up your working directory 
# copy paste your data file into the work directory
# Change your working directory by 'Session' or Ctrl + Shift + H
# read the data file

mkmsc <- read.table("marks.txt", header=TRUE)
mk2nd <- read.table("marks2.txt", header=TRUE)

# check the attributes of a data frame
attributes(mkmsc)
attributes(mk2nd)

# structure of data
str(mkmsc)

#######################################
##  Named arguments and default value #

# The order in which arguments are passed to a function 
# determines what we do with them
# plot(x,y) it will y against x
# intuitively, x corresponds to x axis
?plot # argument, order, default values
attach(mk2nd)
plot(exam1,exam2)
plot(exam2,exam1)

# use the name when you don't want to make the input be read in order
plot(y=exam2,x=exam1)

# change the range of x-axis
plot(exam1,exam2,xlim=c(10,30))

# names attribute of a data frame
names(mk2nd)
colnames(mk2nd)

# These two are equivalent
rownames(mk2nd)
row.names(mk2nd)

# change it by making assignment 
names(mkmsc)[1] <- c('a')
names(mkmsc) <- c("ST402", "ST419","ST422")
rownames(mkmsc) <- c("Ali", "Bet", "Cal", "Dan", "Eli", "Foo")
mkmsc

# 'turn off' the attached data 'mk2nd'
detach(mk2nd)

# the first column of is a column of candidate numbers.
# Giving the column of the table which contains the row names, 
mk2nd <- read.table("marks2.txt", header=TRUE, row.names=1)
attributes(mk2nd)


# Regular sequences
# sequence generator : 
1:10
5:20
10:5 # reverse sequence

(1 + 2)*3
# the operator : has higher priority than some common arithmetic operators
1:10 - 1
1:10*2
1:10/2
# but lower priority than power operator
1:10^2 # 1:100

# The 'seq' function can be used to generated any arithmetic sequence
?seq
# the first four argument
# from,to: starting and end values of the sequence
# by: increment of the sequence
# length: the length of the sequence
seq(1,10) # 1:10

seq(1,10,0.5)  

seq(1,10,length.out=19)

# DO NOT specific 'by' and 'length.out' at the same time
seq(1,10,length.out=19,by=0.25)

# generate replicate elements 
?rep
rep(1,10)
rep(c(1,2),10)
rep(c(1,2),each=10)

# Logical statement can be formed by comparing two expressions
# Logical operator
# equivalent == , inequal !=
# greater than > ,greater and equal >=
# AND &, OR |
# NOT !

tf1 <- c(TRUE,TRUE,FALSE,FALSE)
tf2 <- c(TRUE,FALSE,TRUE,FALSE)
tf1 & tf2
tf1 | tf2

# turn into numeric: multiply by 1
tf1*1

# generate logical vectors from data
attach(mkmsc)
ST419>70
ST419>70 & ST402>70
ST419>70 & ST402>80
# row.names(mkmsc)=="Ali"

# The logical expression can be used to filter
# Selected ST419 marks whose value is higher than 70
ST419[ST419>70];ST419[c(2,4)]
# Selected ST402 marks whose ST419 mark is less than 65
ST402[ST419<65];ST402[c(3,5,6)]
# Selected ST422 marks whose total mark of three courses is less than 200
ST422[(ST402+ST419+ST422)<200]
# Selected ST422 marks whose marks for all the courses are higher than 50
ST422[ST402>50 & ST419>50 & ST422>50]

row.names(mkmsc)[ST419>70]
row.names(mkmsc)[ST402<50 | ST419<50 | ST422<50]
names(mkmsc)[c(sum(ST402), sum(ST419), sum(ST422)) > 350]

# remember to detach when you finish
detach(mkmsc)


######################################
####### character sequence*###########
?paste
# sep, can be used specify what we want between the arguments
# for different length of vectors, R uses a recycling rule
paste(c("x","y"), rep(1:5,each=2), sep="")
str1 <- paste("(x", 1:5, sep="")
str2 <- paste("y", 1:5, ")", sep="")
label1 <- paste(str1,str2,sep=",")
label1
# or just use paste0

# The functions grep and regexpr can be used to 
# find specific patterns of characters.
?grep
?regexpr
cvec <- c("zaa", "aza", "aaz", "aaa", "zzz")
grep("z", cvec)
grep(".z", cvec)
regexpr(".z.", cvec) # "-1" indicates pattern is not found

###########################
######## list ############
# A list is an ordered collection of objects.
# The objects in a list are referred to as components. 
# The components may be of different modes
# In other words, you can put any type of objects into a list

# create a list  
mklst <- list("MSc exam marks", c(10,6,2005), mkmsc)
# check the type of object
mode(mklst)

# subscript by [] will return a list
mklst[2] 
mode(mklst[2]) # itself is still a list
length(mklst[2])

# To access its component, use [[]] for subscription
mklst[[2]]
mode(mklst[[2]])

# Similar to data frame, we can also name its component
names(mklst) <- c("title", "date", "marks")

# then we have other ways to access its component
mklst[['title']]
mklst$title

############################
#### basic R graphic #######

# hist: histogram ,check the distribution for a given data
attach(mk2nd)
hist(exam1)
#setting probability = TRUE will give relative frequencies
hist(exam1, probability = TRUE)
hist(exam1, nclass=10)
hist(exam1, breaks=10)
hist(exam1, breaks=c(0,20,25,30,40))

# the input must be a numeric vector
hist(mk2nd)
# change into numeric
hist(mk2nd[mk2nd<=max(mk2nd)])

# check whether given the data set can be modeled by a normal distribution
qqnorm(exam2)
qqline(exam2,col=2,lwd=2) # lwd specify the thickness of a line

qqnorm(mk2nd[mk2nd<=max(mk2nd)])
qqline(mk2nd[mk2nd<=max(mk2nd)],col=2,lwd=2)

# The labels here are not very informative
boxplot(exam1,exam2,exam3)
# It allows the input to be a data frame
# label is informative now
boxplot(mk2nd)

# scatter plot
# type: specify the plot type
plot(exam1,exam2)
plot(exam1, exam2, type="p") # points (the default)
plot(exam1, exam2, type="l") # lines
plot(exam1, exam2, type="b") # both lines and points
plot(exam1, exam2, type="o") # overlaid lines on points
plot(exam1, exam2, type="h") # high density (vertical lines)

# pch(plotting character): specify the shape of points
plot(exam1, exam2, pch="+")
plot(exam1, exam2, pch="x")
plot(exam1, exam2, pch=2)
plot(exam1, exam2, pch=3)
plot(exam1, exam2, pch=4)

# There are a huge number of graphics parameters
# you can check the list of the graphics parameters and defaults
par() 
par(c("pch", "col"))
plot(exam1, exam2)
par(pch='*')
plot(exam1, exam2)

# interactive graphics
plot(exam1,exam2)
# R will wait while the users selects points on the plot using the mouse
# Note the default marks are the position(row number)
# (row names may be more informative)
identify(exam1,exam2)
identify(exam1,exam2,row.names(mk2nd))
# press Esc to finish 

# R allows you to put more than one plot on the page by mfrow argument
# c(3,2) make the window into a grid with 3 rows and 2 columns
par(mfrow=c(3,2))

hist(exam1)
qqnorm(exam1)
hist(exam2)
qqnorm(exam2)
hist(exam3)
qqnorm(exam3)
par(mfrow=c(1,1))

detach(mk2nd)

# Low level graphics*
# The commands text(...) and lines(...)
# can be used to add text and lines to a plot.
citynames <- c("Athens", "Jo'burg", "London", "NYC", "Shanghai")
longitude <- c(23.72, 28.07, -0.08, -73.47, 121.47)
latitude <- c(37.97, -26.20, 53.42, 40.78, 31.17)
cities <- data.frame(latitude,longitude)
row.names(cities) <- citynames
cities
rm(latitude,longitude,citynames)
attach(cities)
plot(longitude,latitude,type="n")
points(longitude,latitude,pch=".")
text(longitude[1], latitude[1], row.names(cities)[1],pos=1)
text(longitude[2], latitude[2], row.names(cities)[2],pos=4)
text(longitude[3], latitude[3], row.names(cities)[3],pos=4)
text(longitude[4], latitude[4], row.names(cities)[4],pos=4)
text(longitude[5], latitude[5], row.names(cities)[5],pos=2)
lines(c(longitude[1],longitude[3]), c(latitude[1],latitude[3]))
lines(c(longitude[2],longitude[3]), c(latitude[2],latitude[3]))
lines(c(longitude[4],longitude[3]), c(latitude[4],latitude[3]))
lines(c(longitude[5],longitude[3]), c(latitude[5],latitude[3]))
detach(cities)


# 3-D plot
x <- seq(-1,1,length=50)
y <- seq(-1,1,length=50)
z <- outer(x^2,y^3)
contour(x,y,z)
image(x,y,z)
persp(x,y,z)


