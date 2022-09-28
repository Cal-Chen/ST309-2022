# A quick excursion on R

# remove all objects in the current environment
rm(list = ls())

# identifier: a name to a R oject which we can refer after

# assign operator: '<-' or '='

# define a vector (a sequence of integers)
x <- 1:100
x

# apply power of 2 to every integers from 1 to 
y <- (1:100)^2
y

# some usual math operators
x + y
(x + y)/(x + y)
log(x)
exp(x)

# installation of packages
#1. run the code
install.packages('ggplot2')
#2. Go the the 'Packages' panel, click 'install' , enter the name of the package you wish to install and click install

# After installation, one need to load the package before using any functions (other objects, etc)
library(ggplot2)

# Help and Documentation
# One can use 'help' and '?' to check the usage of any functions in R
help(log)
?log
?mean


# ------ Data import/Export ------- #

# Check Working dirctory: a place (folder in your computer) where the R imports and exports data

# check your current working directory
getwd()

#Change your working dirctory
#1. Hot key: Ctrl  + Shift  + H
#2. Session + Setting working Dirctory + Choose directory

# Show all the files in your current working directory
dir()

# Download the data 'SimpleData.txt' and 'Jobs.txt' from Moodle
# Put these data to your working directory

# scan the text file(file name ends in '.txt')
x <- scan('SimpleData.txt',skip=2)
x
length(x) # number of elements of the object assigned to x
# one can run multiple simple commands in one line. Just need to separate them by ';'
mean(x);range(x)

# several summary statistics are calculated and shown
summary(x)

# create a matrix
# byrow, assign the elements from x to a matrix in the order of rows\
?matrix
y <-  matrix(x,byrow=T,ncol=4)
y
dim(y) # dimension of the object y
y[1,] # call the first row of y
y[,2] # call the second column of y
y[2,4] # the (2,4)-the element of matrix y


# analysis of job data
jobs <- read.table("Jobs.txt")
# C1 :ID number
# C2: Job type, 1 - accounting, 2 - finance, 3 - management, 4 -
#   marketing and sales, 5 -others
# C3: Sex, 1 - male, 2 - female
# C4: Job satisfaction, 1 - very satisfied, 2 - satisfied, 3 - not satisfied
# C5: Salary (in thousand pounds)
# C6: No. of jobs after graduation

# check the object
jobs
?head
head(jobs) # check the first 6 rows of the data set

# Set the first row to be the colnames of the data
# Set the first column to be the rownames of the data
jobs <- read.table("Jobs.txt",header=TRUE,row.names = 1)
head(jobs)
dim(jobs)
names(jobs) # colnames of the jobs
class(jobs) # object type
class(jobs[,1]);class(jobs[,2]);class(jobs[,3]);class(jobs[,4]);class(jobs[,5])

# Prespecified the object type when reading the data
jobs <- read.table("Jobs.txt", header=T, row.names=1,
                   colClasses = c("integer", "factor", "factor", "factor",
                                  "numeric", "integer"))
# check the object type again
class(jobs[,1]);class(jobs[,2]);class(jobs[,3]);class(jobs[,4]);class(jobs[,5])

# Some Descriptive analysis
table(jobs[,1])
t <-table(jobs[,2], jobs[,1], deparse.level=2)

100*t[1,]/sum(t[1,]) # Percentages of males with 5 different JobTypes
100*t[2,]/sum(t[2,]) # Percentages of females with 5 different JobTypes

# Difference in job distribution due to gender
barplot(t, main="No. of graduates in 5 different job categories",
        legend.text=c("male", "female"), 
        names.arg=c("accounting","finance", "management", "marketing", "others"),
        col=c('blue','red'))


# We can also draw the pie plot
# cut the window into a 1x3 grid
par(mfrow = c(1,3))
pie(t[1,]+t[2,],label=c("accounting","finance","management",
                        "marketing","others")); text(0,1, "Total", cex=2)
pie(t[1,],label=c("accounting","finance","management",
                    "marketing","others")); text(0,1, "Male", cex=2)
pie(t[2,],label=c("accounting","finance","management",
                    "marketing","others")); text(0,1, "Female", cex=2)

# recover the window back to 1x1
par(mfrow=c(1,1))

# Check salary distribution and the impact due to gender
mSalary <- jobs[,4][jobs[,2]==1]

# extract the salary data from male
fSalary <- jobs[,4][jobs[,2]==2]

# extract the salary data from female
summary(jobs[,4]); summary(mSalary); summary(fSalary)

hist(jobs[,4], col="gray", nclass=15, xlim=c(25,66),
     main="Histogram of Salaries (Total)")

hist(mSalary, col="blue", nclass=15, xlim=c(25,66),
     main="Histogram of Salaries (memale)")

hist(fSalary, col="red", nclass=15, xlim=c(25,66),
     main="Histogram of Salaries (Female)")


# Export the data from R, use 'write.table' or 'write'
write.table(jobs, "Jobs1.txt")
write.table(jobs, "Jobs2.txt", row.names=F, col.names=F)
write.table(jobs, "Jobs3.txt", sep=",")


# saving a session 
fSalary <- jobs[,4][jobs[,2]==2]
# extract the salary data from female
summary(jobs[,4]); summary(mSalary); summary(fSalary)
hist(jobs[,4], col="gray", nclass=15, xlim=c(25,66),
main="Histogram of Salaries (Total)")
hist(jobs[,4], col="gray", xlim=c(25,66),
main="Histogram of Salaries (Total)")
hist(jobs[,4], col="gray", nclass=15, xlim=c(25,66),
main="Histogram of Salaries (Total)")
table(jobs[,1])
t <-table(jobs[,2], jobs[,1], deparse.level=2)
t
head(jobs)
100*t[1,]/sum(t[1,])
barplot(t, main="No. of graduates in 5 different job categories",
legend.text=c("male", "female"), names.arg=c("accounting",
"finance", "management", "marketing", "others"))

# save all the objects (including data sets, loaded functions from added-on packages etc) in your R session
save.image("filename.RData") # the file must have "RData" as its last name.

# To save all the commands used in an R session only
savehistory("filename.Rhistory") # the file must have "Rhistory" as its last name.



