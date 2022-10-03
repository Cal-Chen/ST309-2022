# remove all the objects in the environment
rm(list = ls())
# read the data
# skip, the number of lines of the data file to skip before beginning to read data.
# row.names: giving the column of the table which contains the row names, 
forbes2000 = read.csv("Forbes2000.csv", row.names = 1,skip=3)
?read.csv

# check the data structure
str(forbes2000)
dim(forbes2000)

# make the variables recognizable in R
attach(forbes2000)

# number of different countries
length(unique(country))

# number of different industry
length(unique(category))

#(a) Filter out the top 10 ranked companies
name[rank <= 10]

name[marketvalue >= 174]

name[sales >= sort(sales,decreasing = TRUE)[10]]

# forbes2000[sales > sort(sales,decreasing = TRUE)[10] & marketvalue >=174,]

#(b) Plot the histograms for marketvalue and log(marketvalue)
par(mfrow=c(1,2))
hist(marketvalue,main='histogram of market values',col=1)
hist(log(marketvalue),main='histogram of log market values',col=2)
?hist
# change back to the original structure
par(mfrow=c(1,1))

# (c) Compare the outcomes from mean(profits) and mean(profits, na.rm=T)
mean(profits)
mean(profits,na.rm = TRUE)

#(d) Median profit for the companies in US and UK separately
# the one without attach
median(forbes2000[country == 'United States','profits'],na.rm=TRUE)
# the one with attach
median_profit_us <- median(profits[country=='United States'],na.rm = TRUE)
median_profit_uk <- median(profits[country=='United Kingdom'],na.rm=TRUE)

#(e) Find all German companies with negative profit
name[country == 'Germany'& profits < 0]

#(f) To which business category do most of the Bermuda island companies belong
table(category[country =='Bermuda'])

#(g) For the 50 companies with the highest profits, 
# plot profits against assets, using some suitable 
# transformation for each variable if appropriate.
profits_sort <- sort(profits,decreasing = TRUE)[1:50]
asset_sort <- assets[profits >= profits_sort[50] & !is.na(profits)]

# the convenient way would be 
asset_sort <- subset(assets,profits >= profits_sort[50])

# check the histogram and scatter plot
hist(profits_sort)
hist(asset_sort)
plot(asset_sort,profits_sort)

# apply the log transform and make a scatter plot
log_profits_sort <- log(profits_sort)
log_asset_sort <- log(asset_sort)

hist(log_profits_sort)
hist(log_asset_sort)

# cex the size of point
# cex.lab the size of label
# pch the style of points
plot(log_profits_sort,log_asset_sort,
     xlab='log Assets',ylab = 'log profits',
     cex.lab=1.5,pch=20)

# remember to detach when you finish analysis with the data set
detach(forbes2000)

# top50 <- forbes2000[order(-profits),][1:50,]
# 
# plot(top50$assets,top50$profits)
# 
# profits[profits >= profits_sort[50] & !is.na(profits)]

