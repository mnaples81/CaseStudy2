# Create Matrix X and print

X = matrix( 
  c(4, 1, 2, 5, 0, 1, 1, 3, 8, 2, 5, 2), 
  nrow=3, 
  ncol=4)
X

# Retrieve Financial Data and Plot

library(tseries)

SNPdata <- get.hist.quote('TCP',quote="Close")

SNPret <- log(lag(SNPdata)) - log(SNPdata)

SNPvol <- sd(SNPret) * sqrt(250) * 100

## volatility
getVol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)*var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
  }
  
  sqrt(varlist)
}


## Recreate Figure 6.12 in the text on page 155

volest <- getVol(10,SNPret)

volest2 <- getVol(30,SNPret)

volest3 <- getVol(100,SNPret)

plot(volest,type="l")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")

# Problems using Orange data

library(datasets)
data(Orange)

## fix(Orange)

# Calculate the mean and median of trunk circumference by Tree type
tapply(Orange$circumference, Orange$Tree, mean)
tapply(Orange$circumference, Orange$Tree, median)

# Make a scatter plot of the trunk circumferences against the age of the tree, 
# using different plotting symbols for Tree type

library(ggplot2)

Orange$order <- as.factor(rep(c(3,1,5,2,4), each=7))

## Orange$order <- factor(Orange$order , levels=levels(Orange$order)[c(3,1,5,2,4)])

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(size=2, shape=23, aes(fill=Orange$Tree)) + xlab("Age") + ylab("Circumference") + labs("Tree Type)") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank()) + ggtitle("Circumference Vs. Age")

# Display the trunk circumference on a comparative boxplot against tree
boxplot(circumference ~ Tree, data = Orange)
title("Boxplot of Trunk Circumference, by Tree Diameter Type")

# Import temp data

setwd('C:/R/CaseStudy2')
temp <- read.csv('temp.csv', header=TRUE)
library(plyr)

# calculate temp ranges for each country, and identify and visualize top 20

## Change month data to date object that only works for entries 1900 or later, clean data set by removing NAs,
## rename 'date' to 'month'
cleantemp <- na.omit(temp)
names(cleantemp)[1] <- "month"
cleantemp$month <- as.Date(as.character(cleantemp$month), "%m/%d/%Y")
cleantemp <- na.omit(cleantemp)



## calculate mins and maxes for each Country, create new data set with values
maxes <- aggregate(cleantemp$Monthly.AverageTemp, by=list(cleantemp$Country), FUN=max)
mins <- aggregate(cleantemp$Monthly.AverageTemp, by=list(cleantemp$Country), FUN=min)
range <- cbind(maxes, mins)
names(range)[2] <- "max"
names(range)[4] <- "min"

## add column for difference between min and max, sort descending by range, keep only top 20
range$diff <- range$max - range$min
range.sorted <- range[order(-range$diff),]
range.sorted <- range.sorted[1:20, ]

## Plot
barplot(range.sorted$diff, ylab="Temp Range", names.arg=range.sorted$Group.1, las=2)

# select subset of data (UStemp) for US land temps starting from 1.1.1990
library(dplyr)

UStemp <- subset(cleantemp, Country=="United States" & month >= as.Date("1990-01-01"), select=month:Monthly.AverageTemp)

# create new column for fahrenheit temps

UStemp$ftemp <- with(UStemp, Monthly.AverageTemp * (9/5) + 32)

# calculate average land temp by year and plot

UStemp$year <- format(as.Date(UStemp$month, format="%Y-%m-%d"),"%Y")
annualtemps <- tapply(UStemp$ftemp, UStemp$year, mean)
annualdf <- as.data.frame.table(annualtemps)
annualdf$Var1 <- as.numeric(annualdf$Var1)
annualdf$Var1 <- with(annualdf, Var1 + 1989)
names(annualdf)[1] <- "Year"
names(annualdf)[2] <- "Temp"
annualdf

plot(annualdf, type="l", xlab=("Year"), ylab=("Average Annual Temperature (Fahrentheit)"))

# calculate year-to-year temperature differences, identify max

diff(annualdf$Temp)
tempchanges <- diff(annualdf$Temp)
tempchanges
annualdf$diff <- c(NA, tempchanges)
annualdf

# Import CityTemp
setwd('C:/R/CaseStudy2')
citytemp <- read.csv('CityTemp.csv', header=TRUE)

## clean data set, remove NAs
cleancity <- na.omit(citytemp)
names(cleancity)

## calculate mins and maxes for each city, create new data set with values
citymaxes <- aggregate(cleancity$Monthly.AverageTemp, by=list(cleancity$City), FUN=max)
citymins <- aggregate(cleancity$Monthly.AverageTemp, by=list(cleancity$City), FUN=min)
cityrange <- cbind(citymaxes, citymins)
names(cityrange)[2] <- "max"
names(cityrange)[4] <- "min"

## add column for difference between min and max, sort descending by range, keep only top 20 results
cityrange$diff <- cityrange$max - cityrange$min
cityrange.sorted <- cityrange[order(-cityrange$diff),]
cityrange.sorted <- cityrange.sorted[1:20, ]

## Plot
barplot(cityrange.sorted$diff, ylab="Temp Range", names.arg=cityrange.sorted$Group.1, las=2)
