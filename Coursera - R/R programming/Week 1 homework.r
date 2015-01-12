setwd("~/Coursera - R/hw1_data")
hw1_data <- read.csv("~/Coursera - R/hw1_data/hw1_data.csv")
>   View(hw1_data)
> attach(hw1_data)

bad<- is.na(hw1_data)
clean<-(hw1_data[!bad],)
class(Ozone)
mean(Ozone, na.rm=T)
[1] 42.12931

## What is the value of Ozone in the 47th row?##
hw1_data[47,1]

## How many missing values are in the Ozone column of this data frame?##
bad<-is.na(hw1_data$Ozone)
as.integer(bad)
sum(bad)

## subset Ozone > 31, Temp > 90 ##
sub2 <- hw1_data[Ozone>31,][Temp>90,] ## don't forget the comma

mean(sub2$Solar.R, na.rm=T)

##What is the mean of "Temp" when "Month" is equal to 6? ##
June<-hw1_data[!hw1_data$Month>6,]
June<-June[!June$Month<6,]
mean(June$Temp)

##What was the maximum ozone value in the month of May (i.e. Month = 5)?

may<-hw1_data[!hw1_data$Month > 5,]
good<-complete.cases(may)  #remove nas
may2<-may[good,]
summary(may2$Ozone)



