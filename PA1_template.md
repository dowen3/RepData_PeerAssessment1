---
title: "PA1_template"
author: "Drew Owen"
date: "July 25, 2016"
output: html_document
---
This is is my submission for course project 1 of reproducible research. I chose the base plotting system because it is available on every R system and thus is universal to all users.

##Loading and Processing data

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity_data.zip")
unzip("activity_data.zip")
act<-read.csv("activity.csV")
head(act)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


##What is mean total number of steps taken per day?
Now that data is loaded into R som basic analysis can be performed. First all, the steps from individual days are summed according to the day.

```r
steps_per_day<-aggregate(steps~date,act, sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
aggregate default is to ignore NA so nothing needs to be specified in sum.


```r
hist(steps_per_day$steps, breaks = 10, col =2, xlab = "Number of Steps per Day", main = "Histogram of Steps per Day")
```

![plot of chunk Histogram of Step Data](figure/Histogram of Step Data-1.png)

Some basic stats regarding the per day data.

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
Each day is broken up into a series of five minute intervals. The data for each time interval can be averaged across different days to identify what time of the day had the most number of steps.


```r
steps_per_int<-aggregate(steps~interval,act, mean)
plot(steps_per_int$interval/100, steps_per_int$steps, type= "l", xlab="hour of the day", ylab = "average steps",main= "Average steps during a day")
```

![plot of chunk Steps per Interval, averaged over all days](figure/Steps per Interval, averaged over all days-1.png)

The divide by 100 in the x variable scales the interval vector so the x axis units become hours.


```r
steps_per_int$interval[which(steps_per_int$steps==max(steps_per_int$steps))] 
```

```
## [1] 835
```

```r
#read as hhmm
```

##Imputing missing values
However, many of the days from the original data set had no value and were listed as NA.

```r
na<-is.na(act$steps)
sum(na)
```

```
## [1] 2304
```

These values were replaced using a simple for loop, with the average steps for those intervals using the previously calculated data. A new data frame was created to contain these modifications.


```r
act2<-act
index_na<-which(na)
for (i in 1:sum(na)){
      int<-act2$interval[index_na[i]]
      act2$steps[index_na[i]]<-steps_per_int$steps[steps_per_int$interval==int]
}
steps_per_day2<-aggregate(steps~date,act2, sum)
hist(steps_per_day$steps, breaks = 10, col=rgb(1,0,0,0.5), xlab = "Number of Steps per Day", main = "Histogram of Steps per Day using both methods",sub="Red ori", ylim=c(0,25))
hist(steps_per_day2$steps, breaks = 10, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Original", "Replaced NAs", "Overlap"), col=c("red","blue", "purple"), lwd=10)
```

![plot of chunk Find and replace empty data cells](figure/Find and replace empty data cells-1.png)
Removing the NAs and replacing them with actual numbers just increased the number of total days measured when using aggregate(). The increase in the center of the histogram was because the NAs were replaced with the average values.


```r
mean(steps_per_day2$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day2$steps)
```

```
## [1] 10766.19
```
Notice the mean does not change at all. This is because the previous method used aggregate() with the default setting of na.action=na.omit. The replacement only added the mean back to the original dataset. The median changed because the total number of steps per day has been increased.

##Are there differences in activity patterns between weekdays and weekends?

```r
act2$dayend<-factor(weekdays(as.Date(act2$date))=="Saturday"|weekdays(as.Date(act2$date))=="Sunday",c("TRUE", "FALSE"),c("Weekend","Weekday"))
```

This assigns a factor to each entry designating it as either Weekend or Weekday.


```r
steps_per_int2<-aggregate(steps~interval + dayend,act2, mean)
par(mfrow=c(2,1))
plot(steps_per_int2$interval[steps_per_int2$dayend=="Weekday"]/100, steps_per_int2$steps[steps_per_int2$dayend=="Weekday"], type= "l",xlab="hour of the day", ylab = "average steps", main="Weekday", ylim=c(0,max(steps_per_int2$steps)))
plot(steps_per_int2$interval[steps_per_int2$dayend=="Weekend"]/100, steps_per_int2$steps[steps_per_int2$dayend=="Weekend"], type= "l", xlab="hour of the day", ylab = "average steps", main="Weekend", ylim=c(0,max(steps_per_int2$steps)))
```

![plot of chunk Finding average interval](figure/Finding average interval-1.png)

