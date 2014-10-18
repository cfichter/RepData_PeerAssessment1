---
title: "Coursera Reproducible Research: Peer Assessment 1"
author: ""
date: ""
output: html_document
---
####Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the quantified self movement a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##### Data
The data for this assignment (activity.csv) was downloaded from the course web site.

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA).
- date: The date on which the measurement was taken in YYYY-MM-DD format.
- interval: Identifier for the 5-minute interval in which measurement was taken.

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in it.

##### Loading and preprocessing the data

```r
unzip(zipfile="repdata_data_activity.zip")
data <- read.csv("activity.csv")
```
##### What is mean total number of steps taken per day?

```r
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

hist(total.steps,
     breaks=(max(total.steps)-min(total.steps))/1000,
     main="Histogram of the total number of steps taken each day",
     xlab="Total number of steps taken each day",
     ylab="Frecuency (days)",
     col = "lightblue",border="blue")
```

```r
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```
##### What is the average daily activity pattern?

```r
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),FUN=mean, na.rm=TRUE)

plot(averages,type="l",
     xlab="5-minute interval",
     ylab="Average number of steps taken")
```
##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averages[which.max(averages$steps),]
```
##### Imputing missing values
Collect missing values.

```r
missing <- is.na(data$steps)
```
Resume missing values.

```r
table(missing)
```
This function will replace each missing value with the mean value of its 5-minute interval.

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
```
Generate a new dataset.

```r
filled.data <- data
```
Fill using fill.value function and recalculate total.steps variable.

```r
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
```
Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

```r
hist(total.steps,
     breaks=(max(total.steps)-min(total.steps))/1000,
     main="Histogram of the total number of steps taken each day",
     xlab="Total number of steps taken each day",
     ylab="Frecuency (days)",
     col = "lightblue",border="blue")
```

```r
mean(total.steps)
median(total.steps)
```
Mean and median values are higher after imputing missing data. The original data has some days with steps values NA for any interval. The total number of steps taken in such days are set to 0s by default. After replacing missing steps values with the mean steps of associated interval value, those elements with (originally) 0 values are redistributed, as new histogram shows.

##### Are there differences in activity patterns between weekdays and weekends?
In this part, we have to:

- Create a new factor variable in the dataset with two levels, 'weekday' and 'weekend' indicating whether a given date is a weekday or a weekend day. The function 'weekday.or.weekend' take a date and return the adequated class. This function is used to fill the 'dayfactor' variable.
- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lubridate)
weekday.or.weekend <- function(date) {
  day <- wday(date)
  if (day>=2 & day <= 6) #Monday to Friday
    return("weekday")
  else if (day == 1 | day == 7) #1-Sunday, 7-Saturday
    return("weekend")
}

filled.data$date <- as.Date(filled.data$date)

filled.data$dayfactor <- sapply(filled.data$date, FUN=weekday.or.weekend)

## aggregate and plot
averages <- aggregate(steps ~ interval + dayfactor, data=filled.data, mean)

library(ggplot2)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(dayfactor ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```
