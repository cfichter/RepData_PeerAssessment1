setwd("~/Varios Cristian/Coursera/Data Science Specialization/5-Reproducible Research/course project 1")

## Loading and preprocessing the data
unzip(zipfile="repdata_data_activity.zip")
data <- read.csv("activity.csv")

## What is mean total number of steps taken per day?
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

hist(total.steps,
     breaks=(max(total.steps)-min(total.steps))/1000,
     main="Histogram of the total number of steps taken each day",
     xlab="Total number of steps taken each day",
     ylab="Frecuency (days)",
     col = "lightblue",border="blue")

mean(total.steps, na.rm=TRUE)

median(total.steps, na.rm=TRUE)

## What is the average daily activity pattern?
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)

plot(averages,type="l",
     xlab="5-minute interval",
     ylab="Average number of steps taken")

## Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
averages[which.max(averages$steps),]

## Imputing missing values
## Collect missing values
missing <- is.na(data$steps)

## Resume
table(missing)

# Replace each missing value with the mean value of its 5-minute interval

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}

# Preserve data
filled.data <- data

#Fill
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)

# Make a histogram of the total number of steps taken each day and
# calculate and report the mean and median total number of steps taken per day.
hist(total.steps,
     breaks=(max(total.steps)-min(total.steps))/1000,
     main="Histogram of the total number of steps taken each day",
     xlab="Total number of steps taken each day",
     ylab="Frecuency (days)",
     col = "lightblue",border="blue")

mean(total.steps)

median(total.steps)

## Are there differences in activity patterns between weekdays and weekends?
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
