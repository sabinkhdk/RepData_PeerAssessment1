# Reproducible research Peer assessment 1
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(ggplot2)
library(knitr)
library(devtools)
library(Hmisc)
```
## Loading and preprocessing the data

```r
unzip("./activity.zip")
data <- read.csv("./activity.csv", header=T, na.strings="NA")
totalsteps_day <- aggregate(steps ~ date, data, FUN = sum, simplify = T, na.rm =T)
```

## What is mean total number of steps taken per day?

```r
avgSteps <- mean(totalsteps_day$steps, na.rm =F)
medSteps <- median(totalsteps_day$steps, na.rm=F)
```
        Mean total number of steps taken per day:10766.19
        Median total number if steps taken per day:10765
## What is the average daily activity pattern?

```r
avgSteps_TimeBlock <- aggregate(x=list(avgSteps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=T)
maxSteps_TimeBlock <- which.max(avgSteps_TimeBlock$avgSteps)
Time_maxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgSteps_TimeBlock[maxSteps_TimeBlock,'interval'])
```
### 1. Time series plot

```r
png("./figure/figure2.png")
p1 <- qplot(avgSteps_TimeBlock$interval, avgSteps_TimeBlock$avgSteps, geom="line", type="l") 
p1 <- p1 + xlab("5 minute Time interval") + ylab("Average steps taken")
print(p1)
dev.off()
```
### 2. Time interval at when maximum number of steps occurs
        Maximum number of steps taken in 8:35        
## Imputing missing values

```r
numNA <- length(which(is.na(data$steps)))
data_Imputed <- data
data_Imputed$steps <- impute(data$steps, fun=mean)
class(data_Imputed$steps) <- "integer"
totalsteps_dayAfterImpute <- aggregate(steps ~ date, data_Imputed, FUN = sum, simplify = T)
avgStepsAfterImpute <- mean(totalsteps_dayAfterImpute$steps)
medStepsAfterImpute <- median(totalsteps_dayAfterImpute$steps)
png("./figure/figure3.png")
p3<-qplot(totalsteps_dayAfterImpute$steps, geom = "histogram", xlab = "Total steps per day", binwidth=500) + xlab("Total steps per day") + ylab("frequency after imputation")
print(p3)
dev.off()
```
        Number of missing values in steps taken 2304
        Average steps after imputation 10751.74
        Median steps after imputation 10656
## Are there differences in activity patterns between weekdays and weekends?

```r
data_Imputed$wday <-  ifelse(as.POSIXlt(data_Imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
avgSteps_wday <- aggregate(steps ~ interval + wday, data_Imputed, FUN = mean, simplify = T)
png("./figure/figure4.png")
p4 <- ggplot(avgSteps_wday, aes(interval,steps)) + geom_line() + facet_grid(wday ~ .)
p4 <- p4 +  xlab("5 minute Time interval") + ylab("Average steps taken")
print(p4)
dev.off()
```
