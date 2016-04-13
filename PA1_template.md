# Reproducible Research Assignment: Course Project 1
Latife Vergili  
April 10, 2016  



##Introduction

    This is the first project of Coursera Reproducible Research Class. Data was collected from https://en.wikipedia.org/wiki/Fitbit. It is aimed to answer following questions using this data.

    Data can be downloaded from:
    
        https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

Data has three variables.
    
    
    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

    date: The date on which the measurement was taken in YYYY-MM-DD format

    interval: Identifier for the 5-minute interval in which measurement was taken

## Set working directory and load in the neccessary packages 

```r
setwd("~/Data_Science/Reproducible_Research/Course_Assigment1")
library(knitr)  ## to show as a html page
```

## Loading and preprocessing the data

  The data is a compressed file, so we have to uncompressed it first. Then we have store data in R. To have an idea about data, structure function called. That's why, we can see which variables we have, and also what type of variables(int,char,..etc) data has.
  
  

```r
data <- unzip("activity.zip")
activity<-read.csv(data, head=TRUE, na.strings="NA")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

  It has been seen that data has 3 variables and 17568 observations. "steps" and "interval" are integer variables, "date" is character variable. After having an idea about dataset, we are ready to answer the questions!!!!

## What is mean total number of steps taken per day?

  To calculate the mean total number of steps per day, we need to remove na's, and compute the daily total number of steps.
  

```r
activity$date <- as.Date(activity$date)
activity_rmNA <- subset(activity, !is.na(activity$steps))

steps_by_day <- tapply(activity_rmNA$steps, activity_rmNA$date, sum, na.rm=TRUE, simplify=T)
steps_by_day <- steps_by_day[!is.na(steps_by_day)]

hist(x=steps_by_day,
     breaks=20,
      main="Total Steps for Each Day",ylab="Frequency",xlab="Number of Steps per Day", col="magenta", lwd=8)

abline(v=mean(steps_by_day, na.rm=TRUE), col="yellow", lwd=6)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Histogram shows that average daily steps is above 10K steps. Let's compute the exact value. To do this, we have to calculate mean of the daily steps. 

Calculate mean and median of steps by day

```r
mean(steps_by_day)
```

```
## [1] 10766.19
```

Median of daily steps


```r
median(steps_by_day)
```

```
## [1] 10765
```

Median value and mean value are almost same.

##What is the average daily activity pattern?

For this part of analysis, we use "interval" variable instead of "step". In this analysis, It is created a time series plot of the interval (x-axis) and the average number of steps taken all days (y-axis).


```r
int_avg <- tapply(activity_rmNA$steps, activity_rmNA$interval, mean, na.rm=TRUE, simplify=T)
activity_ia <- data.frame(interval=as.integer(names(int_avg)), avg=int_avg)

with(activity_ia,
     plot(interval,
          avg,
          type="l",
          xlab="Interval ID",
          ylab="Number of Steps",main="Average Number of Steps per Day by Interval",col="blue",lwd="3"))

# plot(activity_ia$interval,activity_ia$steps, type="l", xlab="Interval ID", ylab="Number of Steps",main="Average Number of Steps per Day by Interval",col="blue",lwd="3")

abline(h=mean(activity_ia$avg, na.rm=TRUE), col="red", lwd=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Plot shows that there is a peak between interval id 800 and 900. Let's find out exact value.


```r
interval_for_max_Steps<-max(activity_ia$avg)

interval_for_max_Steps
```

```
## [1] 206.1698
```

Maximum number of steps for maximum interval value(interval id=835)


```r
Max_mean_steps<-activity_ia[activity_ia$avg == interval_for_max_Steps, ]
Max_mean_steps
```

```
##     interval      avg
## 835      835 206.1698
```


##Impute missing values. Compare imputed to non-imputed data.

  Data has NA values. We need to remove missing data first. After cleaning the data, we are going to repeat first part of analysis.
  
  Let's look at how many missing value data has.
  

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
  
  2304 value is missing.Those missing values are going to be replaced with the mean value of its 5-minute interval
  
  

```r
activity_impute <- activity
ndx <- is.na(activity_impute$steps)
int_avg <- tapply(activity_rmNA$steps, activity_rmNA$interval, mean, na.rm=TRUE, simplify=T)
activity_impute$steps[ndx] <- int_avg[as.character(activity_impute$interval[ndx])]
```



```r
new_steps_by_day <- tapply(activity_impute$steps, activity_impute$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_steps_by_day,
     breaks=20,
      main="Total Steps for Each Day",ylab="Frequency",xlab="Number of Steps per Day(missing data removed)", col="blue", lwd=8)

abline(v=mean(new_steps_by_day, na.rm=TRUE), col="yellow", lwd=6)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

As it can be seen, average daily step after missing data removed is above 10K. It is similar the previous result which missing data was not removed. If we look at mean and median value, we can compare the results.


```r
mean(new_steps_by_day)
```

```
## [1] 10766.19
```


```r
median(new_steps_by_day)
```

```
## [1] 10766.19
```
After removed missing values, mean and median are identical. If we compare the mean 10766 and median 10765, the mean is same, and the median has a small change. 

##Are there differences in activity patterns between weekdays and weekends?
To compare activity patterns for weekend and weekdays, we need to create a vector variable to decide whether a given date is a weekend day or weekday.


```r
# decision if given date is weekend or weekday
is_weekday <- function(d) {
    wd <- weekdays(d)
    ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(activity_impute$date, is_weekday)
activity_impute$wk <- as.factor(wx)
head(activity_impute)
```

```
##       steps       date interval      wk
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```


```r
wk_activity <- aggregate(steps ~ wk+interval, data=activity_impute, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=wk_activity)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Plot shows us, there is more activity in early weekdays which interval is between 800 and 900. But, more overall activity accurs during to weekend.
