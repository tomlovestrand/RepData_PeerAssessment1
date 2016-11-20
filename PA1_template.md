# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
starting <- read.csv("activity.csv.")
working <- na.omit(starting)
```

## What is mean total number of steps taken per day?

### > histogram of total number of steps taken each day

```r
library (plyr)
stepsPerDay1 <- ddply(working,.(date), summarize, sum=sum(steps))
hist(stepsPerDay1$sum, 25, col="blue", main="Histogram of total steps per day, NAs omitted", ylab="number of days", xlab="total steps per day")
```

![](PA1_template_files/figure-html/steps per day-1.png)<!-- -->

### > calculate the mean and median of total steps taken per day

```r
stepsMean <- mean(stepsPerDay1$sum); stepsMean
```

```
## [1] 10766.19
```

```r
stepsMedian <- median(stepsPerDay1$sum); stepsMedian
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### > plot time series of intervals v avg steps, across all days

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
stepsByInterval <- group_by(working, interval)
plotStepData1 <- summarize(stepsByInterval, stepAvg=mean(steps, na.rm=FALSE))
plot(plotStepData1$interval, plotStepData1$stepAvg, type="l", main = "Average Daily Activity Pattern", xlab = "interval identifier (5 minute intervals)", ylab = "steps", col="red", lty="solid", lwd = 1)
```

![](PA1_template_files/figure-html/average plot-1.png)<!-- -->

### > find interval across all days with the highest number of steps

```r
intervalMax <- plotStepData1$interval[which.max(plotStepData1$stepAvg)]; intervalMax
```

```
## [1] 835
```

## Imputing missing values

### > calculate the total number of missing values in the dataset

```r
missingPointCount <- sum(is.na(starting$steps)); missingPointCount
```

```
## [1] 2304
```

### > create a new dataset with the missing data filled in

#### The original data set is merged with a df of intervals and avg steps.  the missing values (NAs) in the original dataset are replaced by the mean value of the interval to which each belongs


```r
working2 <- merge(starting, plotStepData1, all=TRUE)
working2 <- arrange(working2, date, interval)
working2$steps <- ifelse(is.na(working2$steps), working2$stepAvg, working2$steps)
```

### > make a histogram of the new dataframe, calculate mean and median

```r
stepsPerDay2<-ddply(working2,.(date),summarize, sum=as.integer(sum(steps)))
hist(stepsPerDay2$sum, 25, col="green", main="Histogram of total steps per day, NAs replaced", ylab="number of days",xlab="total steps per day")
```

![](PA1_template_files/figure-html/hist, mean, median w new df-1.png)<!-- -->

```r
stepsMean2 <- mean(stepsPerDay2$sum); stepsMean2
```

```
## [1] 10766.16
```

```r
stepsMedian2 <- median(stepsPerDay2$sum); stepsMedian2
```

```
## [1] 10766
```

#### This mean value does not differ from the original estimate.  The missing data are uniformly distributed across all intervals so replacing them with the initial interval means does not change the overall mean value.  This median value moves closer to the mean as replacing ~ 13 percent of the dataset with values equal to the means of the initial intervals biases the total median toward the total mean.  

## Are there differences in activity patterns between weekdays and weekends?

### > create a new factor variable in the dataset with two levels - "weekday" and "weekend"

```r
working2$date <- as.Date(working2$date)
dayNames <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
working2$dayType <- factor((weekdays(working2$date) %in% dayNames), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

### > make a panel plot containing a time series plot

```r
library(lattice)
plotSteps <- aggregate(steps ~ dayType+interval, data=working2, FUN=mean)
xyplot(steps ~ interval | factor(dayType), data=plotSteps, layout = c(1, 2), scales=list(cex=.8, col="red"), type=c("l", "g"), col = "blue", xlab= "interval identifier (5 minute intervals)", ylab= "average steps per interval", lty="solid")
```

![](PA1_template_files/figure-html/panel plot, activity by type of day-1.png)<!-- -->

#### There are differences in the average activity levels for weekdays and weekends.  Both have a maximum step value near 8:00 am though the weekday maximum is about 30 percent higher than the weekend maximum.  Weekdays have two smaller peaks near 4:00 pm and 7:00 pm.  The weekend plot has a number of peaks of similar magnitude, having a somewhat more uniform activity distribution until falling off around 8:00 pm.
