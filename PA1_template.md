---
title: "RR-Assignment 1"
author: "Ashutosh Gupta"
date: "Monday, May 11, 2015"
output: html_document
---

## Loading and preprocessing the data


```r
df <- read.csv("C:/Users/admin/Documents/GitHub/RR/activity.csv",header=T)
attach(df)
```

```
## The following objects are masked from df (pos = 3):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 4):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 5):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 6):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 7):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 8):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 9):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 10):
## 
##     date, interval, steps
## 
## The following object is masked from mean_data (pos = 12):
## 
##     interval
## 
## The following objects are masked from df (pos = 14):
## 
##     date, interval, steps
## 
## The following objects are masked from copy_df (pos = 15):
## 
##     date, interval, steps
## 
## The following objects are masked from copy_df (pos = 17):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 18):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 19):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 20):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 21):
## 
##     date, interval, steps
## 
## The following objects are masked from df (pos = 22):
## 
##     date, interval, steps
```

```r
head(df,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

## What is mean total number of steps taken per day?


```r
total_steps <- aggregate(steps~date,df,sum,na.rm=TRUE)
total_steps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
hist(total_steps$steps,main="Histogram of Total Steps by Day", breaks = 10,col = "blue")
    
abline(v = mean(total_steps$steps),col = "red",lty = 1)
abline(v = median(total_steps$steps),col = "green",lty = 3)
text(mean(total_steps$steps),15,labels = "mean",pos = 4,col = "red")
text(median(total_steps$steps),13,labels="median", pos=4, col="green") 
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean_steps <- mean(total_steps$steps)
median_steps <- median(total_steps$steps)
print(mean_steps)
```

```
## [1] 10766.19
```

```r
print(median_steps)
```

```
## [1] 10765
```


## What is the average daily copy_df pattern?


```r
tseries <- aggregate(steps~interval,df,mean,na.rm=TRUE)
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

```r
names(tseries)
```

```
## [1] "interval" "steps"
```

```r
str(tseries)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
summary(tseries)
```

```
##     interval          steps        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
plot(tseries$interval,tseries$steps,col = "blue",type="l",xlab = "5 min Interval",ylab = "Average Number of Steps",main = "Average Daily copy_df Pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
tseries$interval[tseries$steps == max(tseries$steps)]
```

```
## [1] 835
```

## Imputing missing values


```r
NAs <- function(x) {
    as.vector(apply(x, 2, function(x) length(which(is.na(x)))))
    }
sprintf("Total no of NA in Steps are %i ",NAs(df[1]))
```

```
## [1] "Total no of NA in Steps are 2304 "
```

```r
sprintf("Total no of NA in date are : %i",NAs(df[2]))
```

```
## [1] "Total no of NA in date are : 0"
```

```r
sprintf("Total no of NA in interval are : %i",NAs(df[3]))
```

```
## [1] "Total no of NA in interval are : 0"
```

```r
sprintf("Total no of rows with NA: %i",NAs(df[1]))
```

```
## [1] "Total no of rows with NA: 2304"
```

```r
index <- which(is.na(df$steps))

# Create a vector of means
avg_vector <- rep(mean(df$steps, na.rm=TRUE), times=length(index))
df[index, "steps"] <- avg_vector
head(df)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
copy_df <- df
head(copy_df)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
# Clear the workspace
rm(avg_vector, index)
sum_aggregate <- aggregate(df$steps, by=list(df$date), FUN=sum)

# Rename the attributes
names(sum_aggregate) <- c("date", "total")
hist(sum_aggregate$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
x <-mean(sum_aggregate$total)
y <- median(sum_aggregate$total)
sprintf("Revised mean and median adter imputing NAs are %f",round(x,0))
```

```
## [1] "Revised mean and median adter imputing NAs are 10766.000000"
```

```r
sprintf("Revised mean and median adter imputing NAs are %f",round(y,0))
```

```
## [1] "Revised mean and median adter imputing NAs are 10766.000000"
```

## Are there differences in copy_df patterns between weekdays and weekends?


```r
copy_df$weekdays <- weekdays(as.Date(copy_df$date))

#levels(copy_df$weekdays)
#str(copy_df)
#table(copy_df$weekdays)

copy_df$date <- as.Date(copy_df$date,'%Y-%m-%d') 

copy_df$Day<- as.factor(ifelse(weekdays(copy_df$date) %in% c("Saturday","Sunday"),"Weekend","Weekday"))

mean_aggregation <- aggregate(copy_df$steps, 
                       by=list(copy_df$Day, 
                               copy_df$weekdays, copy_df$interval), mean)

# Rename the attributes
names(mean_aggregation) <- c("day", "weekday", "interval", "mean")

class(mean_aggregation)
```

```
## [1] "data.frame"
```

```r
library(lattice)
xyplot(mean_aggregation[,4] ~ mean_aggregation[,3] | mean_aggregation[,1], mean_aggregation, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
