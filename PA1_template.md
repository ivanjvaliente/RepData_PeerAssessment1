---
title: "Reproducible Research Project 1"
author: "ivan valiente"
date: "27 décembre 2016"
output: html_document
---



## Download and reading and preprocessing the project data set
The data for this assignment can be downloaded from the course web site: <http://rmarkdown.rstudio.comhttps://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>.

The following code chunk checks the presence of the project's folder in the current work
directory. If the folder doesn't exist, I create it, download the zip file and unzip it.

The data-set is stored in a comma-separated-value (CSV).
I read the file with read.csv, and stored it in "activity_data" data frame.


```r
if(!file.exists("./project_1_data")){
    dir.create("./project_1_data")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

    download.file(fileUrl, 
                 destfile = "./project_1_data/data_activity.zip", method = "curl")
    unzip(zipfile = "./project_1_data/data_activity.zip",
          exdir = "./project_1_data")

}
files <- list.files("./project_1_data")

activity_data <- read.csv(paste("./project_1_data",files[1],sep = "/"))
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day knowing that the colon "steps" 
contains the number of steps taking in a 5-minute interval (missing values are coded as NA),
and the colon "date" is a factor with the dates of the collected data




```r
Total_steps_day <- sapply(split(activity_data,activity_data$date), function(x) { 
    sum(x[,"steps"], na.rm = TRUE) 
})
str(Total_steps_day)
```

```
##  Named int [1:61] 0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
##  - attr(*, "names")= chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```
Make a histogram of the total number of steps taken each day


```r
hist(Total_steps_day, col = "orangered", xlab = "Number of Steps",
     main = "Total Steps per Day Histogram")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
mean_steps_per_day <- mean(Total_steps_day)
mean_steps_per_day
```

```
## [1] 9354.23
```

```r
median_steps_per_day <- median(Total_steps_day)
median_steps_per_day
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?


```r
plot(activity_data$steps, type = "l", ylab = "Number of Steps",
     xlab = "Interval", main = "Number of Steps as a Function of Time")
mean(activity_data$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
abline(h = mean(activity_data$steps, na.rm = TRUE), col = "red")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
i_max <- which(activity_data$steps == max(activity_data$steps, na.rm = TRUE))

activity_data$steps[i_max]
```

```
## [1] 806
```

```r
activity_data$date[i_max]
```

```
## [1] 2012-11-27
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

```r
activity_data$interval[i_max]
```

```
## [1] 615
```
The 5-minute interval that contains the maximum number of steps is in the day 2012-11-27 
between 6:10 and 6:15.

## Imputing missing values
Calculate and report the total number of missing 
values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```


1. Devise a strategy for filling in all of the missing values in the dataset. The strategy
does not need to be sophisticated. For example, you could use the mean/median for that day,
or the mean for that 5-minute interval, etc.

2. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Here I decided to fill the missing (NA) values with the mean of steps in 5-minutes interval
(37.38 steps). 


```r
activity_data$steps[is.na(activity_data$steps)] <-  mean(activity_data$steps, na.rm = TRUE)
```
Make a histogram of the total number of steps taken each day and Calculate and report the
mean and median total number of steps taken per day. 


```r
Total_steps_day <- sapply(split(activity_data,activity_data$date), function(x) { 
    sum(x[,"steps"]) 
})
str(Total_steps_day)
```

```
##  Named num [1:61] 10766 126 11352 12116 13294 ...
##  - attr(*, "names")= chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```

```r
hist(Total_steps_day, col = "orangered", xlab = "Number of Steps",
     main = "Total Steps per Day")

mean_steps_per_day <- mean(Total_steps_day)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
median_steps_per_day <- median(Total_steps_day)
median_steps_per_day
```

```
## [1] 10766.19
```

```r
abline(v=mean_steps_per_day)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Do these values differ from the
estimates from the first part of the assignment? What is the impact of imputing missing data
on the estimates of the total daily number of steps?

Yes, the values slightly differ from the previous estimation. Namely, the mean and median
in the modified dataset get the same value, and this value is higher than those estimated from the original data (10766 here versus mean = 9354.23 and median = 10395 in row data).

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
indicating whether a given date is a weekday or weekend day.


```r
activity_data$day <- weekdays(as.Date(activity_data$date))

activity_data$weekend_factor <- as.factor( ifelse(activity_data$day == "samedi" | 
                                    activity_data$day == "dimanche", "weekend", "weekday"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
interval (x-axis) and the average number of steps taken, averaged across all weekday 
days or weekend days (y-axis).


```r
par(mfrow = c(2,1))

plot(activity_data$steps[activity_data$weekend_factor =="weekday"], ylab = "steps",
         main = "weekday", xlab = "interval", type = "l" )
weekday_mean_steps <- mean(activity_data$steps[activity_data$weekend_factor =="weekday"])
weekday_mean_steps
```

```
## [1] 35.61058
```

```r
abline(h = weekday_mean_steps, col = "red")
    
plot(activity_data$steps[activity_data$weekend_factor =="weekend"], ylab = "steps",
         main = "weekend", xlab = "interval", type = "l" )
weekend_mean_steps <- mean(activity_data$steps[activity_data$weekend_factor =="weekend"])
weekend_mean_steps
```

```
## [1] 42.3664
```

```r
abline(h = weekend_mean_steps, col = "red")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
From the mean comparaison I can conclude that the activity of during the weekend days is slighly
higher than during the weekdays (42 vs 35 steps per 5-min mean ).



