---
output: 
  html_document: 
    keep_md: yes
editor_options: 
  chunk_output_type: inline
---
### Reproducible Research Course Project: Personal Activity Monitoring Device Data

<br>

Downloading and reading data into R (plus loading the ggplot2 and other packages)

```r
setwd("~/Documents/r-things")
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, "activity.zip")
unzip("activity.zip")
activity <- read.csv("activity.csv")
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.5
```

```
## 
## Attaching package: 'dplyr'
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
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.0.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```
<br>
A. What is the mean total number of steps taken per day?<br>
1. Calculating total number of steps per day<br>

```r
steps_per_day <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(steps_per_day) <- c("date","total_steps")
head(steps_per_day, 10) # First ten records
```

```
##          date total_steps
## 1  2012-10-01          NA
## 2  2012-10-02         126
## 3  2012-10-03       11352
## 4  2012-10-04       12116
## 5  2012-10-05       13294
## 6  2012-10-06       15420
## 7  2012-10-07       11015
## 8  2012-10-08          NA
## 9  2012-10-09       12811
## 10 2012-10-10        9900
```

```r
tail(steps_per_day, 10) # Last ten records
```

```
##          date total_steps
## 52 2012-11-21       12787
## 53 2012-11-22       20427
## 54 2012-11-23       21194
## 55 2012-11-24       14478
## 56 2012-11-25       11834
## 57 2012-11-26       11162
## 58 2012-11-27       13646
## 59 2012-11-28       10183
## 60 2012-11-29        7047
## 61 2012-11-30          NA
```
<br>
2. Histogram of the total number of steps taken each day:

```r
ggplot(steps_per_day, aes(x=total_steps))+geom_histogram(color="white", binwidth=2000)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
<br>
3. Mean and median of the total number of steps taken per day:

```r
mean(steps_per_day$total_steps, na.rm=T) # mean total steps per day
```

```
## [1] 10766.19
```

```r
median(steps_per_day$total_steps, na.rm=T) # median total steps per day
```

```
## [1] 10765
```
<br>
<br>
B. What is the average daily activity pattern?
<br>
1. The time-series plot (of type="l", but int the ggplot case, a geom_line) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
intervals <- aggregate(activity$steps, list(activity$interval), FUN=mean, na.rm=T)
colnames(intervals) <- c("interval", "avg_steps")
ggplot(intervals, aes(x=interval,y=avg_steps))+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
<br>
2. The 5-minute interval, on average across all the days, which contains the maximum number of steps:

```r
intervals[which.max(intervals$avg_steps),]
```

```
##     interval avg_steps
## 104      835  206.1698
```
<br>
C. Imputing missing values:<br>
1. Total number of missing values (i.e. total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```
<br>
2. Strategy for filling in all of the missing values

```r
# Chose to compute the mean of the 'steps' column to tackle this and use the result to fill in the empty (NA) values. Will be making a new dataset, call it 'new_activity,' in the process.

imputate <- function(steps, interval) {
	if (!is.na(steps))
		filler <- c(steps)
	else
		filler <- (intervals[intervals$interval==interval, "avg_steps"])
	return(filler)
}

activity_new <- activity

activity_new$steps <- mapply(imputate, activity_new$steps, activity_new$interval)
```
<br>
3. New data set that is equal to the original data set but with the missing data filled in...

```r
head(activity_new, 10) # First ten records
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```

```r
tail(activity_new, 10) # Last ten records
```

```
##           steps       date interval
## 17559 0.0000000 2012-11-30     2310
## 17560 0.8301887 2012-11-30     2315
## 17561 0.9622642 2012-11-30     2320
## 17562 1.5849057 2012-11-30     2325
## 17563 2.6037736 2012-11-30     2330
## 17564 4.6981132 2012-11-30     2335
## 17565 3.3018868 2012-11-30     2340
## 17566 0.6415094 2012-11-30     2345
## 17567 0.2264151 2012-11-30     2350
## 17568 1.0754717 2012-11-30     2355
```
<br>
4. Histogram of the total number of steps taken each day, using the new data set...<br>

```r
# But first...
new_steps <- aggregate(activity_new$steps, list(activity_new$date),FUN=sum)
colnames(new_steps) <- c("date", "total_steps")
ggplot(new_steps, aes(x=total_steps))+geom_histogram(color="white", binwidth = 2000)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
<br>

```r
mean(new_steps$total_steps, na.rm=T) # Mean total number of steps per day
```

```
## [1] 10766.19
```

```r
median(new_steps$total_steps, na.rm=T) # Median total number of steps per day
```

```
## [1] 10766.19
```
<br>
In making comparisons between the earlier estimates (before filling in missing values) and later (after filling in missing values), only the median values differed (old: 10765, versus new: 10766.19), while the mean values remained the same (10766.19 in both cases). This is indicative that imputing missing data here made a very small impact.
<br>
<br>
D. Using the new dataset with the filled-in missing values, are there any differences in activity patterns between weekdays and weekends?<br>
1. New factor variable in the dataset with two levels: "weekday" and "weekend" to indicate whether a given date is a weekday or a weekend day:

```r
activity_new$date <- as.Date(activity_new$date, format="%Y-%m-%d") # turning 'date' values from character string to date format
activity_new$day_of_week <- weekdays(activity_new$date)
activity_new$day_type <- ifelse(activity_new$day_of_week=="Saturday" | activity_new$day_of_week=="Sunday", "weekend", "weekday") # new factor variable
head(activity_new, 10)
```

```
##        steps       date interval day_of_week day_type
## 1  1.7169811 2012-10-01        0      Monday  weekday
## 2  0.3396226 2012-10-01        5      Monday  weekday
## 3  0.1320755 2012-10-01       10      Monday  weekday
## 4  0.1509434 2012-10-01       15      Monday  weekday
## 5  0.0754717 2012-10-01       20      Monday  weekday
## 6  2.0943396 2012-10-01       25      Monday  weekday
## 7  0.5283019 2012-10-01       30      Monday  weekday
## 8  0.8679245 2012-10-01       35      Monday  weekday
## 9  0.0000000 2012-10-01       40      Monday  weekday
## 10 1.4716981 2012-10-01       45      Monday  weekday
```

```r
activity_new[5600:5610,] # randomly chosen subset of rows just to see whether weekend days were correctly identified as 'weekend'
```

```
##      steps       date interval day_of_week day_type
## 5600     0 2012-10-20     1035    Saturday  weekend
## 5601    85 2012-10-20     1040    Saturday  weekend
## 5602    30 2012-10-20     1045    Saturday  weekend
## 5603     0 2012-10-20     1050    Saturday  weekend
## 5604     0 2012-10-20     1055    Saturday  weekend
## 5605     0 2012-10-20     1100    Saturday  weekend
## 5606    13 2012-10-20     1105    Saturday  weekend
## 5607     0 2012-10-20     1110    Saturday  weekend
## 5608     0 2012-10-20     1115    Saturday  weekend
## 5609    22 2012-10-20     1120    Saturday  weekend
## 5610    11 2012-10-20     1125    Saturday  weekend
```
<br>
2. Panel plot containing a time series plot (of type="l") of the 5-minute interval (x-axis) and average number of steps taken, averaged across all weekdays or weekend days (y-axis):

```r
intervals_new <- aggregate(steps~interval + day_type, data=activity_new, FUN=mean)
ggplot(intervals_new, aes(interval,steps))+geom_line()+facet_grid(day_type ~.) + xlab("interval") + ylab("avg. steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
