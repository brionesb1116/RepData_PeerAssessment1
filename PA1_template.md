### Reproducible Research Course Project: Personal Activity Monitoring Device Data

<br>

Downloading and reading data into R (plus loading the ggplot2 and other packages)
```{r echo=T, results="hide"}
setwd("~/Documents/r-things")
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, "activity.zip")
unzip("activity.zip")
activity <- read.csv("activity.csv")
library(ggplot2)
library(dplyr)
library(lubridate)

```
<br>
A. What is the mean total number of steps taken per day?<br>
1. Calculating total number of steps per day<br>
```{r}
steps_per_day <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(steps_per_day) <- c("date","total_steps")
head(steps_per_day, 10) # First ten records
tail(steps_per_day, 10) # Last ten records
```
<br>
2. Histogram of the total number of steps taken each day:
```{r}
ggplot(steps_per_day, aes(x=total_steps))+geom_histogram(color="white", binwidth=2000)
```
<br>
3. Mean and median of the total number of steps taken per day:
```{r}
mean(steps_per_day$total_steps, na.rm=T) # mean total steps per day
median(steps_per_day$total_steps, na.rm=T) # median total steps per day
```
<br>
<br>
B. What is the average daily activity pattern?
<br>
1. The time-series plot (of type="l", but int the ggplot case, a geom_line) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):
```{r}
intervals <- aggregate(activity$steps, list(activity$interval), FUN=mean, na.rm=T)
colnames(intervals) <- c("interval", "avg_steps")
ggplot(intervals, aes(x=interval,y=avg_steps))+geom_line()
```
<br>
2. The 5-minute interval, on average across all the days, which contains the maximum number of steps:
```{r}
intervals[which.max(intervals$avg_steps),]
```
<br>
C. Imputing missing values:<br>
1. Total number of missing values (i.e. total number of rows with NAs)
```{r}
sum(!complete.cases(activity))
```
<br>
2. Strategy for filling in all of the missing values
```{r}
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
```{r}
head(activity_new, 10) # First ten records
tail(activity_new, 10) # Last ten records
```
<br>
4. Histogram of the total number of steps taken each day, using the new data set...<br>
```{r}
# But first...
new_steps <- aggregate(activity_new$steps, list(activity_new$date),FUN=sum)
colnames(new_steps) <- c("date", "total_steps")
ggplot(new_steps, aes(x=total_steps))+geom_histogram(color="white", binwidth = 2000)
```
<br>
```{r}
mean(new_steps$total_steps, na.rm=T) # Mean total number of steps per day
median(new_steps$total_steps, na.rm=T) # Median total number of steps per day
```
<br>
In making comparisons between the earlier estimates (before filling in missing values) and later (after filling in missing values), only the median values differed (old: 10765, versus new: 10766.19), while the mean values remained the same (10766.19 in both cases). This is indicative that imputing missing data here made a very small impact.
<br>
<br>
D. Using the new dataset with the filled-in missing values, are there any differences in activity patterns between weekdays and weekends?<br>
1. New factor variable in the dataset with two levels: "weekday" and "weekend" to indicate whether a given date is a weekday or a weekend day:
```{r}
activity_new$date <- as.Date(activity_new$date, format="%Y-%m-%d") # turning 'date' values from character string to date format
activity_new$day_of_week <- weekdays(activity_new$date)
activity_new$day_type <- ifelse(activity_new$day_of_week=="Saturday" | activity_new$day_of_week=="Sunday", "weekend", "weekday") # new factor variable
head(activity_new, 10)
activity_new[5600:5610,] # randomly chosen subset of rows just to see whether weekend days were correctly identified as 'weekend'
```
<br>
2. Panel plot containing a time series plot (of type="l") of the 5-minute interval (x-axis) and average number of steps taken, averaged across all weekdays or weekend days (y-axis):
```{r}
intervals_new <- aggregate(steps~interval + day_type, data=activity_new, FUN=mean)
ggplot(intervals_new, aes(interval,steps))+geom_line()+facet_grid(day_type ~.) + xlab("interval") + ylab("avg. steps")
```
