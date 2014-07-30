

# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE)
clean_data <- data[which(data$steps != "NA"), ]
```

## What is mean total number of steps taken per day?

```r
library(plyr)
total_by_day <- ddply(clean_data, .(date), summarise, steps = sum(steps))
hist(total_by_day$steps, main = "Number of Steps", xlab = "Total number of steps taken each day", 
    col = "light blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

# mean and median total number of steps taken per day
mean(total_by_day$steps)
```

```
## [1] 10766
```

```r
median(total_by_day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
average_by_interval <- ddply(clean_data, .(interval), summarise, steps = mean(steps))
plot(average_by_interval$interval, average_by_interval$steps, type = "l", col = "blue", 
    xlab = "5-minute interval", ylab = "Average number of steps taken", main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
average_by_interval[average_by_interval$steps == max(average_by_interval$steps), 
    ]
```

```
##     interval steps
## 104      835 206.2
```

```r
colnames(average_by_interval)[2] <- "intervalAvg"
```

## Imputing missing values

```r
# Total number of missing values in the dataset
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
# Fill NA's with average for that 5-min interval
merged <- arrange(join(data, average_by_interval), interval)
```

```
## Joining by: interval
```

```r
# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
merged$steps[is.na(merged$steps)] <- merged$intervalAvg[is.na(merged$steps)]
# Histogram
new_total_by_day <- ddply(merged, .(date), summarise, steps = sum(steps))
hist(new_total_by_day$steps, main = "Number of Steps", xlab = "Total number of steps taken each day", 
    col = "light blue", )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
# mean and median total number of steps taken per day
mean(new_total_by_day$steps)
```

```
## [1] 10766
```

```r
median(new_total_by_day$steps)
```

```
## [1] 10766
```

```r
total_steps1 <- sum(clean_data$steps)
total_steps2 <- sum(merged$steps)
total_diff <- total_steps2 - total_steps1[]
```

Mean values didn't change, because I supplied interval averge for the given interval, so total average didn't change. Histogram and median changed somewhat. filling values added total of 8.613 &times; 10<sup>4</sup> steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
weekdays <- weekdays(as.Date(merged$date))
data_with_weekdays <- transform(merged, day = weekdays)
data_with_weekdays$wk <- ifelse(data_with_weekdays$day %in% c("Saturday", "Sunday"), 
    "weekend", "weekday")
average_by_interval_wk <- ddply(data_with_weekdays, .(interval, wk), summarise, 
    steps = mean(steps))

xyplot(steps ~ interval | wk, data = average_by_interval_wk, layout = c(1, 2), 
    type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 




