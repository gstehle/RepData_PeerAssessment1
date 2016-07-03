Analyzing Human Activity Level Data 
====================================


This analysis makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. Two months of data (October/November 2012) from an anonymous individual comprise this data set, which include the number of steps taken in 5 minute intervals each day.

First we will load our data set from the .csv source file and change dates to the proper format.


```r
setwd("C://Users//Steve//Documents//R//Activity Monitoring Data")
act <- read.csv("activity.csv")
library(lubridate)
library(dplyr)
library(lattice)
act[,2] <- date(act[,2])
```


We first looked at the data across the 61 days it was recorded.  We plotted a histogram to examine the distribution in the number of steps - were activity levels relatively consistent or did they vary wildly?



```r
actday <- act[!is.na(act$steps), ]
actday <- actday %>% group_by(date) %>% summarize(total.steps = sum(steps))
actday <- as.data.frame(actday)
actday$total.steps <- as.numeric(actday$total.steps)
hist(actday$total.steps, breaks = 10, xlab = "# Steps",  main = "Frequency of Daily Step Totals")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



```r
mean(actday$total.steps)
median(actday$total.steps)
```


The distribution is not uniform and indeed it looks normally distributed at a glance.  The mean across days is 10766.1886792 and the median is 10765


The next analysis performed was to look at the different time intervals each day.  This time we plotted a time series of average number of steps for each of the 288 five minute intervals recorded each day.



```r
actint <- act[!is.na(act$steps), ]
actint <- actint %>% group_by(interval) %>% summarize(average.steps = mean(steps))
actint <- as.data.frame(actint)
actint$average.steps <- as.numeric(actint$average.steps)
plot(actint$interval, actint$average.steps, type = "l", xlab = "Interval", ylab = "# Steps", main = "Daily Activity Trend")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


Unsurprisingly, the sedentary periods correspond to when most people are sleeping.  The subject had a spike in activity level in the mid-morning with several smaller peaks and troughs over the next twelve hours.  The maximum activity level occurs at 835.


```r
actint$interval[(which.max(actint$average.steps))]
```


Our data set has 2304 missing values for the steps variable.



```r
sum(is.na(act$steps))
```


The strategy I've devised to "fill" these missing values is to replace them with the average number of steps for the given interval.  After doing that, we plot the same histogram as before.



```r
actimp <- merge(act, actint, by = "interval")
actimp <- actimp %>% 
    mutate(steps = ifelse(is.na(steps),average.steps, steps))
actimp2 <- actimp %>% group_by(date) %>% summarize(total.steps = sum(steps))
actimp2 <- as.data.frame(actimp2)
actimp2$total.steps <- as.numeric(actimp2$total.steps)
hist(actimp2$total.steps, breaks = 10, xlab = "# Steps", main = "Frequency of Step Totals by Date")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


Notice that it has the same shape with a higher peak in the middle of the distribution.  This makes sense because we aren't missing individual observations within a day, but rather entire days of data.  Given that, the  result of this technique is to add more "average days" to the sample.  

The mean is the same as before (10766.1886792) and due to the introduction of these "average days" mentioned above, the median changes slightly to 10765.



```r
mean(actimp2$total.steps)
median(actimp2$total.steps)
```


The last piece of analysis we did was splitting the data into weekdays and weekends to 
look for patterns.



```r
actbygroup <- actimp %>% mutate(daytype = ifelse(weekdays(date) == "Sunday" | 
            weekdays(date) == "Saturday", "weekend", "weekday"))
actbygroup <- actbygroup %>% group_by(daytype, interval) %>% summarize(average.steps = mean(steps))
xyplot(average.steps~interval|daytype, data = actbygroup, type = "l", ylab = "# Steps",
       main = "Weekday/Weekend Activity Comparison")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


We can perhaps speculate that the peak activity level at the 0835 interval followed by a quick
drop and valley in the activity level reflects the subject walking to work and then 
sitting at his desk to begin his work day.  On the weekend the daytime average activity levels
are higher on average.
