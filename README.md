---
title: "Peer Assessment 1"
author: "Alejandro Villarreal"
date: "Saturday, March 14, 2015"
output: html_document
---

Introducing me:
---------------

I'm Alejandro from MÃ©xico. This is my peer Assessment 1. I hope that can be graduate by some of my peers of the Coursera course Reproducible Research. Please; consider, that I'm not a R language expert. And, I made my best job


## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this data set are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The data set is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
data set.


## Assignment

This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this
assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository and the SHA-1 commit ID for your
repository state.

NOTE: The GitHub repository also contains the data set for the
assignment so you do not have to download the data separately.


```{r, output:md_document, keep_md:true }

```

### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)

*I downloaded and unziped the file from the Cousera Site. And, I loaded the "activity.csv" from my Hard disk (C)*

```{r}
library(psych)
library(lubridate)
# Loading the Dataset
activity = read.csv("C:/Users/x3039568/Desktop/Coursera/Reproducible Research/Peer Assessment 1/repdata-data-activity/activity.csv")
```


2. Process/transform the data (if necessary) into a format suitable for your analysis

*Was not need to Process or transform the data*


### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the data set.

1. Calculate the total number of steps taken per day

*Calculating the total number of steps taken per day*
```{r}
total_of_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_of_steps_per_day
```


2. Make a histogram of the total number of steps taken each day
```{r}
## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)

## Generating the Histogram by each day
hist(total_steps_each_day$steps)
```


3. Calculate and report the mean and median of the total number of steps taken per day

*Calculating the mean and median*
```{r}
total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_median <- median(total_steps_each_day$steps)
```

*mean and median*
```{r, echo=FALSE}
total_steps_each_day_mean
total_steps_each_day_median
```


### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

*creating a dataframe with the mean of each 5-minute interval and use the funtion plot() to make the time series plot*
```{r}
five_minutes_average <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = five_minutes_average$interval, y = five_minutes_average$steps, type = "l") 
```


2. Which 5-minute interval, on average across all the days in the data set, contains the maximum number of steps?

*Using the funtion max() in order to find the maximum number of steps. And, using a for{} loop to find the 5 minute interval with maximum number of steps*
```{r}
max_steps <- max(five_minutes_average$steps)
for (i in 1:288) 
{
    if (five_minutes_average$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
}
five_minute_interval_at_max_steps 
```



### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the data set (i.e. the total number of rows with `NA`s)

*Creating a new variable (total_na) to store the total missing values in the dataset*
```{r}
total_na <- 0
for (i in 1:17568)
{
    if(is.na(activity$steps[i])) 
        total_na <- total_na+1 
}
total_na
```


2. Devise a strategy for filling in all of the missing values in the data set. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* *The strategy will be to fill in the dataset the with the mean of that 5 minute interval*



3. Create a new data set that is equal to the original data set but with the missing data filled in.


*Here, I'm using a pointer in order to find the "na" *
*Is created a new data set (activity_filled_in), copying the original data set *
*After, when is finding a "na" is stored in pointer the position of 'na' in order to replace it with the mean value of that 5 minute interval*

```{r}
activity_filled_in <- activity
for (i in 1:17568) # loop to find the na
{
    if(is.na(activity_filled_in$steps[i])) # if steps is na store the pointer 
    { 
        five_minute_pointer <- activity_filled_in$interval[i] #store the value of pointer to find the mean on five minute interval
        for (j in 1:288)  # loop to find the value of pointer on the data frame of five minute interval
        {
            if (five_minutes_average$interval[j] == five_minute_pointer) # finding the value of mean of five minute interval data frame
                activity_filled_in$steps[i] <- five_minutes_average$steps[j] # replacing the na by the mean in that fime minute interval 

        }
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


*4a.- Make a histogram of the total number of steps taken each day*
```{r}
## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day_filled_in <- aggregate(steps~date, data=activity_filled_in, FUN=sum, na.rm=TRUE)

## Generating the Histogram by each day with new dataset (activity_filled_in)
hist(total_steps_each_day_filled_in$steps)
```


*4b Calculate the mean and median and explain the imoact of imputing missing data on the estimates of the total daily number of steps*

```{r}
## funtion mean and median
total_steps_each_day_mean_filled_in <- mean(total_steps_each_day_filled_in$steps)
total_steps_each_day_median_filled_in <- median(total_steps_each_day_filled_in$steps)
```

```{r, echo=FALSE}
## mean and median
total_steps_each_day_mean_filled_in 
total_steps_each_day_median_filled_in 
```

 #### *Due to that we use the mean value the new estimates of mean did not change*
 
 #### *Due to that we use the mean value the new estimates of median change more close to the mean*


### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the data set with the filled-in missing values for this part.

1. Create a new factor variable in the data set with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

*I used the wday() funtion*

*Using the "week_day" vector to determinate if is a weekday or weekend day and add this vector to the dataset activity_filled_in, now it is a dataset with 4 variables*
```{r}
### creating a char vector (week_day) 
week <- wday(activity_filled_in$date)
week_day <- week
for (i in 1:17568) # loop to find the na
{
    if(week[i] == 1)
        week_day[i] <- 'weekend'
    if(week[i] == 2)
        week_day[i] <- 'weekday'
    if(week[i] == 3)
        week_day[i] <- 'weekday'
    if(week[i] == 4)
        week_day[i] <- 'weekday'
    if(week[i] == 5)
        week_day[i] <- 'weekday'
    if(week[i] == 6)
        week_day[i] <- 'weekday'
    if(week[i] == 7)
        week_day[i] <- 'weekend'
}

### Creating a new factor variable in the dataset "activity_filled_in" 
activity_filled_in$weekday <-week_day
```


2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

*Were created new data frames to divide weekday and weekend days*
*I used the plot() funtion to generate the graphics, That is why are separated*
```{r}
# finding the elements by "weekday" or "weekend"
weekday <- grep("weekday",activity_filled_in$weekday)
weekday_frame <- activity_filled_in[weekday,]
weekend_frame <- activity_filled_in[-weekday,]


# What  is the average daily activity pattern?

## Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
##    averaged across all days (yaxis)
five_minutes_average_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
five_minutes_average_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)

plot(x = five_minutes_average_weekday$interval, y = five_minutes_average_weekday$steps, type = "l") 
plot(x = five_minutes_average_weekend$interval, y = five_minutes_average_weekend$steps, type = "l") 

```


