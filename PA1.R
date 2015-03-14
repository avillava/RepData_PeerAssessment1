library(psych)
library(lubridate)
# Loading the Dataset
activity = read.csv("C:/Users/x3039568/Desktop/Coursera/Reproducible Research/Peer Assessment 1/repdata-data-activity/activity.csv")


# What is mean total number of steps taken per day?

## 1.- Calculate the total number of steps taken per day
total_of_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_of_steps_per_day

## 2.- Make a histogram of the total number of steps taken each day
## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)

## Generating the Histogram by each day
hist(total_steps_each_day$steps)

## 3.- the mean and median
### b) funtion mean and median
total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_median <- median(total_steps_each_day$steps)



# What  is the average daily activity pattern?

## 1.- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
##    averaged across all days (yaxis)
five_minutes_average <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = five_minutes_average$interval, y = five_minutes_average$steps, type = "l") 



## 2.- Which 5-minutes interval, on average across all the days in the dataset, 
##    contains the maximum number of steps
max_steps <- max(five_minutes_average$steps)
for (i in 1:288) 
{
    if (five_minutes_average$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
}
five_minute_interval_at_max_steps 

# Inputing missing values

## 1.- Calculate and report the total number of missing values inthe dataset
total_na <- 0
for (i in 1:17568)
{
    if(is.na(activity$steps[i])) 
        total_na <- total_na+1 
}
total_na

## 2.- Devise a strategy for filling in all of the missing values in the dataset
# The strategy will be to fill in the dataset the with the mean of that 5 five minute interval

## 3.- Create a new dataset that is equal to the original dataset but with the missing data filled in
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


## 4a.- Make a histogram of the total number of steps taken each day
## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day_filled_in <- aggregate(steps~date, data=activity_filled_in, FUN=sum, na.rm=TRUE)

## Generating the Histogram by each day with new dataset (activity_filled_in)
hist(total_steps_each_day_filled_in$steps)

## 4b.- the mean and median

### funtion mean and median
total_steps_each_day_mean_filled_in <- mean(total_steps_each_day_filled_in$steps)
total_steps_each_day_median_filled_in <- median(total_steps_each_day_filled_in$steps)

### Due to that we use the mean value the new estimates of mean did not change
### Due to that we use the mean value the new estimates of median change more close to the mean


# Are there differences in activity patterns between weekdays and weekends?

## 1.- Create a new factor variable in the datset levels (weekday and weekend)

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


## 2.- Make a Panel Plot containing a time series plot of five_minute interval. Weekday and Weekend

##  mean and median by weekday
### b) funtion mean and median

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
