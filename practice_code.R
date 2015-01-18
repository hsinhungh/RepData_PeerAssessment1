
library(data.table)
library(ggplot2)
setwd('~/git_repo/RepData_PeerAssessment1/')

data_file <- 'activity.csv'
dt <- data.table( read.csv(data_file) )

#dt2 <- data.table(df[!is.na(df$steps),])
#typeof(dt2$steps)


# Q1-1: Make a histogram of the total number of steps taken each day
sum_by_date <- dt[,list(sum_steps = sum(steps)),by='date']
barplot(sum_by_date$sum_steps,names.arg=sum_by_date$date, las = 3, cex.axis=1.0, cex.names = 0.75)

# Q1-2: Calculate and report the mean and median total number of steps taken per day
mean_and_median <- sum_by_date[ ,list( mean = mean(sum_steps, na.rm = TRUE), median = median(sum_steps, na.rm = TRUE))]

# Q2-1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
interval_time_series <- dt[,list(sum_steps = sum(steps, na.rm = TRUE), avg_steps = mean(steps, na.rm = TRUE)),by='interval']
#interval_time_series <- dt[,list(sum_steps = sum(steps), avg_steps = mean(steps)),by='interval']
plot(interval_time_series$interval, interval_time_series$avg_steps, type = "l")

# Q2-2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval_record <- interval_time_series[,.SD[which.max(sum_steps)]]

# Q3-1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
num_of_na <- sum(is.na(dt$steps))

# Q3-2: Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Q3-3: Create a new dataset that is equal to the original dataset but with the missing data filled in.
dt <- merge(dt, interval_time_series, by="interval" )
dt[,new_steps := ifelse(is.na(steps),avg_steps,steps)]

# Q3-4-1: Make a histogram of the total number of steps taken each day 
new_sum_by_date <- merge(sum_by_date, dt[,list(new_sum_steps = sum(new_steps)),by='date'], by = "date")
barplot(new_sum_by_date$new_sum_steps,names.arg=new_sum_by_date$date, las = 3, cex.axis=1.0, cex.names = 0.75)


# Q3-4-2: and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
mean_and_median_2 <- new_sum_by_date[ ,list( mean = mean(new_sum_steps, na.rm = TRUE), median = median(new_sum_steps, na.rm = TRUE))]

# Q4-1: Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#new_sum_by_date[,weekday:=weekdays(as.Date(date))]
#new_sum_by_date[,date_type:=ifelse(weekday=="Saturday" | weekday=="Sunday","Weekend","Weekday")]
dt[,weekday:=weekdays(as.Date(date))]
dt[,date_type:=ifelse(weekday=="Saturday" | weekday=="Sunday","Weekend","Weekday")]


# Q4-2: Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
interval_time_series_2 <- dt[,list(sum_steps = sum(new_steps, na.rm = TRUE), avg_steps = mean(new_steps, na.rm = TRUE)),by=c('interval','date_type')]
qplot(interval,avg_steps,data = interval_time_series_2,facets = date_type ~ ., main = "Test", geom = "line")
