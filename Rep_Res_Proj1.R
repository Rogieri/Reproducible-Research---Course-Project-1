library(lubridate)
library(scales)
library(ggplot2)
library(dplyr)

data<-read.csv("activity.csv")

data$Date_Time<-ymd_hm(paste(data$date,sep="_",substr((10000+data$interval),2,5)))



# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# Calculate the total number of steps taken per day

step_data <-aggregate(steps~date,data,sum)

# If you do not understand the difference between a histogram and a barplot, 
# research the difference between them. Make a histogram of the total number of steps 
# taken each day

hist(step_data$steps,breaks=8,main="Total number of steps per day", xlab="Steps per day",
     col="dark blue")
dev.copy(png,"Histogram.png", width=480, height=480)
dev.off()

# Calculate and report the mean and median of the total number of steps taken per day

mean1<-round(mean(step_data$steps))
median1<-median(step_data$steps)

paste("The mean of the total number of steps taken per day is",mean1,"and the median is",median1)

# What is the average daily activity pattern?
# Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number 
# of steps taken, averaged across all days (y-axis)


interval <- data %>% 
  filter(!is.na(steps)) %>% 
  group_by(interval) %>% 
  summarize(steps = sum(steps))

interval$Time<-dmy_hm(paste("1/1/1900_",paste(substr(10000+interval$interval,2,3),substr(10000+interval$interval,4,5),sep=":")))


ggplot(interval, aes(x=Time, y=steps)) +
  geom_line(color = "mistyrose4") +
  scale_x_datetime(breaks = date_breaks("2 hour"), 
                   labels = date_format("%H:%M"),
                   limits = c(interval$Time[1], interval$Time[288])) +
  labs(title = "Average Number of Steps taken (Averaged Across All Days)", 
       x = "Time of Day", 
       y = "Average Steps")+
  geom_hline(yintercept=max(interval$steps), color="red")

dev.copy(png,"tmsr_stepsintraday.png", width=480, height=480)
dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max.interval <- interval[which.max(interval$steps),]
paste("The interval with the maximum number of steps is",
      round(max.interval$steps[1]), "at",
      max.interval$Time[1])



# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)

miss<-sum(is.na(data$steps))
paste("The number of missing values is",miss)

# Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use the mean/median
# for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

data_complete <- data
missing_data <- is.na(data_complete$steps)
mean_interval <- tapply(data_complete$steps,
                        data_complete$interval, 
                        mean, na.rm=TRUE, simplify=TRUE)

data_complete$steps[missing_data] <- mean_interval[as.character(data_complete$interval[missing_data])]

# Make a histogram of the total number of steps taken each day and Calculate and report 
# the mean and median total number of steps taken per day. Do these values differ from the
# estimates from the first part of the assignment? 

step_data_complete <-aggregate(steps~date,data_complete,sum)

hist(step_data_complete$steps,breaks=8,main="Total number of steps per day", xlab="Steps per day",
     col="dark blue")
dev.copy(png,"Histogram2.png", width=480, height=480)
dev.off()



# What is the impact of imputing missing data on the estimates of the total daily number of steps?

mean2<-mean(step_data_complete$steps)
median2<-median(step_data_complete$steps)

paste("The mean and median of the total number of steps taken per day is the same in both cases but frequency increases")


# Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
# indicating whether a given date is a weekday or weekend day.

data_complete<-mutate(data_complete,weektype = ifelse(weekdays(data$Date_Time)%in%c("sábado","domingo"),"Weekend","Weekday"))

# Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all weekday days or 
# weekend days (y-axis). See the README file in the GitHub repository to see an example 
# of what this plot should look like using simulated data.

data_complete$weektype <- as.factor(data_complete$weektype)
data.wkd <- data_complete %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))

data.wkd$Time<-dmy_hm(paste("1/1/1900_",paste(substr(10000+data.wkd$interval,2,3),substr(10000+data.wkd$interval,4,5),sep=":")))

ggplot(data.wkd, aes(x=Time, y=steps, color = weektype)) +
  geom_line(color = "mistyrose4") +
  facet_wrap(~weektype, ncol = 1, nrow=2)+
  scale_x_datetime(breaks = date_breaks("2 hour"), 
                   labels = date_format("%H:%M"),
                   limits = c(min(data.wkd$Time), max(data.wkd$Time))) +
  labs(title = "Average Number of Steps taken (Averaged Across All Days)", 
       x = "Time of Day", 
       y = "Average Steps")+
  geom_hline(yintercept=max(data.wkd$steps), color="red")

dev.copy(png,"stepsintraday_wkday_vs_wkend.png.png", width=480, height=480)
dev.off()