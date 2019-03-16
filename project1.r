##Question1
library(ggplot2)
library(plyr)
act_data<-read.csv("activity.csv", header=TRUE)
head(act_data)
str(act_data)
act_data$date<-as.Date(act_data$date)
act_data$day<-weekdays(as.Date(act_data$date))
no_na_data <-act_data[!is.na(act_data$steps),]
sumtable<-aggregate(act_data$steps~act_data$date, FUN=sum)
colnames(sumtable)<-c("date", "sum")
hist(sumtable$sum, xlab = "Step number per day", main="Total number of steps taken each day")

mean(sumtable$sum)
median(sumtable$sum)

###Question2
interval_table<-aggregate(act_data$steps~act_data$interval, FUN=mean)
str(interval_table)
colnames(interval_table)<-c("interval","mean")

ggplot(interval_table, aes(interval, mean))+geom_line()
###Question3
interval_table[which(interval_table$mean == max(interval_table$mean)),1]
sum(is.na(act_data$steps))

new_data<-transform(act_data, steps=ifelse(is.na(act_data$steps), interval_table$mean[match(act_data$interval,interval_table$interval)],act_data$steps))
new_sumtable<-aggregate(new_data$steps~new_data$date, FUN = sum)
colnames(new_sumtable)<-c("date", "sum")
hist(new_sumtable$sum, xlab = "Step number per day", main="Total number of steps taken each day", col="blue")
hist(sumtable$sum, xlab = "Step number per day", main="Total number of steps taken each day", col="red", add=TRUE)
legend("topright", c("Without NA", "With NA"), fill=c("blue", "red"))

mean(new_sumtable$sum)
median(new_sumtable$sum)
##Question4
week<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
new_data$weekday<-ifelse(is.element(weekdays(new_data$date),week), "Weekday","Weekend")

weekday_interval_table<-aggregate(steps~interval+weekday,new_data,mean)
weekday_g<-ggplot(weekday_interval_table,aes(interval, steps))
weekday_g+geom_line() +facet_grid(.~weekday_interval_table$weekday)
