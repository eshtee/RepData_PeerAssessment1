install.packages("ggvis")
library(dplyr)
library(plyr)
setwd("C:\\Users\\Admin\\Documents\\GitHub\\RepData_PeerAssessment1")
setDefaults('as.Date.character', format = '%Y-%M-%d')
activity_data <- tbl_df(read.csv ("activity\\activity.csv"))

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

fixed_activity_data <- ddply(activity_data, ~interval, transform, steps = impute.mean(steps))
activity_data$date <- as.Date(activity_data$date) 
glimpse(activity_data)


fixed_activity_data$date <- as.Date(fixed_activity_data$date)
#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
fixed_activity_data$wDay <- factor((weekdays(fixed_activity_data$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') )

average_steps_taken_by_interval_and_wday <- fixed_activity_data %>% 
  group_by(interval,wDay)  %>%
  summarise(average_steps = mean(steps))
?qplot
qplot(interval, average_steps, data = average_steps_taken_by_interval_and_wday, facets=wDay~., geom = "line") 
  + geom_line()
  + geom_smooth( method = "lm", col = "red") +
  + facet_wrap(~Line) +
  + theme_bw()

activity_data_no_na<-activity_data[!is.na(activity_data$steps),]

total_steps_taken_per_day <- activity_data_no_na %>% 
  group_by(date)  %>%
  summarise(total_steps = sum(steps))

average_steps_taken_by_interval <- activity_data_no_na %>% 
  group_by(interval)  %>%
  summarise(average_steps = mean(steps))


average_steps_taken_by_interval %>%
  ggvis(~interval, ~average_steps) %>%
  layer_lines() %>%
  layer_points()

average_steps_taken_by_interval %>%
  filter(average_steps == max(average_steps))

total_steps_taken_per_day %>% 
  ggvis(~total_steps)

mean(total_steps_taken_per_day$total_steps)

median(total_steps_taken_per_day$total_steps)

hist(activity_data_no_na)

type.convert(activity_data$date)


activity_data_only_na<-activity_data[is.na(activity_data$steps),]

nrow(activity_data_only_na)

average_steps_taken_by_interval[average_steps_taken_by_interval$interval==0,]$average_steps


median_steps_taken_per_day <- activity_data_no_na %>% 
  group_by(date)  %>%
  summarise(total_steps = median(steps))

f<-function (interval){
  activity_data[activity_data$interval==interval 
                & is.na(activity_data$steps),]$steps  <- average_steps_taken_by_interval[average_steps_taken_by_interval$interval==interval,]$average_steps
}

impute.mean <- function(df) replace(df$step, is.na(df$step), aggregate(. ~ Name, d[-2], mean)mean(df$step, na.rm = TRUE))
impute.mean(activity_data$steps)
dt<- data.table(activity_data)
activity_data[, activity_data$step = mean(activity_data$val, na.rm = TRUE), activity_data$interval]

class(as.Date(activity_data$date))




?data.table

fixed_activity_data <- activity_data %>% 
  group_by(interval) %>% 
   transform(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

head(fixed_activity_data)

mean(activity_data$steps,na.rm=TRUE)

something<-lapply(average_steps_taken_by_interval$interval,f)
