library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggplot2)
library(tidyr)

#prepare the dataset
activity <- read.csv("C:/Users/dehghani/Desktop/Sandra course R analysis/data/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
calories <- read.csv("C:/Users/dehghani/Desktop/Sandra course R analysis/data/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
sleep <- read.csv("C:/Users/dehghani/Desktop/Sandra course R analysis/data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight <- read.csv("C:/Users/dehghani/Desktop/Sandra course R analysis/data/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

head(activity)
head(calories)

calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

head(calories)
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")

#sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")


head(activity)

head(sleep)

weight$Date=as.POSIXct(weight$Date, format="%m/%d/%Y", tz=Sys.timezone())

head(weight)

head(calories)


n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)


activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()



activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()


calories %>%
  select(Calories) %>%
  summary()

sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

weight %>%
  select(WeightKg, BMI) %>%
  summary()

calories %>%
  select(Calories) %>%
  summary()

sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

weight %>%
  select(WeightKg, BMI) %>%
  summary()

#merged data?
merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)

#Relationship between total number of steps and number of calories

ggplot(data = activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(color = "blue") +  # Change the color of the points to blue
  geom_smooth(color = "black") +  # Change the color of the smoothed line to red
  labs(title = "Total Steps vs. Calories")

#Relationship between Total Distance VS number of calories

ggplot(data = activity, aes(x = TotalDistance, y = Calories)) +
  geom_point(color = "blue") +  # Change the color of the points to blue
  geom_smooth(color = "black") +  # Change the color of the smoothed line to red
  labs(title = "Total distance vs. Calories")

#Relationship between Total Minutes Sleep VS Total Time in Bed
ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point(color = "blue") +  
  geom_smooth(color = "black") +
  labs(title="Total Minutes Asleep vs. Total Time in Bed")

total_time <- colSums(activity[, c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes")])

# Create a data frame for the pie chart
pie_data <- data.frame(
  Activity = c("Very Active", "Moderately Active", "Lightly Active", "Sedentary"),
  Time = total_time
)


#calculate the proportion of activity
pie_data$Proportion <- pie_data$Time / sum(pie_data$Time)

#Pie chart

pie_chart <- ggplot(pie_data, aes(x = "", y = Proportion, fill = Activity)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Activity")

print(pie_chart)

