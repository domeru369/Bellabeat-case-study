#load the core library 'tidyverse'
library(tidyverse)

#read the csv files 
daily_activity <- read_csv("C:/Users/user/Documents/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
hourly_calories2 <- read_csv("C:/Users/user/Documents/coursera/hourlyCalories_merged2.csv")
weight_info <- read_csv("C:/Users/user/Documents/coursera/weightLogInfo_merged2.csv")
hourly_steps2 <- read_csv("C:/Users/user/Documents/coursera/hourlySteps_merged2.csv")
sleep_day2 <- read_csv("C:/Users/user/Documents/coursera/sleepDay_merged2.csv")

#Data exploration
#to get the top 6 rows of each data
head(daily_activity)
head(hourly_calories2)
head(weight_info)
head(hourly_steps2)
head(sleep_day2)

#to get the structure of each data
str(daily_activity)
str(hourly_calories2)
str(weight_info)
str(hourly_steps2)
str(sleep_day2)


#to check for each column names across every data
colnames(daily_activity)
colnames(hourly_calories2)
colnames(weight_info)
colnames(hourly_steps2)
colnames(sleep_day2)

#find unique user IDS computed in the dataset
n_distinct(daily_activity$Id)
n_distinct(hourly_calories2$Id)
n_distinct(weight_info$Id)
n_distinct(hourly_steps2$Id)
n_distinct(sleep_day2$Id)
#there are 33 distinct users in the daily activity, hourly calories and hourly steps data
#24 distinct users in the sleep day data 
#and 8 unique users in the weight info data

#check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(hourly_calories2))
sum(duplicated(weight_info))
sum(duplicated(hourly_steps2))
sum(duplicated(sleep_day2))

#there are 3 duplicates in the sleep day data so i drop duplicates
sleep_day4 <- sleep_day2 %>%
  distinct(.keep_all = TRUE)
#view the data
glimpse(sleep_day2)

#find the count of missing values 
sum(is.na(daily_activity))
sum(is.na(hourly_calories2))
sum(is.na(weight_info))
sum(is.na(hourly_steps2))
sum(is.na(sleep_day4))
#weight data has 65 missing values from the fat column

#drop the fat column
weight_clean <- weight_info[colSums(is.na(weight_info)) == 0]
print(weight_clean)

#to get summary statistics of the variables in the datasets
daily_activity %>% 
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()
hourly_calories2 %>% 
  select(ActivityHour, Calories) %>% 
  summary()
weight_clean %>% 
  select(WeightPounds, BMI) %>% 
  summary()
hourly_steps2 %>% 
  select(ActivityHour, StepTotal) %>% 
  summary()
sleep_day4y%>% 
  select(SleepDay, TotalSleepRecords, TotalMinutesAsleep,TotalTimeInBed) %>% 
  summary()


#Before merging the dataframes, we need to ensure their date columns speak the same language, meaning we will rename the date columns in all dataframes to a consistent format.
daily_activity3 <- daily_activity %>% 
  rename(Date=ActivityDate)
hourly_calories7 <- hourly_calories2 %>% 
  rename(Date=ActivityHour)
weight_clean1 <- weight_clean %>%
  rename(Date=Date)
hourly_steps3 <-  hourly_steps2 %>% 
  rename(Date=Date)
sleep_day5 <- sleep_day4 %>% 
  rename(Date=SleepDay)

#we will convert the date format making it consistent
#note : no need to change the time column data type because its already in the appropriate time datatype
#convert the date format of daily activity
daily_activity_cleaned2 <- daily_activity3 %>% 
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column 
str(daily_activity_cleaned)
View(daily_activity_cleaned)

#convert the date format of hourly calories
hourly_calories_cleaned7 <- hourly_calories7 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column 
str(hourly_calories_cleaned7)
View(hourly_calories_cleaned7)


#convert the date format of weight info
weight_info_cleaned <- weight_clean1 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column  
str(weight_info_cleaned)
View(weight_info_cleaned)


#convert the date format of hourly steps
hourly_steps_cleaned <- hourly_steps3 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column  
str(hourly_steps_cleaned)
View(hourly_steps_cleaned)


#convert the date format of hourly steps
sleepday_cleaned <- sleep_day5 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column  
str(sleepday_cleaned)
View(sleepday_cleaned)


#merge the dataframes together 
#merge the daily activity and sleepday data together
daily_sleep_merged_data <- merge(daily_activity_cleaned,sleepday_cleaned, 
                                 by=c('Id','Date'))
head(daily_sleep_merged_data)
View(daily_sleep_merged_data)


#merge the hourly calories and hourly steps data
hourly_calories_steps_data <- merge(hourly_calories_cleaned7,hourly_steps_cleaned, 
                                          by=c('Id','Date', 'Time'))
head(hourly_calories_steps_data)
View(hourly_calories_steps_data)


#merge the daily sleep merged data with the hourly calories steps merged data
final_merged_data1 <- merge(daily_sleep_merged_data,hourly_calories_steps_data, 
                                 by=c('Id','Date'))
head(final_merged_data1)
View(final_merged_data1)
colnames(final_merged_data1)

#get summary statistics of the merged data
final_merged_data1 %>% 
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories.x,Calories.y,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes,
         TotalMinutesAsleep, TotalTimeInBed, StepTotal,TotalSleepRecords) %>%
  summary()

#merge daily activities with weight info 
#i did not merge the weight info to the final dataset in order to avoid reduction of the data
#because only 8 distinct users gave weight information 
activity_weight_data <- merge(daily_activity_cleaned,weight_info_cleaned, 
                            by=c('Id','Date'))
head(activity_weight_data)
View(activity_weight_data)
colnames(activity_weight_data)


#Visualizations
#Understanding Activity Level
#we will first plot a pie chart to break down the different ways users were active in minutes
#get the total of each activity minutes 
total_activity_mins <- final_merged_data1 %>%
  summarize(sum_veryactivemins = sum(VeryActiveMinutes),
            sum_fairlyactivemins = sum(FairlyActiveMinutes),
            sum_lightlyactivemins = sum(LightlyActiveMinutes),
            sum_sedentaryactivemins = sum(SedentaryMinutes))
View(total_activity_mins)

#create a dataframe of each types of activity minutes categories in order to visualize 
activity_mins_df <- data.frame(
  activity_mins = c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes"),
  value = c(245569, 175849, 2119145, 6978942)
)
head(activity_mins_df)

#visualize the data with a barplot
barplot_activity_mins <- ggplot(activity_mins_df, aes(x="", y=value, fill=activity_mins))+
  geom_bar(width = 1, stat = "identity")
barplot_activity_mins

#create a pie chart
pie_activity_mins <- barplot_activity_mins + 
  coord_polar("y", start=0) +
  labs(title = "Pie chart of all types of activity minutes") 
pie_activity_mins

#sedentary minutes are consistently higher than other activity minutes (very active, fairly active, and light active) in the Fitbit data, 
#it suggests a predominantly inactive lifestyle for the users generally. A high percentage of sedentary minutes indicates the user spends most of their day sitting, lying down, 
#or engaging in activities with minimal physical exertion.

#2. exploring potential relationships between variables
#total steps and very active minutes
ggplot(data = final_merged_data1, aes(x = TotalSteps, y = VeryActiveMinutes)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Scatterplot of Total Daily Steps and Very Active Minutes",
       x = "Total daily steps",
       y = "Very active minutes")

#total steps and average intensity
ggplot(data = final_merged_data, aes(x = TotalSteps, y = AverageIntensity)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Scatterplot of Total Daily Steps and Very Average Intensity",
       x = "Total daily steps",
       y = "Average Intensity")


#the scatterplot indicates that there is a weak positive correlation between total steps and very active minutes/average intensity,
#while there's a general tendency for increased steps to correspond with more very active minutes, it's not a  definitive relationship.
#Many other factors can influence how steps translate into very active minutes.
#Possible explanations for this weak correlation:

#Steps Don't Equal Intensity: High step counts can be achieved through low-intensity activities like slow walking, which might not significantly elevate heart rate and contribute to very active minutes.
#Varied Activities: The data might include users who engage in activities not well captured by steps, such as swimming or cycling. These activities could burn significant calories without registering many steps.
#Individual Fitness Levels: Someone with a higher baseline fitness level might need to take more steps to reach the same intensity level (very active minutes) compared to someone who is less fit.

#daily steps and daily calories
ggplot(data = final_merged_data1, aes(x = TotalSteps, y = Calories.x)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Scatterplot between Total Daily Steps and Daily Calories Burned", 
       x = "Total daily steps",
       y = "Daily calories burned")

#total distance and daily calories
ggplot(data = final_merged_data1, aes(x = TotalDistance, y = Calories.x)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Scatterplot of Total Distance and Daily Calories Burned",
       x = "Total distance",
       y = "Daily calories burned")
#According to the scatterplot, there is a positive correlation between total steps/distance and calories burned 
#It indicates that users who take more steps or travel further distances tend to burn more calories.

#we will create a barplot for daily calories/steps and hourly calories/steps to breakdown the analysis further in hours
#we will first extract day from the date column
final_merged_data1$weekday <- wday(final_merged_data$Date, label=TRUE, abbr=FALSE)
View(final_merged_data1)

# get the average of the data 
average_data1 <- final_merged_data1 %>%
  group_by(Date) %>% # Group data by date
  mutate(avg_steps = mean(TotalSteps),# Calculate average steps for each date group
         avg_calories = mean(Calories.x),
         avg_veryactivemins = mean(VeryActiveMinutes),
         avg_totalminsasleep = mean(TotalMinutesAsleep),
         avg_totaltimeinbed = mean(TotalTimeInBed),
         avg_sedendarymins = mean(SedentaryMinutes),
         avg_lowactivemins = mean(LightlyActiveMinutes),
         avg_totaldistance = mean(TotalDistance)) # Calculate average calories for each date group
View(average_data1)

#get average number of steps taken by day
ggplot(average_data1, aes(x = weekday, y = avg_steps)) +
  geom_bar(stat = "identity", fill = "pink") + 
  labs(title = "Average number of steps taken by day",
       x = "Day of the Week",
       y = "Average Steps")


# average calories burned per day
ggplot(average_data1, aes(x = weekday, y = avg_calories)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(title = "Average number of calories burned by day",
       x = "Day of the Week",
       y = "Average Calories Burned")
#people take more steps and burn more calories on Tuesday compared to other days of the week
#and less steps are taken,less calories are burned on Sunday and Monday.

#hourly data 
#steps in hours
ggplot(average_data1, aes(x = Time, y = StepTotal)) +
  geom_bar(stat = "identity", fill = "purple") + 
  labs(title = "Steps taken in each hour of the day",
       x = "Hour of Day",
       y = "Total Steps")
#calories burned in hours
ggplot(average_data1, aes(x = Time, y = Calories.y)) +
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "Steps taken in each hour of the day",
       x = "Hour of Day",
       y = "Total Calories Burned")
#he bar cahrt shows a trend of increased activity levels, measured by steps taken and calories burned, in the evening hours around 5pm to 6pm, 
#compared to lower activity levels in the morning from 12am to 7am.

#sleep patterns and recovery 
#total time in bed vs total minutes asleep
ggplot(data = final_merged_data1, aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Minutes Asleep Vs Total Time in Bed", 
       x = "Total Time in Bed",
       y = "Total Minutes Asleep")
#there is a positive correlation between total minutes asleep and total time in bed
#which signifies that as the total time spent in bed increases, the total minutes of sleep also tend to increase.
#his suggests good sleep hygiene - users are giving themselves enough time in bed to fall asleep 
#and stay asleep for a significant portion of that time.


#average minutes asleep per day
ggplot(average_data1, aes(x = weekday, y = avg_totalminsasleep)) +
  geom_bar(stat = "identity", fill = "orange") + 
  labs(title = "Average Minutes of Sleep per day",
       x = "Hour of Day",
       y = "Average Minutes of Sleep")
#people sleep more on Wednesdays and less on Mondays.


#total steps and total minutes asleep
ggplot(data = final_merged_data1, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Minutes Asleep Vs Total Steps", 
       x = "Total Steps",
       y = "TotalMinutesAsleep")
#there is a weak negative correlation between total minutes asleep and steps taken
#indicating that as the total steps increase, there's a tendency for total minutes of sleep to decrease (and vice versa).
#However, the data points are scattered around the trendline, signifying a weak correlation. 
#There are many instances where high step counts don't necessarily correspond with low sleep time, and vice versa.

#Weight info
#Total steps and BMI
ggplot(data = activity_weight_data, aes(x = TotalSteps, y = BMI)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Steps Vs BMI", 
       x = "Total Steps",
       y = "BMI")
#Calories and BMI
ggplot(data = activity_weight_data, aes(x = Calories, y = BMI)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Calories Burned Vs BMI", 
       x = "Calories Burned",
       y = "BMI")
#there is a  NEGATIVE relationship between total steps and BMI indicating that  
# people with lower BMIs tend to take more steps and burn more calories,
#while those with higher BMIs tend to take fewer steps and burn fewer calories. 


#More Analysis
#we need to find out why most people in this dataset spend most of their time doing nothing
#as found in the pie chart earlier, sedentary minutes has the highest proportion than others 
#average minutes asleep per day
smd <- ggplot(average_data1, aes(x = weekday, y = SedentaryMinutes)) +
  geom_bar(stat = "identity", fill = "orange") + 
  labs(title = "Sedentary Active Minutes per day",
       x = "Week Day",
       y = "Sedentary Active Mins")
vamd <- ggplot(average_data1, aes(x = weekday, y = VeryActiveMinutes)) +
  geom_bar(stat = "identity", fill = "pink") + 
  labs(title = "Very Active Minutes per day",
       x = "Week Day",
       y = "Very Active Mins")
lamd <- ggplot(average_data1, aes(x = weekday, y = LightlyActiveMinutes)) +
  geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Fairly Active Minutes per day",
       x = "Week Day",
       y = "Lightly Active Mins")
famd <- ggplot(average_data1, aes(x = weekday, y = FairlyActiveMinutes)) +
  geom_bar(stat = "identity", fill = "black") + 
  labs(title = "Fairly Active Minutes per day",
       x = "Week Day",
       y = "Fairly Active Mins")
#arrange the plots in one page
ggarrange(smd, vamd, famd, lamd + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
#there are no significant trends discovered in sedentary minutes per day compared to the other activity levels

ggplot(data = final_merged_data1, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Minutes Asleep Vs Sedentary Minutes", 
       x = "Total Minutes Asleep",
       y = "Sedentary Minutes ")
#from the scatterplot, it is shown that there is a negative correlation between the two variables.
#There are many instances where high sleep time doesn't necessarily correspond with low sedentary minutes, and vice versa.
#over variables may be needed to check why there is high sedentary minutes in the dataset such as occupation, age and gender which the data is limited to


