# Bellabeat Case Study - How Can a Wellness Technology Company Play It Smart?

# About The Company
Bellabeat, a health-focused tech company founded by Urška Sršen, empowers women with health data through its smart devices. Despite early success and diverse marketing channels, Sršen believes user data holds untapped growth potential. She tasks the marketing analytics team to analyze user behavior and translate those insights into high-level marketing strategies for Bellabeat.

# Scenario
As a data analyst at Bellabeat, a company revolutionizing women’s health with its smart devices, I’m tasked with delving into user data. Our goal? Unearth hidden gems within fitness data specific to our Bellabeat product. This user behavior analysis will be the key to unlocking new market growth for Bellabeat. By understanding how women currently utilize their smart devices, I can translate those insights into high-level marketing strategies, presented directly to the Bellabeat executive team. This data-driven approach will propel Bellabeat towards becoming a major force in the global smart device market.

# Business Task
* Analyze user data specifically focused on fitness metrics collected through Fitbit dataset. 
* Gain and translate insights into actionable marketing strategies for Bellabeat. 
* Presentation of findings and recommendations to the Bellabeat executive team. 
* Leverage user data to unlock new growth opportunities/market strategies and propel Bellabeat towards becoming a major player in the global smart device market.

# Data Analysis Process
I will follow the 6 steps of the data analysis process - Ask, Prepare, Process, Analyize, Share and Act.
# 1. Ask 
Sršen asks me to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart
devices. She then wants me to select one Bellabeat product to apply these insights to in my presentation. These questions will guide my analysis:
* What are some trends in smart device usage?
* How could these trends apply to Bellabeat customers?
* How could these trends help influence Bellabeat marketing strategy?

# 2. Prepare 
I am using a public dataset from Kaggle pointed out by Sršen. The FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius) that explores smart device users’ daily habits.This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty three eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and weight that can be used to explore users’ habits.

# Information about the Dataset
* The dataset is generated by respondents from a survey via Amazon Mechanical Turk between 12 March 2016 to 12 May 2016.
* 30 FitBit users consented to the submission of personal tracker data.
* Data collected includes physical activity recorded in minutes, heart rate, sleep monitoring, daily activity and steps.

# Limitations of the Data
* The data lacks crucial demographic information like age, occupation, location and gender.
* The data does not represent the entire Fitbit user base. Users who choose to share their data might have different habits compared to the general population.
* The data was collected 7 years ago in 2016. Technology and user behavior related to fitness trackers may have evolved since then.

# 3. Process
* I chose daily activities, hourly activities(hourly steps and hourly calories), sleep and weight as metrics for this analysis because they provide a more comprehensive picture of user behavior and health.
* I chose R to perform analysis because it excels at statistical analysis, which is crucial for tasks like examining correlations between sleep, activity, and weight in the Fitbit data. R also offers powerful tools for data visualization through packages like ggplot2.
* I performed Exploratary Data Analysis (EDA) in order to understand the data better and to check if all data types are consistent through out all datasets to enable efficient merging. 
* I ensured the data is clean by checking and dropping missing values and duplicates.
* I  manipulated the date column by extracting weekdays in order to analyze trends in the data in relation to days.
* I finally merged the data together and called out the summary function in order to get summary statistics of the data which provides a snapshot of the data, offering valuable insights into its central tendencies, spread, and potential issues.
  
### Loading the core library
```
# load the core library 'tidyverse'
library(tidyverse)

# read the csv files 
daily_activity <- read_csv("C:/Users/user/Documents/coursera/Fitbit Dataset/dailyActivity_merged.csv")
hourly_calories <- read_csv("C:/Users/user/Documents/coursera/Fitbit Dataset/hourlyCalories_merged2.csv")
weight_info <- read_csv("C:/Users/user/Documents/coursera/Fitbit Dataset/weightLogInfo_merged2.csv")
hourly_steps <- read_csv("C:/Users/user/Documents/coursera/Fitbit Dataset/hourlySteps_merged2.csv")
sleep_day <- read_csv("C:/Users/user/Documents/coursera/Fitbit Dataset/sleepDay_merged2.csv")
```




### Data exploration
```
#get the top 6 rows of each data
head(daily_activity)
head(hourly_calories)
head(weight_info)
head(hourly_steps)
head(sleep_day)

#get the structure of each data
str(daily_activity)
str(hourly_calories)
str(weight_info)
str(hourly_steps)
str(sleep_day)

#check for each column names across every data
colnames(daily_activity)
colnames(hourly_calories)
colnames(weight_info)
colnames(hourly_steps)
colnames(sleep_day)

#find unique user IDS computed in the dataset
n_distinct(daily_activity$Id)
n_distinct(hourly_calories$Id)
n_distinct(weight_info$Id)
n_distinct(hourly_steps$Id)
n_distinct(sleep_day$Id)
```
* There are 33 distinct users in the daily activity, hourly calories and hourly steps data
* 24 distinct users in the sleep day data 
* 8 unique users in the weight info data




### Data cleaning
```
#check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(hourly_calories))
sum(duplicated(weight_info))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_day))

#distinct sleep day data
sleep_day1 <- sleep_day %>%
  distinct(.keep_all = TRUE)
#view the data
glimpse(sleep_day1)

#find the count of missing values 
sum(is.na(daily_activity))
sum(is.na(hourly_calories))
sum(is.na(weight_info))
sum(is.na(hourly_steps))
sum(is.na(sleep_day1))

#drop the fat column
weight_info1 <- weight_info[colSums(is.na(weight_info)) == 0]
print(weight_info1)

#get summary statistics of the variables in the datasets
daily_activity %>% 
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()
hourly_calories %>% 
  select(ActivityHour, Calories) %>% 
  summary()
weight_info1 %>% 
  select(WeightPounds, BMI) %>% 
  summary()
hourly_steps %>% 
  select(ActivityHour, StepTotal) %>% 
  summary()
sleep_day1%>% 
  select(SleepDay, TotalSleepRecords, TotalMinutesAsleep,TotalTimeInBed) %>% 
  summary()
```
* There are 3 duplicates in the sleep day data so I dropped the duplicates.
* Weight data has 65 missing values from the fat column so I dropped the fat column
* The other data has no duplicates or missing values



###  Changing date Column names  
* Before merging the dataframes, we need to ensure their date columns speak the same language, meaning we will rename the date columns in all dataframes to a consistent format.
```
daily_activity1 <- daily_activity %>% 
  rename(Date=ActivityDate)
hourly_calories1 <- hourly_calories %>% 
  rename(Date=ActivityHour)
weight_info2 <- weight_info1 %>%
  rename(Date=Date)
hourly_steps1 <-  hourly_steps %>% 
  rename(Date=ActivityHour)
sleep_day2 <- sleep_day1 %>% 
  rename(Date=SleepDay)
```



### Converting Date Column
* note : no need to change the time column data type because its already in the appropriate time datatype
```
#convert the date format of daily activity
daily_activity_cleaned <- daily_activity1 %>% 
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column 
str(daily_activity_cleaned)
View(daily_activity_cleaned)

#convert the date format of hourly calories
hourly_calories_cleaned <- hourly_calories1 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column 
str(hourly_calories_cleaned)
View(hourly_calories_cleaned)

#convert the date format of weight info
weight_info_cleaned <- weight_info2 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column  
str(weight_info_cleaned)
View(weight_info_cleaned)

#convert the date format of hourly steps
hourly_steps_cleaned <- hourly_steps1 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column  
str(hourly_steps_cleaned)
View(hourly_steps_cleaned)

#convert the date format of hourly steps
sleepday_cleaned <- sleep_day2 %>%
  mutate(Date=as_date(Date,format="%m/%d/%Y"))
#confirm the format of the date column  
str(sleepday_cleaned)
View(sleepday_cleaned)
```




### Merging the dataframes 
```
#merge the daily activity and sleepday data together
daily_sleep_merged_data <- merge(daily_activity_cleaned,sleepday_cleaned, 
                                 by=c('Id','Date'))
head(daily_sleep_merged_data)
View(daily_sleep_merged_data)

#merge the hourly calories and hourly steps data
hourly_calories_steps_data <- merge(hourly_calories_cleaned,hourly_steps_cleaned, 
                                          by=c('Id','Date', 'Time'))
head(hourly_calories_steps_data)
View(hourly_calories_steps_data)

#merge the daily sleep merged data with the hourly calories steps merged data
final_merged_data <- merge(daily_sleep_merged_data,hourly_calories_steps_data, 
                                 by=c('Id','Date'))
head(final_merged_data)
View(final_merged_data)
colnames(final_merged_data)

#merge daily activities with weight info 
#i did not merge the weight info to the final dataset in order to avoid reduction of the data because only 8 distinct users gave weight information 
activity_weight_data <- merge(daily_activity_cleaned,weight_info_cleaned, 
                            by=c('Id','Date'))
head(activity_weight_data)
View(activity_weight_data)
colnames(activity_weight_data)
```



### Extract week day from the date column
```
final_merged_data$weekday <- wday(final_merged_data$Date, label=TRUE, abbr=FALSE)
View(final_merged_data)
```



### Summary statistics of the merged data
```
final_merged_data %>% 
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories.x,Calories.y,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes,
         TotalMinutesAsleep, TotalTimeInBed, StepTotal,TotalSleepRecords,weekday) %>%
  summary()
```



# 4. Analyze
### Understanding Activity Level



```
#we will first plot a pie chart to break down the different ways users were active in minutes
#get the total of each activity minutes 
total_activity_mins <- final_merged_data %>%
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
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Pie%20chart%20of%20activity%20minutes.png)

* Sedentary minutes are consistently higher than other activity minutes (very active, fairly active, and light active) in the Fitbit data, 
* It reveals a predominantly inactive lifestyle for the users generally. A high percentage of sedentary minutes indicates the user spends most of their day sitting, lying down, or engaging in activities with minimal physical exertion.





### Exploring potential relationships between variables
```
#total steps and very active minutes
ggplot(data = final_merged_data, aes(x = TotalSteps, y = VeryActiveMinutes)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Scatterplot of Total Daily Steps and Very Active Minutes",
       x = "Total daily steps",
       y = "Very active minutes")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20steps%20and%20very%20active%20minutes.png)

* The scatterplot indicates that there is a positive correlation between total steps and very active minutes.



```
#total steps and calories
ggplot(data = final_merged_data, aes(x = TotalSteps, y = Calories.x)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Scatterplot of Total Daily Steps and Daily Calories Burned", 
       x = "Total daily steps",
       y = "Daily calories burned")
```

![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20Steps%20and%20calories%20burned.png)

* There is a positive correlation between total steps and calories burned.





### Week day data
```
#total number of steps taken by day
ggplot(final_merged_data, aes(x = weekday, y = TotalSteps)) +
  geom_bar(stat = "identity", fill = "pink") + 
  labs(title = "Total number of steps taken by day",
       x = "Day of the Week",
       y = "Total Steps")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20number%20of%20steps%20by%20per%20day.png)



```
#total calories burned per day
ggplot(final_merged_data, aes(x = weekday, y = Calories.x)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(title = "Total number of calories burned by day",
       x = "Day of the Week",
       y = "Total Calories Burned")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20calories%20burned%20per%20day.png)

* People take more steps and burn more calories on Tuesday compared to other days of the week and less steps are taken,less calories are burned on Sunday and Monday.





### Hourly data 
```
#steps in hours
ggplot(final_merged_data, aes(x = Time, y = StepTotal)) +
  geom_bar(stat = "identity", fill = "purple") + 
  labs(title = "Steps taken in each hour of the day",
       x = "Hour of Day",
       y = "Total Steps")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20steps%20taken%20in%20each%20hour%20of%20the%20day.png)



```
#calories burned in hours
ggplot(final_merged_data, aes(x = Time, y = Calories.y)) +
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "Total calories burned in each hour of the day",
       x = "Hour of Day",
       y = "Total Calories Burned")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/total%20calories%20burned%20in%20each%20hour%20of%20the%20day.png)

* The bar chart shows a trend of increased activity levels, measured by steps taken and calories burned, in the evening hours around 5pm to 7pm, compared to lower activity levels in the morning from 12am to 4am.





### Sleep patterns and recovery 
```
#total time in bed vs total minutes asleep
ggplot(data = final_merged_data, aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Minutes Asleep Vs Total Time in Bed", 
       x = "Total Time in Bed",
       y = "Total Minutes Asleep")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20Minutes%20Asleep%20Vs%20Total%20Time%20in%20Bed.png)

* There is a positive correlation between total minutes asleep and total time in bed.


```
#total minutes asleep per day
ggplot(final_merged_data, aes(x = weekday, y = TotalMinutesAsleep)) +
  geom_bar(stat = "identity", fill = "orange") + 
  labs(title = "Total Minutes of Sleep per day",
       x = "Hour of Day",
       y = "Total Minutes of Sleep")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20Minutes%20of%20Sleep%20per%20day.png)

* People sleep more on Wednesdays and less on Mondays.



```
#total steps and total minutes asleep
ggplot(data = final_merged_data, aes(x = TotalSteps, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Minutes Asleep Vs Total Steps", 
       x = "Total Steps",
       y = "TotalMinutesAsleep")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20Minutes%20Asleep%20Vs%20Total%20Steps.png)
              
* There is a weak negative correlation between total minutes asleep and steps taken. 




### Weight info
```
#Total steps and BMI
ggplot(data = activity_weight_data, aes(x = TotalSteps, y = BMI)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Steps Vs BMI", 
       x = "Total Steps",
       y = "BMI")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20Steps%20Vs%20BMI.png)



```
#Calories burned and BMI
ggplot(data = activity_weight_data, aes(x = Calories, y = BMI)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Calories Burned Vs BMI", 
       x = "Calories Burned",
       y = "BMI")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Calories%20burned%20vs%20BMI.png)

* There is a  negative relationship between total steps/ calories burned. 




### More Analysis
* As found in the pie chart earlier, sedentary minutes has the highest proportion than others, I was interested in finding out why that is, so i analyzed sedentary minutes in relation to other variables.
```
#average minutes asleep per day
smd <- ggplot(final_merged_data, aes(x = weekday, y = SedentaryMinutes)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Sedentary Active Minutes per day",
       x = "Week Day",
       y = "Sedentary Active Mins")
vamd <- ggplot(final_merged_data, aes(x = weekday, y = VeryActiveMinutes)) +
  geom_bar(stat = "identity", fill = "pink") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Very Active Minutes per day",
       x = "Week Day",
       y = "Very Active Mins")
lamd <- ggplot(final_merged_data, aes(x = weekday, y = LightlyActiveMinutes)) +
  geom_bar(stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Fairly Active Minutes per day",
       x = "Week Day",
       y = "Lightly Active Mins")
famd <- ggplot(final_merged_data, aes(x = weekday, y = FairlyActiveMinutes)) +
  geom_bar(stat = "identity", fill = "black") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Fairly Active Minutes per day",
       x = "Week Day",
       y = "Fairly Active Mins")
#arrange the plots in one page
#load the ggpubr library
library(ggpubr)
ggarrange(smd, vamd, famd, lamd + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Sedentary%20Minutes%20in%20relation%20to%20other%20activity%20minutes.png)

* There are no significant trends discovered in sedentary minutes per day compared to the other activity levels.



```
#total minutes asleep and sedentary minutes
ggplot(data = final_merged_data, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Total Minutes Asleep Vs Sedentary Minutes", 
       x = "Total Minutes Asleep",
       y = "Sedentary Minutes ")
```
![image](https://github.com/domeru369/Bellabeat-project/blob/main/Visualizations/Total%20Minutes%20Asleep%20Vs%20Sedentary%20Minutes.png)

* There is a negative correlation between the total minutes asleep and sedentary minutes.



# 5. Share
## Key Findings 
### a) Data Completeness:
* Not all users tracked sleep and weight consistently. There is more complete data for daily activity and hourly steps (914 records, 33 users) compared to sleep (414 records, 24 users) and weight (68 records, 8 users).

### b) Sleep Analysis:
* Users slept an average of 6 hours 59 minutes (419.2 minutes) and spent an average of 7 hours 28 minutes (448.5 minutes) in bed.
* There's a positive correlation between total sleep time and total time in bed, indicating good sleep hygiene for many users.
* Wednesday nights showed the highest average sleep duration, while Mondays had the lowest.
* There's a weak negative correlation between sleep duration and total steps, suggesting a slight tendency for people who sleep more to take fewer steps on average (and vice versa). However, the effect is weak, and other factors likely play a bigger role.
  
### c) Activity Analysis:
* There's a weak positive correlation between total steps and both very active minutes and average intensity, indicating that users who take more steps tend to engage in slightly more intense activity.
* There's a positive correlation between total steps and calories burned, as expected with increased activity leading to higher calorie expenditure.
* Tuesdays showed the highest average steps and calorie expenditure compared to other days, with Sundays and Mondays having the lowest.
* People tend to be more active in the evening (5pm-6pm) compared to mornings (12am-7am).

### d) Body Composition:
* There's a negative relationship between total steps/calories burned and BMI. This suggests a trend where people with lower BMIs tend to be more active, while those with higher BMIs tend to be less active.

### e) Sedentary Behavior:
* Sedentary minutes are consistently higher than other activity minutes, indicating a significant portion of the day is spent inactive.
* There's a weak negative correlation between sleep duration and sedentary minutes, but it's not a strong relationship. Further investigation into factors like occupation, age, and gender might be needed to understand high sedentary time.


# 6. Act 
## Recommendations
* Highlight the Sleep Benefits of Activity: Marketing materials can emphasize how increased activity can contribute to better sleep, which is crucial for overall health and well-being.
* Promote Evening Activity Routines: Encourage users to incorporate short bursts of activity into their evenings (outside of the typical 5pm-6pm peak) to help manage weight and improve overall health.
* Focus on Holistic Health: Move beyond just steps and emphasize the importance of a balanced lifestyle that includes good sleep, a healthy diet, and regular physical activity of varying intensities.
* Develop Educational Content: Offer informative content within the app or through other channels to educate users on the connection between sleep, activity, weight management, and overall well-being.
* Target Specific User Groups: Consider segmenting your marketing efforts based on user data (e.g., sleep duration, activity levels) to provide more tailored recommendations and support healthier habits.
* Address Sedentary Behavior: Develop strategies to help users reduce sedentary time throughout the day. This could involve reminders to get up and move, challenges that incorporate short activity breaks, or promoting specific features within the app that address inactivity.
By leveraging these insights and recommendations, Bellabeat can develop a more comprehensive marketing strategy that empowers women to take control of their health and well-being through a holistic approach that considers sleep, activity, and overall lifestyle habits.







  
  
  
  
