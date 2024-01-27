library("tidyverse")
library("janitor")
library("ggpubr")
library("here")
library("skimr")
library("lubridate")
library("ggrepel")
library(sqldf)


head(dailyAct)
str(dailyAct)
head(dailyCal)
str(dailyCal)
head(dailySteps)
str(dailySteps)
head(hourlyCal)
str(hourlyCal)
head(hrps)
str(hrps)
head(sleep)
str(sleep)
head(stepsHr)
str(stepsHr)
head(weightLog)
str(weightLog)

n_distinct(dailyAct$Id)
n_distinct(dailyCal$Id)
n_distinct(dailySteps$Id)
n_distinct(hourlyCal$Id)
n_distinct(hrps$Id)
n_distinct(sleep$Id)
n_distinct(stepsHr$Id)
n_distinct(weightLog$Id)

sum(duplicated(dailyAct))
sum(duplicated(dailyCal))
sum(duplicated(dailySteps))
sum(duplicated(hourlyCal))
sum(duplicated(hrps))
sum(duplicated(sleep))
sum(duplicated(stepsHr))
sum(duplicated(weightLog))

sleep<- sleep %>% 
  distinct() %>% 
  drop_na()

colnames(dailyCal)
clean_names(dailyCal)
colnames(dailyAct)
dailyCal<- rename_with(dailyCal, tolower)

dailyAct1<- sqldf("select activitydate from dailyAct")
view(dailyAct1)
remove(dailyAct1)

dailyAct<- dailyAct %>% 
  rename(date=date) %>% 
  mutate(date=as_date(date, format= "%m/%d/%Y"))
remove(dailyAc)

dailyAct <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
view(dailyAct)
colnames(dailyAct)
dailyAct<-rename_with(dailyAct, tolower)            
colnames(dailyAct)
dailyAct<- dailyAct %>% 
  rename(date=activitydate)
view(dailyAct)

remove(dailyAct)

class(dailyAct$date)
dailyAct$date<- mdy(dailyAct$date)
class(dailyAct$date)
view(dailyAct)

dailyAct<- dailyAct %>% 
  mutate(day=format(dailyAct$date, format= "%a"))
view(dailyAct)

############################################################################################################################################################################

dailyCal <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
view(dailyCal)

str(dailyCal)
colnames(dailyCal)
view(dailyCal)
class(dailyCal$day)

dailyCal<- dailyCal %>% 
rename(day=activityday)
dailyCal$day<-mdy(dailyCal$day)

dailyCal<- dailyCal %>% 
  mutate(day = format(day,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

colnames(dailySteps)
class(dailySteps$ActivityDay)

dailySteps<- dailySteps %>% 
  rename(date = ActivityDay) %>% 
  rename_with(tolower)
dailySteps$date<- mdy(dailySteps$date)
class(dailySteps$date)

dailySteps<- dailySteps %>% 
  mutate(date = format(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
view(dailySteps)  


############################################################################################################################################################################
remove(hrps)

hrps <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

view(hrps)
head(hrps,10)
colnames(hrps)
hrps<-rename_with(hrps,tolower)
class(hrps$time)


hrps[c('date', 'time')] <- str_split_fixed(hrps$time, ' ', 2)
hrps$time<-lubridate::hms(hrps$time)
hrps$date<-lubridate::mdy(hrps$date)

############################################################################################################################################################################

head(sleep)
sleep<- rename_with(sleep,tolower) 
clean_names(sleep)
sleep<- rename(sleep, sleep_date=sleepday)
sleep<- rename(sleep, sleep_record=totalsleeprecords, total_min_sleep=totalminutesasleep,time_in_bed=totaltimeinbed)
sleep$sleep_date<-lubridate::mdy_hms(sleep$sleep_date)
class(sleep$sleep_record)

############################################################################################################################################################################
remove(stepsHr)

stepsHr <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

head(stepsHr,40)

stepsHr<-rename_with(stepsHr,tolower)

stepsHr$activityhour<-lubridate::mdy_hms(stepsHr$activityhour)

stepsHr[c('date', 'time')] <- str_split_fixed(stepsHr$activityhour, ' ', 2)

class(stepsHr$activityhour)
stepsHr$time<-lubridate::hms(stepsHr$time)
stepsHr$date<-lubridate::mdy(stepsHr$date)

remove(stepsHr1)

############################################################################################################################################################################

head(weightLog)

weightLog<-rename_with(weightLog,tolower)

weightLog$activityhour<-lubridate::mdy_hms(weightLog$date)

weightLog[c('date', 'time')] <- str_split_fixed(weightLog$date, ' ', 2)

class(weightLog$date)
weightLog$time<-lubridate::hms(weightLog$time)
weightLog$date<-lubridate::mdy(weightLog$date)
weightLog<- weightLog[,-4]

class(weightLog$ismanualreport)

############################################################################################################################################################################
#TOTAL USAGE OF THE BAND 


head(dailyAct)
colnames(dailyAct)
totalUsage<-dailyAct %>% 
group_by(id) %>% 
summarise(days_used=sum(n())) 
head(totalUsage)

%>% 
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))
head(totalUsage)

############################################################################################################################################################################
#mean values

colnames(dailyAct)
head(sleep)

daily_mean<- dailyAct %>% 
  group_by(id) %>% 
  summarise(mean_steps=mean(totalsteps), mean_distance=mean(totaldistance), mean_calories= mean(calories))

head(daily_mean)

mean_sleep<-sleep %>% 
  group_by(id) %>% 
summarise(mean_sleep_minute=mean(total_min_sleep))
head(mean_sleep)

daily_mean<-merge(x=daily_mean,y=mean_sleep,by="id")
head(daily_mean)
head(sleep,10)

#mean_sl=sqldf("select avg(total_min_sleep) as sum from sleep where id=1503960366")
#head(mean_sl)

head(dailySteps)
max_steps<- sqldf("select max(StepTotal) as max_step,Id,ActivityDay from dailySteps")
head(max_steps)
total_steps_per_id<- sqldf("
                           select Id, sum(StepTotal) as total_step 
                           from dailySteps
                           group by Id
                           ")
head(total_steps_per_id)

mean_steps_per_id<- sqldf("
                           select Id, avg(StepTotal) as total_step 
                           from dailySteps
                           group by Id
                           ")
head(mean_steps_per_id)
rename_with(mean_steps_per_id,tolower)

daily_mean<- merge(x=daily_mean,y=mean_steps_per_id, by="id")




view(mean_steps_per_id)

install.packages("powerjoin")
library(powerjoin)
power_full_join(daily_mean, mean_steps_per_id, by = "id")



df5<- merge(daily_mean,mean_steps_per_id, by="id")
head(daily_mean)
head(mean_steps_per_id)

mean_steps_per_id<-rename_with(mean_steps_per_id,tolower)
head(df5)
daily_mean<-df5
head(daily_mean)
view(daily_mean)
daily_mean<- daily_mean %>% 
  rename(total_mean_steps = total_step)
head(daily_mean)



############################################################################################################################################################################
#Activity level

daily_mean<- daily_mean %>% 
  mutate(user_type = case_when(
    total_mean_steps < 5000 ~ "sedentary",
    total_mean_steps >= 5000 & total_mean_steps < 7499 ~ "lightly active", 
    total_mean_steps >= 7500 & total_mean_steps < 9999 ~ "fairly active", 
    total_mean_steps >= 10000 ~ "very active"
  ))
head(daily_mean)


############################################################################################################################################################################
#related to sleep


head(sleep)

sleep1<-sleep

sleep1[c("date")]<-str_split_fixed(sleep1$sleep_date,' ',1)
sleep1<- sleep1[,-2]
head(sleep1)
sleep1$date<-lubridate::ymd(sleep1$date)

sleep<-sleep1
head(sleep)

remove(sleep1)

total_sleep<- sleep %>% 
  group_by(id) %>% 
  summarise(total_sleep=sum(total_min_sleep))
head(total_sleep,10)



############################################################################################################################################################################
#active time
colnames(dailyAct)
perDay_activi_min<- dailyAct %>% 
  group_by(id) %>% 
  summarise(id,date,tota_act_min = lightlyactiveminutes+sedentaryminutes+veryactiveminutes+fairlyactiveminutes)

head(perDay_activi_min)

head(dailyAct)

total_activi_min<-perDay_activi_min %>% 
  group_by(id) %>% 
  summarise(total_act = sum(tota_act_min))
head(total_activi_min)

head(total_activi_min)

head(total_sleep)


total_sleep_acti<- merge(total_activi_min,total_sleep, by="id")
total_sleep_acti<-merge(total_sleep_acti, totalUsage, by="id")
head(total_sleep_acti)

head(daily_mean)

write.csv(daily_mean,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/daily_mean2.csv", row.names = FALSE)

write.csv(total_sleep_acti,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/total_sleep_acti.csv", row.names = FALSE)
write.csv(total_steps_per_id,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/total_steps_per_id.csv", row.names = FALSE)
write.csv(perDay_activi_min,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/perDay_activi_min.csv", row.names = FALSE)
write.csv(weightLog,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/weightLog.csv", row.names = FALSE)
write.csv(hrps,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/hrps.csv", row.names = FALSE)

write.csv(hourlyCal,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/hourlyCal.csv", row.names = FALSE)
write.csv(dailyCal,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/dailyCal.csv", row.names = FALSE)
write.csv(dailySteps,"C:/Users/sm200/Desktop/Data Analysis project/google data analytics/excersciseFitBit/data/dailySteps.csv", row.names = FALSE)



remove(dailyCal)




######################################################################################################################################################################################################
head(dailySteps)

dailySteps<- dailySteps %>% 
  mutate(user_type = case_when(
    steptotal < 5000 ~ "sedentary",
    steptotal >= 5000 & steptotal < 7499 ~ "lightly active", 
    steptotal >= 7500 & steptotal < 9999 ~ "fairly active", 
    steptotal >= 10000 ~ "very active"
  ))
df<- dplyr::count(dailySteps,user_type,sort = TRUE)
 dplyr::count(dailySteps,id,sort = FALSE)
idCount<- nrow(dailySteps)
head(df)
categ_per<- (df$n/idCount)*100
print(categ_per[2])
    class(categ_per)
categ_per_roun<- ceiling(categ_per[2])






dailyCal <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
head(dailyCal)

dailyCal<- dailyCal %>% 
  rename(day=ActivityDay)
dailyCal$day<-mdy(dailyCal$day)



head(hourlyCal)
hourlyCal[c('date', 'time')] <- str_split_fixed(hourlyCal$ActivityHour, ' ', 2)
hourlyCal<-hourlyCal[,-2]
hourlyCal$date<-mdy(hourlyCal$date)
hourlyCal$time<-hms(hourlyCal$time)

remove(dailySteps)
dailySteps <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
head(dailySteps)

dailySteps[c('date', 'time')] <- str_split_fixed(dailySteps$date, ' ', 2)
dailySteps<-dailySteps[,-2]
dailySteps$date<-mdy(dailySteps$ActivityDay)
dailySteps$time<-hms(dailySteps$time)


typeof(hourlyCal$time)


remove(hourlyCal)

hourlyCal <- read_csv("C:/Users/sm200/Desktop/Data Analysis project/google data analytics/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
head(hourlyCal)













