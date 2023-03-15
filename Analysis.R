library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("E:/Coursera/Google Data Analytics Professional Certificate/C8 - Google Data Analytics Capstone/Data2")
getwd()

#collecting data

q2_2019 <- read_csv("2019_Q2.csv")
q3_2019 <- read_csv("2019_Q3.csv")
q4_2019 <- read_csv("2019_Q4.csv")
q1_2020 <- read_csv("2020_Q1.csv")

#data wrangling and combining into a single file
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

q2_2019 <- rename(q2_2019,
                   ride_id="01 - Rental Details Rental ID",
                   rideable_type="01 - Rental Details Bike ID",
                   started_at="01 - Rental Details Local Start Time",
                   ended_at="01 - Rental Details Local End Time",
                   start_station_name = "03 - Rental Start Station Name",
                   start_station_id = "03 - Rental Start Station ID",
                   end_station_name = "02 - Rental End Station Name",
                   end_station_id = "02 - Rental End Station ID",
                   member_casual = "User Type")

q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)
                  
q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)


#inspecting data frames and looking for inconsistencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#converting ride_id and rideable_type to character
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),
                  rideable_type=as.character(ride_id))

q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))


#joining data
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#removing lat, long, birthyear, and gender fields as they were dropped in 2020
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

#cleaning data for analysis
colnames(all_trips)
nrow(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature

all_trips$member_casual

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

#checking column structure
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


#getting rid of bad data
#The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative


# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2<-all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
head(all_trips_v2)

#descriptive analysis
#ride length
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)


#comparing members and casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday)


# Let's visualize the average ride duration by rider type for each day
all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position="dodge")

# Analyzing ridership data by type and month
all_trips_v2 %>% 
  mutate(month=month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, month)

#Let's plot it in a line chart.
all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, color=member_casual, group=member_casual))+
  geom_line()

# Let's visualize the average ride duration by rider type for each month.
all_trips_v2 %>% 
  mutate(month=month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=average_duration, fill=member_casual))+
  geom_col(position="dodge")

#Let's visualize a line chart as well.
all_trips_v2 %>% 
  mutate(month=month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=average_duration, color=member_casual, group=member_casual))+
  geom_line()

#exporting summary
analysis<-aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual+all_trips_v2$day_of_week, FUN=mean)

write.csv(analysis, file = 'E:/Coursera/Google Data Analytics Professional Certificate/C8 - Google Data Analytics Capstone/Data2/avg_ride_length.csv')

