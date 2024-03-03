library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
colnames(q1_2019)
colnames(q1_2020)

#rename col names to match 
q1_2019 <- rename(q1_2019, ride_id=trip_id, rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype)
str(q1_2019)
str(q1_2020)
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
all_trips <- bind_rows(q1_2019, q1_2020)
View(all_trips)''

#remove start_lat, start_lng, end_lat, end_lng, birthyear, genders, tripdruration from all_trips *not in 2020 data
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  tripduration))
head(all_trips)
colnames(all_trips)

# replace "Subscriber" with "member" and "Customer" with "casual"
table(all_trips$member_casual)
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))
table(all_trips$member_casual)
View(all_trips)

# add YYYY-mm-dd and "day_of_week" columns
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#add "ride_length" using difftime 
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
View(all_trips)

#change ride_length from Factor to Numeric (removes "secs" from data)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#remove negatives and 0s in ride_length column and "HQ QR" from start_station_name column
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#calculate mean, min, median and max of ride_length median is optional **ride_lentgh in secs)
mean(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
#OR USE
summary(all_trips_v2$ride_length)

#compare data members and casual
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

#calculate average ride time of members vs casual by day
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#calculate number or rides and average_duration
all_trips_v2 %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length))
##by weekdays
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarize(number_of_rides = n()        
            ,average_duration = mean(ride_length)) %>%          
  arrange(member_casual, weekday)

#visualizations bar chart (number of rides by weekday)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma)

#visualizations bar chart (average duration by weekday)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Lolipop chart number of rides by weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x=weekday, y=number_of_rides)) +
  geom_point(size=3) +
  geom_segment(aes(x=weekday, 
                   xend=weekday, 
                   y=0, 
                   yend=number_of_rides)) +
  scale_y_continuous(labels = scales::label_comma(accuracy=1)) +
  labs(title="Number of rides per day", 
       subtitle="Member vs Casual riders", 
       caption="lower point is member, higher point is casual") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) 

#Lolipop Chart avrg durations by weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x=weekday, y=average_duration)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=weekday, 
                   xend=weekday, 
                   y=0, 
                   yend=average_duration)) + 
  labs(title="Average duration of rides per day in seconds", 
       subtitle="Member vs Casual riders", 
       caption=("lower point is member, higher point is casual")) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
mean(ride_length)
mean(all_trips_v2$ride_length)
