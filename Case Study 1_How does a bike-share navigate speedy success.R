install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
colnames(q1_2019)
colnames(q1_2020)
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

colnames(q1_2019)
colnames(q1_2020)

str(q1_2019)
str(q1_2020)

q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

(all_trips$member_casual)

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

(all_trips$ride_length)

factor(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

(all_trips_v2$ride_length)

mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # creates weekday field using wday()
  group_by(member_casual, weekday) %>%  # groups by usertype and weekday
  summarise(
    number_of_rides = n(),              # calculates the number of rides
    average_duration = mean(ride_length) # calculates the average duration
  ) %>%
  arrange(member_casual, weekday)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


library(scales)  # Make sure you load this for the comma function

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length, na.rm = TRUE)
  ) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +  # <-- This fixes the 1e+04 format
  labs(
    title = "Number of Rides by Rider Type and Weekday",
    x = "Day of the Week",
    y = "Number of Rides",
    fill = "Rider Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),  # Larger title
    axis.title.x = element_text(size = 14),  # x-axis title size
    axis.title.y = element_text(size = 14)   # y-axis title size
  )

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  ) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Ride Duration by Rider Type and Weekday",  # Header title
    x = "Day of the Week",   # x-axis label
    y = "Average Duration (Minutes)",  # y-axis label
    fill = "Rider Type"      # Fill legend label
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),  # Set title size to 24 and bold
    axis.title.x = element_text(size = 14),  # x-axis title size
    axis.title.y = element_text(size = 14)   # y-axis title size
  )


counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

write.csv(all_trips_v2, file = 'all_trips_v2.csv')

library(ggplot2)
library(scales)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +  # This formats the y-axis labels with commas
  labs(
    title = "Comparison of Ride Frequency by Rider Type",
    x = "Rider Type",
    y = "Number of Rides"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )


library(ggplot2)
library(scales)

all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = "", y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_y_continuous(labels = scales::comma) +  # Formats the y-axis labels with commas
  labs(
    title = "Distribution of Rides by Rider Type",
    x = "",
    y = "Number of Rides"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )

library(ggplot2)
library(scales)

all_trips_v2 %>%
  mutate(month = floor_date(started_at, "month")) %>%
  group_by(month) %>%
  summarise(average_duration = mean(ride_length, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = average_duration)) +
  geom_line(color = "blue", size = 1) +
  scale_y_continuous(labels = scales::comma) +  # Formats the y-axis labels with commas
  labs(
    title = "Average Ride Duration Over Time",
    x = "Month",
    y = "Average Ride Duration (Minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )

library(ggplot2)
library(scales)

all_trips_v2 %>%
  ggplot(aes(x = member_casual, y = ride_length, fill = member_casual)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +  # Formats the y-axis labels with commas
  labs(
    title = "Ride Duration by Rider Type",
    x = "Rider Type",
    y = "Ride Duration (Minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )












