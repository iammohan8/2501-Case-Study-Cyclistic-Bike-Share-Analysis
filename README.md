# 🚲 Cyclistic Bike Share Analysis – Case Study 1  
**Author:** Mohan Anbazhagan  
**Date:** April 8, 2025

## Table of Contents

- [Project Overview](#project-overview)  - [Tools Used](#tools-used)  - [Skills Applied](#skills-applied)  - [Source and Deliverables](#source-and-deliverables)  - [Summary of Analysis](#summary-of-analysis)  - [Recommendations](#recommendations)  - [Key Visualizations](#key-visualizations)  - [Final Deliverables](#final-deliverables) - [Full R Markdown Code (Inline Summary)](#full-r-markdown-code-inline-summary)  - [Summary for Recruiters](#summary-for-recruiters)



## Project Overview
This project is part of the Google Data Analytics Capstone. It analyzes Cyclistic’s bike-share data to compare casual riders and annual members, aiming to provide actionable insights for increasing membership subscriptions.

---

## Tools Used
- R & RStudio  
- tidyverse (dplyr, ggplot2, lubridate, scales)  
- R Markdown  
- Google Drive (for source file storage)

---

## Skills Applied
- Data Cleaning  
- Data Wrangling  
- Data Visualization  
- Descriptive Analytics  
- Business Recommendations  
- Data-Driven Decision Making

---

## Source and Deliverables
- **Input File (.csv)**: [Download here](https://drive.google.com/drive/folders/1jGGJzeLx9-E-4s6ohIHM1cVIWDN24JSS?usp=sharing)  
- **Cleaned Output File (.csv)** – Processed in R: [Download here](https://drive.google.com/drive/folders/15mr06VAaeir2LQo6PIxK38_aNKjl6g72?usp=sharing)  
- **R Markdown File (.Rmd)**: [Download here](https://drive.google.com/drive/folders/1Jcsns2wbnbfWCxsGvWuTjUqtxXyH8gQO?usp=drive_link)  
- **R Script File (.R)**: [Download here](https://drive.google.com/drive/folders/1YiWOjrEOS_Wtqo8rUFv5n5mzGFzw1EZ9?usp=drive_link)  
- **R Plot Visualizations (.pdf)**: [Download here](https://drive.google.com/drive/folders/1EDN0EkpohjX7AtjThHsQscjSMKWGOj3z?usp=drive_link)


---

## Summary of Analysis  

- **Annual members** ride more frequently and mostly on **weekdays**.
- **Casual riders** have **longer ride durations** and prefer **weekends**.
- Casual riders heavily use **docked bikes**, whereas members use more **classic/electric bikes**.

---

## Recommendations  

1. **Incentivize frequent casual riders** with time-limited offers to convert them into members.  
2. **Promote weekend-exclusive deals** tailored for casual riders based on their preferred usage days.  
3. **Use targeted ads and personalized messages** using available demographic and usage pattern data.

---

## Key Visualizations  


- Number of Rides by Rider Type and Weekday
  

- Average Ride Duration by Weekday  
- Pie Chart: Rider Type Distribution  
- Line Graph: Avg Ride Duration Over Time  
- Boxplot: Ride Duration by Rider Type  

---

## Final Deliverables  
- Detailed Analysis using R  
- Cleaned and aggregated datasets
- 
- Visualizations showing behavioral trends  
- Business strategy recommendations  

---

## Full R Markdown Code (Inline Summary)


# Case Study R Markdown Setup


## Install and Load Packages

```{r}
install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
```

## Load and Clean Data

```{r}
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

q1_2019 <- rename(q1_2019,
  ride_id = trip_id,
  rideable_type = bikeid,
  started_at = start_time,
  ended_at = end_time,
  start_station_name = from_station_name,
  start_station_id = from_station_id,
  end_station_name = to_station_name,
  end_station_id = to_station_id,
  member_casual = usertype
)

q1_2019 <- mutate(q1_2019,
  ride_id = as.character(ride_id),
  rideable_type = as.character(rideable_type)
)

all_trips <- bind_rows(q1_2019, q1_2020)

all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, tripduration))

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
    "Subscriber" = "member",
    "Customer" = "casual"
  ))
```


## Feature Engineering

```{r}
all_trips <- all_trips %>%
  mutate(date = as.Date(started_at),
         month = format(date, "%m"),
         day = format(date, "%d"),
         year = format(date, "%Y"),
         day_of_week = format(date, "%A"),
         ride_length = as.numeric(difftime(ended_at, started_at)))

# Clean Invalid Data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0), ]



```

## Descriptive Stats

```{r}
summary(all_trips_v2$ride_length)
aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = mean)


```

## Weekly Trends

```{r}
library(lubridate)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
)


```

## Visualizations

```{r}
library(ggplot2)
library(scales)

# Rides per Day
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Rides by Rider Type and Weekday")



```

## Install and Load Packages

```{r}
install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


```

## Save Processed Data

```{r}
write.csv(all_trips_v2, "all_trips_v2.csv")
counts <- aggregate(ride_length ~ member_casual + day_of_week, data = all_trips_v2, FUN = mean)
write.csv(counts, "avg_ride_length.csv")


```

# Summary for Recruiters
This project demonstrates my ability to carry out a full data analysis cycle using R — including cleaning, transformation, visualization, and deriving business insights. I followed the six-step process: Ask, Prepare, Process, Analyze, Share, and Act. The final recommendations aim to drive actionable marketing decisions for Cyclistic's growth.


## Connect with Me
-📧 avmohaneee@gmail.com
-💼 [LinkedIn](https://www.linkedin.com/services/page/055039336401562023)
-💻 [GitHub](https://github.com/iammohan8)

