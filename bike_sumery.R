library(tidyverse)

Divvy_Trips_2019_Q1$trip_id <- as.character(Divvy_Trips_2019_Q1$trip_id)
Divvy_Trips_2020_Q1$ride_id <- as.character(Divvy_Trips_2020_Q1$ride_id)

# I noties that that the rideable_type column is just saying that they ride bikes so I deleted the column. 

Divvy_Trips_2020_Q1$rideable_type <- NULL
# I plan to merge the data_frame so I change the column names so they can agree once they are merge.--------------  
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "trip_id"] <- "ride_id"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "start_time"] <- "started_at"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "end_time"] <- "ended_at"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "from_station_id"] <- "start_station_id"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "from_station_name"] <- "start_station_name"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1)== "to_station_name"]<- "end_station_name"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "to_station_id"]<- "end_station_id"
names(Divvy_Trips_2019_Q1)[names(Divvy_Trips_2019_Q1) == "usertype"]<- "member_casual"


# This code change the all of Subcribers and Customer to member and casual.
Divvy_Trips_2019_Q1$member_casual[Divvy_Trips_2019_Q1$member_casual == "Subscriber"] <- "member"
Divvy_Trips_2019_Q1$member_casual[Divvy_Trips_2019_Q1$member_casual == "Customer"] <- "casual"

# Time to check if I have any misspelled Staring stations
unique(Divvy_Trips_2019_Q1$start_station_name)
unique(Divvy_Trips_2020_Q1$start_station_name)

# I made a new data frame called station_coordinate with start_station_name, start_lat, and start_lng
station_coordinates <- Divvy_Trips_2020_Q1 %>%
  select(start_station_name, start_lat, start_lng) %>%
  distinct()
#This line of code performs a left join to merge coordinate information into 2019 ride data, matching by start_station_name.
Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>%
  left_join(station_coordinates, by = "start_station_name", suffix = c("", "_ref"))

#I will be repeating the process from above for the ending coordinates.------------------------------------------
unique(Divvy_Trips_2019_Q1$start_station_name)
unique(Divvy_Trips_2020_Q1$start_station_name)

station_coordinates_ending <- Divvy_Trips_2020_Q1 %>%
  select(end_station_name, end_lat, end_lng) %>%
  distinct()

Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>%
  left_join(station_coordinates_ending, by = "end_station_name", suffix = c("", "_ref"))
#Now we merge the data frames-----------------------------------------------------------------------------------
#This one is too long do blind_rows function. 
joined_divvy_trips_1 <- merge(Divvy_Trips_2019_Q1,Divvy_Trips_2020_Q1, by.x = c("ride_id", "started_at","ended_at","start_station_name", "start_station_id","end_station_name","end_station_id","start_lat","start_lng","end_lat","end_lng", "member_casual" ),
                          by.y = c("ride_id", "started_at","ended_at","start_station_name", "start_station_id","end_station_name","end_station_id","start_lat","start_lng","end_lat","end_lng", "member_casual" ),all.x = TRUE,all.y = TRUE)

# for bind_rows I need to change ride_id. One is in (double) and the other is in (character) 

library(dplyr)

Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>%
  mutate(ride_id = as.character(ride_id))

Divvy_Trips_2020_Q1 <- Divvy_Trips_2020_Q1 %>%
  mutate(ride_id = as.character(ride_id))

# Now bind_rows will work
all_trips <- bind_rows(Divvy_Trips_2019_Q1, Divvy_Trips_2020_Q1)


joined_divvy_trips_2 <- bind_rows(Divvy_Trips_2019_Q1, Divvy_Trips_2020_Q1) #this one is best

#checking how the two are different.----------------------------------------------------------------------------
setdiff(names(joined_divvy_trips_2), names(joined_divvy_trips_1))
str(joined_divvy_trips_1)
str(joined_divvy_trips_2)







#graphing code-------------------------------------------------------------------------------------------
  # Ride counts by user type------
ggplot(joined_divvy_trips_2, aes(x = member_casual)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Ride Counts by User Type", x = "User Type", y = "Number of Rides") +
  theme_minimal()

summary_df <- joined_divvy_trips_2 %>%
  count(member_casual) %>% 
  mutate(percent = n/sum(n) * 100)

ggplot(summary_df, aes(x = member_casual, y = percent, fill = member_casual)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            vjust = -0.5, size = 5) +
  labs(title = "Percentage of Rides by User Type",
       x = "User Type", y = "Percent") +
  theme_minimal()


#Graphs bases on distance averages--------------
#install.packages("geosphere")
library(geosphere)

library(dplyr)
library(geosphere)

# Step 1: Clean and validate coordinates

cleaned_trips <- joined_divvy_trips_2 %>%
  mutate(
    start_lng = ifelse(start_lng > 180, start_lng - 360, start_lng),
    end_lng   = ifelse(end_lng > 180, end_lng - 360, end_lng)
  ) %>%
  filter(
    !is.na(start_lat) & !is.na(start_lng) &
      !is.na(end_lat)   & !is.na(end_lng),
    between(start_lat, -90, 90),
    between(start_lng, -180, 180),
    between(end_lat, -90, 90),
    between(end_lng, -180, 180)
  )


# Step 2: Calculate distance in meters
joined_divvy_trips_2$distance_m <- distHaversine(
  cbind(joined_divvy_trips_2$start_lng, joined_divvy_trips_2$start_lat),
  cbind(joined_divvy_trips_2$end_lng, joined_divvy_trips_2$end_lat)
)

# Step 3: Convert to kilometers
joined_divvy_trips_2$distance_km <- joined_divvy_trips_2$distance_m / 1000

  -------------------
#checking for invaled information
summary(joined_divvy_trips_2$start_lat)
summary(joined_divvy_trips_2$start_lng)
summary(joined_divvy_trips_2$end_lat)
summary(joined_divvy_trips_2$end_lng)




# Now safely calculate distance
cleaned_trips$distance_m <- distHaversine(
  cbind(cleaned_trips$start_lng, cleaned_trips$start_lat),
  cbind(cleaned_trips$end_lng, cleaned_trips$end_lat)
)
joined_divvy_trips_2$distance_m <- distHaversine(
  cbind(joined_divvy_trips_2$start_lng, joined_divvy_trips_2$start_lat),
  cbind(joined_divvy_trips_2$end_lng,   joined_divvy_trips_2$end_lat)
)


cleaned_trips$distance_km <- cleaned_trips$distance_m / 1000

#Calculate average distance by user type.
avg_distance_by_type <- cleaned_trips %>%
  group_by(member_casual) %>%
  summarise(
    avg_distance_km = mean(distance_km, na.rm = TRUE),
    count = n()
  )
#Taking look at the trip duration-------------------------------------

library(dplyr)
# We only have about 56% of trip durations so we need to filter out all of the NA's

valid_trips <- joined_divvy_trips_2 %>%
  filter(!is.na(tripduration) & tripduration > 0)

# Check how NA is distributed between groups
table(joined_divvy_trips_2$member_casual, is.na(joined_divvy_trips_2$tripduration))

library(dplyr)

summary_df <- valid_trips %>%
  group_by(member_casual) %>%
  summarise(avg_duration_min = mean(tripduration /60))

library(ggplot2)
ggplot(summary_df, aes(x = member_casual, y = avg_duration_min, fill = member_casual)) +
  geom_col() +
  labs(title = "Average Trip Duration by User Type",
       x = "User Type", y = "Average Duration (Minutes)") +
  theme_minimal()





 

