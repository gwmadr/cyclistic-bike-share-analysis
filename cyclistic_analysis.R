#structure of dataframe.
str(Divvy_Trips_2019_Q1)
#summary of dataframe.
summary(Divvy_Trips_2019_Q1)
#first few rows
head(Divvy_Trips_2019_Q1)

#structure
str(Divvy_Trips_2020_Q1)
#summary
summary(Divvy_Trips_2020_Q1)
#first few rows
head(Divvy_Trips_2020_Q1)


install.packages("tidyverse")
install.packages("janitor")

library(tidyverse)
library(janitor)

#bind_rows from dplyr
all_trips <- bind_rows(Divvy_Trips_2019_Q1, Divvy_Trips_2020_Q1)

#new structure
str(all_trips)
#new number of rows
nrow(all_trips)

#add columns
all_trips <- all_trips %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = weekdays(started_at),
    month = format(started_at, "%B")
  )

#look new columns
glimpse(all_trips)

summary(all_trips)

#removal
all_trips_clean <- all_trips %>%
  filter(
    !is.na(start_station_name), #not na
    !is.na(end_station_name), #not na 
    ride_length > 0 #positive
  )

#after removal
nrow(all_trips_clean)
nrow(all_trips) #compare to OG

#na test
summary(all_trips_clean)

#casual column
table(all_trips_clean$member_casual)

#or

all_trips_clean %>%
  count(member_casual)

#avg ride by user
all_trips_clean %>%
  group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length, na.rm = TRUE),
            mean_ride_length = mean(ride_length, na.rm = TRUE))

#week patterns
all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop')

#sorting
day_summary <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(member_casual, desc(number_of_rides)) 

#sorted
print(day_summary, n = 14)

#bike preference
bike_preference <- all_trips_clean %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(member_casual, desc(number_of_rides))

print (bike_preference)

#bike pref with percentages
bike_preference_percent <- all_trips_clean %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  group_by(member_casual) %>%
  mutate(percentage = round(number_of_rides / sum(number_of_rides) * 100, 1)) %>%
  arrange(member_casual, desc(number_of_rides))

print(bike_preference_percent)

#unique values in type column
unique(all_trips_clean$rideable_type)

avg_ride_length <- all_trips_clean %>%
  group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length))

#barchart
ggplot(avg_ride_length, aes(x = member_casual, y = average_ride_length, fill = member_casual)) +
  geom_col() +
  labs(title = "Casual Riders Take Longer Trips",
       subtitle = "Average ride length by user type",
       x = "User Type",
       y = "Average Ride Length (Minutes)",
       fill = "User Type") +
  theme_minimal()

#daily plot
weekday_plot <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Casual Riders Peak on Weekends",
       subtitle = "Ride volume by day of week",
       x = "Day of Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#display
weekday_plot

#seasonal trend chart
seasonal_trends <- all_trips_clean %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = month, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Seasonal Trends: Casual Ridership Surges in Summer",
       subtitle = "Ride volume by month",
       x = "Month",
       y = "Number of Rides",
       color = "User Type") +
  theme_minimal () +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seasonal_trends

#simple
bike_chart <- all_trips_clean %>%
  count(rideable_type) %>%
  ggplot(aes(x = rideable_type, y = n, fill = rideable_type)) +
  geom_col() +
  labs(title = "Fleet CompositionL 100% Classic Bikes",
       subtitle = "During analyzed period (2019-2020 Q1)",
       x = "Bike Type",
       y = "Number of Rides") +
  theme_minimal()

bike_chart

#maybethisisbetter
seasonal_trends <- all_trips_clean %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = month, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seasonal Trends: Both Groups Ride More in Warmer Months",
       subtitle = "Ride volume shows clear seasonal patterns",
       x = "Month",
       y = "Number of Rides",
       color = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seasonal_trends

#monthorderfix
month_order <- c("January", "February", "March")

seasonal_trends <- all_trips_clean %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(month = factor(month, levels = month_order)) %>%
  ggplot(aes(x = month, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Seasonal Trends: Ride Volume Increase Through Q1",
       subtitle = "Both groups show growth from January to March",
       x = "Month",
       y = "Number of Rides",
       color = "User Type") +
  theme_minimal () +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

seasonal_trends