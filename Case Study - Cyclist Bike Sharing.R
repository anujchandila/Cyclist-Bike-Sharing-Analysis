# Set up the R environment
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization

install.packages("tidyverse")
library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(janitor) #helps data exploration and cleaning
library(ggplot2) #helps visualize data
setwd("~/Downloads/Divvy- tripdata/trip_data-2020-2021") #sets working directory



# Step 1: Collect Data

# Uploading Divvy trip datasets (.csv files)
Trips_Apr20 <- read_csv('202004-divvy-tripdata.csv')
Trips_May20 <- read_csv('202005-divvy-tripdata.csv')
Trips_June20 <- read_csv('202006-divvy-tripdata.csv')
Trips_July20 <- read_csv('202007-divvy-tripdata.csv')
Trips_Aug20 <- read_csv('202008-divvy-tripdata.csv')
Trips_Sep20 <- read_csv('202009-divvy-tripdata.csv')
Trips_Oct20 <- read_csv('202010-divvy-tripdata.csv')
Trips_Nov20 <- read_csv('202011-divvy-tripdata.csv')
Trips_Dec20 <- read_csv('202012-divvy-tripdata.csv')
Trips_Jan21 <- read_csv('202101-divvy-tripdata.csv')
Trips_Feb21 <- read_csv('202102-divvy-tripdata.csv')
Trips_Mar21 <- read_csv('202103-divvy-tripdata.csv')
Trips_Apr21 <- read_csv('202104-divvy-tripdata.csv')
Trips_May21 <- read_csv('202105-divvy-tripdata.csv')
Trips_June21 <- read_csv('202106-divvy-tripdata.csv')
Trips_July21 <- read_csv('202107-divvy-tripdata.csv')
Trips_Aug21 <- read_csv('202108-divvy-tripdata.csv')
Trips_Sep21 <- read_csv('202109-divvy-tripdata.csv')
Trips_Oct21 <- read_csv('202110-divvy-tripdata.csv')
Trips_Nov21 <- read_csv('202111-divvy-tripdata.csv')



# Step 2: Wrangle data and combine into a single data frame

# Comparing columns name of each of the files
# Names do not have to be in the same order but they do need to match before we can use a command line to join the data into one file
colnames(Trips_Apr20)
colnames(Trips_May20)
colnames(Trips_June20)
colnames(Trips_July20)
colnames(Trips_Aug20)
colnames(Trips_Sep20)
colnames(Trips_Oct20)
colnames(Trips_Nov20)
colnames(Trips_Dec20)
colnames(Trips_Jan21)
colnames(Trips_Feb21)
colnames(Trips_Mar21)
colnames(Trips_Apr21)
colnames(Trips_May21)
colnames(Trips_June21)
colnames(Trips_July21)
colnames(Trips_Aug21)
colnames(Trips_Sep21)
colnames(Trips_Oct21)
colnames(Trips_Nov21)

# Inspecting Dataframe and looking for inconstant
str(Trips_Apr20)
str(Trips_May20)
str(Trips_June20)
str(Trips_July20)
str(Trips_Aug20)
str(Trips_Sep20)
str(Trips_Oct20)
str(Trips_Nov20)
str(Trips_Dec20)
str(Trips_Jan21)
str(Trips_Feb21)
str(Trips_Mar21)
str(Trips_Apr21)
str(Trips_May21)
str(Trips_June21)
str(Trips_July21)
str(Trips_Aug21)
str(Trips_Sep21)
str(Trips_Oct21)
str(Trips_Nov21)

# Comparing columns datatypes across all data frame by using compare_df_cols
compare_df_cols(Trips_Apr20, Trips_May20, Trips_June20, Trips_July20, 
                Trips_Aug20,Trips_Sep20,Trips_Oct20,Trips_Nov20, Trips_Dec20, 
                Trips_Jan21, Trips_Feb21, Trips_Mar21, Trips_Apr21, Trips_May21, 
                Trips_June21, Trips_July21, Trips_Aug21, Trips_Sep21, 
                Trips_Oct21, Trips_Nov21, return = 'mismatch')

# Converting end_station_id and start_station_id from numeric to character
Trips_Apr20 <- mutate(Trips_Apr20, end_station_id = 
                        as.character(end_station_id), start_station_id = 
                        as.character(start_station_id))
Trips_May20 <- mutate(Trips_May20, end_station_id = 
                        as.character(end_station_id), start_station_id = 
                        as.character(start_station_id))
Trips_June20 <- mutate(Trips_June20, end_station_id = 
                         as.character(end_station_id), start_station_id = 
                         as.character(start_station_id))
Trips_July20 <- mutate(Trips_July20, end_station_id = 
                         as.character(end_station_id), start_station_id = 
                         as.character(start_station_id))
Trips_Aug20 <- mutate(Trips_Aug20, end_station_id = 
                        as.character(end_station_id), start_station_id = 
                        as.character(start_station_id))
Trips_Sep20 <- mutate(Trips_Sep20, end_station_id = 
                        as.character(end_station_id), start_station_id = 
                        as.character(start_station_id))
Trips_Oct20 <- mutate(Trips_Oct20, end_station_id = 
                        as.character(end_station_id), start_station_id = 
                        as.character(start_station_id))
Trips_Nov20 <- mutate(Trips_Nov20, end_station_id = 
                        as.character(end_station_id), start_station_id = 
                        as.character(start_station_id))

# Doublechecking the columns datatypes
compare_df_cols(Trips_Apr20, Trips_May20, Trips_June20, Trips_July20, 
                Trips_Aug20,Trips_Sep20,Trips_Oct20,Trips_Nov20, Trips_Dec20, 
                Trips_Jan21, Trips_Feb21, Trips_Mar21, Trips_Apr21, Trips_May21, 
                Trips_June21, Trips_July21, Trips_Aug21, Trips_Sep21, 
                Trips_Oct21, Trips_Nov21, return = 'mismatch')

# Compiling all data frames into a single data frame named All_trips
All_trips <- bind_rows(Trips_Apr20, Trips_May20, Trips_June20, Trips_July20, 
                       Trips_Aug20,Trips_Sep20,Trips_Oct20,Trips_Nov20, 
                       Trips_Dec20, Trips_Jan21, Trips_Feb21, Trips_Mar21, 
                       Trips_Apr21, Trips_May21, Trips_June21, Trips_July21, 
                       Trips_Aug21, Trips_Sep21, Trips_Oct21, Trips_Nov21)



# Step 3: Clean up and Add Data for Analysis

# Removing unused columns
All_trips <- All_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Renaming column names
All_trips <- All_trips %>% 
  rename(trip_id= ride_id,ride_type= rideable_type, start_time= started_at,
         end_time= ended_at, from_station_name= start_station_name, 
         from_station_id= start_station_id,
         to_station_name= end_station_name, to_station_id= end_station_id, 
         usertype= member_casual)

# List of updated column names
colnames(All_trips)

# Summary of data, checking missing data
skim(All_trips)

#dimensions of data frame
dim(All_trips)

#First 6 rows of data frame
head(All_trips)

#List of columns and data types
str(All_trips)

# Statistical summary of data
summary(All_trips)

# Adding columns that list the date, month, day, and year of each ride
# It allows us to aggregate ride data for each month, day or year
# Before completing these operations, I could only aggregate at the ride level
# The default format is yyyy-mm-dd
All_trips$date <- as.Date(All_trips$start_time)
All_trips$month <- format(as.Date(All_trips$date), "%m")
All_trips$day <- format(as.Date(All_trips$date), "%d")
All_trips$year <- format(as.Date(All_trips$date), "%Y")
All_trips$day_of_week <- format(as.Date(All_trips$date), "%A")

#"ride_length" calculation to All_trips(in-seconds)
All_trips$ride_length <- difftime(All_trips$end_time, All_trips$start_time)
All_trips

#converting "ride-length" from factor to numeric
is.factor(All_trips$ride_length)
All_trips$ride_length <- as.numeric(as.character(All_trips$ride_length))
is.numeric(All_trips$ride_length)

# Removing bad data
#The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# I will create a new version of the dataframe(V2) since data is being removed
All_trips_v2 <- All_trips[!(All_trips$ride_length<0),]
skim(All_trips_v2)



# Step 4: Conducting Descriptive Analysis

summary(All_trips_v2$ride_length)

# Comparing usertype(member and casual users)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$usertype, FUN = mean)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$usertype, FUN = median)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$usertype, FUN = max)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$usertype, FUN = min)

# Average ride time by each day for usertype(member and casual)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$usertype + 
            All_trips_v2$day_of_week, FUN = mean)

# Fixing days of the week out of order
All_trips_v2$day_of_week <- ordered(All_trips_v2$day_of_week, levels= c("Sunday",
                                                                        "Monday" , "Tuesday",
                                                                        "Wednesday", "Thursday",
                                                                        "Friday", "Saturday"))

# Running average ride time by each day for usertype(casual and member)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$usertype +
            All_trips_v2$day_of_week, FUN = mean)

# Analyzing ridership data by type and weekday
All_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  # create weekday fields using wday()
  group_by(usertype, weekday) %>% # Group by usertype and weekday
  summarise(number_of_rides = n()  # calculates the number of rides and average duration 
            , average_duartion = mean(ride_length)) %>%
  # calculate average duration
  arrange(usertype, weekday) # sorts


# Step 5: Visualizing the data

# Visualizing the number of rides by rider type
All_trips_v2 %>% 
  mutate(weekday = wday(start_time, label= TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides= n(), average_duration= mean(ride_length)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x= weekday, y= number_of_rides, fill= usertype))+
  geom_col(position= "dodge")

# Visualization for average duration
All_trips_v2 %>% 
  mutate(weekday= wday(start_time, label= TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides= n(),
            average_duration= mean(ride_length)) %>% 
  arrange(usertype, weekday) %>% 
  ggplot(aes(x= weekday, y= average_duration, fill= usertype)) +
  geom_col(position= "dodge")

# Step 6: Export summary for further analysis
write.csv(All_trips_v2, "data.csv")