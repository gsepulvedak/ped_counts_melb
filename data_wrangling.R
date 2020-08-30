library(plyr)
library(tidyverse)

# Laad datasets
sensors <- read_csv("data/Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")
peds <- read_csv("data/Pedestrian_Counting_System_2019 (Exercise 2).csv")

# Check sensor names differences in both datasets
sens_names <- as_tibble(list(name = unique(sensors$sensor_name), orig = "sensors"))
peds_sens_names <- as_tibble(list(name = unique(peds$Sensor_Name), orig = "peds"))
missing <- full_join(sens_names, peds_sens_names, by = "name")

# After inspection of "missing" data frame, the following fixes were identified
# peds sensor renaming
# Current names
curr_names <- c("Collins St (North)", "Flinders la - Swanston St (West) Temp", "Flinders La - Swanston St (West) Temp",
                   "Lincoln-Swanston(West)", "Pelham St (S)", "Swanston St - RMIT Building 14", "Swanston St - RMIT Building 80")

# Relacement names
updt_names <- c("Collins Street (North)", "Flinders La - Swanston St (West) Temporary", "Flinders La - Swanston St (West) Temporary",
                   "Lincoln-Swanston (West)", "Pelham St (South)", "RMIT Building 14", "RMIT Building 80")

# Update peds
peds$Sensor_Name <- mapvalues(peds$Sensor_Name, from = curr_names, to = updt_names)


# sensors sensor renaming
curr_names <- c("Building 80 RMIT", "Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)")
updt_names <- c("RMIT Building 80", "Melbourne Central-Elizabeth St (East)")

# Update sensors
sensors$sensor_name <- mapvalues(sensors$sensor_name, from = curr_names, to = updt_names)

# Merge fixed dataframes
#data <- left_join(peds, sensors, by = c("Sensor_Name" = "sensor_name"))

# filter sensors with pedestrian counts data
sensors <- sensors %>% filter(sensor_name %in% unique(peds$Sensor_Name))

# Get pedestrian mean by sensor and add the column to sensors
peds_avg <- peds %>% group_by(Sensor_Name) %>% summarise(ped_avg = mean(Hourly_Counts))
sensors <- inner_join(sensors, peds_avg, by = c("sensor_name" = "Sensor_Name"))

# Add cut levels for map legend (ends of intervals as separated columns)
sensors <- sensors %>% mutate(avg_interval = gsub("]|\\(", "", cut(.$ped_avg, breaks = 6))) %>% 
  separate(avg_interval, into = c("low_int", "high_int"), sep = ",", convert = TRUE)

# Summarise peds data as needed
peds <- peds %>% group_by(Sensor_Name, Time,
                        Day = forcats::as_factor(Day) %>% 
                          fct_relevel(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  summarise(avg_count = mean(Hourly_Counts))

# Remove no longer needed objects
rm(list = setdiff(ls(), c("sensors", "peds")))
