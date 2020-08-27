library(plyr)
library(dplyr)

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
data <- left_join(peds, sensors, by = c("Sensor_Name" = "sensor_name"))

# Remove no longer needed objects
rm(list = setdiff(ls(), "data"))