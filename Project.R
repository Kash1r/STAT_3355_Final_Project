library(tidyverse)
library(lubridate)
library(geosphere)

# Load crime and weather data
crime_data <- read.csv("C:/Users/Kashi/Desktop/Data Analysis/Police_Arrests.csv")
weather_data <- read.csv("C:/Users/Kashi/Desktop/Data Analysis/Weather_Data.csv")

# Format dates
crime_data$Arrest.Date <- mdy(crime_data$Arrest.Date)
weather_data$DATE <- as.Date(weather_data$DATE)

# Filter Dallas weather entries
weather_data <- weather_data[grepl("dallas", weather_data$NAME, ignore.case = TRUE), ]

# Define and format common_dates
common_dates <- as.Date(common_dates, origin = "1970-01-01")

# Filter data on common dates
crime_data_filtered <- crime_data[crime_data$Arrest.Date %in% common_dates, ]
weather_data_filtered <- weather_data[weather_data$DATE %in% common_dates, ]

# Merge datasets on date, including specific columns
merged_data <- merge(
  x = crime_data_filtered %>%
    select(Arrest.Date, Arrest.Time, Drug.Related, Drug.Type, Arrest.Location, Arrest.Weapon, Arrestee.Age.At.Arrest.Time, Latitude, Longitude) %>%
    mutate(Drug.Related = ifelse(Drug.Related == "Yes", TRUE, FALSE)),
  y = weather_data_filtered %>%
    select(LATITUDE, LONGITUDE, DATE, NAME, PRCP, SNOW, TMAX, TMIN, WT01, WT02, WT03, WT04, WT05, WT06, WT07, WT08, WT09),
  by.x = "Arrest.Date",
  by.y = "DATE"
)

# Rename columns for clarity
merged_data <- merged_data %>%
  rename(city = NAME, Weather_Latitude = LATITUDE, Weather_Longitude = LONGITUDE)

# Ensure numeric types for latitude and longitude
merged_data <- merged_data %>%
  mutate_at(vars(Longitude, Latitude, Weather_Longitude, Weather_Latitude), as.numeric)

# Calculate distance between crime and weather locations, converting to miles
merged_data$Distance <- mapply(function(crime_lon, crime_lat, weather_lon, weather_lat) {
  if (any(is.na(c(crime_lon, crime_lat, weather_lon, weather_lat)))) {
    return(NA)
  } else {
    distm(c(crime_lon, crime_lat), c(weather_lon, weather_lat), fun = distHaversine) * 0.000621371
  }
}, crime_lon = merged_data$Longitude, crime_lat = merged_data$Latitude,
weather_lon = merged_data$Weather_Longitude, weather_lat = merged_data$Weather_Latitude)

# Filter for distances <= 10 miles and select closest weather station per crime event
merged_data <- merged_data %>%
  filter(Distance <= 10) %>%
  group_by(Arrest.Date, Arrest.Time, Arrest.Location) %>%
  filter(Distance == min(Distance)) %>%
  ungroup()

# Export the filtered merged dataset
write.csv(merged_data, "C:/Users/Kashi/Desktop/Data Analysis/Merged_Data.csv", row.names = FALSE)

# Identify and filter data for extreme weather conditions
extreme_temp_threshold <- quantile(merged_data$TMAX, 0.75, na.rm = TRUE)
extreme_low_temp_threshold <- quantile(merged_data$TMIN, 0.25, na.rm = TRUE)

merged_data <- merged_data %>%
  mutate_at(vars(starts_with("WT")), ~ifelse(is.na(.), 0, .))

extreme_data <- merged_data %>%
  filter(TMAX >= extreme_temp_threshold | 
           TMIN <= extreme_low_temp_threshold | 
           rowSums(select(., starts_with("WT")), na.rm = TRUE) > 0)

# Export the extreme weather subset
write.csv(extreme_data, "C:/Users/Kashi/Desktop/Data Analysis/Merged_Extreme_Weather_Data.csv", row.names = FALSE)

colnames(merged_data)

# Output the number of observations
cat("Number of observations:", nrow(merged_data), "\n")

# Output summary statistics
cat("Summary statistics:\n")
print(summary(merged_data))

# Output dimensions of the dataframe
cat("Dimensions of the dataframe:", dim(merged_data), "\n")

# Output structure of the dataframe
cat("Structure of the dataframe:\n")
str(merged_data)

# Output head and tail of the dataframe
cat("Head of the dataframe:\n")
print(head(merged_data))
cat("Tail of the dataframe:\n")
print(tail(merged_data))
