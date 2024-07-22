
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)

# Define colors
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# Read the data
apr <- read.csv("/Users/nityareddy/Desktop/uber-raw-data-apr14.csv")

# Print the dimensions and first few rows of the data
cat("The dimensions of the data are:", dim(apr), "\n")
head(apr)

# Check the structure of the Date.Time column
str(apr$Date.Time)

# Check the first few values of the Date.Time column to understand its format
head(apr$Date.Time)

# Attempt to convert Date.Time to POSIXct using the correct format (without seconds)
apr$Date.Time <- as.POSIXct(apr$Date.Time, format="%m/%d/%Y %H:%M", tz="UTC")

# Check if the conversion was successful
if (all(is.na(apr$Date.Time))) {
  cat("Conversion failed using format '%m/%d/%Y %H:%M'. Trying with lubridate...\n")
  apr$Date.Time <- parse_date_time(apr$Date.Time, orders = c("mdY HM", "Ymd HM"))
}

# Verify the conversion
cat("Summary of Date.Time column after conversion:\n")
summary(apr$Date.Time)
head(apr$Date.Time)

# Extract time components
apr$Time <- format(apr$Date.Time, format="%H:%M")

# Create individual columns for day and day of the week
apr$day <- factor(day(apr$Date.Time))
apr$dayofweek <- factor(wday(apr$Date.Time, label=TRUE))

# Add Time variables (without seconds) as well
apr$minute = factor(minute(apr$Date.Time))
apr$hour = factor(hour(apr$Date.Time))

# Look at the data
head(apr)
#new
hourly_data <- data %>% 
              group_by(hour) %>% 
                     dplyr::summarize(Total = n())

# Shos data in a searchable js table
datatable(hourly_data)

# Aggregate data by hour
hourly_data <- apr %>% group_by(hour) %>% dplyr::summarize(Total = n())

# Base R plot for data by hour
plot(as.numeric(as.character(hourly_data$hour)), hourly_data$Total, 
     type = "h", 
     lwd = 2, 
     col = "steelblue", 
     main = "Trips Every Hour", 
     xlab = "Hour of the Day", 
     ylab = "Number of Trips")
grid()


# Aggregate data by day of the month
day_data <- apr %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data

# Base R plot for data by day of the month
plot(as.numeric(as.character(day_data$day)), day_data$Trips, 
     type = "h", 
     lwd = 2, 
     col = "steelblue", 
     main = "Trips by Day of the Month", 
     xlab = "Day of the Month", 
     ylab = "Number of Trips")
grid()

# Aggregate data by day of the week
dayofweek_data <- apr %>% group_by(dayofweek) %>% dplyr::summarize(Trips = n())
dayofweek_data

# Base R plot for data by day of the week
plot(as.numeric(dayofweek_data$dayofweek), dayofweek_data$Trips, 
     type = "h", 
     lwd = 2, 
     col = "steelblue", 
     main = "Trips by Day of the Week", 
     xlab = "Day of the Week", 
     ylab = "Number of Trips", 
     xaxt = "n")
axis(1, at = 1:7, labels = levels(dayofweek_data$dayofweek))
grid()

