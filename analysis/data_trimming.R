# ------------------------------------------------------------------------------
# Title: Canadian Ice Service (CIS) Iceberg Tracking Beacon Database
#
# Created by: Adam Garbo
#
# Date: June 22, 2020
#
# Description: 
#   - Trimming trajectories contained in the CIS iceberg beacon database.
#
# Comments: 
#   - 
#
# ------------------------------------------------------------------------------

# Install packages
p <- c("anytime", "cowplot", "geosphere", "ggspatial",  "lubridate", "RcppRoll",
       "RColorBrewer", "rgeos", "rnaturalearth","rnaturalearthdata", "rnaturalearthhires", 
       "scales", "sf", "tidyverse")
#install.packages(p) # Warning: Un-commenting this may take several minutes

# Load the required packages
lapply(p, library, character.only = TRUE)

# ------------------------------------------------------------------------------
# Configure libraries
# ------------------------------------------------------------------------------

# RColorBrewer palettes
display.brewer.pal(n = 9, name = 'Set1')
palette = brewer.pal(n = 9, name = 'Set1')

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

# Load rnaturalearth shapefile (Canada and Greenland only)
coast_sf <- ne_countries(scale = 10, type = "countries", 
                         country = c('canada','greenland'), returnclass = "sf")

# Read database
data <- read_csv("~/Desktop/cis_iceberg_beacon_database/output_data/csv/database_20200708.csv", 
                 col_types = cols(bp = col_double(), distance = col_double(), 
                                  gps_delay = col_double(), heading = col_double(), 
                                  latitude = col_double(), longitude = col_double(), 
                                  pitch = col_double(), roll = col_double(), 
                                  satellites = col_double(), snr = col_double(), 
                                  speed = col_double(), ta = col_double(), 
                                  ti = col_double(), ts = col_double(), 
                                  ttff = col_double(), vbat = col_double()))

# Subset a specific beacon

# 2011
subset <- subset(data, beacon_id == "2011_300234010031950")
subset <- subset(data, beacon_id == "2011_300234010955690")
subset <- subset(data, beacon_id == "2011_300234010959690")
subset <- subset(data, beacon_id == "2011_300234010958690_PII-A")


# 2012
subset <- subset(data, beacon_id == "2012_300234010132070")

#2013
subset <- subset(data, beacon_id == "2013_300234011240410")
subset <- subset(data, beacon_id == "2013_300234011241410")
subset <- subset(data, beacon_id == "2013_300234011242410")

#2015
subset <- subset(data, beacon_id == "2015_300234060435010")

# 2017
subset <- subset(data, beacon_id == "2017_300234060169280")


# Subset date range
subset <- subset(subset, (datetime_data > as.POSIXct("2011-08-29 00:00:00")))
                 
subset <- subset(subset, (datetime_data > as.POSIXct("2015-10-01 00:00:00")) & 
                   (datetime_data < as.POSIXct("2016-11-01 00:00:00")))

# ------------------------------------------------------------------------------
# Convert data
# ------------------------------------------------------------------------------

# Change Date to date format
subset$datetime <- as.POSIXct(subset$datetime_data)

# Convert data to simple feature (sf)
points_sf <- st_as_sf(subset, coords = c("longitude", "latitude"), crs = 4326) # Entire database

# Window
window = 3

# Temperature rolling mean & standard deviation
# Priority: 1) surface, 2) atmosphere, 3) internal
if (all(!is.na(points_sf$ts))) { 
  points_sf$t_x = roll_mean(points_sf$ts, n = window, fill = NA)
  points_sf$t_sd = roll_sd(points_sf$ts, n = window, fill = NA)
} else if (all(!is.na(points_sf$ta))) {
  points_sf$t_x = roll_mean(points_sf$ta, n = window, fill = NA)
  points_sf$t_sd = roll_sd(points_sf$ta, n = window, fill = NA)
} else {
  points_sf$t_x = roll_mean(points_sf$ti, n = window, fill = NA)
  points_sf$t_sd = roll_sd(points_sf$ti, n = window, fill = NA)
}

# Speed rolling mean & standard deviation
points_sf$speed_x = roll_mean(points_sf$speed, n = window, fill = NA)
points_sf$speed_sd = roll_sd(points_sf$speed, n = window, fill = NA)

# ------------------------------------------------------------------------------
# Plot data
# ------------------------------------------------------------------------------

# Plot map
map <- ggplot() +
  geom_sf(data = coast_sf, fill = NA, size = 0.2, colour= "black") +
  geom_sf(data = points_sf, size = 1, shape = 21) + 
  theme_bw()

# ------------------------------------------------------------------------------
# Temperature
# ------------------------------------------------------------------------------

beacon = paste("Beacon:", points_sf$beacon_id[1])
interval = "1 day"

# Plot temperature
p0 <- ggplot(data = points_sf) +
  geom_line(aes(datetime, ts), colour = palette[1]) +
  xlab(NULL) +
  ylab("Temperature (°C)") +
  scale_x_datetime(date_breaks = interval) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face="bold")) 

# Plot temperature rolling mean
p1 <- ggplot(data = points_sf) +
  geom_line(aes(datetime, t_x), colour = palette[1]) +
  xlab(NULL) +
  ylab("Mean (°C)") +
  ggtitle(paste("Temperature: 7-day rolling mean & standard deviation", beacon, sep="\n")) +
  scale_x_datetime(date_breaks = interval) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face="bold")) 

# Plot temperature standard deviation
p2 <- ggplot(data = points_sf) +
  geom_line(aes(datetime, t_sd), colour = palette[2]) +
  xlab(NULL) +
  ylab("Standard deviation (°C)") +
  scale_x_datetime(date_breaks = interval) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Rolling temperature mean and standard deviation
plot_grid(p1, p2, ncol = 1, align = "hv")

# ------------------------------------------------------------------------------
# Speed
# ------------------------------------------------------------------------------

# Plot speed rolling mean
p3 <- ggplot(data = points_sf) +
  geom_line(aes(datetime, speed_x), colour = palette[1]) +
  xlab(NULL) +
  ylab("Mean (m/s)") +
  ggtitle(paste("Speed: 7-day rolling mean & standard deviation", beacon, sep="\n")) +  scale_x_datetime(date_breaks = "1 days") +
  scale_x_datetime(date_breaks = interval) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face="bold"))

# Plot speed standard deviation
p4 <- ggplot(data = points_sf) +
  geom_line(aes(datetime, speed_sd), colour = palette[2]) +
  xlab(NULL) +
  ylab("Standard deviation (m/s)") +
  scale_x_datetime(date_breaks = interval) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Rolling speed
plot_grid(p3, p4, ncol = 1, align = "hv")

# ------------------------------------------------------------------------------
# Extra code
# ------------------------------------------------------------------------------

# Convert data to simple feature (sf)
points <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326) # Entire database

# Plot map of entire database
ggplot() +
  geom_sf(data = coast_sf, fill = NA, size = 0.2, colour= "black") +
  geom_sf(data = points, size = 1, shape = 21) + 
  theme_bw()

# ArcticNet 2011 subset
data_subset <- subset(data, (beacon_id %in% c("300234010031950_2011",
                                              "300234010955690_2011",
                                              "300234010959690_2011")) &
                        (datetime_data > as.POSIXct("2011-07-01 00:00:00")) & 
                        (datetime_data < as.POSIXct("2011-10-01 00:00:00")))

# Plot
# You can change the format to suit your requirements
ggplot(aaci, aes(x=dt, y=Average, colour=Species, group=Species)) +
  geom_point(size=4) +
  geom_line(size=1.3) + 
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")