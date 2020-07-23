# ------------------------------------------------------------------------------
# Title: Canadian Ice Service (CIS) Iceberg Tracking Beacon Database
#
# Created by: Adam Garbo
#
# Date: June 22, 2020
#
# Description: 
#   - Plotting of each trajectory contained in the CIS iceberg beacon database.
#   - Includes: map, speed and distance
# Comments: 
#   - 
#
# ------------------------------------------------------------------------------

# Install packages
p <- c("anytime", "cowplot", "geosphere", "ggspatial",  "lubridate", "RColorBrewer", 
       "rgeos", "rnaturalearth","rnaturalearthdata", "rnaturalearthhires", 
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

# Subset data according to desired criteria
#data <- subset(data, (speed <= 5) & (distance <= 100000))

# Subset a specific beacon
data <- subset(data, beacon_id == "2011_300234010958690_PII-A")

# Subset by multiple beacon_id 
data <- subset(data, (beacon_id %in% c("2018_300434063418130",
                                       "2018_300434063415110",
                                       "2018_300434063419120",
                                       "2018_300434063411050",
                                       "2018_300434063415160",
                                       "2018_300434063416060",
                                       "2019_300234063265700",
                                       "2019_300234065254740",
                                       "2019_300434063496100",
                                       "2019_300434063392350",
                                       "2019_300434063292950",
                                       "2019_300434063498160",
                                       "2019_300434063494100",
                                       "2019_300434063392070",
                                       "2019_300434063394110",
                                       "2019_300434063495310")))

# Subset data according to desired criteria
data_subset <- subset(data, (latitude >= 50 & latitude <= 85) & (longitude >= -90 & longitude <= -40))

# ------------------------------------------------------------------------------
# Loop
# ------------------------------------------------------------------------------

# Execute loop for each unique beacon ID
for (beacon in unique(data$beacon_id)) {
  
  # Subset by beacon_id
  data_subset <- subset(data, beacon_id == beacon)
  
  # 1 - Convert data to simple feature (sf)
  #points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  points_sf <- st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = 4326)
  
  # 2 - Creating bounding box based on extents of point
  limits <- st_bbox(points_sf)
  
  # ------------------------------------------------------------------------------
  # Plots
  # ------------------------------------------------------------------------------
  
  # Plot map
  map <- ggplot() +
    geom_sf(data = coast_sf, fill = NA, size = 0.2, colour= "black") +
    geom_sf(data = points_sf, size = 1, shape = 21, colour = palette[1]) + 
    coord_sf(xlim = c(limits["xmin"], limits["xmax"]), 
             ylim = c(limits["ymin"], limits["ymax"])) +
    labs(caption = paste(points_sf$beacon_type[1], " ", beacon, "\n",
                         min(points_sf$datetime_data), " to ",
                         max(points_sf$datetime_data), "\n",
                         round(max(points_sf$datetime_data) - min(points_sf$datetime_data)), " days / ",
                         sum(points_sf$distance / 1000, na.rm = TRUE), " km",
                         sep = "")) +
    theme_bw() +
    theme(panel.grid = element_line(colour = NA),
          plot.caption = element_text(hjust = 0))
  
  # Plot speed
  p1 <- ggplot(data = points_sf) +
    geom_point(aes(datetime_data, speed), size = 1, shape = 21, colour = palette[2]) +
    xlab(NULL) +
    ylab("Speed (m/s)") +
    theme_classic()
  
  # Plot distance
  p2 <- ggplot(data = points_sf) +
    geom_point(aes(datetime_data, distance/1000), size = 1, shape = 21, colour = palette[3]) + 
    xlab(NULL) +
    ylab("Distance (km)") +
    theme_classic()
  
  p3 <- ggplot(data = points_sf) +
    xlab("Datetime") +
    ylab("Temperature (°C)") +
    theme_classic()
  
  # Plot temperature. Priority: 1) surface, 2) atmosphere, 3) internal
  if (all(!is.na(points_sf$ts))) { 
    p3 <- p3 + geom_point(aes(datetime_data, ts), size = 1, shape = 21, colour = palette[4])
  } else if (all(!is.na(points_sf$ta))) {
    p3 <- p3 + geom_point(aes(datetime_data, ta), size = 1, shape = 21, colour = palette[4])
  } else {
    p3 <- p3 + geom_point(aes(datetime_data, ti), size = 1, shape = 21, colour = palette[4])
  }
  
  # -----------------------------------------------------------------------------
  # Nested plot grids
  # -----------------------------------------------------------------------------
  
  #left <- plot_grid(map)
  
  #right <- plot_grid(p1, p2, p3, ncol = 1, align = "hv")
  
  # Plot grid
  #combined <- plot_grid(left, right)
  combined <- plot_grid(map)
  
  # Save plot
  save_plot(filename = paste(beacon, ".png", sep = ""), 
            plot = combined,
            base_width = 12,
            base_height = 7,
            dpi = 100)
}

# ------------------------------------------------------------------------------
# Extra code
# ------------------------------------------------------------------------------
scale_colour_brewer()


# 
p1 <- ggline(data = points_sf, 
             x = "datetime_data", 
             y = "speed", 
             xlab = FALSE,
             ylab = "Speed (m/s)",
             color = palette[1],
             shape = 21,
             plot_type = "p")

p2 <- ggline(data = points_sf, 
             x = "datetime_data", 
             y = "distance/1000", 
             xlab = FALSE,
             #xlab = "Datetime",
             ylab = "Distance (km)",
             color = palette[2],
             shape = 21,
             plot_type = "p")

p3 <- ggline(data = points_sf, 
             x = "datetime_data", 
             y = "ti", 
             #xlab = FALSE,
             xlab = "Datetime",
             ylab = "Temperature (°C)",
             color = palette[3],
             shape = 21,
             plot_type = "p")

p4 <- ggline(data = points_sf, 
             x = "datetime_data", 
             y = "vbat", 
             xlab = "Datetime",
             ylab = "Voltage (V)",
             color = palette[4],
             shape = 21,
             plot_type = "p")

plot_grid(p1, p2, p3, p4, ncol = 1, align = "hv")

sprintf(buffer, "You have %2d minutes left.", min);

#geom_line(aes(datetime_data, distance/1000), colour = palette[3]) +

# Subset by beacon_id
data_subset <- subset(data, beacon_id == ("2018_300434063415110"))
data_subset <- subset(data, beacon_id == ("2018_300434063418130"))
data_subset <- subset(data, beacon_id == ("2018_300434063415160"))
data_subset <- subset(data, beacon_id == ("2018_300234063265700"))

sprintf("%.png", points_sf$beacon_id[1])
ggsave(filename = paste(points_sf$beacon_id[1], ".png", sep = ""), 
       plot = combined, 
       device = "png",
       width = 5, units = "in", dpi = 600)