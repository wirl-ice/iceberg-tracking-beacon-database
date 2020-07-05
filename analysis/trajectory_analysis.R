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
data <- read_csv("~/Desktop/cis_iceberg_beacon_database/csv/database_20200704.csv", 
                 col_types = cols(bp = col_double(), distance = col_double(), 
                                  gps_delay = col_double(), heading = col_double(), 
                                  latitude = col_double(), longitude = col_double(), 
                                  pitch = col_double(), roll = col_double(), 
                                  satellites = col_double(), snr = col_double(), 
                                  speed = col_double(), ta = col_double(), 
                                  ti = col_double(), ts = col_double(), 
                                  ttff = col_double(), vbat = col_double()))

# Subset data according to desired criteria
data <- subset(data, (speed <= 5) & (distance <= 100000))

# Subset a specific beacon
data <- subset(data, beacon_id == "300234060563100_2017")

# Subset by multiple beacon_id 
data <- subset(data, (beacon_id %in% c("300434063418130_2018",
                                       "300434063415110_2018",
                                       "300434063419120_2018",
                                       "300434063411050_2018",
                                       "300434063415160_2018",
                                       "300434063416060_2018",
                                       "300234063265700_2019",
                                       "300234065254740_2019",
                                       "300434063496100_2019",
                                       "300434063392350_2019",
                                       "300434063292950_2019",
                                       "300434063498160_2019",
                                       "300434063494100_2019",
                                       "300434063392070_2019",
                                       "300434063394110_2019",
                                       "300434063495310_2019")))

# Subset data according to desired criteria
data_subset <- subset(data, (latitude >= 50 & latitude <= 85) & (longitude >= -90 & longitude <= -40))

# ------------------------------------------------------------------------------
# Loop
# ------------------------------------------------------------------------------

i = 0

# Execute loop for each unique beacon ID
for (id in unique(data$beacon_id)) {
  
  # Increment counter
  i = i + 1
  
  # Subset by beacon_id
  data_subset <- subset(data, beacon_id == id)
  
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
    labs(caption = paste(points_sf$beacon_type[1], " ", id, "\n",
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
  
  left <- plot_grid(map)
  
  right <- plot_grid(p1, p2, p3, ncol = 1, align = "hv")
  
  # Plot grid
  combined <- plot_grid(left, right)
  
  # Save plot
  save_plot(filename = paste(i, "_", points_sf$beacon_id[1], ".png", sep = ""), 
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
data_subset <- subset(data, beacon_id == ("300434063415110_2018"))
data_subset <- subset(data, beacon_id == ("300434063418130_2018"))
data_subset <- subset(data, beacon_id == ("300434063415160_2018"))
data_subset <- subset(data, beacon_id == ("300234063265700_2019"))

sprintf("%.png", points_sf$beacon_id[1])
ggsave(filename = paste(points_sf$beacon_id[1], ".png", sep = ""), 
       plot = combined, 
       device = "png",
       width = 5, units = "in", dpi = 600)