# ------------------------------------------------------------------------------
# Title: Canadian Ice Service (CIS) Iceberg Tracking Beacon Database
#
# Created by: Adam Garbo
#
# Date: June 21, 2020
#
# Description: 
#   - Plot deployment start positions of all beacons contained in the database
#   
# Comments: 
#   - 
#
# ------------------------------------------------------------------------------

# Install packages
p <- c("anytime", "geosphere", "ggspatial",  "lubridate", "RColorBrewer", 
       "rgeos", "rnaturalearth","rnaturalearthdata", "rnaturalearthhires", 
       "scales", "sf", "tidyverse")
#install.packages(p) # Warning: Un-commenting this may take several minutes

# Load the required packages
lapply(p, library, character.only = TRUE)

# ------------------------------------------------------------------------------
# Configure libraries
# ------------------------------------------------------------------------------
# Make colour pattern, add to plot where needed 
pal <- colorRampPalette(c("dark blue", "blue", "cyan", "yellow", "red", "dark red"))

# EPSG
# https://epsg.io/3347

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

database <- read_csv("Downloads/cis_iceberg_beacon_database.csv", 
                     col_types = cols(DEPLOYMENT_START = col_datetime(format = "%Y-%m-%d %H:%M")))

# Load rnaturalearth shapefile (Canada and Greenland only)
coast_sf <- ne_countries(scale = 10, type = "countries", country = c('canada','greenland', 'united states of america'), returnclass = "sf")
plot(coast_sf$geometry)

# Convert data to simple feature (sf)
points_sf <- st_as_sf(database, coords = c("LONGITUDE_START", "LATITUDE_START"), crs = 4326)
plot(points_sf$geometry)

points_sf$year <- year(points_sf$DEPLOYMENT_START)

points_sf$year <- as.character(points_sf$year)

# ------------------------------------------------------------------------------
# Establish limits for coord_sf() 
# ------------------------------------------------------------------------------

# Custom WGS84 limits
xmin = -90
ymin = 42.5
xmax = -47.5
ymax = 85

# Used for most figures already generated
xmin = -90
ymin = 44
xmax = -46.5
ymax = 84

# Arctic emphasis figures
xmin = -94
ymin = 60.25
xmax = -59
ymax = 83.5

# East Coast figures
xmin = -63
ymin = 45
xmax = -47
ymax = 60

# Calculate longitude center for custom CRS
(abs(xmin) + abs(xmax)) / 2

# EPSG 3347
crs_custom <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-76.5 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 

# Create domain bounding box 
limits_4326 <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326))

# Transform bounding box to custom CRS and extract coordinates
limits_3347 <- st_bbox(st_transform(st_as_sfc(limits_4326), crs_custom))

# ------------------------------------------------------------------------------
# Plot map EPSG:3347
# ------------------------------------------------------------------------------

# Transform features to LCC
coast_sf_3347 <- st_transform(coast_sf, crs_custom)
points_sf_3347 <- st_transform(points_sf, crs_custom)

# Plot map (antiquewhite1 + aliceblue + )
ggplot() +
  geom_sf(data = coast_sf_3347, fill = "antiquewhite1", size = 0.3) +
  #geom_sf(data = points_sf_3347, aes(fill = year), colour = "black", size = 3, shape = 24) +
  geom_sf(data = points_sf_3347, fill = "red", colour = "black", size = 3, shape = 24) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "none") +
  coord_sf(xlim = c(limits_3347["xmin"], limits_3347["xmax"]), 
           ylim = c(limits_3347["ymin"], limits_3347["ymax"]), expand = FALSE) 


# Plot map (ivory + light blue)
ggplot() +
  geom_sf(data = coast_sf_3347, fill = "ivory", size = 0.2, colour= "black") +
  #geom_sf(data = points_sf_3347, colour = "black", aes(fill = year), size = 3, shape = 24) +
  geom_sf(data = points_sf_3347, fill = "red", colour = "black", size = 3, shape = 24) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid = element_line(colour = NA), 
        panel.background = element_rect(fill = "light blue"),
        legend.position = "none") +
  coord_sf(xlim = c(limits_3347["xmin"], limits_3347["xmax"]), 
           ylim = c(limits_3347["ymin"], limits_3347["ymax"]), expand = FALSE)

# ------------------------------------------------------------------------------
# Plot map EPSG:4326
# ------------------------------------------------------------------------------

# Plot map
ggplot() +
  geom_sf(data = coast_sf, fill = "antiquewhite", size = 0.3) +
  geom_sf(data = points_sf, mapping = aes(colour = BEACON_ID), size = 3, shape = 20, stroke = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "none") +
  coord_sf(xlim = c(bbox_4326["xmin"], bbox_4326["xmax"]), 
           ylim = c(bbox_4326["ymin"], bbox_4326["ymax"]), expand = FALSE)


# ------------------------------------------------------------------------------
# Extra code
# ------------------------------------------------------------------------------
#scale_color_gradientn(colours = rainbow(5)) 
#scale_fill_gradientn(colours = rainbow(10)) 