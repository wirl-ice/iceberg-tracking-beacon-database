#------------------------------------------------------------------------------
# Title: Canadian Ice Service (CIS) Iceberg Tracking Beacon Database
#
# Created by: Adam Garbo
#
# Date: April 23, 2020
#
# Changelog:
#
#
# Description: 
#   - Spatial analysis and graphical outputs.
#
# Necessary file(s): 
#   - 
#   
# Notes: 
#   -
#
#------------------------------------------------------------------------------

# Install packages
p <- c("anytime", "geosphere", "ggspatial", "lubridate", "RColorBrewer", "scales", "sf", "tidyverse")
#install.packages(p) # Warning: Uncommenting this may take several minutes

# Load the required packages
lapply(p, library, character.only = TRUE)

# Load data -------------------------------------------------------------------

# Set working directory
setwd("~/Desktop/cis_iceberg_tracking_beacon_database/analysis/Shapefiles")

# Read data
data <- read_csv("~/Desktop/cis_iceberg_tracking_beacon_database/output_data/database_csv/database_20200410.csv", 
                 col_types = cols(bp = col_double(), distance = col_double(), 
                                  gps_delay = col_double(), heading = col_double(), 
                                  latitude = col_double(), longitude = col_double(), 
                                  pitch = col_double(), roll = col_double(), 
                                  satellites = col_double(), snr = col_double(), 
                                  speed = col_double(), ta = col_double(), 
                                  ti = col_double(), ts = col_double(), 
                                  ttff = col_double(), vbat = col_double()))

# Subset data -----------------------------------------------------------------

# Subset by beacon_id
#data_subset <- subset(data, beacon_id == "300434063415110_2018")

# Subset data according to desired criteria
data_subset <- subset(data, (latitude >= 40 & latitude <= 90) & (longitude >= -90 & longitude <= -40))

# Convert to spatial vector data ----------------------------------------------

# Read coast shapefile from current working directory
coast_sf <- st_read("coast_poly.shp")

# Convert data to simple feature (sf)
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326) # Entire database
points_sf <- st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = 4326) # data subset

# Get geometry from sf objects
st_geometry(coast_sf)
st_geometry(points_sf)

# Plot shapefiles
plot(coast_sf$geometry)
plot(points_sf$geometry)

# Transform features to WGS84 coordinate reference system
coast_sf <- st_transform(coast_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
points_sf <- st_transform(points_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# Plot transformed shapefiles
plot(coast_sf$geometry)
plot(points_sf$geometry)

# Return bounding of simple features
extents_coast <- st_bbox(coast_sf)
extents_points <- st_bbox(points_sf)

#----------------------------------------------------
# Optional: Manual grid creation based on coordinates
new_bb = c(-80, 45, -40, 90)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
e <- st_as_sfc(new_bb)
#----------------------------------------------------

# Create extent object for points
#e <- st_as_sfc(st_bbox(extents_points))

# Create grid over the bounding box of points sf object
grid <- st_make_grid(e, cellsize = c(1, 0.5)) %>%
  st_set_crs(4326)

# Get geometry from sf objects
st_geometry(grid)

# Return bounding of simple features
extents_grid <- st_bbox(grid)

# Plot grid
plot(grid)

# Dissolve polygon
grid_outline <- st_union(grid)
plot(grid_outline)

# Optional: Clip data ---------------------------------------------------------

# Intersect polygons. Note that sf is intelligent with attribute data!
coast_clipped <- st_intersection(coast_sf, grid_outline)

# Get geometry from sf objects
st_geometry(coast_clipped)

# Plot clipped polygon
plot(coast_clipped$geometry)

# Transform data --------------------------------------------------------------

# Transform features to WGS84 coordinate reference system
#coast_clipped <- st_transform(coast_clipped, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#grid <- st_transform(grid, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# Transform features to LCC coordinate reference system
coast_sf <- st_transform(coast_sf, "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=40 +lon_0=-100, ellps=WGS84")
points_sf <- st_transform(points_sf, "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=40 +lon_0=-100, ellps=WGS84")
coast_clipped <- st_transform(coast_clipped, "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=40 +lon_0=-60, ellps=WGS84")
grid <- st_transform(grid, "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=40 +lon_0=-100, ellps=WGS84")

# Plot shapefiles
plot(grid)
plot(points, add = TRUE, col = 'red')
plot(coast_clipped, add = TRUE, col='white')

# Spatial analysis ------------------------------------------------------------

# Create grid to sf (simple feature), which extends data.frame-like objects with a simple feature list column
grid_sf <- st_sf(grid)

# Assign unique id values to each grid polygon
grid_sf$id = 1:length(grid)

# Find points within polygons and perform a spatial join
points_in_grid <- st_join(points_sf, grid_sf, join = st_within)

# Count points per grid cell
grid_count <- count(as_tibble(points_in_grid), id) %>%
  print()

# Join point count with grid_sf data frame
grid_sf <- left_join(grid_sf, grid_count)

# Calculate mean according to beacon_id and unique grid id
aggdata <- aggregate(points_in_grid$speed, by=list(points_in_grid$beacon_id, points_in_grid$id), 
                     FUN = mean, na.rm = TRUE)

# Calculate mean of means according to unique grid id
aggdata2 <- aggregate(aggdata$x, by = list(aggdata$Group.2), FUN = mean, na.rm = TRUE)

# Join mean of means with grid_sf data frame
grid_sf <- left_join(grid_sf, aggdata2, by = c("id"="Group.1"))

# Convert aggregated data to simple feature (sf) for plotting
grid_agg <- st_sf(grid_sf)

# Transform features to LCC coordinate reference system
#grid_agg <- st_transform(grid_agg, "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=40 +lon_0=-100, ellps=WGS84")

# Remove all NA values
grid_plot <- na.omit(grid_agg[1:3])

# Inspect data ----------------------------------------------------------------
plot(grid_agg$x)

ggplot(grid_agg, aes(x)) + 
  geom_histogram(binwidth = 0.1) +
  xlab("mean speed (m/s)")

# Create maps -----------------------------------------------------------------

# Make colour pattern, add to plot where needed 
pal <- colorRampPalette(c("dark blue", "blue", "cyan", "yellow", "red", "dark red"))

# Create map with limits
m <- ggplot() +
  geom_sf(data = grid_plot, aes(fill = x), colour = "black", size = 0.2) +
  geom_sf(data = coast_clipped, fill = "antiquewhite", size = 0.3) +
  #geom_sf(data = points_sf, mapping = aes(colour = speed), size = 1, stroke = 0) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(expand = FALSE)

# Plot map
#m

# Plot map with colour gradient
m + scale_fill_gradientn(colours = pal(16), name = "mean speed (m/s)", limits = c(0,1), oob = squish) + 
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.size = unit(2.0, "cm"), 
        legend.key.width = unit(0.75,"cm"))


# Create map with limits (no clipping)
n <- ggplot() +
  geom_sf(data = grid_plot, aes(fill = x), colour = "black", size = 0.2) +
  geom_sf(data = coast_sf, fill = "antiquewhite", size = 0.3) +
  #geom_sf(data = points_sf, mapping = aes(colour = speed), size = 1, stroke = 0) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"),
           xlim = c(-85, -45), ylim = c(45, 80), expand = FALSE, clip = "on")

# Plot map
n

# Create mean speed map (old basic version)
o <- ggplot() +
  #geom_sf(data = grid_plot, aes(fill = x), colour = "black", size = 0.2) +
  #geom_sf(data = coast_sf, fill = "antiquewhite", size = 0.3) +
  geom_sf(data = coast_clipped, fill = "antiquewhite", size = 0.3) +
  #geom_sf(data = points_sf, mapping = aes(colour = beacon_id), size = .5, stroke = 0) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "none") +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering)
  #coord_sf(expand = FALSE)

# Plot map
o

# Create map with limits (no clipping)
n <- ggplot() +
  geom_sf(data = coast_sf, fill = "antiquewhite", size = 0.3) +
  #geom_sf(data = points_sf, mapping = aes(colour = speed), size = 1, stroke = 0) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(crs = st_crs("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-75 +x_0=0 +y_0=-8000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs no_defs"),
           xlim = c(xlim[1], ylim[2]), ylim = c(xlim[2], ylim[1]), expand = FALSE, clip = "on")

# Plot map
n

from ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
to = "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-75 +x_0=0 +y_0=-8000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs no_defs"
x_pts = cbind(45, -85)
y_pts = cbind(80, -45)
xlim = sf_project(from, to, x_pts)
ylim = sf_project(from, to, y_pts)

xlim
ylim
