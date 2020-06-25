# -----------------------------------------------------------------------------
# Title: Canadian Ice Service (CIS) Iceberg Tracking Beacon Database
#
# Created by: Adam Garbo
#
# Date: June 6, 2020
#
# Description: 
#   - Spatial analysis and graphical outputs.
#   
# Comments: 
#   - Updated on June 6th to allow for limits to be applied without clipping
#
# -----------------------------------------------------------------------------

# Install packages
p <- c("anytime", "geosphere", "ggspatial",  "lubridate", "RColorBrewer", 
       "rgeos", "rnaturalearth","rnaturalearthdata", "rnaturalearthhires", 
       "scales", "sf", "tidyverse")
#install.packages(p) # Warning: Un-commenting this may take several minutes

# Manually install rnaturalearth using devtools
#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearth")
#devtools::install_github("ropensci/rnaturalearthdata")
#devtools::install_github("ropensci/rnaturalearthhires")

# Load the required packages
lapply(p, library, character.only = TRUE)

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

# Set working directory
#setwd("~/Desktop/cis_iceberg_beacon_database/analysis/Shapefiles")

cryologger <- read_csv("Google Drive/University of Ottawa/Thesis Proposal/Presentation/R/cryologger_deployments.csv")

# Read data
data <- read_csv("~/Desktop/cis_iceberg_tracking_beacon_database/output_data/database_csv/database_20200611.csv", 
                 col_types = cols(bp = col_double(), distance = col_double(), 
                                  gps_delay = col_double(), heading = col_double(), 
                                  latitude = col_double(), longitude = col_double(), 
                                  pitch = col_double(), roll = col_double(), 
                                  satellites = col_double(), snr = col_double(), 
                                  speed = col_double(), ta = col_double(), 
                                  ti = col_double(), ts = col_double(), 
                                  ttff = col_double(), vbat = col_double()))

# -------------------------------------
# Subset data 
# -------------------------------------

# Subset by beacon_id
data_subset <- subset(data, beacon_id == ("300434063415110_2018"))

# Subset by multiple beacon_id 
data_subset <- subset(data, (beacon_id %in% c("300434063418130_2018",
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

# -----------------------------------------------------------------------------
# Convert to spatial vector data 
# -----------------------------------------------------------------------------

# Load rnaturalearth shapefile (Canada and Greenland only)
coast_sf <- ne_countries(scale = 10, type = "countries", country = c('canada','greenland'), returnclass = "sf")

# Convert data to simple feature (sf)
points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326) # Entire database
points_sf <- st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = 4326) # Data subset

# Cryologger deployments
cryologger_sf <- st_as_sf(cryologger, coords = c("longitude", "latitude"), crs = 4326) # Data subset

# Get geometry from sf objects
st_geometry(coast_sf)
st_geometry(points_sf)

# Plot shapefiles
plot(coast_sf$geometry)
plot(points_sf$geometry)

# Transform features to WGS84 coordinate reference system
coast_sf <- st_transform(coast_sf, 4326)
points_sf <- st_transform(points_sf, 4326)

# Plot transformed shapefiles
plot(coast_sf$geometry)
plot(points_sf$geometry)

# ------------------------------------
# Create bounding boxes and grids
# ------------------------------------

# -------------------------------------
# Option 1: Get object extents
extents_coast <- st_bbox(coast_sf)
extents_points <- st_bbox(points_sf)
e <- st_as_sfc(st_bbox(extents_points))

# Option 2: Manual grid creation 
new_bb = c(-100, 40, -25, 90)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
e <- st_as_sfc(new_bb)
# -------------------------------------

# Create grid over the bounding box of points sf object
grid <- st_make_grid(e, cellsize = c(1, 0.5)) %>%
  st_set_crs(4326)

# Get geometry from sf objects
st_geometry(grid)

# Return bounding of simple features
extents_grid <- st_bbox(grid)

# Plot grid
#plot(grid)

# Dissolve polygon
grid_outline <- st_union(grid)
#plot(grid_outline)

# -----------------------------------------------------------------------------
# Spatial analysis 
# -----------------------------------------------------------------------------
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

# Remove all NA values
grid_plot <- na.omit(grid_agg[1:3])

# -------------------------------------
# Inspect data 
# -------------------------------------
plot(grid_agg$x)
plot(grid_agg$n)

ggplot(grid_agg, aes(n)) + 
  geom_histogram(binwidth = 100) +
  xlab("mean speed (m/s)") +
  xlim(c(0,5000))

# -----------------------------------------------------------------------------
# Establish limits for coord_sf() 
# -----------------------------------------------------------------------------

# Custom WGS84 extents
xmin = -95
ymin = 49
xmax = -35
ymax = 84

# Custom 3347 extents
xmin = -80
ymin = 49
xmax = -45
ymax = 83.5

# Calculate longitude centre for custom CRS
(abs(xmin) + abs(xmax)) / 2

# EPSG 3347
crs_custom <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-62.5 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
#crs_stere <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-60 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Create domain bounding box
domain_4326 <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326))
domain_4326 <- st_as_sfc(domain_4326)

# Extract coordinates from bounding box
disp_coord_4326 <- st_bbox(domain_4326)

# Transform bounding box to custom CRS
domain_custom <- st_transform(domain_4326, crs_custom)

# Extract coordinates from bounding box (3347)
disp_coord_custom <- st_bbox(domain_custom)


# -----------------------------------------------------------------------------
# Study Area Box 
# -----------------------------------------------------------------------------

# Custom WGS84 extents
xmin = -80
ymin = 60
xmax = -45
ymax = 82.5

study_area <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326))
study_area <- st_as_sfc(study_area)
study_area_custom <- st_transform(study_area, crs_custom)

# -----------------------------------------------------------------------------
# Create maps 
# -----------------------------------------------------------------------------

# Make colour pattern, add to plot where needed 
pal <- colorRampPalette(c("dark blue", "blue", "cyan", "yellow", "red", "dark red"))

# -------------------------------------
# Count map with colour ramp CRS 4326 
# -------------------------------------

# Create map
m <- ggplot() +
  geom_sf(data = grid_plot, aes(fill = n), colour = "black", size = 0.2) +
  geom_sf(data = coast_sf, fill = "antiquewhite", size = 0.3) +
  #geom_sf(data = study_area, colour = "red", fill = NA, size = 0.75) +
  #geom_sf(data = points_sf, mapping = aes(colour = beacon_id), size = 0.75, stroke = 0.1) +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue")) +
  coord_sf(xlim = c(disp_coord_4326["xmin"], disp_coord_4326["xmax"]), 
           ylim = c(disp_coord_4326["ymin"], disp_coord_4326["ymax"]), expand = FALSE)

# Plot map
#m

# Plot count colour gradient 
m + scale_fill_gradientn(colours = pal(16), name = "Count", limits = c(0,1000), oob = squish) + 
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(1.5, "cm"), 
        legend.key.width = unit(0.75,"cm"))

# -------------------------------------
# Count map with colour ramp CRS 3347 
# -------------------------------------

# Transform features to LCC
coast_sf_custom <- st_transform(coast_sf, crs_custom)



# Create map (750 x 650)
n <- ggplot() +
  #geom_sf(data = grid_plot_custom, aes(fill = n), colour = "black", size = 0.2) +
  geom_sf(data = coast_sf_custom, fill = "antiquewhite", size = 0.3) +
  geom_sf(data = points_sf, mapping = aes(colour = beacon_id), size = 0.75, stroke = 0.1) +
  #geom_sf(data = cryologger_sf, size = 4, stroke = 0.5, shape = 23, fill = "red") +
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(1, "cm"), pad_y = unit(1, "cm"), style = north_arrow_fancy_orienteering) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA),
        panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.1), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = "none") +
  coord_sf(xlim = c(disp_coord_custom["xmin"], disp_coord_custom["xmax"]), 
           ylim = c(disp_coord_custom["ymin"], disp_coord_custom["ymax"]), expand = FALSE)

# Plot map
n

# Plot count colour gradient 
n + scale_fill_gradientn(colours = pal(16), name = "Count", limits = c(0,1000), oob = squish) + 
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(1.5, "cm"), 
        legend.key.width = unit(0.75,"cm"))



