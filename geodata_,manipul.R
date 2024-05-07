# --------------------------------------------
# Script Name: geodata_,manipul.R
# Purpose 1: set 2-km buffer along the Doubs river and clip from the map to
#            extract the raster values of the catchment area and slope 
#            for each point with the qgisprocess package.
# Purpose 2: merge the extracted data with other environmental factors 
#            from Doubs dataset to form a dataframe, and finally transfer 
#            the dataframe to a sf object, which contains a geometry column.


# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-30
#
# --------------------------------------------


cat("\014") #clears rhe console
rm(list=ls()) #remove all variales

# Install and load necessary packages

library(osmdata)
library(mapview)
library(sf)
library(terra)
library(dplyr)

# Define the bounding box for France-Switzerland

bbox <- c(left = 5.5, bottom = 46.5, right = 7.5, top = 48)

# Query OpenStreetMap for waterways named "Le Doubs" within the bounding box

doubs_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  add_osm_feature(key = "name", value = "Le Doubs") %>%
  osmdata_sf() 

# Visualize Le Doubs on a map

mapview(doubs_query$osm_lines)

# Look at query output

library(dplyr)
doubs_query
glimpse(doubs_query$osm_lines)
glimpse(doubs_query$osm_multilines)

# Bind into a sf object

river_sf <- bind_rows(st_cast(doubs_query$osm_lines, "MULTILINESTRING"), # Cast lines to multilinestrings
                      doubs_query$osm_multilines) %>% # Bind rows of lines and multilinestrings
select(name, osm_id, role)

head(river_sf)

# plot doubs_sf

plot(river_sf)

unique(river_sf$role) # view the unique values of the 'role' column

# Clean data to Filter out unneeded shapes making sure shapes are valid

river_sf_clean <-
  river_sf %>%
  filter(is.na(role) == FALSE) %>%  # remove role NAs
  rename(doubs_type = role) %>% # rename role to doubs_type
  st_make_valid() # ensure the geometry is valid

# Write the cleaned river data to a shapefile
st_write(river_sf_clean, "data/SPdata/doubs_river.shp")

## Read the DEM data from a raster file
doubs_dem <- terra::rast("data/data-driven/doubs_dem.tif")
doubs_river <- sf::st_read ("data/data-driven/doubs_river.shp")
doubs_pts <- sf::st_read("data/data-driven/doubs_point.shp")
head(doubs_river)

# Convert river data to EPSG:32631 coordinate system
doubs_river_utm <- st_transform(doubs_river, 32631)

# Convert point data to EPSG:32631 coordinate system
doubs_point_pts <- st_transform(doubs_pts, 32631)

st_crs("EPSG:32631")

utm_crs <- "EPSG:32631"

# Project DEM data to the coordinate system specified by utm_crs
doubs_dem_utm <- terra::project(doubs_dem, utm_crs)

# Check the coordinate system of the converted DEM data
terra::crs(doubs_dem_utm)

#Creating and visualizing the buffer
# Method1 on class
doubs_river_buff <- st_buffer(doubs_river_utm,dis=8000)
plot(st_geometry(doubs_river_buff),axes = TRUE)

library(ggplot2)
ggplot(doubs_river) + geom_sf(data = doubs_river_buff)

# Method2 Reference: https://r-spatial.github.io/qgisprocess/articles/qgisprocess.html
qgis_version()
qgis_plugins()
#> # A tibble: 3 × 2
#>   name                    enabled
#>   <chr>                   <lgl>  
#> 1 grassprovider           FALSE  
#> 2 processing              TRUE   
#> 3 processing_saga_nextgen FALSE
qgis_enable_plugins(c("grassprovider", "processing_saga_nextgen"))
qgis_providers()
#> # A tibble: 6 × 3
#>   provider provider_title    algorithm_count
#>   <chr>    <chr>                       <int>
#> 1 gdal     GDAL                           56
#> 2 grass    GRASS                         307
#> 3 qgis     QGIS                           42
#> 4 3d       QGIS (3D)                       1
#> 5 native   QGIS (native c++)             263
#> 6 sagang   SAGA Next Gen                 509

algs <- qgis_algorithms()
algs

qgis_show_help("native:buffer")
#find out the arguments of a specific geoalgorithm
qgis_get_argument_specs("native:buffer")
# attach QGIS output
# either do it "manually":
doubs_buf <- read_sf(qgis_extract_output(buff_2km, "OUTPUT"))
# or use the st_as_sf.qgis_result method:
doubs_buf <- sf::st_as_sf(buff_2km)

# Clip or intersect dem covered by river buffer
# reprojecting raster data
doubs_dem <- terra::rast("data/data-driven/doubs_dem.tif")
doubs_dem_utm <- terra::project(doubs_dem,utm_crs)
terra::crs(doubs_dem_utm)

#Clip or intersect dem by doubs river
doubs_dem_utm_cropped <- crop(doubs_dem_utm, doubs_river_buff)
doubs_dem_utm_masked <- mask(doubs_dem_cropped, doubs_river_buff)

plot(doubs_dem_utm_masked,axes=TRUE)

#Extracting raster values of points as predictors

pak::pak("qgisprocess")
qgis_configure()` 
library(qgisprocess)

qgis_search_algorithms("wetness") %>% dplyr::select(provider_title,algorithm) %>% head(2)

algorithms <- qgis_search_algorithms("wetness")
#Note: result does not inherit from class data.frame ,so debug as fellows:

# Extract the provider title and algorithm name of the first two algorithms
if (length(algorithms) > 0) {
  provider_titles <- sapply(algorithms[1:2], function(alg) alg$provider_title)
  algorithms_names <- sapply(algorithms[1:2], function(alg) alg$algorithm)
  
  # Create a data frame to store the results
  result_df <- data.frame(provider_title = provider_titles, algorithm = algorithms_names)

  print(result_df)
} else {
  print("Not found")
}

# Run the Sag Wetness Index algorithm using QGIS Processing
# The algorithm calculates topographic attributes based on a DEM (Digital Elevation Model)
 topo_total = qgisprocess::qgis_run_algorithm(
   alg = "sagang:sagwetnessindex",      # The algorithm name
   DEM = doubs_dem_utm_masked,          # The masked DEM in the UTM coordinate system
   SLOPE_TYPE = 1,                      # The slope type (e.g., percentage, degrees, etc.)
   SLOPE = tempfile(fileext = ".sdat"), # Temporary file to store the slope output
   AREA = tempfile(fileext = ".sdat"),  # Temporary file to store the area output
   .quiet = TRUE)                       # Run the algorithm without showing any messages
 
# Extract the slope and area values from the topo_total output
# Convert the extracted values to a raster object
 topo_select <- topo_total[c("AREA","SLOPE")] |>
   unlist() |>
   rast()
 
# Rename the layers in the topo_select raster object
 names(topo_select) = c("carea","cslope")
 
# Set the origin of the topo_select raster to match the DEM's origin
 origin(topo_select) = origin(doubs_dem_utm_masked)
 
# Combine the DEM and the topo_select rasters into a single raster object
 topo_char = c(doubs_dem_utm_masked, topo_select)
 
# Extract the topographic attributes (DEM, slope, and area) at the points' locations
# The extracted values will be returned as a data frame
 topo_env <- terra::extract(topo_char, doubs_points_utm, ID = FALSE)

# Agggregating topo and water env
Doubs <- load("data/data-driven/Doubs.RData")
Doubs

water_env <- env
doubs_env <- cbind(doubs_pts_utm, topo_env, water_env)

# Convert the data frame to an sf object
doubs_sf <- st_as_sf(doubs_env, coords = c("Longitude", "Latitude"))

# save as sf object
st_write(doubs_sf, "doubs_sf.shp")
