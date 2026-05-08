# 11_extend_river_network.R
# Re-clip HydroRIVERS with a 75 km buffer around the study area to capture
# downstream segments that extend beyond the original district clip.
# This was needed because L21 Karamber traced only 2.3 km in the original
# clip due to NEXT_DOWN references pointing outside the dataset.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(dplyr)

hydrorivers_path <- list.files("data/raw",
                               pattern = "HydroRIVERS.*\\.shp$",
                               full.names = TRUE,
                               recursive = TRUE)[1]
cat("Loading from:", hydrorivers_path, "\n")

# Study area + 75 km buffer
districts <- sf::st_read("data/processed/study_area_districts.gpkg",
                         quiet = TRUE)
districts <- sf::st_transform(districts, 32642)
study_buffer <- sf::st_buffer(sf::st_union(districts), dist = 75000)
study_buffer_4326 <- sf::st_transform(study_buffer, 4326)

# Read with bbox filter for speed
bbox <- sf::st_bbox(study_buffer_4326)
rivers_all <- sf::st_read(
  hydrorivers_path,
  wkt_filter = sf::st_as_text(sf::st_as_sfc(bbox)),
  quiet = TRUE
)
cat("Loaded", nrow(rivers_all), "river segments in bbox\n")

# Spatial clip
rivers_all <- sf::st_transform(rivers_all, 32642)
rivers_extended <- rivers_all[
  sf::st_intersects(rivers_all, study_buffer, sparse = FALSE)[, 1], ]

cat("After spatial clip:", nrow(rivers_extended), "river segments\n")

sf::st_write(rivers_extended,
             "data/processed/HydroRIVERS_extended_GB_Chitral.gpkg",
             delete_dsn = TRUE,
             quiet = TRUE)

