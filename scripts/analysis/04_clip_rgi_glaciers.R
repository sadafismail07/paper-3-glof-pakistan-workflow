# 04_clip_rgi_glaciers.R
# Load RGI 7.0 Region 14 (South Asia West) and clip to study area.
# Reports surge-type breakdown and total glacier area.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)

glaciers_full <- sf::st_read(
  "data/raw/RGI2000-v7.0-G-14_south_asia_west/RGI2000-v7.0-G-14_south_asia_west.shp",
  quiet = TRUE
)

study_area <- sf::st_read(
  "data/processed/study_area_boundary.gpkg",
  quiet = TRUE
)

# Make geometries valid
study_area    <- sf::st_make_valid(study_area)
glaciers_full <- sf::st_make_valid(glaciers_full)

# Stage 1: bbox filter (fast)
study_bbox <- sf::st_as_sfc(sf::st_bbox(study_area))
in_bbox <- sf::st_intersects(glaciers_full, study_bbox, sparse = FALSE)[, 1]
glaciers_bbox <- glaciers_full[in_bbox, ]

# Stage 2: exact intersection
glaciers_study <- sf::st_intersection(glaciers_bbox, study_area)

cat("Glaciers in study area:", nrow(glaciers_study), "\n")
cat("Total glacier area :", round(sum(glaciers_study$area_km2), 1), "km^2\n")

cat("\nSurge-type breakdown (0=none, 1=possible, 2=probable, 3=observed):\n")
print(table(glaciers_study$surge_type))

sf::st_write(
  glaciers_study,
  "data/processed/RGI7_glaciers_GB_Chitral.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

