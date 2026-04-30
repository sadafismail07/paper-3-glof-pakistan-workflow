# 07_clip_hydrosheds.R
# Clip HydroRIVERS and HydroBASINS to study area.
# Outputs: full river network, main-rivers-only, L8 and L9 sub-basins.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)

study_area <- sf::st_read("data/processed/study_area_boundary.gpkg",
                          quiet = TRUE)
study_area <- sf::st_make_valid(study_area)
study_bbox <- sf::st_as_sfc(sf::st_bbox(study_area))

# --- HydroRIVERS ---
rivers_full <- sf::st_read(
  "data/raw/HydroRIVERS_v10_as/HydroRIVERS_v10_as.shp",
  quiet = TRUE
)
rivers_full <- sf::st_make_valid(rivers_full)

in_bbox <- sf::st_intersects(rivers_full, study_bbox, sparse = FALSE)[, 1]
rivers_bbox <- rivers_full[in_bbox, ]
rivers_study <- sf::st_intersection(rivers_bbox, study_area)

sf::st_write(rivers_study,
             "data/processed/HydroRIVERS_GB_Chitral.gpkg",
             delete_dsn = TRUE, quiet = TRUE)

rivers_main <- rivers_study |> dplyr::filter(ORD_STRA >= 4)
sf::st_write(rivers_main,
             "data/processed/HydroRIVERS_main_GB_Chitral.gpkg",
             delete_dsn = TRUE, quiet = TRUE)

cat("Rivers: ", nrow(rivers_study), " total, ", nrow(rivers_main), " main\n")

# --- HydroBASINS ---
basin_files <- list(
  L8 = "data/raw/hybas_as_L8/hybas_as_lev08_v1c.shp",
  L9 = "data/raw/hybas_as_L9/hybas_as_lev09_v1c.shp"
)

for (level in names(basin_files)) {
  basins <- sf::st_read(basin_files[[level]], quiet = TRUE)
  basins <- sf::st_make_valid(basins)

  in_bbox <- sf::st_intersects(basins, study_bbox, sparse = FALSE)[, 1]
  basins_bbox <- basins[in_bbox, ]
  basins_intersect <- basins_bbox[
    sf::st_intersects(basins_bbox, study_area, sparse = FALSE)[, 1], ]

  sf::st_write(basins_intersect,
               paste0("data/processed/HydroBASINS_", level,
                      "_GB_Chitral.gpkg"),
               delete_dsn = TRUE, quiet = TRUE)

  cat("HydroBASINS", level, ":", nrow(basins_intersect), "sub-basins\n")
}

