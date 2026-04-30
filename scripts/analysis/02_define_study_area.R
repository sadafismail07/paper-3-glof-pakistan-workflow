# 02_define_study_area.R
# Build the study area boundary from OCHA Pakistan administrative
# boundaries: all 14 GB districts plus Upper and Lower Chitral.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)

pak_districts <- sf::st_read(
  "data/raw/pak_admin_boundaries/pak_admin2.shp",
  quiet = TRUE
)

# Filter to GB + Chitral; defensive grepl handles spelling variations
gb_pattern <- "Gilgit.?Baltistan"

study_area <- pak_districts |>
  dplyr::filter(
    grepl(gb_pattern, adm1_name) |
    grepl("Chitral", adm2_name, ignore.case = TRUE)
  ) |>
  dplyr::mutate(
    study_unit = ifelse(
      grepl(gb_pattern, adm1_name),
      "GB",
      "Chitral"
    )
  ) |>
  dplyr::select(adm2_name, adm2_pcode, adm1_name, study_unit, geometry)

# Make valid (handles small topology issues from boundary data)
study_area <- sf::st_make_valid(study_area)

# Save 3 versions
sf::st_write(
  study_area,
  "data/processed/study_area_districts.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

study_area_boundary <- study_area |>
  dplyr::summarise(geometry = sf::st_union(geometry))

sf::st_write(
  study_area_boundary,
  "data/processed/study_area_boundary.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

study_area_buffered <- study_area_boundary |>
  sf::st_transform(crs = 32642) |>
  sf::st_buffer(dist = 5000)

sf::st_write(
  study_area_buffered,
  "data/processed/study_area_buffered_5km.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

cat("Saved 3 study area files.\n")
cat("Districts in study area:", nrow(study_area), "\n")

