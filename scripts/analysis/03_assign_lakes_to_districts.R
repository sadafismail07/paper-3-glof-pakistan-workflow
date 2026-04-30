# 03_assign_lakes_to_districts.R
# Spatially join lake inventory to study area districts.
# Flags lakes outside the study area as out-of-scope but retained
# for transparency.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)

lakes <- sf::st_read(
  "data/processed/lakes_vulnerable_GB_Chitral.gpkg",
  quiet = TRUE
)

study_area <- sf::st_read(
  "data/processed/study_area_districts.gpkg",
  quiet = TRUE
)

# Spatial join
lakes_assigned <- sf::st_join(
  lakes, study_area, join = sf::st_intersects
)

# Flag in-study-area vs out-of-scope
lakes_assigned <- lakes_assigned |>
  dplyr::mutate(
    in_study_area       = !is.na(adm2_name),
    out_of_scope_reason = ifelse(
      !in_study_area,
      "Coordinate falls outside GB+Chitral study area",
      NA
    )
  )

cat("In-study-area lakes:", sum(lakes_assigned$in_study_area), "\n")
cat("Out-of-scope lakes :", sum(!lakes_assigned$in_study_area), "\n")

# Save full + analysis subset
sf::st_write(
  lakes_assigned,
  "data/processed/lakes_with_district.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

lakes_analysis <- lakes_assigned |> dplyr::filter(in_study_area)
sf::st_write(
  lakes_analysis,
  "data/processed/lakes_analysis_set.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

cat("\nSaved:\n")
cat("  data/processed/lakes_with_district.gpkg (full)\n")
cat("  data/processed/lakes_analysis_set.gpkg  (in-study-area only)\n")

