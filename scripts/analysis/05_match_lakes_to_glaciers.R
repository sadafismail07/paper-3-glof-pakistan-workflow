# 05_match_lakes_to_glaciers.R
# Spatially match each working-set lake to its RGI parent glacier.
# Direct intersection first, nearest-neighbor for unmatched lakes.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)

lakes      <- sf::st_read("data/processed/lakes_analysis_set.gpkg",
                          quiet = TRUE)
glaciers   <- sf::st_read("data/processed/RGI7_glaciers_GB_Chitral.gpkg",
                          quiet = TRUE)

working_set <- lakes |> dplyr::filter(vulnerability >= 4)

# Direct match by intersection
lakes_matched <- sf::st_join(
  working_set,
  glaciers |> dplyr::select(rgi_id, glac_name, area_km2,
                            surge_type, term_type, zmin_m,
                            zmax_m, zmed_m, slope_deg),
  join = sf::st_intersects
)

n_matched <- sum(!is.na(lakes_matched$rgi_id))
cat("Direct matches:", n_matched, "of", nrow(working_set), "\n")

# Nearest neighbor for unmatched
unmatched_idx <- is.na(lakes_matched$rgi_id)
if (any(unmatched_idx)) {
  unmatched   <- working_set[unmatched_idx, ]
  nearest_idx <- sf::st_nearest_feature(unmatched, glaciers)
  nearest     <- glaciers[nearest_idx, ]
  distances   <- sf::st_distance(unmatched, nearest, by_element = TRUE)

  cat("\nNearest-neighbor matches for unmatched lakes:\n")
  print(data.frame(
    lake_id      = unmatched$lake_id,
    glacier_name = unmatched$glacier_name,
    rgi_id       = nearest$rgi_id,
    rgi_name     = nearest$glac_name,
    distance_m   = round(as.numeric(distances), 0)
  ))
}

sf::st_write(
  lakes_matched,
  "data/processed/lakes_working_set_with_rgi.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

