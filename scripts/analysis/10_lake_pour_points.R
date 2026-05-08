# 10_lake_pour_points.R
# Snap lake centroids to nearest HydroRIVERS segment to identify pour points
# (where lake outflow enters the river network).

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(dplyr)

# --- Load layers ---
lakes <- sf::st_read("data/processed/lakes_polygons_verified.gpkg",
                     quiet = TRUE)
working_set <- sf::st_read("data/processed/working_set_with_tiers.gpkg",
                           quiet = TRUE)
rivers <- sf::st_read("data/processed/HydroRIVERS_main_GB_Chitral.gpkg",
                      quiet = TRUE)

# Project to UTM 42N
lakes <- sf::st_transform(lakes, 32642)
working_set <- sf::st_transform(working_set, 32642)
rivers <- sf::st_transform(rivers, 32642)

cat("Verified lakes:", nrow(lakes), "\n")
cat("River segments:", nrow(rivers), "\n")

# --- Get lake centroids ---
all_lake_ids <- lakes$lake_id

get_lake_centroid <- function(lid, lakes_sf) {
  lake <- lakes_sf |> dplyr::filter(lake_id == lid)
  if (nrow(lake) > 0 && !sf::st_is_empty(lake)) {
    return(sf::st_centroid(sf::st_geometry(lake))[[1]])
  }
  return(NULL)
}

centroid_list <- list()
for (lid in all_lake_ids) {
  pt <- get_lake_centroid(lid, lakes)
  if (!is.null(pt)) centroid_list[[lid]] <- pt
}

# Override L30 with literature coordinate (Shimshal Valley Khurdopin)
l30_lit <- sf::st_transform(
  sf::st_sfc(sf::st_point(c(75.47, 36.34)), crs = 4326),
  32642
)
centroid_list[["L30"]] <- l30_lit[[1]]

centroid_sf <- sf::st_sf(
  lake_id = names(centroid_list),
  geometry = sf::st_sfc(centroid_list, crs = 32642)
)

# --- Snap to nearest river ---
nearest_idx <- sf::st_nearest_feature(centroid_sf, rivers)
nearest_rivers <- rivers[nearest_idx, ]

pour_lines <- sf::st_nearest_points(centroid_sf, nearest_rivers,
                                    pairwise = TRUE)
pour_pts_list <- lapply(pour_lines, function(g) {
  pts <- sf::st_cast(sf::st_sfc(g), "POINT")
  pts[[2]]  # river-side endpoint
})

pour_pts_sf <- sf::st_sf(
  lake_id = centroid_sf$lake_id,
  hydroriver_id = nearest_rivers$HYRIV_ID,
  next_down = nearest_rivers$NEXT_DOWN,
  geometry = sf::st_sfc(pour_pts_list, crs = 32642)
)

sf::st_write(pour_pts_sf,
             "data/processed/lake_pour_points.gpkg",
             delete_dsn = TRUE,
             quiet = TRUE)

cat("Saved pour points for", nrow(pour_pts_sf), "lakes\n")

