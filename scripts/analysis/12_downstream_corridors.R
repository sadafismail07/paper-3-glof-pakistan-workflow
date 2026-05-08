# 12_downstream_corridors.R
# Trace 50 km downstream from each lake pour point via NEXT_DOWN connectivity
# and buffer the path by 1 km to delineate the GLOF exposure corridor.
# Approach follows Taylor et al. (2023) consequence-based corridor methodology.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(dplyr)

pour_pts <- sf::st_read("data/processed/lake_pour_points.gpkg",
                        quiet = TRUE)
rivers <- sf::st_read("data/processed/HydroRIVERS_extended_GB_Chitral.gpkg",
                      quiet = TRUE)
rivers <- sf::st_transform(rivers, 32642)

# Trace function
trace_downstream <- function(start_hyriv_id, rivers_sf,
                             max_distance_km = 50) {
  current_id <- start_hyriv_id
  visited <- c()
  cumulative_distance <- 0
  while (!is.na(current_id) && current_id != 0 &&
         cumulative_distance < max_distance_km * 1000) {
    if (current_id %in% visited) break
    visited <- c(visited, current_id)
    seg <- rivers_sf |> dplyr::filter(HYRIV_ID == current_id)
    if (nrow(seg) == 0) break
    seg_length_m <- as.numeric(sf::st_length(seg))
    cumulative_distance <- cumulative_distance + seg_length_m
    current_id <- seg$NEXT_DOWN[1]
  }
  return(list(hyriv_ids = visited,
              total_distance_m = cumulative_distance))
}

cat("Tracing 50 km downstream from", nrow(pour_pts), "lakes...\n\n")

corridors_list <- list()
for (i in 1:nrow(pour_pts)) {
  lid <- pour_pts$lake_id[i]
  start_id <- pour_pts$hydroriver_id[i]
  trace <- trace_downstream(start_id, rivers, max_distance_km = 50)

  if (length(trace$hyriv_ids) == 0) next

  segs <- rivers |> dplyr::filter(HYRIV_ID %in% trace$hyriv_ids)
  combined_line <- sf::st_union(segs)
  buffer <- sf::st_buffer(combined_line, dist = 1000)
  total_km <- round(trace$total_distance_m / 1000, 1)

  cat(sprintf("%s: %2d segments, %5.1f km\n",
              lid, length(trace$hyriv_ids), total_km))

  corridors_list[[lid]] <- sf::st_sf(
    lake_id = lid,
    n_segments = length(trace$hyriv_ids),
    distance_km = total_km,
    geometry = sf::st_sfc(buffer, crs = 32642)
  )
}

corridors_sf <- do.call(rbind, corridors_list)

sf::st_write(corridors_sf,
             "data/processed/lake_downstream_corridors.gpkg",
             delete_dsn = TRUE,
             quiet = TRUE)

cat("\nSaved corridors for", nrow(corridors_sf), "lakes\n")

