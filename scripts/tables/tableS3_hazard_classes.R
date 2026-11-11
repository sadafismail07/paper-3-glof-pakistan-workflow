# Hazard classes

library(terra)
library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

SCENARIOS <- list(
  L27 = list(
    depth = "hecras_models/L27_Shisper_v2/L27_Mid/Depth (Max).L27_terrain_v2.L27_terrain_v2.tif",
    vel   = "hecras_models/L27_Shisper_v2/L27_Mid/Velocity (Max).L27_terrain_v2.L27_terrain_v2.tif"
  ),
  L29 = list(
    depth = "hecras_models/L29_Passu_v2/L29_Mid/Depth (Max).L29_terrain_v2.L29_terrain_v2.tif",
    vel   = "hecras_models/L29_Passu_v2/L29_Mid/Velocity (Max).L29_terrain_v2.L29_terrain_v2.tif"
  )
)

lake_label <- c(L27 = "L27 Shisper", L29 = "L29 Passu")

osm_buildings <- st_read("data/processed/OSM_buildings_GB_Chitral.gpkg", quiet = TRUE)
osm_roads     <- st_read("data/processed/OSM_roads_GB_Chitral.gpkg",     quiet = TRUE)
osm_bridges   <- st_read("data/processed/OSM_bridges_GB_Chitral.gpkg",   quiet = TRUE)
ghs_pop       <- rast("data/raw/GHS_POP_2025_GB_Chitral.tif")
worldpop      <- rast("data/raw/WorldPop_2020_GB_Chitral.tif")
ghs_built     <- rast("data/raw/GHS_BUILT_S_2030_GB_Chitral.tif")

classify_H <- function(d, v) {
  dv <- d * v
  h  <- rast(d); values(h) <- 6
  h[dv <= 4.0 & d <= 4.0 & v <= 4.0] <- 5
  h[dv <= 1.0 & d <= 2.0 & v <= 2.0] <- 4
  h[dv <= 0.6 & d <= 1.2 & v <= 2.0] <- 3
  h[dv <= 0.6 & d <= 0.5 & v <= 2.0] <- 2
  h[dv <= 0.3 & d <= 0.3 & v <= 2.0] <- 1
  h[is.na(d)] <- NA
  h
}

exposure_for_class <- function(h_class, class_val, cell_size) {
  mask <- terra::ifel(h_class == class_val, 1, NA)
  n_cells <- sum(terra::values(mask) == 1, na.rm = TRUE)
  area_km2 <- round(n_cells * cell_size / 1e6, 3)

  if (n_cells == 0) {
    return(data.frame(
      `Area (km²)` = 0, `Population (GHS)` = 0, `Population (WP)` = 0,
      `Population (mean)` = 0, `Buildings (n)` = 0, `Roads (km)` = 0,
      `Bridges (n)` = 0, `Built-up area (m²)` = 0, check.names = FALSE
    ))
  }

  poly <- terra::as.polygons(mask, dissolve = TRUE) |> sf::st_as_sf() |> sf::st_make_valid()
  poly_osm <- sf::st_transform(poly, sf::st_crs(osm_buildings))

  pop_ghs <- terra::extract(ghs_pop, terra::vect(sf::st_transform(poly, terra::crs(ghs_pop))),
                             fun = "sum", na.rm = TRUE)[, 2]
  pop_wp  <- terra::extract(worldpop, terra::vect(sf::st_transform(poly, terra::crs(worldpop))),
                             fun = "sum", na.rm = TRUE)[, 2]
  pop_ghs <- ifelse(is.na(pop_ghs), 0, pop_ghs)
  pop_wp  <- ifelse(is.na(pop_wp),  0, pop_wp)

  built <- terra::extract(ghs_built, terra::vect(sf::st_transform(poly, terra::crs(ghs_built))),
                           fun = "sum", na.rm = TRUE)[, 2]
  built <- ifelse(is.na(built), 0, built)

  n_buildings <- nrow(sf::st_filter(osm_buildings, poly_osm, .predicate = sf::st_intersects))

  road_hits <- sf::st_filter(osm_roads, poly_osm, .predicate = sf::st_intersects)
  road_km <- 0
  if (nrow(road_hits) > 0) {
    clipped <- suppressWarnings(sf::st_intersection(sf::st_geometry(road_hits), poly_osm))
    road_km <- round(as.numeric(sum(sf::st_length(clipped))) / 1000, 3)
  }

  n_bridges <- nrow(sf::st_filter(osm_bridges, poly_osm, .predicate = sf::st_intersects))

  data.frame(
    `Area (km²)` = area_km2,
    `Population (GHS)` = round(pop_ghs, 0),
    `Population (WP)` = round(pop_wp, 0),
    `Population (mean)` = round((pop_ghs + pop_wp) / 2, 0),
    `Buildings (n)` = n_buildings,
    `Roads (km)` = road_km,
    `Bridges (n)` = n_bridges,
    `Built-up area (m²)` = round(built, 0),
    check.names = FALSE
  )
}

result_rows <- list()
for (lid in names(SCENARIOS)) {
  cat("Processing", lid, "...\n")
  depth <- rast(SCENARIOS[[lid]]$depth)
  vel   <- rast(SCENARIOS[[lid]]$vel)
  if (!compareGeom(depth, vel, stopOnError = FALSE)) {
    vel <- resample(vel, depth, method = "bilinear")
  }
  h_class   <- classify_H(depth, vel)
  cell_size <- prod(res(depth))

  for (k in 1:6) {
    row <- exposure_for_class(h_class, k, cell_size)
    row <- cbind(Lake = as.character(lake_label[lid]), Class = paste0("H", k), row)
    result_rows[[paste(lid, k)]] <- row
  }
}

tableS3 <- do.call(rbind, result_rows)
rownames(tableS3) <- NULL

stopifnot(
  nrow(tableS3) == 12,
  all(tableS3$`Area (km²)` >= 0),
  all(tableS3$`Roads (km)` >= 0)
)
cat("Check passed.\n")

tableS3 <- tableS3 |>
  dplyr::mutate(
    `Area (km²)`  = sprintf("%.3f", `Area (km²)`),
    `Roads (km)`  = sprintf("%.3f", `Roads (km)`)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS3$`Area (km²)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS3$`Roads (km)`))
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS3, "figures/tableS3_hazard_classes.csv", row.names = FALSE)
cat("Saved: figures/tableS3_hazard_classes.csv (", nrow(tableS3), "rows )\n")
