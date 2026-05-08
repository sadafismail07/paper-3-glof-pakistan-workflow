# 13_extract_population.R
# Extract per-corridor population sums from GHS-POP 2025 and WorldPop 2020.
# Both datasets reported for cross-comparison; mean used in priority scoring.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(terra)
library(dplyr)

corridors <- sf::st_read("data/processed/lake_downstream_corridors.gpkg",
                         quiet = TRUE)
corridors_4326 <- sf::st_transform(corridors, 4326)

ghs <- terra::rast("data/raw/GHS_POP_2025_GB_Chitral.tif")
worldpop <- terra::rast("data/raw/WorldPop_2020_GB_Chitral.tif")

extract_pop_sum <- function(corridors_sf, raster) {
  vec <- terra::vect(corridors_sf)
  vals <- terra::extract(raster, vec, fun = "sum", na.rm = TRUE)
  return(vals[, 2])
}

corridors_4326$pop_ghs2025 <- extract_pop_sum(corridors_4326, ghs)
corridors_4326$pop_worldpop2020 <- extract_pop_sum(corridors_4326, worldpop)
corridors_4326$pop_mean <- (corridors_4326$pop_ghs2025 +
                            corridors_4326$pop_worldpop2020) / 2

sf::st_write(corridors_4326,
             "data/processed/lake_corridors_with_population.gpkg",
             delete_dsn = TRUE,
             quiet = TRUE)

cat("Saved corridors with population estimates\n")
print(corridors_4326 |>
        sf::st_drop_geometry() |>
        dplyr::arrange(desc(pop_mean)) |>
        dplyr::select(lake_id, distance_km,
                      pop_ghs2025, pop_worldpop2020, pop_mean))

