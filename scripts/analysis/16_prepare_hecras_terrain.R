# 16_prepare_hecras_terrain.R
# Clip DEM to top-2 lake corridors with 500m buffer for HEC-RAS terrain.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(terra)
library(dplyr)

corridors <- sf::st_read("data/processed/lake_corridors_scored.gpkg",
                         quiet = TRUE)
dem <- terra::rast("data/processed/DEM_UTM42N.tif")

top2 <- corridors |> dplyr::filter(lake_id %in% c("L29", "L27"))
top2 <- sf::st_transform(top2, 32642)
top2_buffered <- sf::st_buffer(top2, dist = 500)

if (!dir.exists("data/processed/hecras_terrain")) {
  dir.create("data/processed/hecras_terrain", recursive = TRUE)
}

for (i in 1:nrow(top2_buffered)) {
  lid <- top2_buffered$lake_id[i]
  poly <- top2_buffered[i, ]

  dem_clip <- terra::crop(dem, terra::vect(poly))
  dem_clip <- terra::mask(dem_clip, terra::vect(poly))

  out_path <- sprintf("data/processed/hecras_terrain/%s_terrain.tif", lid)
  terra::writeRaster(dem_clip, out_path, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
}

cat("HEC-RAS terrain rasters saved\n")

