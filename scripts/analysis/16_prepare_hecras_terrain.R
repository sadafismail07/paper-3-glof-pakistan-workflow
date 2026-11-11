# Terrain rasters for HEC-RAS

setwd("C:/Users/sadaf/Documents/PPR3")
library(sf)
library(terra)

dem <- terra::rast("data/processed/DEM_UTM42N.tif")
if (!dir.exists("data/processed/hecras_terrain")) dir.create("data/processed/hecras_terrain", recursive = TRUE)

for (lid in c("L27", "L29")) {
  per <- sf::st_read(sprintf("hecras_models/v2_inputs/%s_perimeter_v2.shp", lid), quiet = TRUE)
  bb  <- sf::st_bbox(sf::st_buffer(per, 1000))
  clip <- terra::crop(dem, terra::ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))
  terra::writeRaster(clip, sprintf("data/processed/hecras_terrain/%s_terrain_v2.tif", lid),
                     overwrite = TRUE, NAflag = -9999,
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES"))
}
cat("HEC-RAS terrain rasters saved\n")
