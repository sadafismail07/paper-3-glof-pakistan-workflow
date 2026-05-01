# 09_process_dem.R
# Load Copernicus 30m DEM exported from GEE, reproject to UTM 42N,
# generate hillshade and slope rasters.

setwd("C:/Users/sadaf/Documents/PPR3")
library(terra)

# --- Load DEM ---
dem_files <- list.files("data/raw",
                        pattern = "Copernicus_DEM_GB_Chitral.*\\.tif$",
                        full.names = TRUE,
                        ignore.case = TRUE)

if (length(dem_files) == 1) {
  dem_wgs84 <- terra::rast(dem_files[1])
} else if (length(dem_files) > 1) {
  rasters <- lapply(dem_files, terra::rast)
  dem_wgs84 <- do.call(terra::mosaic, c(rasters, fun = "mean"))
} else {
  stop("No DEM files found in data/raw/")
}

cat("WGS84 stats (m):\n")
print(terra::global(dem_wgs84, c("min", "max", "mean"), na.rm = TRUE))

# --- Reproject to UTM 42N ---
dem_utm <- terra::project(dem_wgs84, "EPSG:32642",
                          method = "bilinear", res = 30)
terra::writeRaster(dem_utm, "data/processed/DEM_UTM42N.tif",
                   overwrite = TRUE,
                   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))

# --- Hillshade ---
slope_rad  <- terra::terrain(dem_utm, v = "slope",  unit = "radians")
aspect_rad <- terra::terrain(dem_utm, v = "aspect", unit = "radians")
hillshade  <- terra::shade(slope_rad, aspect_rad, angle = 45, direction = 315)
terra::writeRaster(hillshade, "data/processed/DEM_hillshade_UTM42N.tif",
                   overwrite = TRUE,
                   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))

# --- Slope (degrees) ---
slope_deg <- terra::terrain(dem_utm, v = "slope", unit = "degrees")
terra::writeRaster(slope_deg, "data/processed/DEM_slope_UTM42N.tif",
                   overwrite = TRUE,
                   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))

cat("\nDEM processing complete.\n")

