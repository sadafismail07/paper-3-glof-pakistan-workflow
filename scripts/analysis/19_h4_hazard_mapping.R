# 19_h4_hazard_mapping.R
# Apply Westoby 2015 high-hazard (H4) threshold to all 14 HEC-RAS scenarios.
# H4 criterion: depth > 1.5 m OR (depth x velocity) > 0.7 m^2/s
# Outputs: binary H4 rasters per scenario + summary CSV.

setwd("C:/Users/sadaf/Documents/PPR3")
library(terra)
library(dplyr)

# Locate all result rasters
l29_files <- list.files("hecras_models/L29_Passu",
                         pattern = "\\.tif$",
                         recursive = TRUE,
                         full.names = TRUE)
l27_files <- list.files("hecras_models/L27_Shisper",
                         pattern = "\\.tif$",
                         recursive = TRUE,
                         full.names = TRUE)
all_rasters <- c(l29_files, l27_files)
result_rasters <- all_rasters[grepl("Depth|Velocity", basename(all_rasters))]

# Build scenario file mapping
scenario_files <- data.frame(
  path = result_rasters,
  folder = basename(dirname(result_rasters)),
  type = ifelse(grepl("Depth", basename(result_rasters)), "depth", "velocity"),
  stringsAsFactors = FALSE
)

scenarios <- scenario_files |>
  dplyr::group_by(folder) |>
  dplyr::summarize(
    depth_path = path[type == "depth"][1],
    velocity_path = path[type == "velocity"][1],
    .groups = "drop"
  ) |>
  as.data.frame()

h4_dir <- "data/processed/h4_zones"
if (!dir.exists(h4_dir)) dir.create(h4_dir, recursive = TRUE)

compute_h4 <- function(depth_path, velocity_path, scenario_label) {
  depth <- terra::rast(depth_path)
  velocity <- terra::rast(velocity_path)

  if (!terra::compareGeom(depth, velocity, stopOnError = FALSE)) {
    velocity <- terra::resample(velocity, depth, method = "bilinear")
  }

  dv <- depth * velocity
  h4_mask <- (depth > 1.5) | (dv > 0.7)
  h4 <- terra::ifel(h4_mask, 1, NA)

  cell_size <- prod(terra::res(depth))
  n_h4 <- sum(terra::values(h4) == 1, na.rm = TRUE)
  h4_area_km2 <- n_h4 * cell_size / 1e6
  n_wetted <- sum(terra::values(depth > 0) == 1, na.rm = TRUE)
  total_wetted_km2 <- n_wetted * cell_size / 1e6

  out_path <- file.path(h4_dir, paste0(scenario_label, "_H4.tif"))
  terra::writeRaster(h4, out_path, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE"))

  data.frame(
    scenario = scenario_label,
    total_wetted_km2 = round(total_wetted_km2, 3),
    h4_area_km2 = round(h4_area_km2, 3),
    h4_fraction = round(h4_area_km2 / total_wetted_km2, 3),
    stringsAsFactors = FALSE
  )
}

results_list <- list()
for (i in 1:nrow(scenarios)) {
  scn <- scenarios$folder[i]
  cat("Processing", scn, "\n")
  results_list[[scn]] <- compute_h4(scenarios$depth_path[i],
                                     scenarios$velocity_path[i],
                                     scn)
}

summary_df <- do.call(rbind, results_list)
summary_df$lake_id <- ifelse(grepl("L27", summary_df$scenario), "L27", "L29")
write.csv(summary_df, "data/processed/h4_hazard_summary.csv",
          row.names = FALSE)

