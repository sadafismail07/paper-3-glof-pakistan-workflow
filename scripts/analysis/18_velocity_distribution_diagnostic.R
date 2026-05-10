# 18_velocity_distribution_diagnostic.R
# Compute velocity distribution statistics from HEC-RAS Velocity (Max) rasters.
# Reports 95th percentile as primary metric to handle localized mesh artifacts.

setwd("C:/Users/sadaf/Documents/PPR3")
library(terra)

velocity_diagnostic <- function(raster_path, label = NULL) {
  if (!file.exists(raster_path)) {
    cat("File not found:", raster_path, "\n")
    return(NULL)
  }

  vel <- terra::rast(raster_path)
  vals <- terra::values(vel)
  vals <- vals[!is.na(vals) & vals > 0]

  if (is.null(label)) label <- basename(raster_path)
  cat("=== Velocity diagnostic:", label, "===\n")
  cat("Wetted cells:", length(vals), "\n")

  results <- list(
    label = label,
    n_cells = length(vals),
    median = median(vals),
    mean = mean(vals),
    p95 = as.numeric(quantile(vals, 0.95)),
    p99 = as.numeric(quantile(vals, 0.99)),
    max = max(vals),
    pct_above_10 = sum(vals >= 10) / length(vals) * 100,
    pct_above_14 = sum(vals >= 14) / length(vals) * 100
  )

  cat(sprintf("  Median:           %.2f m/s\n", results$median))
  cat(sprintf("  95th percentile:  %.2f m/s (recommended for paper)\n", results$p95))
  cat(sprintf("  99th percentile:  %.2f m/s\n", results$p99))
  cat(sprintf("  Max:              %.2f m/s\n", results$max))
  cat(sprintf("  %% above 10 m/s:   %.2f%%\n", results$pct_above_10))
  cat(sprintf("  %% above 14 m/s:   %.2f%%\n", results$pct_above_14))
  cat("\n")

  invisible(results)
}

# Example usage:
# vel_path <- "hecras_models/L27_Shisper/L27_Mid/Velocity (Max).Terrain.L27_terrain.tif"
# diag <- velocity_diagnostic(vel_path, "L27 Mid")

