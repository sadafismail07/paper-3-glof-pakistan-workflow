
# ════════════════════════════════════════════════════════════════
# 20_simulation_results.R
# PURPOSE: L27_Shisper_simulation_results_VERIFIED.csv
#          L29_Passu_simulation_results_VERIFIED.csv
#          ALL_simulation_results_VERIFIED.csv
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
# ════════════════════════════════════════════════════════════════

# verify_all_metrics.R
# Extracts depth, velocity, and wetted area metrics from HEC-RAS rasters
# Covers both L27_Shisper and L29_Passu

library(terra)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

# -------------------------------------------------------------
# 1. Configure paths and thresholds
# -------------------------------------------------------------

WET_THRESHOLD_M <- 0.05  # 5 cm noise filter

# Scenario table — folder names now match what list.files() actually found
scenarios <- data.frame(
  lake = c(
    rep("L27", 7),
    rep("L29", 7)
  ),
  scenario = c(
    "Low", "Mid_baseline", "High", "Mid_n04", "Mid_n06", "Mid_bulk1.5", "Mid_bulk2.0",
    "Low", "Mid_baseline", "High", "Mid_n04", "Mid_n06", "Mid_b15",    "Mid_b20"
  ),
  # These folder names are taken directly from the list.files() output
  folder = c(
    "L27_Low", "L27_Mid", "L27_High", "L27_Mid_n04", "L27_Mid_n06",
    "L27_Mid_bulk1.5", "L27_Mid_bulk2.0",
    "L29_Low", "L29_Mid", "L29_High", "L29_Mid_n04", "L29_Mid_n06",
    "L29_Mid_b15", "L29_Mid_b20"
  ),
  base_dir = c(
    rep("hecras_models/L27_Shisper", 7),
    rep("hecras_models/L29_Passu",  7)
  ),
  stringsAsFactors = FALSE
)

# -------------------------------------------------------------
# 2. Helper: find depth or velocity raster inside a folder
# -------------------------------------------------------------

find_raster <- function(folder, kind, base_dir) {
  folder_path <- file.path(base_dir, folder)

  if (!dir.exists(folder_path)) {
    warning("Folder not found: ", folder_path)
    return(NA_character_)
  }

  all_tifs <- list.files(folder_path,
                         pattern = "\.tif$",
                         full.names = TRUE,
                         recursive = TRUE,
                         ignore.case = TRUE)

  if (length(all_tifs) == 0) {
    warning("No .tif files in: ", folder_path)
    return(NA_character_)
  }

  # Match on kind ("Depth" or "Velocity") — case-insensitive
  hits <- all_tifs[grepl(kind, basename(all_tifs), ignore.case = TRUE)]

  # Prefer files that also contain "max" in the name
  max_hits <- hits[grepl("max", basename(hits), ignore.case = TRUE)]
  if (length(max_hits) > 0) hits <- max_hits

  if (length(hits) == 0) {
    warning("No ", kind, " raster found in: ", folder_path)
    return(NA_character_)
  }

  hits[1]
}

# -------------------------------------------------------------
# 3. Extract metrics from one scenario row
# -------------------------------------------------------------

extract_metrics <- function(row) {
  depth_file <- find_raster(row$folder, "Depth",    row$base_dir)
  vel_file   <- find_raster(row$folder, "Velocity", row$base_dir)

  base_result <- data.frame(
    lake             = row$lake,
    scenario         = row$scenario,
    folder           = row$folder,
    peak_depth_m     = NA_real_,
    peak_vel_p95     = NA_real_,
    peak_vel_max     = NA_real_,
    wetted_area_km2  = NA_real_,
    wetted_cells     = NA_integer_,
    depth_file       = depth_file,
    vel_file         = vel_file,
    stringsAsFactors = FALSE
  )

  if (is.na(depth_file) || is.na(vel_file)) return(base_result)

  cat(sprintf("  [%s | %s]
    depth: %s
    vel:   %s
",
              row$lake, row$scenario,
              basename(depth_file), basename(vel_file)))

  d <- terra::rast(depth_file)
  v <- terra::rast(vel_file)

  if (!terra::compareGeom(d, v, stopOnError = FALSE)) {
    warning("Geometry mismatch for ", row$folder, " — resampling velocity to match depth")
    v <- terra::resample(v, d, method = "near")
  }

  cell_area_m2 <- prod(terra::res(d))
  d_vec <- terra::values(d)[, 1]
  v_vec <- terra::values(v)[, 1]

  wet   <- !is.na(d_vec) & d_vec > WET_THRESHOLD_M
  v_wet <- v_vec[wet]
  v_wet <- v_wet[!is.na(v_wet) & v_wet > 0]

  base_result$peak_depth_m    <- round(max(d_vec[wet], na.rm = TRUE), 2)
  base_result$peak_vel_p95    <- round(quantile(v_wet, 0.95, na.rm = TRUE), 2)
  base_result$peak_vel_max    <- round(max(v_wet, na.rm = TRUE), 2)
  base_result$wetted_area_km2 <- round(sum(wet) * cell_area_m2 / 1e6, 4)
  base_result$wetted_cells    <- sum(wet)

  base_result
}

# -------------------------------------------------------------
# 4. Run for all scenarios
# -------------------------------------------------------------

cat("Extracting metrics from HEC-RAS rasters...

")

results <- do.call(rbind, lapply(
  seq_len(nrow(scenarios)),
  function(i) extract_metrics(scenarios[i, ])
))

# -------------------------------------------------------------
# 5. Quick sanity check — flag any all-NA rows
# -------------------------------------------------------------

cat("
=== Extraction summary ===
")
print(results[, c("lake", "scenario", "peak_depth_m",
                  "peak_vel_p95", "wetted_area_km2", "wetted_cells")])

missing <- results[is.na(results$peak_depth_m), c("lake", "scenario", "folder",
                                                    "depth_file", "vel_file")]
if (nrow(missing) > 0) {
  cat("
⚠ MISSING RASTERS for:
")
  print(missing)
} else {
  cat("
✓ All rasters found and extracted successfully.
")
}

# -------------------------------------------------------------
# 6. Compare against existing CSV (if present)
# -------------------------------------------------------------

csv_path <- "data/processed/simulation_results.csv"

if (file.exists(csv_path)) {
  current <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Merge on lake + scenario
  comparison <- merge(
    current[, c("lake", "scenario", "peak_depth_m",
                "peak_vel_p95", "peak_vel_max", "wetted_area_km2")],
    results[,  c("lake", "scenario", "peak_depth_m",
                 "peak_vel_p95", "peak_vel_max", "wetted_area_km2")],
    by = c("lake", "scenario"),
    suffixes = c("_current", "_verified")
  )

  for (col in c("peak_depth_m", "peak_vel_p95", "peak_vel_max", "wetted_area_km2")) {
    pct <- 100 * (comparison[[paste0(col, "_verified")]] -
                  comparison[[paste0(col, "_current")]]) /
      comparison[[paste0(col, "_current")]]
    comparison[[paste0(col, "_pct_diff")]] <- round(pct, 1)
  }

  cat("
=== Comparison vs existing CSV ===
")
  print(comparison[, c("lake", "scenario",
                        "peak_depth_m_current",  "peak_depth_m_verified",  "peak_depth_m_pct_diff",
                        "wetted_area_km2_current","wetted_area_km2_verified","wetted_area_km2_pct_diff")])

  big <- comparison[abs(comparison$peak_depth_m_pct_diff)    > 5 |
                    abs(comparison$wetted_area_km2_pct_diff) > 5, ]
  if (nrow(big) > 0) {
    cat("
⚠ DISCREPANCIES > 5%:
")
    print(big[, c("lake", "scenario", "peak_depth_m_pct_diff", "wetted_area_km2_pct_diff")])
  }
} else {
  cat("
No existing CSV found at", csv_path, "— skipping comparison.
")
}

# -------------------------------------------------------------
# 7. Save verified results (one CSV per lake + a combined one)
# -------------------------------------------------------------

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write.csv(results[results$lake == "L27", ],
          "data/processed/L27_Shisper_simulation_results_VERIFIED.csv",
          row.names = FALSE)

write.csv(results[results$lake == "L29", ],
          "data/processed/L29_Passu_simulation_results_VERIFIED.csv",
          row.names = FALSE)

write.csv(results,
          "data/processed/ALL_simulation_results_VERIFIED.csv",
          row.names = FALSE)

cat("
Output saved:
")
cat("  data/processed/L27_Shisper_simulation_results_VERIFIED.csv
")
cat("  data/processed/L29_Passu_simulation_results_VERIFIED.csv
")
cat("  data/processed/ALL_simulation_results_VERIFIED.csv
")


