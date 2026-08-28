# ============================================================
# 23_table03_data_extraction.R
#
# ANALYSIS SCRIPT (raw data -> canonical CSV). Per the project's
# revised convention: this script is the ONLY place that touches
# raw simulation outputs (HEC-RAS rasters, breach hydrograph time
# series) and primary parameter tables for Table 3. Downstream,
# scripts/tables/table03_simulation_results.R reads ONLY
# the CSV this script writes -- it does not open a raster or a
# hydrograph file itself.
#
# WHY THIS EXISTS (history, for anyone auditing this later):
#   v1/v2 of the table03 script read pre-aggregated summary CSVs
#   (all_simulations_combined_v2.csv, table3_extracted.csv) whose
#   L29 Peak-Q values were traced to a duplicate/conflicting
#   hydrograph file (see below).
#   v3 rewrote Peak Q/Volume to come from raw hydrographs directly
#   inside the table script, which was correct -- but its raster
#   depth/velocity/wetted-area extraction (a new terra-based
#   function) gave wrong numbers for 16 of 20 scenarios (confirmed
#   by an independent Python/rasterio read of the same rasters,
#   which reproduced the ORIGINAL/manuscript numbers exactly, not
#   v3's). Root cause not fully isolated (suspected terra::values()
#   / dplyr::rowwise() interaction on this machine's R install), but
#   rather than debug it further, this version reuses the raster
#   extraction logic from scripts/analysis/20_simulation_results.R
#   verbatim (values()[,1] vector indexing, 0.05 m wet threshold,
#   positive-velocity-only filter for the velocity stats) --
#   that logic is the one that PRODUCED data/processed/
#   table3_extracted.csv, which an independent Python raster read
#   (rasterio, no terra involved) confirmed is correct for L29 Low
#   (depth 12.50 m / vel p95 2.13 / vel max 3.67, exact match).
#   CONFIRMED (2026-08-26): rerunning with this logic reproduced
#   table3_extracted.csv's depth/velocity/wetted-area values for
#   all 14 core scenarios exactly.
#
# Peak Q -- max() of the raw breach hydrograph time series
#   (data/processed/*breach_hydrograph*.csv). Explicitly uses
#   L29_Passu_breach_hydrograph_Mid.csv, NOT L29_breach_hydrograph_
#   Mid.csv -- the latter is a STALE duplicate (max 1,258.1 vs the
#   correct 1,261.3) and is also the parent of the stale pre-scaled
#   bulk hydrographs (L29_breach_hydrograph_Mid_bulk15/20.csv).
#   Bulked-scenario Peak Q is computed here as bulking_factor x the
#   corrected Mid baseline -- never read from a pre-scaled bulk
#   hydrograph file for either lake.
#
# Volume -- data/processed/lake_volume_scenarios.csv for Low/Mid/
#   High; bulked rows = Mid volume x bulking factor; partial-
#   drainage rows = Mid volume x the released fraction reported by
#   Muhammad et al. (2021) for the 2019/2020 events (45%, 28%) --
#   a literature constant, not derivable from any file in this repo.
#
# Depth / velocity (p95, max) / wetted area -- from the HEC-RAS
#   "Depth (Max)" / "Velocity (Max)" GeoTIFFs, for all 20 scenarios,
#   using the same extraction logic as 20_simulation_results.R.
#
# Output: data/processed/table03_master_data.csv (raw/unformatted;
#   table03_simulation_results.R does the display formatting)
# ============================================================

library(dplyr)
library(readr)
library(terra)

setwd("C:/Users/sadaf/Documents/PPR3")

WET_THRESHOLD_M <- 0.05   # matches 20_simulation_results.R exactly

# ---- 1. Peak Q for every scenario, from raw hydrograph time series --------

hyd_max <- function(path, qcol = "Q_m3_s") {
  d <- read_csv(path, show_col_types = FALSE)
  if (!qcol %in% names(d)) qcol <- names(d)[ncol(d)]  # partial-drainage files use plain "Q"
  round(max(d[[qcol]], na.rm = TRUE), 1)
}

q_low_L29  <- hyd_max("data/processed/L29_breach_hydrograph_Low.csv")
q_mid_L29  <- hyd_max("data/processed/L29_Passu_breach_hydrograph_Mid.csv")   # corrected file
q_high_L29 <- hyd_max("data/processed/L29_breach_hydrograph_High.csv")
q_low_L27  <- hyd_max("data/processed/L27_breach_hydrograph_Low.csv")
q_mid_L27  <- hyd_max("data/processed/L27_breach_hydrograph_Mid.csv")
q_high_L27 <- hyd_max("data/processed/L27_breach_hydrograph_High.csv")

q_p45_L27 <- hyd_max("data/processed/L27_hyd_partial45.csv", qcol = "Q")
q_p28_L27 <- hyd_max("data/processed/L27_hyd_partial28.csv", qcol = "Q")

cat("Peak Q from raw hydrographs -- L29: Low=", q_low_L29, " Mid=", q_mid_L29,
    " High=", q_high_L29, "\n")
cat("Peak Q from raw hydrographs -- L27: Low=", q_low_L27, " Mid=", q_mid_L27,
    " High=", q_high_L27, "\n")
cat("Cross-check: rejected stale file L29_breach_hydrograph_Mid.csv gives",
    hyd_max("data/processed/L29_breach_hydrograph_Mid.csv"),
    "-- confirming why it is not used above.\n")

q_bulk <- function(q_mid, factor) round(q_mid * factor, 1)

# ---- 2. Volume, from the primary parameter table ---------------------------

vol_src <- read_csv("data/processed/lake_volume_scenarios.csv", show_col_types = FALSE)
get_vol <- function(lk, scn) vol_src$volume_m3[vol_src$lake_id == lk & vol_src$scenario == scn]

frac_45 <- 0.45   # Muhammad et al. (2021) 2019/2020 released-fraction constants
frac_28 <- 0.28

# ---- 3. Depth / velocity (p95, max) / wetted area, from the HEC-RAS -------
# ---- rasters -- logic copied verbatim from 20_simulation_results.R --------

raster_stats <- function(depth_path, vel_path) {
  d <- terra::rast(depth_path)
  v <- terra::rast(vel_path)

  if (!terra::compareGeom(d, v, stopOnError = FALSE)) {
    v <- terra::resample(v, d, method = "near")   # "near", matching 20_simulation_results.R
  }

  cell_area_m2 <- prod(terra::res(d))
  d_vec <- terra::values(d)[, 1]
  v_vec <- terra::values(v)[, 1]

  wet   <- !is.na(d_vec) & d_vec > WET_THRESHOLD_M
  v_wet <- v_vec[wet]
  v_wet <- v_wet[!is.na(v_wet) & v_wet > 0]

  list(
    depth_max  = round(max(d_vec[wet], na.rm = TRUE), 2),
    vel_p95    = round(as.numeric(quantile(v_wet, 0.95, na.rm = TRUE)), 2),
    vel_max    = round(max(v_wet, na.rm = TRUE), 2),
    wetted_km2 = round(sum(wet) * cell_area_m2 / 1e6, 3)
  )
}

l27_dp <- function(folder) file.path("hecras_models/L27_Shisper", folder, "Depth (Max).Terrain.L27_terrain.tif")
l27_vp <- function(folder) file.path("hecras_models/L27_Shisper", folder, "Velocity (Max).Terrain.L27_terrain.tif")
l29_dp <- function(folder) file.path("hecras_models/L29_Passu", folder, "Depth (Max).Terrain.Terrain.L29_terrain_final.tif")
l29_vp <- function(folder) file.path("hecras_models/L29_Passu", folder, "Velocity (Max).Terrain.Terrain.L29_terrain_final.tif")

# ---- 4. Row definitions ----------------------------------------------------

rows <- tribble(
  ~lake_id, ~scenario_code,        ~folder,           ~manning_n, ~bulking_factor, ~vol_m3,                        ~peak_q_m3s,
  "L29", "Low",                    "L29_Low",         0.05,       1.0,             get_vol("L29","Low"),           q_low_L29,
  "L29", "Mid",                    "L29_Mid",         0.05,       1.0,             get_vol("L29","Mid"),           q_mid_L29,
  "L29", "High",                  "L29_High",        0.05,       1.0,             get_vol("L29","High"),          q_high_L29,
  "L29", "Mid_n04",                "L29_Mid_n04",     0.04,       1.0,             get_vol("L29","Mid"),           q_mid_L29,
  "L29", "Mid_n06",                "L29_Mid_n06",     0.06,       1.0,             get_vol("L29","Mid"),           q_mid_L29,
  "L29", "Mid_b15",                "L29_Mid_b15",     0.05,       1.5,             get_vol("L29","Mid") * 1.5,     q_bulk(q_mid_L29, 1.5),
  "L29", "Mid_b20",                "L29_Mid_b20",     0.05,       2.0,             get_vol("L29","Mid") * 2.0,     q_bulk(q_mid_L29, 2.0),
  "L29", "Mid_b15_n010",           "L29_b15_n010",    0.10,       1.5,             get_vol("L29","Mid") * 1.5,     q_bulk(q_mid_L29, 1.5),
  "L29", "Mid_b20_n010",           "L29_b20_n010",    0.10,       2.0,             get_vol("L29","Mid") * 2.0,     q_bulk(q_mid_L29, 2.0),
  "L27", "Low",                    "L27_Low",         0.05,       1.0,             get_vol("L27","Low"),           q_low_L27,
  "L27", "Mid",                    "L27_Mid",         0.05,       1.0,             get_vol("L27","Mid"),           q_mid_L27,
  "L27", "High",                   "L27_High",        0.05,       1.0,             get_vol("L27","High"),          q_high_L27,
  "L27", "Mid_n04",                "L27_Mid_n04",     0.04,       1.0,             get_vol("L27","Mid"),           q_mid_L27,
  "L27", "Mid_n06",                "L27_Mid_n06",     0.06,       1.0,             get_vol("L27","Mid"),           q_mid_L27,
  "L27", "Mid_b15",                "L27_Mid_bulk1.5", 0.05,       1.5,             get_vol("L27","Mid") * 1.5,     q_bulk(q_mid_L27, 1.5),
  "L27", "Mid_b20",                "L27_Mid_bulk2.0", 0.05,       2.0,             get_vol("L27","Mid") * 2.0,     q_bulk(q_mid_L27, 2.0),
  "L27", "Mid_b15_n010",           "L27_b15_n010",    0.10,       1.5,             get_vol("L27","Mid") * 1.5,     q_bulk(q_mid_L27, 1.5),
  "L27", "Mid_b20_n010",           "L27_b20_n010",    0.10,       2.0,             get_vol("L27","Mid") * 2.0,     q_bulk(q_mid_L27, 2.0),
  "L27", "p45",                    "L27_p45",         0.05,       NA,              get_vol("L27","Mid") * frac_45, q_p45_L27,
  "L27", "p28",                    "L27_p28",         0.05,       NA,              get_vol("L27","Mid") * frac_28, q_p28_L27
)

# ---- 5. Process each row's rasters -----------------------------------------

table03_master <- rows |>
  rowwise() |>
  mutate(
    stats = list(
      if (lake_id == "L29") raster_stats(l29_dp(folder), l29_vp(folder))
      else                  raster_stats(l27_dp(folder), l27_vp(folder))
    )
  ) |>
  ungroup() |>
  mutate(
    volume_m3    = vol_m3,
    depth_max_m  = sapply(stats, `[[`, "depth_max"),
    vel_p95_ms   = sapply(stats, `[[`, "vel_p95"),
    vel_max_ms   = sapply(stats, `[[`, "vel_max"),
    wetted_km2   = sapply(stats, `[[`, "wetted_km2")
  ) |>
  select(lake_id, scenario_code, manning_n, bulking_factor, volume_m3,
         peak_q_m3s, depth_max_m, vel_p95_ms, vel_max_ms, wetted_km2)

cat("\nAll 20 rows processed from rasters + hydrographs:\n")
print(as.data.frame(table03_master), row.names = FALSE)

# ---- 6. Sanity checks -------------------------------------------------------
# NOTE: peak_q_m3s intentionally keeps raw decimal precision in this master
# CSV (e.g. 1261.3, not 1261) -- table03_simulation_results.R rounds
# it for display. Checks below round() before comparing for that reason.
q_l29_mid <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L29" & table03_master$scenario_code == "Mid"])
q_l29_b15 <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L29" & table03_master$scenario_code == "Mid_b15"])
q_l29_b20 <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L29" & table03_master$scenario_code == "Mid_b20"])
q_l27_mid <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L27" & table03_master$scenario_code == "Mid"])

stopifnot(
  nrow(table03_master) == 20,
  q_l29_mid == 1261,
  q_l29_b15 == 1892,
  q_l27_mid == 12546,
  # depth/vel/wetted for the 14 core scenarios should reproduce table3_extracted.csv,
  # independently confirmed correct against the raw rasters via Python/rasterio
  abs(table03_master$depth_max_m[table03_master$lake_id == "L29" & table03_master$scenario_code == "Low"] - 12.50) < 0.05,
  abs(table03_master$vel_max_ms[table03_master$lake_id == "L29" & table03_master$scenario_code == "Low"] - 3.67) < 0.05
)
cat("\nSanity check passed: 20 rows; Peak Q traced to corrected hydrographs;",
    "depth/velocity for L29 Low confirmed matching the independently-verified",
    "raster read (12.50 m / 3.67 m/s).\n")

if (q_l29_b20 != 2522) {
  cat("NOTE: L29 x2.0 bulk Peak Q rounds to", q_l29_b20,
      "here (raw hydrograph max x2, rounded to nearest integer) vs 2,522 in",
      "the manuscript -- a 1-unit rounding-path difference (1261.3 x 2 =",
      "2522.6, which rounds up), not a data error. Decide which convention",
      "the paper should use before finalising this row.\n")
}

dir.create("data/processed", showWarnings = FALSE)
write.csv(table03_master, "data/processed/table03_master_data.csv", row.names = FALSE)
cat("\nSaved: data/processed/table03_master_data.csv (", nrow(table03_master), "rows )\n")
