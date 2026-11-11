# Collect simulation results into one table

library(dplyr)
library(readr)
library(terra)

setwd("C:/Users/sadaf/Documents/PPR3")

WET_THRESHOLD_M <- 0.05   # matches 20_simulation_results.R exactly

hyd_max <- function(path, qcol = "Q_m3_s") {
  d <- read_csv(path, show_col_types = FALSE)
  if (!qcol %in% names(d)) qcol <- names(d)[ncol(d)]  # partial-drainage files use plain "Q"
  round(max(d[[qcol]], na.rm = TRUE), 1)
}

q_low_L29  <- hyd_max("data/processed/L29_breach_hydrograph_Low.csv")
q_mid_L29  <- hyd_max("data/processed/L29_breach_hydrograph_Mid.csv")
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

q_bulk <- function(q_mid, factor) round(q_mid * factor, 1)

vol_src <- read_csv("data/processed/lake_volume_scenarios.csv", show_col_types = FALSE)
get_vol <- function(lk, scn) vol_src$volume_m3[vol_src$lake_id == lk & vol_src$scenario == scn]

frac_45 <- 0.45
frac_28 <- 0.28

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

l27_dp <- function(folder) file.path("hecras_models/L27_Shisper_v2", folder, "Depth (Max).L27_terrain_v2.L27_terrain_v2.tif")
l27_vp <- function(folder) file.path("hecras_models/L27_Shisper_v2", folder, "Velocity (Max).L27_terrain_v2.L27_terrain_v2.tif")
l29_dp <- function(folder) file.path("hecras_models/L29_Passu_v2", folder, "Depth (Max).L29_terrain_v2.L29_terrain_v2.tif")
l29_vp <- function(folder) file.path("hecras_models/L29_Passu_v2", folder, "Velocity (Max).L29_terrain_v2.L29_terrain_v2.tif")

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

q_l29_mid <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L29" & table03_master$scenario_code == "Mid"])
q_l29_b15 <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L29" & table03_master$scenario_code == "Mid_b15"])
q_l29_b20 <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L29" & table03_master$scenario_code == "Mid_b20"])
q_l27_mid <- round(table03_master$peak_q_m3s[table03_master$lake_id == "L27" & table03_master$scenario_code == "Mid"])

stopifnot(
  nrow(table03_master) == 20,
  q_l29_mid == 1258,
  q_l29_b15 == 1887,
  q_l27_mid == 12546,
  all(is.finite(table03_master$depth_max_m) & table03_master$depth_max_m > 0),
  all(is.finite(table03_master$vel_max_ms)  & table03_master$vel_max_ms  > 0),
  all(is.finite(table03_master$wetted_km2)  & table03_master$wetted_km2  > 0)
)
cat("Check passed.\n")

dir.create("data/processed", showWarnings = FALSE)
write.csv(table03_master, "data/processed/table03_master_data.csv", row.names = FALSE)
cat("\nSaved: data/processed/table03_master_data.csv (", nrow(table03_master), "rows )\n")
