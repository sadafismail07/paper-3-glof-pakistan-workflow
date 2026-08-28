# 21_h4_exposure_and_volume_conservation_audit.R
#
# Two independent jobs for the reviewer-response round:
#
#   PART A — H4 exposure for the four n=0.10 bulked scenarios that were run
#            (L27/L29 x bulk1.5/bulk2.0) but never extracted into
#            table04_h4_exposure.csv. Mirrors the logic already used in
#            19_h4_hazard_mapping.R (H4 = depth>1.5 OR depth*velocity>0.7)
#            and 13_extract_population.R (terra::extract fun="sum") --
#            this replicates the existing pipeline exactly, just applied
#            to the four scenarios missing from the CSV.
#
#   PART B — Volume-accounting / conservation error, read directly out of
#            the HEC-RAS plan HDF files, for every scenario, to get a
#            trustworthy current number for R3.33 rather than relying on
#            the possibly-stale "Vol. cons. (%)" column sitting in
#            figures/table03_simulation_results.csv. HEC-RAS 7.0's exact
#            internal HDF5 layout for this figure is not confirmed from
#            documentation and varies by version, so this part first
#            prints the HDF5 structure it finds, then tries a set of
#            candidate paths. Read the console output before trusting the
#            CSV it writes -- if none of the candidates match, the printed
#            structure shows which path to add instead.
#
# Outputs (both written to data/processed/):
#   h4_exposure_n010_scenarios.csv
#   volume_conservation_all_scenarios.csv
#
# Run this from the project root (C:/Users/sadaf/Documents/PPR3), or just
# open it in RStudio with that as the working directory — setwd() below
# handles it either way.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(terra)
library(sf)
library(dplyr)

# ══════════════════════════════════════════════════════════════════
# PART A — H4 exposure for the four n=0.10 bulked scenarios
# ══════════════════════════════════════════════════════════════════

cat("\n========== PART A: H4 exposure, n=0.10 bulked scenarios ==========\n\n")

# Scenario definitions: raster folders match what is already on disk.
n010_scenarios <- data.frame(
  lake     = c("L27", "L27", "L29", "L29"),
  scenario = c("Mid (\u00d71.5 bulk, n=0.10)", "Mid (\u00d72.0 bulk, n=0.10)",
               "Mid (\u00d71.5 bulk, n=0.10)", "Mid (\u00d72.0 bulk, n=0.10)"),
  depth_path = c(
    "hecras_models/L27_Shisper/L27_b15_n010/Depth (Max).Terrain.L27_terrain.tif",
    "hecras_models/L27_Shisper/L27_b20_n010/Depth (Max).Terrain.L27_terrain.tif",
    "hecras_models/L29_Passu/L29_b15_n010/Depth (Max).Terrain.Terrain.L29_terrain_final.tif",
    "hecras_models/L29_Passu/L29_b20_n010/Depth (Max).Terrain.Terrain.L29_terrain_final.tif"
  ),
  vel_path = c(
    "hecras_models/L27_Shisper/L27_b15_n010/Velocity (Max).Terrain.L27_terrain.tif",
    "hecras_models/L27_Shisper/L27_b20_n010/Velocity (Max).Terrain.L27_terrain.tif",
    "hecras_models/L29_Passu/L29_b15_n010/Velocity (Max).Terrain.Terrain.L29_terrain_final.tif",
    "hecras_models/L29_Passu/L29_b20_n010/Velocity (Max).Terrain.Terrain.L29_terrain_final.tif"
  ),
  stringsAsFactors = FALSE
)

# Population, built-up, and OSM layers — same sources as the rest of the
# pipeline (13_extract_population.R and the corridor-exposure step).
ghs_pop   <- terra::rast("data/raw/GHS_POP_2025_GB_Chitral.tif")
worldpop  <- terra::rast("data/raw/WorldPop_2020_GB_Chitral.tif")
ghs_built <- terra::rast("data/raw/GHS_BUILT_S_2030_GB_Chitral.tif")

osm_buildings <- sf::st_read("data/processed/OSM_buildings_GB_Chitral.gpkg", quiet = TRUE)
osm_roads     <- sf::st_read("data/processed/OSM_roads_GB_Chitral.gpkg",     quiet = TRUE)
osm_bridges   <- sf::st_read("data/processed/OSM_bridges_GB_Chitral.gpkg",   quiet = TRUE)

compute_h4_exposure <- function(lake, scenario, depth_path, vel_path) {

  cat(sprintf("Processing %s %s ...\n", lake, scenario))

  depth <- terra::rast(depth_path)
  velocity <- terra::rast(vel_path)

  if (!terra::compareGeom(depth, velocity, stopOnError = FALSE)) {
    velocity <- terra::resample(velocity, depth, method = "bilinear")
  }

  # --- H4 mask: identical criterion to 19_h4_hazard_mapping.R ---
  dv <- depth * velocity
  h4_mask <- (depth > 1.5) | (dv > 0.7)
  h4 <- terra::ifel(h4_mask, 1, NA)

  cell_size <- prod(terra::res(depth))
  n_h4 <- sum(terra::values(h4) == 1, na.rm = TRUE)
  h4_area_km2 <- round(n_h4 * cell_size / 1e6, 3)

  n_wetted <- sum(terra::values(depth > 0) == 1, na.rm = TRUE)
  total_wetted_km2 <- round(n_wetted * cell_size / 1e6, 3)

  # --- Polygonise H4 zone for vector overlay (buildings/roads/bridges) ---
  h4_poly <- terra::as.polygons(h4, dissolve = TRUE) |> sf::st_as_sf()
  h4_poly <- sf::st_transform(h4_poly, sf::st_crs(osm_buildings))
  h4_poly <- sf::st_make_valid(h4_poly)

  # --- Population: simple cell-count sum, matching Section 3.10's method ---
  pop_ghs <- terra::extract(ghs_pop, terra::vect(sf::st_transform(h4_poly, terra::crs(ghs_pop))),
                             fun = "sum", na.rm = TRUE)[, 2]
  pop_wp  <- terra::extract(worldpop, terra::vect(sf::st_transform(h4_poly, terra::crs(worldpop))),
                             fun = "sum", na.rm = TRUE)[, 2]
  pop_ghs <- ifelse(is.na(pop_ghs), 0, pop_ghs)
  pop_wp  <- ifelse(is.na(pop_wp),  0, pop_wp)
  pop_mean <- (pop_ghs + pop_wp) / 2

  # --- Built-up area (GHS-BUILT-S), summed within H4 zone ---
  built <- terra::extract(ghs_built, terra::vect(sf::st_transform(h4_poly, terra::crs(ghs_built))),
                           fun = "sum", na.rm = TRUE)[, 2]
  built <- ifelse(is.na(built), 0, built)

  # --- Buildings: count OSM building footprints intersecting the H4 zone ---
  bld_hits <- sf::st_filter(osm_buildings, h4_poly, .predicate = sf::st_intersects)
  n_buildings <- nrow(bld_hits)

  # --- Roads: length (km) of OSM road segments clipped to the H4 zone ---
  road_hits <- sf::st_filter(osm_roads, h4_poly, .predicate = sf::st_intersects)
  road_km <- 0
  if (nrow(road_hits) > 0) {
    clipped <- suppressWarnings(sf::st_intersection(sf::st_geometry(road_hits), h4_poly))
    road_km <- round(sum(sf::st_length(clipped)) / 1000, 2)
  }

  # --- Bridges: count OSM bridge points within the H4 zone ---
  bridge_hits <- sf::st_filter(osm_bridges, h4_poly, .predicate = sf::st_intersects)
  n_bridges <- nrow(bridge_hits)

  data.frame(
    Lake = lake,
    Scenario = scenario,
    `H4 area (km2)` = h4_area_km2,
    `Total wetted (km2)` = total_wetted_km2,
    `Population (GHS)` = round(pop_ghs, 0),
    `Population (WP)` = round(pop_wp, 0),
    `Population (mean)` = round(pop_mean, 0),
    `Buildings (n)` = n_buildings,
    `Roads (km)` = road_km,
    `Bridges (n)` = n_bridges,
    `Built-up area (m2)` = round(built, 0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

h4_results <- do.call(rbind, lapply(seq_len(nrow(n010_scenarios)), function(i) {
  row <- n010_scenarios[i, ]
  compute_h4_exposure(row$lake, row$scenario, row$depth_path, row$vel_path)
}))

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(h4_results, "data/processed/h4_exposure_n010_scenarios.csv", row.names = FALSE)

cat("\n--- H4 exposure results (n=0.10 bulked scenarios) ---\n")
print(h4_results)
cat("\nSaved: data/processed/h4_exposure_n010_scenarios.csv\n")


# ══════════════════════════════════════════════════════════════════
# PART B — Volume accounting / conservation error from plan HDF files
# ══════════════════════════════════════════════════════════════════

cat("\n\n========== PART B: Volume accounting from plan HDF files ==========\n\n")
cat("NOTE: HEC-RAS 7.0's exact internal HDF5 layout for this is not fully\n")
cat("confirmed. This script explores the file structure first and prints it,\n")
cat("then tries several candidate locations. Check the printed\n")
cat("structure/attributes against the final CSV before trusting it.\n\n")

if (!requireNamespace("rhdf5", quietly = TRUE)) {
  cat("Package 'rhdf5' not found. Install it with:\n")
  cat('  if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")\n')
  cat('  BiocManager::install("rhdf5")\n')
  stop("Install rhdf5 and re-run PART B.")
}
library(rhdf5)

# Find every plan HDF file across both lake project folders.
plan_hdfs <- c(
  list.files("hecras_models/L27_Shisper", pattern = "\\.p[0-9]+\\.hdf$", full.names = TRUE),
  list.files("hecras_models/L29_Passu",   pattern = "\\.p[0-9]+\\.hdf$", full.names = TRUE)
)
cat(sprintf("Found %d plan HDF files.\n\n", length(plan_hdfs)))

# --- Helper: get the human-readable plan title/short ID so we can match
#     each HDF file to a scenario name, instead of guessing from the p-number.
get_plan_title <- function(hdf_path) {
  title <- tryCatch({
    a <- rhdf5::h5readAttributes(hdf_path, "/Plan Data/Plan Information")
    if (!is.null(a[["Plan Title"]])) a[["Plan Title"]]
    else if (!is.null(a[["Plan ShortID"]])) a[["Plan ShortID"]]
    else NA_character_
  }, error = function(e) NA_character_)
  if (is.na(title)) {
    title <- tryCatch({
      a <- rhdf5::h5readAttributes(hdf_path, "/")
      if (!is.null(a[["Plan Title"]])) a[["Plan Title"]] else NA_character_
    }, error = function(e) NA_character_)
  }
  title
}

# --- Helper: recursively list every group/dataset path in the file so we
#     can spot anything with "Volume" or "Accounting" or "Error" in it.
find_volume_paths <- function(hdf_path) {
  tryCatch({
    contents <- rhdf5::h5ls(hdf_path, recursive = TRUE)
    full_paths <- file.path(contents$group, contents$name)
    full_paths <- gsub("^\\.", "", full_paths)
    hits <- full_paths[grepl("volume|accounting|error", full_paths, ignore.case = TRUE)]
    unique(hits)
  }, error = function(e) character(0))
}

# --- Helper: try a set of known/plausible candidate locations for the
#     volume error figure. Extend this list if the printed structure
#     (below) shows a different path.
candidate_paths <- c(
  "/Results/Unsteady/Summary/Volume Accounting",
  "/Results/Unsteady/Summary",
  "/Results/Unsteady/Summary/Volume Accounting/Volume Accounting"
)

extract_volume_error <- function(hdf_path) {
  # First try dataset reads at the candidate paths.
  for (p in candidate_paths) {
    val <- tryCatch({
      d <- rhdf5::h5read(hdf_path, p)
      d
    }, error = function(e) NULL)
    if (!is.null(val)) {
      return(list(path = p, value = val, mode = "dataset"))
    }
  }
  # Then try reading it as an attribute on the Unsteady Summary group.
  for (p in c("/Results/Unsteady/Summary", "/Results/Unsteady")) {
    val <- tryCatch({
      a <- rhdf5::h5readAttributes(hdf_path, p)
      a
    }, error = function(e) NULL)
    if (!is.null(val)) {
      err_names <- grep("volume|error", names(val), ignore.case = TRUE, value = TRUE)
      if (length(err_names) > 0) {
        return(list(path = p, value = val[err_names], mode = "attribute"))
      }
    }
  }
  list(path = NA, value = NA, mode = "not_found")
}

volume_results <- list()

for (hdf in plan_hdfs) {
  cat("----------------------------------------------------------------\n")
  cat("File:", hdf, "\n")

  title <- get_plan_title(hdf)
  cat("  Plan title/ID:", ifelse(is.na(title), "(not found)", title), "\n")

  vol_paths <- find_volume_paths(hdf)
  if (length(vol_paths) > 0) {
    cat("  Candidate Volume/Accounting/Error paths found in file:\n")
    for (vp in vol_paths) cat("    -", vp, "\n")
  } else {
    cat("  (no path containing 'volume', 'accounting', or 'error' found by h5ls)\n")
  }

  res <- extract_volume_error(hdf)
  cat("  Extraction attempt:", res$mode,
      if (res$mode != "not_found") paste("at", res$path) else "", "\n")

  volume_results[[hdf]] <- data.frame(
    hdf_file = hdf,
    plan_title = title,
    extraction_mode = res$mode,
    extraction_path = ifelse(is.na(res$path), NA_character_, res$path),
    raw_value = paste(utils::capture.output(print(res$value)), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

volume_df <- do.call(rbind, volume_results)
write.csv(volume_df, "data/processed/volume_conservation_all_scenarios.csv", row.names = FALSE)

cat("\n\n=== SUMMARY: volume_conservation_all_scenarios.csv ===\n")
print(volume_df[, c("plan_title", "extraction_mode", "extraction_path")])
cat("\nSaved: data/processed/volume_conservation_all_scenarios.csv\n")
cat("\nThe 'raw_value' column has the FULL raw extraction dump — open the CSV\n")
cat("in a text editor (not Excel, it may be wide/nested) and check it against\n")
cat("HEC-RAS's own Results > Volume Accounting report on screen for one or\n")
cat("two well-known scenarios, e.g. L27 Mid baseline.\n")
cat("If extraction_mode is 'not_found' for everything, the printed\n")
cat("'Candidate Volume/Accounting/Error paths' output above shows what to\n")
cat("add to the candidate_paths list.\n")

cat("\n\n========== DONE ==========\n")
cat("Two files written to data/processed/:\n")
cat("  1. h4_exposure_n010_scenarios.csv\n")
cat("  2. volume_conservation_all_scenarios.csv\n")
cat("Fold these into Tables 3/4 and the R3.33 text fix.\n")
