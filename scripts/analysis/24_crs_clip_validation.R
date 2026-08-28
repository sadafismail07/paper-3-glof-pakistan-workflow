# 24_crs_clip_validation.R
#
# Reviewer 3, comment R3.4: R3 asked us to explicitly confirm that (a) the H4
# hazard raster and the OSM buildings layer share a common CRS before the
# building-exposure overlay, and (b) the clip/intersection is not silently
# returning an empty result (which would produce an artificially low
# building count that looks like a "real" undercount but is actually a bug).
#
# This script does NOT re-run the exposure pipeline (that's
# 19_h4_hazard_mapping.R + 21_reviewer_response_h4_and_volume.R). It just
# takes the H4 raster and OSM buildings layer already on disk for the L27
# Shisper Mid (baseline) scenario -- the scenario behind Table 4's Hassanabad
# numbers -- and explicitly checks CRS + intersection non-emptiness,
# independent of the main pipeline code, as a standalone audit.
#
# Output: prints a plain-English PASS/FAIL to the console, and writes
# data/processed/crs_clip_validation.csv with the numbers for the record.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(terra)
library(sf)

cat("========== R3.4 CRS / clip-intersection check (L27 Shisper, Mid baseline) ==========\n\n")

# --- 1. Load the H4 raster (already-thresholded, binary) and OSM buildings ---
h4_raster <- terra::rast("data/processed/h4_zones/L27_Mid_H4.tif")
osm_buildings <- sf::st_read("data/processed/OSM_buildings_GB_Chitral.gpkg", quiet = TRUE)

cat("--- Step 1: raw CRS of each input, BEFORE any reprojection ---\n")
h4_crs_orig  <- terra::crs(h4_raster, describe = TRUE)
osm_crs_orig <- sf::st_crs(osm_buildings)

cat("H4 raster CRS   : EPSG:", h4_crs_orig$code, " (", h4_crs_orig$name, ")\n", sep = "")
cat("OSM buildings CRS: EPSG:", osm_crs_orig$epsg, " (", osm_crs_orig$input, ")\n", sep = "")

same_crs_before <- !is.na(h4_crs_orig$code) && !is.na(osm_crs_orig$epsg) &&
  as.character(h4_crs_orig$code) == as.character(osm_crs_orig$epsg)

if (same_crs_before) {
  cat("=> Inputs are ALREADY in the same CRS on disk.\n\n")
} else {
  cat("=> Inputs are in DIFFERENT CRSs on disk -- this is normal (raster in a\n")
  cat("   projected UTM zone for area calculations, OSM vector data delivered in\n")
  cat("   WGS84 EPSG:4326) and is exactly why the pipeline explicitly reprojects\n")
  cat("   the H4 polygon to match OSM's CRS before intersecting (see below).\n\n")
}

# --- 2. Reproduce the exact reprojection step used in the real pipeline ---
# (21_reviewer_response_h4_and_volume.R, compute_h4_exposure(): h4_poly is
# st_transform()'d to sf::st_crs(osm_buildings) before any st_filter/
# st_intersects call. This block repeats that step standalone so it can be
# checked in isolation from the rest of that script.)

cat("--- Step 2: polygonise H4 raster and explicitly reproject to OSM's CRS ---\n")
h4_poly <- terra::as.polygons(h4_raster, dissolve = TRUE) |> sf::st_as_sf()
cat("H4 polygon CRS before transform: EPSG:", sf::st_crs(h4_poly)$epsg, "\n")

h4_poly <- sf::st_transform(h4_poly, sf::st_crs(osm_buildings))
h4_poly <- sf::st_make_valid(h4_poly)
cat("H4 polygon CRS AFTER transform : EPSG:", sf::st_crs(h4_poly)$epsg, "\n")

crs_match_after <- sf::st_crs(h4_poly) == sf::st_crs(osm_buildings)
cat("CRS of H4 polygon now matches OSM buildings CRS: ", crs_match_after, "\n\n")

# --- 3. Confirm the polygon itself is valid and has non-zero area ---
h4_area_m2 <- as.numeric(sf::st_area(sf::st_union(h4_poly)))
cat("--- Step 3: sanity-check the H4 polygon geometry ---\n")
cat("H4 polygon area after reprojection:", round(h4_area_m2 / 1e6, 4), "km2\n")
cat("(Table 4 reports the L27 Mid baseline H4 area as 9.000 km2 -- if the\n")
cat("number above is wildly different, the reprojection or the raster path\n")
cat("above is wrong and the rest of this check is not meaningful.)\n\n")

# --- 4. The actual clip: does the intersection come back empty? ---
cat("--- Step 4: intersect H4 polygon with OSM buildings, check for emptiness ---\n")
n_osm_total <- nrow(osm_buildings)
bld_hits <- sf::st_filter(osm_buildings, h4_poly, .predicate = sf::st_intersects)
n_hits <- nrow(bld_hits)

cat("OSM buildings layer total features (whole study area):", n_osm_total, "\n")
cat("OSM buildings intersecting the H4 zone                :", n_hits, "\n\n")

clip_is_empty <- n_hits == 0

if (clip_is_empty) {
  cat("*** FAIL: the intersection returned ZERO buildings. This is EXACTLY the\n")
  cat("failure mode R3 was worried about (CRS mismatch or a bounding-box error\n")
  cat("silently producing an empty clip). Do not report the low building count\n")
  cat("as a genuine OSM-undercount until this is investigated further. ***\n\n")
} else {
  cat("PASS: the intersection is non-empty (", n_hits, " buildings), and Table 4\n", sep = "")
  cat("already reports 2 buildings for this exact scenario (L27 Mid baseline).\n")
  if (n_hits != 2) {
    cat("NOTE: this script found", n_hits, "buildings, which does not equal the 2\n")
    cat("reported in Table 4. That is worth reconciling before citing this check\n")
    cat("as confirmation -- it may mean this script's H4 raster or buildings file\n")
    cat("is not byte-for-byte the one Table 4's number came from (e.g. an updated\n")
    cat("OSM extract since the original run), not necessarily a CRS/clip bug.\n\n")
  } else {
    cat("This matches Table 4 exactly, which is strong evidence the low count is a\n")
    cat("genuine reflection of OSM's sparse building coverage in this area, not a\n")
    cat("silent empty-clip bug.\n\n")
  }
}

# --- 5. Write the record ---
out <- data.frame(
  check = c("h4_raster_epsg_before", "osm_epsg", "h4_polygon_epsg_after_transform",
            "crs_match_after_transform", "h4_polygon_area_km2", "osm_buildings_total_n",
            "osm_buildings_intersecting_h4_n", "clip_returned_empty",
            "matches_table4_reported_n"),
  value = c(as.character(h4_crs_orig$code), as.character(osm_crs_orig$epsg),
            as.character(sf::st_crs(h4_poly)$epsg), crs_match_after,
            round(h4_area_m2 / 1e6, 4), n_osm_total, n_hits, clip_is_empty,
            n_hits == 2),
  stringsAsFactors = FALSE
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(out, "data/processed/crs_clip_validation.csv", row.names = FALSE)
cat("Saved: data/processed/crs_clip_validation.csv\n")
cat("\n========== DONE ==========\n")
cat("Use the console output (or the CSV) to draft the R3.4 response sentence\n")
cat("from the actual result.\n")
