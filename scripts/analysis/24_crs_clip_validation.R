# Overlay check of hazard raster and buildings

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(terra)
library(sf)

cat("========== CRS and clip-intersection check (L27 Shisper, mid baseline) ==========\n\n")

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

cat("--- Step 2: polygonise H4 raster and explicitly reproject to OSM's CRS ---\n")
h4_poly <- terra::as.polygons(h4_raster, dissolve = TRUE) |> sf::st_as_sf()
cat("H4 polygon CRS before transform: EPSG:", sf::st_crs(h4_poly)$epsg, "\n")

h4_poly <- sf::st_transform(h4_poly, sf::st_crs(osm_buildings))
h4_poly <- sf::st_make_valid(h4_poly)
cat("H4 polygon CRS AFTER transform : EPSG:", sf::st_crs(h4_poly)$epsg, "\n")

crs_match_after <- sf::st_crs(h4_poly) == sf::st_crs(osm_buildings)
cat("CRS of H4 polygon now matches OSM buildings CRS: ", crs_match_after, "\n\n")

h4_area_m2 <- as.numeric(sf::st_area(sf::st_union(h4_poly)))
cat("--- Step 3: check the H4 polygon geometry ---\n")
cat("H4 polygon area after reprojection:", round(h4_area_m2 / 1e6, 4), "km2\n")

cat("--- Step 4: intersect H4 polygon with OSM buildings, check for emptiness ---\n")
n_osm_total <- nrow(osm_buildings)
bld_hits <- sf::st_filter(osm_buildings, h4_poly, .predicate = sf::st_intersects)
n_hits <- nrow(bld_hits)

cat("OSM buildings layer total features (whole study area):", n_osm_total, "\n")
cat("OSM buildings intersecting the H4 zone                :", n_hits, "\n\n")

clip_is_empty <- n_hits == 0

t4 <- read.csv("figures/table04_h4_exposure.csv", check.names = FALSE)
n_table4 <- t4[t4$Lake == "L27 Shisper" & t4$Scenario == "Mid (baseline)", "Buildings (n)"]

if (clip_is_empty) {
  cat("FAIL: the intersection returned no buildings. Check the CRS of both layers\n")
  cat("and the extent of the H4 raster before using the building counts.\n\n")
} else {
  cat("PASS: the intersection is non-empty (", n_hits, " buildings); table value ",
      n_table4, ".\n\n", sep = "")
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
            n_hits == n_table4),
  stringsAsFactors = FALSE
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(out, "data/processed/crs_clip_validation.csv", row.names = FALSE)
cat("Saved: data/processed/crs_clip_validation.csv\n")
cat("\n========== DONE ==========\n")
