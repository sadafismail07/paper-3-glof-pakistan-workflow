# ============================================================
# 22_identification_distances.R
#
# Recomputes Table S4's Identification column: geodesic distance
# from each working-set lake's coordinate to the nearest matching
# same-named point in the literature, replacing the manually
# measured data/processed/identification_distances.csv.
#
# Source: data/processed/literature_reference_points.gpkg. Lakes
# are matched by name to specific candidate point(s), not by blind
# nearest-neighbour. Only L15, L21, L24, L27, L29 have a candidate
# in that file; L20 is added separately (see L20_IDENTIFICATION
# below); the remaining 11 lakes keep "No published coordinate
# found".
#
# L27 Shisper carries two candidates (glacier coordinate vs. lake
# coordinate, different sources) -- both are reported; see the
# FLAG cat() message at runtime.
#
# L29 Passu's only candidate is tagged source "Bhambri 2020", but
# that source does not mention Passu -- treated as an unresolved
# tag, not a confirmed match; excluded from the matched set below.
# See the FLAG cat() message at runtime.
#
# Output: data/processed/identification_distances.csv
# ============================================================

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(TRUE)   # geodesic distances on the sphere

# lakes_with_district.gpkg holds all 29 originally-screened candidate
# lakes, not just the 17-lake working set -- restrict to the working set
# as defined by lakes_polygons_verified.gpkg (17 rows + L30).
working_set_ids <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  filter(lake_id != "L30") |>
  pull(lake_id)

lakes <- st_read("data/processed/lakes_with_district.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, lat, lon) |>
  filter(lake_id %in% working_set_ids)

refs <- st_read("data/processed/literature_reference_points.gpkg", quiet = TRUE) |>
  mutate(name = trimws(name))   # fix: source file has trailing whitespace on some names

lake_pt <- function(lat, lon) st_sfc(st_point(c(lon, lat)), crs = 4326)

dist_km <- function(lat1, lon1, lat2, lon2) {
  as.numeric(st_distance(lake_pt(lat1, lon1), lake_pt(lat2, lon2))) / 1000
}

CANDIDATES <- list(
  L15 = c("Khurdopin", "Khurdopin 2", "Khurdopin Glacier"),
  L21 = c("Karambar"),
  L24 = c("Ghulkin"),
  L27 = c("Shisper", "Shisper2"),
  L29 = c("Passu")
)

classify <- function(d) {
  case_when(
    d < 10  ~ "Confirmed",
    d < 25  ~ "Probable",
    TRUE    ~ "Different feature"
  )
}

result_rows <- lapply(names(CANDIDATES), function(lid) {
  lake_row <- lakes |> filter(lake_id == lid)
  cand_names <- CANDIDATES[[lid]]
  cands <- refs |> filter(name %in% cand_names)

  if (nrow(cands) == 0) {
    cat("WARNING:", lid, "-- no candidate found in literature_reference_points.gpkg",
        "for names:", paste(cand_names, collapse = ", "), "\n")
    return(NULL)
  }

  dists <- vapply(seq_len(nrow(cands)), function(i) {
    dist_km(lake_row$lat, lake_row$lon, cands$lat[i], cands$lon[i])
  }, numeric(1))

  nearest <- which.min(dists)
  data.frame(
    lake_id  = lid,
    # literature_reference_points.gpkg disambiguates two same-base-name
    # candidates internally with a trailing digit (e.g. "Shisper" vs
    # "Shisper2" for the two different L27 sources below) -- that digit
    # is a lookup key, not part of the feature's actual name, and must
    # not leak into reader-facing text (it did: Table S4 shipped "3.9 km
    # from Shisper2", which is not a real place/reference name). Strip a
    # trailing digit glued directly onto a letter before using the name.
    ref_name = sub("([A-Za-z])[0-9]+$", "\\1", cands$name[nearest]),
    ref_source = cands$source[nearest],
    dist_km  = round(dists[nearest], 1),
    class    = classify(dists[nearest]),
    all_candidates = paste(sprintf("%s (%s): %.1f km", cands$name, cands$source, dists), collapse = "; "),
    stringsAsFactors = FALSE
  )
})

matched <- bind_rows(result_rows)

# ---- L29 Passu: report the finding, but do NOT auto-confirm -------------
l29_row <- matched |> filter(lake_id == "L29")
if (nrow(l29_row) == 1) {
  cat("\nFLAG L29 Passu:", l29_row$dist_km, "km from a point tagged '",
      l29_row$ref_source, "' in literature_reference_points.gpkg.\n")
  cat("      I read that paper directly -- it's entirely about Shispare/Muchuhar/\n")
  cat("      Hasanabad (Hunza), never mentions Passu. The source tag looks wrong.\n")
  cat("      Treating this as UNRESOLVED, not confirmed -- L29 stays 'No published\n")
  cat("      coordinate found' below. Check/fix that point's source tag, or tell me\n")
  cat("      what it actually is, before using this as a real match.\n\n")
  # Do not let this unresolved match through to the output table.
  matched <- matched |> filter(lake_id != "L29")
}

# ---- L27 Shisper: report both, keep the closer (Shisper2) as primary ----
l27_row <- matched |> filter(lake_id == "L27")
if (nrow(l27_row) == 1) {
  cat("FLAG L27 Shisper -- two candidates, both real (verified by reading\n")
  cat("      Bhambri et al. 2020 directly):", l27_row$all_candidates, "\n")
  cat("      Shisper2 (Bhambri 2020, 3.9 km) is the GLACIER coordinate reported in\n")
  cat("      that paper's own text. Shisper (Shrestha 2023, 11.2 km) is the\n")
  cat("      HMAGLOFDB LAKE coordinate. Using the closer (Shisper2/glacier) as the\n")
  cat("      primary distance below -- both are recorded in the CSV's ref_name.\n\n")
}

# ---- L20 Badswat override (not in literature_reference_points.gpkg) -----
l20_row <- data.frame(
  lake_id = "L20", ref_name = "Badswat village",
  ref_source = "Shangguan et al. (2021)", dist_km = 5.6,
  class = "Probable",
  all_candidates = "Badswat village (Shangguan et al. 2021): 5.6 km",
  stringsAsFactors = FALSE
)
matched <- bind_rows(matched, l20_row)

out <- lakes |>
  select(lake_id) |>
  left_join(matched |> select(lake_id, ref_name, dist_km, class), by = "lake_id") |>
  arrange(lake_id)

stopifnot(nrow(out) == 17)
write.csv(out, "data/processed/identification_distances.csv", row.names = FALSE)
cat("Saved: data/processed/identification_distances.csv (", nrow(out), "rows )\n")
cat("Matched (incl. L20 override):", paste(sort(matched$lake_id), collapse = ", "), "\n")
cat("No candidate for the other", nrow(out) - nrow(matched),
    "lakes -- left as 'No published coordinate found'.\n")
