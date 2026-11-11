# Distances between inventory points and reference locations

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(TRUE)   # geodesic distances on the sphere

working_set_ids <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  filter(lake_id != "L30") |>
  pull(lake_id)

lakes <- st_read("data/processed/lakes_with_district.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, lat, lon) |>
  filter(lake_id %in% working_set_ids)

refs <- st_read("data/processed/literature_reference_points.gpkg", quiet = TRUE) |>
  mutate(name = trimws(name))   # some names carry trailing whitespace

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
    ref_name = sub("([A-Za-z])[0-9]+$", "\\1", cands$name[nearest]),
    ref_source = cands$source[nearest],
    dist_km  = round(dists[nearest], 1),
    class    = classify(dists[nearest]),
    all_candidates = paste(sprintf("%s (%s): %.1f km", cands$name, cands$source, dists), collapse = "; "),
    stringsAsFactors = FALSE
  )
})

matched <- bind_rows(result_rows)

l29_row <- matched |> filter(lake_id == "L29")
if (nrow(l29_row) == 1) {
  cat("\nFLAG L29 Passu:", l29_row$dist_km, "km from a point tagged '",
      l29_row$ref_source, "'; tag treated as an error.\n")
  matched <- matched |> filter(lake_id != "L29")
}

l27_row <- matched |> filter(lake_id == "L27")
if (nrow(l27_row) == 1) {
  cat("L27: two reference points:", l27_row$all_candidates, "\n")
}

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
