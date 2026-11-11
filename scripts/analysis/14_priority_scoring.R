# Priority scoring

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(dplyr)

corridors <- sf::st_read("data/processed/lake_corridors_with_exposure.gpkg",
                         quiet = TRUE)
lakes_v <- sf::st_read("data/processed/lakes_polygons_verified.gpkg",
                       quiet = TRUE)

lakes_meta <- sf::st_drop_geometry(lakes_v) |>
  dplyr::select(lake_id, vulnerability, lake_type, area_km2)

surge_lookup <- data.frame(
  lake_id = c("L01","L02","L03","L04","L05","L06","L15","L19","L20",
              "L21","L22","L24","L25","L26","L27","L28","L29"),
  surge_type = c(0,0,0,0,0,0,0,0,0,
                 1,0,0,0,1,1,0,1),
  stringsAsFactors = FALSE
)

score_data <- sf::st_drop_geometry(corridors) |>
  dplyr::filter(lake_id != "L30") |>
  dplyr::left_join(lakes_meta, by = "lake_id") |>
  dplyr::left_join(surge_lookup, by = "lake_id") |>
  dplyr::mutate(
    is_ice_dammed = grepl("ice", lake_type, ignore.case = TRUE),
    surge_credit  = surge_type * as.integer(is_ice_dammed),
    area_for_score = ifelse(is.na(area_km2), 0, area_km2)
  )

minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}
log10p <- function(x) log10(x + 1)

score_data <- score_data |>
  dplyr::mutate(
    # Hazard composite (3 inputs -- tier removed)
    h_vuln  = minmax(vulnerability),
    h_area  = minmax(log10p(area_for_score * 1e6)),  # convert km^2 -> m^2 before log
    h_surge = surge_credit,

    # Exposure composite (5 inputs, all log10-transformed)
    e_pop        = minmax(log10p(pop_mean)),
    e_buildings  = minmax(log10p(building_count)),
    e_roads      = minmax(log10p(road_length_km)),
    e_bridges    = minmax(log10p(bridge_count)),
    e_built_area = minmax(log10p(built_area_m2)),

    # Composites
    hazard_score   = (h_vuln + h_area + h_surge) / 3,
    exposure_score = (e_pop + e_buildings + e_roads + e_bridges + e_built_area) / 5,

    # Final score = mean of two composites
    final_score = (hazard_score + exposure_score) / 2
  )

ranking <- score_data |>
  dplyr::arrange(desc(final_score)) |>
  dplyr::mutate(rank = dplyr::row_number())

# Join scores back to corridors (filtered) for export
corridors_filtered <- corridors |>
  dplyr::filter(lake_id != "L30")

corridors_scored <- corridors_filtered |>
  dplyr::left_join(
    score_data |>
      dplyr::select(lake_id, hazard_score, exposure_score, final_score),
    by = "lake_id"
  )

sf::st_write(corridors_scored,
             "data/processed/lake_corridors_scored.gpkg",
             delete_dsn = TRUE, quiet = TRUE)

write.csv(
  ranking |>
    dplyr::select(rank, lake_id, lake_type, h_vuln, h_area, h_surge,
                  hazard_score, exposure_score, final_score),
  "data/processed/lake_priority_scores.csv",
  row.names = FALSE
)

cat("\n=== Priority scoring complete (no-tier, surge restricted to ice-dammed lakes) ===\n")
cat("Top 2 selected for HEC-RAS simulation:\n\n")
print(ranking |>
        dplyr::filter(rank <= 2) |>
        dplyr::select(rank, lake_id, lake_type, hazard_score,
                      exposure_score, final_score),
      row.names = FALSE)

cat("\nFull ranking saved to data/processed/lake_corridors_scored.gpkg\n")
cat("Full ranking table saved to data/processed/lake_priority_scores.csv\n")
