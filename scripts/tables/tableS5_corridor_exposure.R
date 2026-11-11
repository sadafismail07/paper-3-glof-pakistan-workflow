# Corridor exposure

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

corridors <- st_read("data/processed/lake_corridors_scored.gpkg", quiet = TRUE) |>
  st_drop_geometry()

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name)

fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

fmt_int <- function(x) format(round(x), scientific = FALSE, trim = TRUE)

tableS5 <- corridors |>
  left_join(names_lu, by = "lake_id") |>
  arrange(desc(final_score)) |>
  mutate(Rank = row_number()) |>
  transmute(
    Rank,
    `Lake ID` = lake_id,
    `Glacier / lake name` = fix_khurdupin_spelling(glacier_name, lake_id),
    `Population (GHS / WP / mean)` = paste(
      fmt_int(pop_ghs2025), fmt_int(pop_worldpop2020), fmt_int(pop_mean), sep = " / "
    ),
    `Road length (km)` = round(road_length_km, 3),
    `Buildings (n)` = building_count,
    `Bridges (n)` = bridge_count,
    `Built-up area (m²)` = fmt_int(built_area_m2),
    `Power features (n)*` = power_features_count,
    `Hazard score` = round(hazard_score, 3),
    `Exposure score` = round(exposure_score, 3),
    `Final score` = round(final_score, 3)
  )

stopifnot(
  tableS5$`Glacier / lake name`[tableS5$`Lake ID` == "L15"] == "Khurdupin",
  tableS5$`Lake ID`[tableS5$Rank == 1] == "L29",
  tableS5$`Lake ID`[tableS5$Rank == 2] == "L27"
)
cat("Check passed.\n")

tableS5 <- tableS5 |>
  mutate(
    `Road length (km)` = sprintf("%.3f", `Road length (km)`),
    `Hazard score`     = sprintf("%.3f", `Hazard score`),
    `Exposure score`   = sprintf("%.3f", `Exposure score`),
    `Final score`      = sprintf("%.3f", `Final score`)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS5$`Road length (km)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS5$`Hazard score`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS5$`Exposure score`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS5$`Final score`))
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS5, "figures/tableS5_corridor_exposure.csv", row.names = FALSE)
cat("Saved: figures/tableS5_corridor_exposure.csv (", nrow(tableS5), "rows )\n")
