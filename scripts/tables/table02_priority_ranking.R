# Priority ranking

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

corridors <- st_read("data/processed/lake_corridors_scored.gpkg", quiet = TRUE) |>
  st_drop_geometry()

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name)

strip_bualtar <- function(name) sub("\\s*\\(Bualtar\\)\\s*$", "", name)

fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

table02 <- corridors |>
  left_join(names_lu, by = "lake_id") |>
  arrange(desc(final_score)) |>
  mutate(Rank = row_number()) |>
  transmute(
    Rank,
    ID = lake_id,
    Lake = fix_khurdupin_spelling(strip_bualtar(glacier_name), lake_id),
    `Population (mean)` = format(round(pop_mean), big.mark = ",", scientific = FALSE, trim = TRUE),
    Hazard = round(hazard_score, 3),
    Exposure = round(exposure_score, 3),
    Final = round(final_score, 3)
  )

stopifnot(
  table02$Lake[table02$ID == "L15"] == "Khurdupin",
  table02$ID[table02$Rank == 1] == "L29",
  table02$ID[table02$Rank == 2] == "L27"
)
cat("Check passed.\n")

table02 <- table02 |>
  mutate(
    Hazard   = sprintf("%.3f", Hazard),
    Exposure = sprintf("%.3f", Exposure),
    Final    = sprintf("%.3f", Final)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table02$Hazard)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table02$Exposure)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table02$Final))
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table02, "figures/table02_priority_ranking.csv", row.names = FALSE)
cat("Saved: figures/table02_priority_ranking.csv (", nrow(table02), "rows )\n")
