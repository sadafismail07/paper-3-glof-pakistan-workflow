# ============================================================
# tableS5_corridor_exposure.R
#
# Purpose: build Table S5 (full corridor-scale exposure indicator
# detail underlying Table 2 -- population by product, road length,
# buildings, bridges, built-up area, power features, plus the
# composite scores -- for the 17 working-set lakes) and write it
# to a clean CSV for the GitHub repository.
#
# UPDATED (fix for R1.10 / R4.07): this table used to be the
# main-text "Table 2"; it is now Supplementary Table S5. Per
# R1.10/R4.07, the main text now carries only the condensed
# ranking (see table02_priority_ranking.R, which used to
# occupy this Table S5 slot), and this full per-indicator detail
# has moved here as its supplementary companion.
#
# Source (read-only, not modified by this script):
#   data/processed/lake_corridors_scored.gpkg
#     -- one 50 km downstream corridor per lake, already carrying
#        population (GHS/WorldPop/mean), road/building/bridge/power
#        infrastructure counts, built-up area, and the final
#        hazard_score / exposure_score / final_score fields computed
#        by the priority-scoring pipeline (scripts/analysis/14_*.R).
#   data/processed/lakes_polygons_verified.gpkg
#     -- glacier/lake display name, joined by lake_id.
#
# This script does NOT recompute hazard/exposure/final scores -- it
# assembles the table from values already produced by the scoring
# pipeline, and only derives the display Rank column (by sorting on
# final_score) and the formatted "GHS / WP / mean" population string.
#
# Output: figures/tableS5_corridor_exposure.csv
# ============================================================

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

# ---- 1. Load sources -------------------------------------------------------

corridors <- st_read("data/processed/lake_corridors_scored.gpkg", quiet = TRUE) |>
  st_drop_geometry()

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name)

# The verified GIS layer (lakes_polygons_verified.gpkg) records L15's own
# name using the literature spelling "Khurdopin". NDMA's source list
# (data/raw/ndma_vulnerable_glof_sites.csv) spells this feature
# "Khurdupin", and the manuscript documents these as two distinct,
# unrelated features 208 km apart (Table S4 footnote) -- L15 is the NDMA
# feature, not the published Khurdopin Glacier. Corrected here so this
# working-set lake's own name matches its NDMA source and Table 1/S2.
fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

# ---- 2. Join name, derive rank and display columns -------------------------

fmt_int <- function(x) format(round(x), scientific = FALSE, trim = TRUE)  # no thousands separator -- required by the target journal's table formatting guidelines

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

# ---- 3. Sanity check against known reference values -------------------------
# L27 Shisper and L29 Passu are the two top-ranked, simulated lakes --
# confirmed against the manuscript body text (Section 4.1).
stopifnot(
  tableS5$`Glacier / lake name`[tableS5$`Lake ID` == "L15"] == "Khurdupin",
  tableS5$`Lake ID`[tableS5$Rank == 1] == "L27",
  tableS5$`Lake ID`[tableS5$Rank == 2] == "L29"
)
cat("Sanity check passed: L15 = Khurdupin; rank 1 = L27 Shisper, rank 2 = L29 Passu\n")

# ---- 4. Standardize display to exactly 3 decimal places (2026-08-26)
# `Road length (km)` was rounded to 1 decimal; the three scores were already
# round(..., 3) but, like the other tables, still displayed a variable
# number of digits once written to CSV. All four are now formatted with
# sprintf("%.3f", ...).
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
cat("Sanity check passed: Road length / Hazard / Exposure / Final all display exactly 3 decimal places\n")

# ---- 5. Write out ------------------------------------------------------------

dir.create("figures", showWarnings = FALSE)
write.csv(tableS5, "figures/tableS5_corridor_exposure.csv", row.names = FALSE)
cat("Saved: figures/tableS5_corridor_exposure.csv (", nrow(tableS5), "rows )\n")
