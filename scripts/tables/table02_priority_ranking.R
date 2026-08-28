# ============================================================
# table02_priority_ranking.R
#
# Purpose: build Table 2 (condensed priority ranking -- rank,
# lake ID/name, and the three composite scores -- for the 17
# working-set lakes) and write it to a clean CSV for the GitHub
# repository.
#
# UPDATED (fix for R1.10 / R4.07): this table used to be the
# condensed "Table S5" companion; it is now the main-text Table 2.
# Per R1.10/R4.07, Table 2 was flagged as overcrowded (12 columns:
# population by product, road length, buildings, bridges, built-up
# area, power features, plus the three scores). The full per-
# indicator detail has moved to Supplementary Table S5
# (see tableS5_corridor_exposure.R, which used to occupy
# this Table 2 slot) -- this script now produces the condensed
# version that replaces it in the main text.
#
# This is a condensed companion to Table S5: same ranking and same
# underlying scores, fewer columns (drops the individual
# infrastructure counts and the GHS/WorldPop population split,
# keeping only their mean).
#
# Sources (read-only, not modified by this script):
#   data/processed/lake_corridors_scored.gpkg -- pop_mean,
#       hazard_score, exposure_score, final_score per lake, already
#       computed by the priority-scoring pipeline (scripts/analysis/
#       14_priority_scoring.R). This script does not
#       recompute any score.
#   data/processed/lakes_polygons_verified.gpkg -- lake display name.
#
# Output: figures/table02_priority_ranking.csv
# ============================================================

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

corridors <- st_read("data/processed/lake_corridors_scored.gpkg", quiet = TRUE) |>
  st_drop_geometry()

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name)

# "(Bualtar)" in L26's glacier_name is a literature association, not an
# NDMA name (see table01_working_set_lakes.R) -- stripped here too.
strip_bualtar <- function(name) sub("\\s*\\(Bualtar\\)\\s*$", "", name)

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

# ---- Sanity check against Table S5, which ranks the same lakes on the ----
# same scores -- rank 1/2 must agree (confirmed in Table S5's own script).
stopifnot(
  table02$Lake[table02$ID == "L15"] == "Khurdupin",
  table02$ID[table02$Rank == 1] == "L27",
  table02$ID[table02$Rank == 2] == "L29"
)
cat("Sanity check passed: L15 = Khurdupin, rank 1 = L27, rank 2 = L29 (matches Table S5)\n")

# ---- Standardize display to exactly 3 decimal places (2026-08-26) --
# round(x, 3) alone still lets each value show its own natural number of
# decimals once written to CSV (0.7 next to 0.667), which reads as
# inconsistent precision down the column. Format as a fixed 3-decimal
# string only now, after the numeric ID-based sanity check above.
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
cat("Sanity check passed: Hazard/Exposure/Final all display exactly 3 decimal places\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table02, "figures/table02_priority_ranking.csv", row.names = FALSE)
cat("Saved: figures/table02_priority_ranking.csv (", nrow(table02), "rows )\n")
