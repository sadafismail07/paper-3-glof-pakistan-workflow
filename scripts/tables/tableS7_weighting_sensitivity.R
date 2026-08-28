# ============================================================
# tableS7_weighting_sensitivity.R
#
# Purpose: build Table S7 (composite priority ranking under the
# adopted 50/50 hazard:exposure weighting and two alternative
# weightings, 60/40 and 70/30 hazard:exposure) for all 17
# working-set lakes, and write it to a clean CSV for the GitHub
# repository.
#
# New table for R4.1 (weighting-sensitivity request): demonstrates
# that L27 Shisper and L29 Passu retain the top two ranks under
# all three weightings, so the two-lake selection is not an
# artefact of the specific 50/50 weighting adopted in Section 3.6.
#
# This script does NOT recompute the underlying hazard/exposure
# scores or re-run the sensitivity analysis -- it formats the
# output already produced by scripts/analysis/25_composite_weighting_
# sensitivity.R for the manuscript's supplementary tables.
#
# Sources (read-only, not modified by this script):
#   data/processed/composite_weighting_sensitivity.csv -- lake_id and
#       final_score/rank under the 50/50, 60/40 and 70/30
#       weightings, produced by scripts/analysis/25_composite_weighting_
#       sensitivity.R.
#   data/processed/lakes_polygons_verified.gpkg -- lake display name.
#
# Output: figures/tableS7_weighting_sensitivity.csv
# ============================================================

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

# ---- 1. Load sources -------------------------------------------------------

weighting <- read.csv("data/processed/composite_weighting_sensitivity.csv",
                       stringsAsFactors = FALSE)

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

# ---- 2. Join name, order by adopted (50/50) rank, format for display -------

tableS7 <- weighting |>
  left_join(names_lu, by = "lake_id") |>
  arrange(rank_5050) |>
  transmute(
    `Lake ID` = lake_id,
    Lake = fix_khurdupin_spelling(strip_bualtar(glacier_name), lake_id),
    `Final score (50/50)` = round(score_5050, 3),
    `Rank (50/50)` = rank_5050,
    `Final score (60/40)` = round(score_6040, 3),
    `Rank (60/40)` = rank_6040,
    `Final score (70/30)` = round(score_7030, 3),
    `Rank (70/30)` = rank_7030
  )

# ---- 3. Sanity check -------------------------------------------------------
# L27 Shisper and L29 Passu must hold ranks 1-2 under all three weightings
# (this is the finding the table exists to demonstrate -- see Section 4.1
# and Response R4.1).
stopifnot(
  tableS7$Lake[tableS7$`Lake ID` == "L15"] == "Khurdupin",
  all(tableS7$`Lake ID`[tableS7$`Rank (50/50)` %in% 1:2] %in% c("L27", "L29")),
  all(tableS7$`Lake ID`[tableS7$`Rank (60/40)` %in% 1:2] %in% c("L27", "L29")),
  all(tableS7$`Lake ID`[tableS7$`Rank (70/30)` %in% 1:2] %in% c("L27", "L29"))
)
cat("Sanity check passed: L15 = Khurdupin; L27 and L29 hold ranks 1-2 under 50/50, 60/40 and 70/30\n")

# ---- 4. Standardize display to exactly 3 decimal places (2026-08-26)
# The three final-score columns were already round(..., 3) but, like the
# other tables, still displayed a variable number of digits once written
# to CSV. Formatted here with sprintf("%.3f", ...); rank columns are
# integers and untouched.
tableS7 <- tableS7 |>
  mutate(
    `Final score (50/50)` = sprintf("%.3f", `Final score (50/50)`),
    `Final score (60/40)` = sprintf("%.3f", `Final score (60/40)`),
    `Final score (70/30)` = sprintf("%.3f", `Final score (70/30)`)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS7$`Final score (50/50)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS7$`Final score (60/40)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS7$`Final score (70/30)`))
)
cat("Sanity check passed: all three final-score columns display exactly 3 decimal places\n")

# ---- 5. Write out -----------------------------------------------------------

dir.create("figures", showWarnings = FALSE)
write.csv(tableS7, "figures/tableS7_weighting_sensitivity.csv", row.names = FALSE)
cat("Saved: figures/tableS7_weighting_sensitivity.csv (", nrow(tableS7), "rows )\n")
