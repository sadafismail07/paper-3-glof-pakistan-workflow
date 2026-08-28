# ============================================================
# tableS2_ranking_stability.R
#
# Builds Table S2 (rank of each working-set lake across seven
# scoring variants -- with/without the retired tier indicator,
# with/without surge, and three normalisation schemes on the
# adopted composite) and writes it to a clean CSV.
#
# Source: data/processed/TableS2_ranking_stability.csv, computed
# by the self-contained scoring-variants script (rebuilds inputs
# from lake_corridors_with_exposure.gpkg and
# lakes_polygons_verified.gpkg, computes all seven variants in one
# pass). This script only formats that output for publication --
# it does not recompute any score.
#
# NOTE: a same-named but stale file also exists at
# data/processed/scoring_diagnostics/TableS2_ranking_stability.csv
# -- that one predates the tier removal and is not used here.
#
# Output: figures/tableS2_ranking_stability.csv
# ============================================================

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

S2 <- read_csv("data/processed/TableS2_ranking_stability.csv", show_col_types = FALSE)

tableS2 <- S2 |>
  transmute(
    `Lake ID` = lake_id,
    Lake = lake_name,
    `With tier`,
    `With tier, no surge`,
    `Adopted (min-max)`,
    `Adopted, no surge`,
    `Rank norm`,
    `Z-score`,
    `Robust 5-95`,
    `Rank range`
  ) |>
  arrange(`Adopted (min-max)`)

# ---- Sanity check ------------------------------------------------------------
stopifnot(
  nrow(tableS2) == 17,
  tableS2$`Lake ID`[tableS2$`With tier` == 1] == "L27",
  tableS2$`Lake ID`[tableS2$`With tier` == 2] == "L29"
)
cat("Sanity check passed: 17 rows, with-tier variant reproduces the published ranking (L27, L29).\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS2, "figures/tableS2_ranking_stability.csv", row.names = FALSE)
cat("Saved: figures/tableS2_ranking_stability.csv (", nrow(tableS2), "rows )\n")
