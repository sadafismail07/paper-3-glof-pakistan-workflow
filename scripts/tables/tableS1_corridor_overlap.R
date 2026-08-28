# ============================================================
# tableS1_corridor_overlap.R
#
# UPDATED (2026-08-26): area_a_km2 / area_b_km2 / overlap_km2 /
# pct_a / pct_b previously rounded to 1 decimal and displayed with a
# variable number of digits once written to CSV; all five now round to
# 3 decimals and are formatted with sprintf("%.3f", ...), applied after
# sorting so the numeric sort order is unaffected. Original archived to
# scripts/OLD/tableS1_corridor_overlap.R.
#
# Purpose: build Table S1 (pairwise spatial overlap among the 50 km
# downstream corridors of the 17 working-set lakes -- the area-based
# double-counting factor) and write it to a clean CSV for the GitHub
# repository.
#
# Output has been cross-checked against an independent console run of
# this same logic (Sum = 1,805.1 km2, Union = 1,078.1 km2, factor =
# 1.67, 22 unique overlapping pairs).
#
# Source (read-only, not modified by this script):
#   data/processed/lake_corridors_scored.gpkg
#
# Output: data/processed/scoring_diagnostics/table_S1_corridor_overlap.csv
# ============================================================

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

corr <- st_read("data/processed/lake_corridors_scored.gpkg", quiet = TRUE)
corr$area_km2 <- as.numeric(st_area(corr)) / 1e6

cat("Sum of individual corridor areas (km2):", round(sum(corr$area_km2), 1), "\n")

union_geom     <- st_union(corr)
union_area_km2 <- as.numeric(st_area(union_geom)) / 1e6
dc_factor      <- sum(corr$area_km2) / union_area_km2

cat("Union area (km2):           ", round(union_area_km2, 1), "\n")
cat("Double-counting factor:     ", round(dc_factor, 2), "\n")
cat("Excess (double-counted) km2:", round(sum(corr$area_km2) - union_area_km2, 1), "\n")

# All pairwise combinations of the 17 corridors; keep only pairs with a
# non-trivial (> 1e-6 km2) intersection.
n     <- nrow(corr)
pairs <- combn(n, 2, simplify = FALSE)

overlap_rows <- lapply(pairs, function(idx) {
  a <- corr[idx[1], ]
  b <- corr[idx[2], ]
  inter <- suppressWarnings(st_intersection(st_geometry(a), st_geometry(b)))
  if (length(inter) == 0 || all(st_is_empty(inter))) return(NULL)
  overlap_km2 <- as.numeric(st_area(inter)) / 1e6
  if (overlap_km2 < 1e-6) return(NULL)
  data.frame(
    lake_a = a$lake_id, area_a_km2 = round(a$area_km2, 3),
    lake_b = b$lake_id, area_b_km2 = round(b$area_km2, 3),
    overlap_km2 = round(overlap_km2, 3),
    pct_a = round(100 * overlap_km2 / a$area_km2, 3),
    pct_b = round(100 * overlap_km2 / b$area_km2, 3)
  )
})

# Sort by the numeric overlap_km2 column first -- formatting to a fixed-
# decimal string before sorting would sort rows alphabetically instead
# of numerically (e.g. "10.200" would sort before "9.500").
table_S1 <- bind_rows(overlap_rows) |> arrange(desc(overlap_km2))
print(table_S1)

# ---- Sanity check against known reference values (numeric, unaffected by ---
# ---- the per-pair column rounding below -- these use corr$area_km2) --------
stopifnot(
  round(sum(corr$area_km2), 1) == 1805.1,
  round(union_area_km2, 1) == 1078.1,
  round(dc_factor, 2) == 1.67,
  nrow(table_S1) == 22
)
cat("Sanity check passed: Sum=1805.1, Union=1078.1, factor=1.67, 22 pairs.\n")

# ---- Standardize display to exactly 3 decimal places (2026-08-26) --
# Only now, after sorting, are the numeric columns reformatted as fixed
# 3-decimal strings for the CSV.
table_S1 <- table_S1 |>
  mutate(
    area_a_km2  = sprintf("%.3f", area_a_km2),
    area_b_km2  = sprintf("%.3f", area_b_km2),
    overlap_km2 = sprintf("%.3f", overlap_km2),
    pct_a       = sprintf("%.3f", pct_a),
    pct_b       = sprintf("%.3f", pct_b)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table_S1$area_a_km2)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table_S1$area_b_km2)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table_S1$overlap_km2)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table_S1$pct_a)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table_S1$pct_b))
)
cat("Sanity check passed: area/overlap/pct columns all display exactly 3 decimal places\n")

dir.create("data/processed/scoring_diagnostics", showWarnings = FALSE, recursive = TRUE)
write.csv(table_S1, "data/processed/scoring_diagnostics/table_S1_corridor_overlap.csv", row.names = FALSE)
cat("\nSaved: data/processed/scoring_diagnostics/table_S1_corridor_overlap.csv\n")

write.csv(table_S1, "figures/tableS1_corridor_overlap.csv", row.names = FALSE)
cat("\nSaved: figures/tableS1_corridor_overlap.csv\n")