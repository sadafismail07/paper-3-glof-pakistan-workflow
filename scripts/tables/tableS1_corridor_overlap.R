# Corridor overlap

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

table_S1 <- bind_rows(overlap_rows) |> arrange(desc(overlap_km2))
print(table_S1)

stopifnot(
  nrow(corr) == 17,
  union_area_km2 <= sum(corr$area_km2),
  dc_factor >= 1,
  all(table_S1$overlap_km2 > 0)
)
cat(sprintf("Sum=%.1f, Union=%.1f, factor=%.2f, %d overlapping pairs.\n",
            sum(corr$area_km2), union_area_km2, dc_factor, nrow(table_S1)))

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
cat("Check passed.\n")

dir.create("data/processed/scoring_diagnostics", showWarnings = FALSE, recursive = TRUE)
write.csv(table_S1, "data/processed/scoring_diagnostics/table_S1_corridor_overlap.csv", row.names = FALSE)
cat("\nSaved: data/processed/scoring_diagnostics/table_S1_corridor_overlap.csv\n")

write.csv(table_S1, "figures/tableS1_corridor_overlap.csv", row.names = FALSE)
cat("\nSaved: figures/tableS1_corridor_overlap.csv\n")
