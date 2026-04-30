# 01_clean_inventory.R
# Load the NDMA Vulnerable GLOF Sites dataset, clean column names,
# add a stable lake_id, and save processed outputs.

setwd("C:/Users/sadaf/Documents/PPR3")

library(readr)
library(dplyr)
library(sf)

# Load
inv <- readr::read_csv(
  "data/raw/ndma_vulnerable_glof_sites.csv",
  show_col_types = FALSE
)

# Clean
inv <- inv |>
  dplyr::rename(
    glacier_name  = Name,
    lat           = Lat,
    vulnerability = VURNERBITL,
    lon           = Long
  ) |>
  dplyr::mutate(
    lake_id = sprintf("L%02d", dplyr::row_number())
  ) |>
  dplyr::select(lake_id, glacier_name, vulnerability, lat, lon)

# Save tabular
readr::write_csv(inv, "data/processed/lakes_vulnerable_GB_Chitral.csv")

# Convert to sf and save spatial
inv_sf <- sf::st_as_sf(
  inv,
  coords = c("lon", "lat"),
  crs    = 4326,
  remove = FALSE
)

sf::st_write(
  inv_sf,
  "data/processed/lakes_vulnerable_GB_Chitral.gpkg",
  delete_dsn = TRUE,
  quiet      = TRUE
)

cat("Saved cleaned inventory.\n")
cat("  data/processed/lakes_vulnerable_GB_Chitral.csv\n")
cat("  data/processed/lakes_vulnerable_GB_Chitral.gpkg\n")

