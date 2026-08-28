# ============================================================
# table01_working_set_lakes.R
#
# Purpose: build Table 1 (the 17-lake working-set inventory --
# ID, lake name, coordinates, NDMA vulnerability rating, damming
# type, empirically delineated area, and area source) directly
# from the verified GeoPackage attribute tables, and write it to
# a clean CSV for the GitHub repository.
#
# Table 1 carries only the core physical inventory columns.
# Lake-identification confidence, published characterisation, and
# literature references now live in the companion Table S4 (see
# tableS4_lake_identification.R) rather than here -- this keeps
# Table 1 compact and avoids duplicating the ID/name/coordinate
# columns across both tables.
#
# Sources (read-only, not modified by this script):
#   data/processed/lakes_with_district.gpkg   -- lat/lon
#   data/processed/lakes_polygons_verified.gpkg -- vulnerability,
#       lake_type, area_km2, area_source (raw)
#
# Output: figures/table01_working_set_lakes.csv
#
# UPDATED (2026-08-26): area_source_display() now expands the
# raw single-token in-text citation (e.g. "Khan 2021") to full
# author-year citation style "Khan et al., 2021" for display in
# the Area source column, matching the citation style used
# elsewhere in the manuscript's tables (e.g. Table 5).
# ============================================================

library(sf)
library(dplyr)
library(stringr)

setwd("C:/Users/sadaf/Documents/PPR3")

# ---- 1. Load sources ----------------------------------------------------

coords <- st_read("data/processed/lakes_with_district.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, lat, lon)

verified <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name, vulnerability, lake_type,
         area_km2, area_source)

# ---- 2. Join, restrict to the 17-lake NDMA-only working set -------------

working_set <- verified |>
  filter(lake_id != "L30") |>
  left_join(coords, by = "lake_id")

# ---- 3. Derive display columns -------------------------------------------

# Area in m^2, comma-formatted, "--" where no polygon exists (lake not
# visible in imagery). area_km2 is NA for those rows.
fmt_area <- function(area_km2, lake_id) {
  base <- ifelse(is.na(area_km2), "—",
         format(round(area_km2 * 1e6), big.mark = ",", scientific = FALSE, trim = TRUE))
  # L01's area is a manual digitisation of a small/ambiguous feature --
  # matches the manuscript's own existing footnote below Table 1 ("L01
  # Reshun area was manually digitised from Esri World Imagery...").
  ifelse(lake_id == "L01", paste0(base, "\u00b9"), base)
}

# Area-source category, inferred from the free-text area_source field by
# keyword. This mirrors the categories already used in the published
# Table 1 (Manual digitisation / Google Earth digitisation / Literature
# value / Not visible)
categorise_source <- function(raw) {
  raw_low <- tolower(raw)
  case_when(
    is.na(raw) ~ "—",
    str_detect(raw_low, "no visible lake|not visible") ~ "Not visible",
    str_detect(raw_low, "literature")                  ~ "Literature value",
    str_detect(raw_low, "google")                       ~ "Google Earth digitisation",
    str_detect(raw_low, "esri|water mask|manually verified") ~ "Manual digitisation",
    TRUE ~ "Manual digitisation"
  )
}

# "(Bualtar)" in L26's glacier_name is a literature association, not an
# NDMA name (see verification_notes) -- stripped from the display name.
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

# The verified-polygons GeoPackage's L15 raw `lake_type` attribute reads
# "moraine dammed / proglacial" -- missing the hyphen used everywhere else
# in this column ("moraine-dammed") and using a slash where every sibling
# row instead uses a parenthetical qualifier (e.g. "moraine-dammed
# (provisional)", "moraine-dammed (cirque/proglacial)"). Corrected here at
# display time, matching the fix_khurdupin_spelling()/strip_bualtar()
# precedent, rather than editing the source GeoPackage attribute.
fix_l15_type_display <- function(type_raw, lake_id) {
  ifelse(lake_id == "L15" & type_raw == "moraine dammed / proglacial",
         "moraine-dammed (proglacial)", type_raw)
}

# Surfaces the specific citation named in area_source's raw text (e.g.
# L27's raw "(Khan 2021)") as a full author-year citation, "Khan et al.,
# 2021", instead of the generic "Literature value" label or the bare
# single-token surname+year the raw text stores.
area_source_display <- function(raw, category) {
  m <- str_match(raw, "\\(([A-Za-z]+)\\s*(\\d{4})\\)")
  surname <- m[, 2]
  year <- m[, 3]
  formatted_cite <- paste0(surname, " et al., ", year)
  ifelse(category == "Literature value" & !is.na(surname),
         paste0("Literature value (", formatted_cite, ")"),
         category)
}

table01 <- working_set |>
  mutate(area_source_cat = categorise_source(area_source)) |>
  transmute(
    `Lake ID` = lake_id,
    Lake = fix_khurdupin_spelling(strip_bualtar(glacier_name), lake_id),
    Coordinates = sprintf("%.3f°N, %.3f°E", lat, lon),
    Vulnerability = vulnerability,
    Type = fix_l15_type_display(lake_type, lake_id),
    `Area (m²)` = fmt_area(area_km2, lake_id),
    `Area source` = area_source_display(area_source, area_source_cat)
  ) |>
  arrange(`Lake ID`)

# ---- 4. Sanity check against known reference values ----------------------
stopifnot(
  table01$Lake[table01$`Lake ID` == "L15"] == "Khurdupin",
  table01$`Area (m²)`[table01$`Lake ID` == "L29"] == "141,988",
  table01$`Area (m²)`[table01$`Lake ID` == "L27"] == "400,000",
  table01$`Area source`[table01$`Lake ID` == "L27"] == "Literature value (Khan et al., 2021)",
  table01$`Area (m²)`[table01$`Lake ID` == "L01"] == "1,259\u00b9"
)
cat("Sanity check passed: L15 = Khurdupin, L29 = 141,988 m2, L27 = 400,000 m2, L27 area source = 'Literature value (Khan et al., 2021)', L01 footnoted\n")

# ---- 5. Write out ----------------------------------------------------------

dir.create("figures", showWarnings = FALSE)
write.csv(table01, "figures/table01_working_set_lakes.csv", row.names = FALSE, na = "—")
cat("Saved: figures/table01_working_set_lakes.csv (", nrow(table01), "rows )\n")
