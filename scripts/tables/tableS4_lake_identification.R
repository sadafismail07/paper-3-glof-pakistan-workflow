# ============================================================
# tableS4_lake_identification.R
#
# Purpose: build Table S4 (lake-identification confidence and
# published characterisation for the 17 working-set lakes,
# companion to Table 1) and write it to a clean CSV for the
# GitHub repository.
#
# Table S4 carries the columns that don't belong in the compact
# Table 1 inventory: how confidently each working-set lake matches
# a name/coordinate already published elsewhere, what published
# record (if any) exists for it, and which sources that record
# comes from.
#
# Sources:
#   data/processed/lakes_with_district.gpkg    -- lat/lon
#   data/processed/lakes_polygons_verified.gpkg -- lake name
#   data/processed/identification_distances.csv -- distance from
#     each working-set lake to the nearest same-named feature in
#     a published inventory (HMAGLOFDB), and the resulting
#     confidence class (<10 km = Probable, 10-25 km = Probable,
#     >25 km = Different feature -- see manuscript Table S4
#     caption for the exact thresholds). NA rows mean no published
#     coordinate for that lake was found within any threshold.
#
# NOTE on identification_distances.csv coverage: as of the last
# run, this file does not yet carry a row for L20 Badswat, whose
# distance-to-village figure (5.6 km, Shangguan et al. 2021) was
# derived differently (from a named village, not a HMAGLOFDB
# lake-name match). L20's Identification text is set directly
# below (L20_IDENTIFICATION) until that source CSV is extended --
# if an L20 row is added to identification_distances.csv later,
# remove the override so the script picks it up from the CSV
# instead.
#
# The "Published characterisation" and "Reference(s)" columns are
# curated narrative content, not derived from a GIS/attribute
# source -- they are set out explicitly in the LAKE_LIT tribble
# below. 
# Output: figures/tableS4_lake_identification.csv
# ============================================================

library(sf)
library(dplyr)
library(readr)
library(tibble)

setwd("C:/Users/sadaf/Documents/PPR3")

# ---- 1. Load name + coordinate sources -----------------------------------

coords <- st_read("data/processed/lakes_with_district.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, lat, lon)

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  filter(lake_id != "L30") |>
  select(lake_id, glacier_name)

ident_dist <- read_csv("data/processed/identification_distances.csv", show_col_types = FALSE)

# The verified GIS layer (lakes_polygons_verified.gpkg) records L15's own
# name using the literature spelling "Khurdopin". NDMA's source list
# (data/raw/ndma_vulnerable_glof_sites.csv) spells this feature
# "Khurdupin", and the manuscript documents these as two distinct,
# unrelated features 208 km apart (this table's own footnote, and
# Section giving the identification narrative) -- L15 is the NDMA
# feature, not the published Khurdopin Glacier named in "Identification"
# below. Corrected here so this working-set lake's own name matches its
# NDMA source and Table 1/S2, without altering the "Identification"
# text, which correctly names the literature feature by its own spelling.
fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

# ---- 2. Identification text, derived from distance + class ---------------

# L20 override -- see header note above.
L20_IDENTIFICATION <- "5.6 km from Badswat village (Shangguan et al. 2021)"

fmt_identification <- function(lake_id, ref_name, dist_km, class) {
  case_when(
    lake_id == "L20" ~ L20_IDENTIFICATION,
    is.na(dist_km)   ~ "No published coordinate found",
    class == "Different feature" ~ sprintf(
      "%.1f km from published %s — different feature", dist_km, ref_name
    ),
    TRUE ~ sprintf("%.1f km from %s", dist_km, ref_name)
  )
}

identification <- ident_dist |>
  transmute(
    lake_id,
    Identification = fmt_identification(lake_id, ref_name, dist_km, class)
  )

# ---- 3. Published characterisation + references (curated) ----------------
#
# FOOTNOTE1_LAKES: superscript ¹ is appended to the Lake name for
# these two rows in step 4 below -- L15 and L26 are the two entries
# whose name does not correspond to the feature at the listed
# coordinate (footnote 1). Footnote TEXT is deliberately not
# embedded in the table
FOOTNOTE1_LAKES <- c("L15", "L26")

LAKE_LIT <- tribble(
  ~lake_id, ~characterisation,                              ~references,
  "L01", "Outburst documented (2013)",                      "Ashraf et al. (2021); Sarwar & Mahmood (2024)",
  "L02", "Outburst documented (2010)",                       "Rasul et al. (2011)",
  "L03", "None peer-reviewed",                                "—",
  "L04", "None peer-reviewed",                                "—",
  "L05", "None peer-reviewed",                                "—",
  "L06", "None found",                                        "—",
  "L15", "None for this location",                             "Hussain et al. (2020); Shrestha et al. (2023) (HMAGLOFDB); Bazai et al. (2021); Hewitt & Liu (2010)",
  "L19", "Outbursts documented (1978\u20132010)",              "Ashraf et al. (2021)",
  "L20", "Outburst + dam geometry",                           "Shangguan et al. (2021); Shrestha et al. (2023); Hassan et al. (2023)",
  "L21", "Outbursts (11, 1844\u20131994) + dam geometry",      "Shrestha et al. (2023); Hewitt & Liu (2010)",
  "L22", "None lake-specific",                                 "—",
  "L24", "Outbursts documented",                                "Shrestha et al. (2023); conflated with Hussain et al. (2020)",
  "L25", "None peer-reviewed",                                  "Non-peer-reviewed sources only",
  "L26", "No outburst record (Hewitt & Liu 2010 explicitly note this glacier did not dam the valley)", "Gardner & Hewitt (1990); Hewitt & Liu (2010)",
  "L27", "Outbursts (13, 1894\u20132022) + volume",             "Bhambri et al. (2020); Muhammad et al. (2021); Bazai et al. (2021); Shrestha et al. (2023)",
  "L28", "None found",                                          "—",
  "L29", "Outbursts (\u22652, 2000s) + lake area",               "Rasul et al. (2011); Ashraf et al. (2021); Muneeb et al. (2021); Mazhar et al. (2024)"
)

# ---- 4. Assemble table ----------------------------------------------------

tableS4 <- names_lu |>
  left_join(coords, by = "lake_id") |>
  left_join(identification, by = "lake_id") |>
  left_join(LAKE_LIT, by = "lake_id") |>
  transmute(
    `Lake ID` = lake_id,
    Lake = ifelse(lake_id %in% FOOTNOTE1_LAKES,
                   paste0(fix_khurdupin_spelling(glacier_name, lake_id), "\u00b9"),
                   fix_khurdupin_spelling(glacier_name, lake_id)),
    Coordinates = sprintf("%.3f\u00b0N, %.3f\u00b0E", lat, lon),
    Identification,
    `Published characterisation` = characterisation,
    `Reference(s)` = references
  ) |>
  arrange(`Lake ID`)

# ---- 5. Sanity check -------------------------------------------------------
stopifnot(
  tableS4$Lake[tableS4$`Lake ID` == "L15"] == "Khurdupin\u00b9",
  nrow(tableS4) == 17,
  all(!is.na(tableS4$Identification)),
  all(!is.na(tableS4$`Published characterisation`))
)
cat("Sanity check passed: L15 = Khurdupin\u00b9, 17 rows, no missing Identification/characterisation text\n")
cat("REMINDER: Bazai et al. (2021) and Shangguan et al. (2021) need to be added\n")
cat("to the manuscript's own reference list if not already present.\n")

# ---- 6. Write out -----------------------------------------------------------

dir.create("figures", showWarnings = FALSE)
write.csv(tableS4, "figures/tableS4_lake_identification.csv", row.names = FALSE, na = "—")
cat("Saved: figures/tableS4_lake_identification.csv (", nrow(tableS4), "rows )\n")
