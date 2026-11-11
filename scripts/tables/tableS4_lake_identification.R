# Lake identification

library(sf)
library(dplyr)
library(readr)
library(tibble)

setwd("C:/Users/sadaf/Documents/PPR3")

coords <- st_read("data/processed/lakes_with_district.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, lat, lon)

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  filter(lake_id != "L30") |>
  select(lake_id, glacier_name)

ident_dist <- read_csv("data/processed/identification_distances.csv", show_col_types = FALSE)

fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

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

stopifnot(
  tableS4$Lake[tableS4$`Lake ID` == "L15"] == "Khurdupin\u00b9",
  nrow(tableS4) == 17,
  all(!is.na(tableS4$Identification)),
  all(!is.na(tableS4$`Published characterisation`))
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS4, "figures/tableS4_lake_identification.csv", row.names = FALSE, na = "—")
cat("Saved: figures/tableS4_lake_identification.csv (", nrow(tableS4), "rows )\n")
