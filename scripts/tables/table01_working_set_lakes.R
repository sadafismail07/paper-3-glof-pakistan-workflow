# Lake list

library(sf)
library(dplyr)
library(stringr)

setwd("C:/Users/sadaf/Documents/PPR3")

coords <- st_read("data/processed/lakes_with_district.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, lat, lon)

verified <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name, vulnerability, lake_type,
         area_km2, area_source)

working_set <- verified |>
  filter(lake_id != "L30") |>
  left_join(coords, by = "lake_id")

fmt_area <- function(area_km2, lake_id) {
  base <- ifelse(is.na(area_km2), "—",
         format(round(area_km2 * 1e6), big.mark = ",", scientific = FALSE, trim = TRUE))
  ifelse(lake_id == "L01", paste0(base, "\u00b9"), base)
}

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

strip_bualtar <- function(name) sub("\\s*\\(Bualtar\\)\\s*$", "", name)

fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

fix_l15_type_display <- function(type_raw, lake_id) {
  ifelse(lake_id == "L15" & type_raw == "moraine dammed / proglacial",
         "moraine-dammed (proglacial)", type_raw)
}

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

stopifnot(
  table01$Lake[table01$`Lake ID` == "L15"] == "Khurdupin",
  table01$`Area (m²)`[table01$`Lake ID` == "L29"] == "141,988",
  table01$`Area (m²)`[table01$`Lake ID` == "L27"] == "400,000",
  table01$`Area source`[table01$`Lake ID` == "L27"] == "Literature value (Khan et al., 2021)",
  table01$`Area (m²)`[table01$`Lake ID` == "L01"] == "1,259\u00b9"
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table01, "figures/table01_working_set_lakes.csv", row.names = FALSE, na = "—")
cat("Saved: figures/table01_working_set_lakes.csv (", nrow(table01), "rows )\n")
