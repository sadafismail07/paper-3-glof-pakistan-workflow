# Hazard-zone exposure

library(dplyr)
library(readr)
library(tidyr)

setwd("C:/Users/sadaf/Documents/PPR3")

h4 <- read_csv("data/processed/h4_exposure_summary.csv", show_col_types = FALSE)

lake_label <- c(L29 = "L29 Passu", L27 = "L27 Shisper")

scenario_label <- c(
  Low          = "Low",
  Mid          = "Mid (baseline)",
  High         = "High",
  Mid_n04      = "Mid (n = 0.04)",
  Mid_n06      = "Mid (n = 0.06)",
  Mid_bulk15   = "Mid (×1.5 bulk, n=0.05)",
  Mid_bulk20   = "Mid (×2.0 bulk, n=0.05)"
)

lake_order     <- c("L27", "L29")
scenario_order <- names(scenario_label)

table04_main <- h4 |>
  separate(scenario, into = c("lake_code", "scenario_code"), sep = "_", extra = "merge") |>
  mutate(
    Lake     = factor(lake_code, levels = lake_order),
    Scenario = factor(scenario_code, levels = scenario_order)
  ) |>
  arrange(Lake, Scenario) |>
  transmute(
    Lake     = lake_label[as.character(Lake)],
    Scenario = scenario_label[as.character(Scenario)],
    `H4 area (km²)` = round(h4_area_km2, 3),
    `Population (GHS)` = pop_ghs_h4,
    `Population (WP)` = pop_wp_h4,
    `Population (mean)` = pop_mean_h4,
    `Buildings (n)` = buildings_h4,
    `Roads (km)` = round(roads_h4_km, 3),
    `Bridges (n)` = bridges_h4,
    `Built-up area (m²)` = built_area_h4_m2
  )

n010_raw <- read_csv("data/processed/h4_exposure_n010_scenarios.csv", show_col_types = FALSE)

table04_n010 <- n010_raw |>
  mutate(
    Lake = lake_label[Lake],
    Scenario = gsub("x", "×", Scenario, fixed = TRUE)
  ) |>
  transmute(
    Lake,
    Scenario,
    `H4 area (km²)` = round(`H4 area (km2)`, 3),
    `Population (GHS)` = `Population (GHS)`,
    `Population (WP)` = `Population (WP)`,
    `Population (mean)` = `Population (mean)`,
    `Buildings (n)` = `Buildings (n)`,
    `Roads (km)` = round(`Roads (km)`, 3),
    `Bridges (n)` = `Bridges (n)`,
    `Built-up area (m²)` = `Built-up area (m2)`
  )

table04 <- bind_rows(
  table04_main |> filter(Lake == "L27 Shisper"),
  table04_n010 |> filter(Lake == "L27 Shisper"),
  table04_main |> filter(Lake == "L29 Passu"),
  table04_n010 |> filter(Lake == "L29 Passu")
)

stopifnot(
  nrow(table04) == 18,
  !any(is.na(table04$Lake)), !any(is.na(table04$Scenario)),
  all(table04$`H4 area (km²)` > 0),
  # n=0.10 rows landed correctly
  nrow(table04 |> filter(grepl("n=0.10", Scenario))) == 4
)
cat("Check passed.\n")

table04 <- table04 |>
  mutate(
    `H4 area (km²)` = sprintf("%.3f", `H4 area (km²)`),
    `Roads (km)`    = sprintf("%.3f", `Roads (km)`)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table04$`H4 area (km²)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table04$`Roads (km)`))
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table04, "figures/table04_h4_exposure.csv", row.names = FALSE)
cat("Saved: figures/table04_h4_exposure.csv (", nrow(table04), "rows )\n")
