# ============================================================
# table04_h4_exposure.R
#
# Builds Table 4 (H4 hazard-zone exposure -- area, population,
# buildings, roads, bridges, built-up area) for all production
# simulations, directly from the H4 exposure summary CSVs.
#
# UPDATED (2026-08-26): H4 area and Roads previously rounded
# to 2 decimals and displayed with a variable number of digits once
# written to CSV; both are now rounded to 3 decimals and formatted
# with sprintf("%.3f", ...) so every value shows exactly 3 decimal
# places. Original archived to scripts/OLD/table04_h4_exposure.R.
#
# Sources (read-only, not modified by this script):
#   data/processed/h4_exposure_summary.csv -- 7 canonical scenarios
#     per lake (Low, Mid, High, Mid_n04, Mid_n06, Mid_bulk15,
#     Mid_bulk20); "scenario" combines lake_id and scenario code.
#   data/processed/h4_exposure_n010_scenarios.csv -- the Mid bulk
#     n=0.10 scenarios (computed by
#     scripts/analysis/21_reviewer_response_h4_and_volume.R),
#     row-bound onto the canonical table.
#
# Output: figures/table04_h4_exposure.csv
# ============================================================

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

# Row order matching the published Table 4: L27 Shisper first (9
# scenarios in canonical order, including the two n=0.10 additions
# below), then L29 Passu.
lake_order     <- c("L27", "L29")
scenario_order <- names(scenario_label)

table04_main <- h4 |>
  # "scenario" here is actually "<lake_id>_<scenario_code>" -- split it
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

# ---- n = 0.10 bulk scenarios (added 2026-08-24) ----------------------------
n010_raw <- read_csv("data/processed/h4_exposure_n010_scenarios.csv", show_col_types = FALSE)

table04_n010 <- n010_raw |>
  mutate(
    Lake = lake_label[Lake],
    # match the canonical table's bullet-point multiplication sign/style
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

# Keep the same lake order as the main block, n=0.10 rows immediately
# after each lake's 7 canonical rows.
table04 <- bind_rows(
  table04_main |> filter(Lake == "L27 Shisper"),
  table04_n010 |> filter(Lake == "L27 Shisper"),
  table04_main |> filter(Lake == "L29 Passu"),
  table04_n010 |> filter(Lake == "L29 Passu")
)

# ---- Sanity check (numeric, before display formatting) ----------------------
# H4 area for L27 Mid raw = 9.002 km2 -- checked at 3 dp now, not the old
# 2-dp value (which happened to round to a clean 9).
stopifnot(
  table04$`H4 area (km²)`[table04$Lake == "L27 Shisper" &
    table04$Scenario == "Mid (baseline)"] == 9.002,
  table04$`Population (mean)`[table04$Lake == "L29 Passu" &
    table04$Scenario == "Mid (baseline)"] == 52,
  # n=0.10 rows landed correctly
  nrow(table04 |> filter(grepl("n=0.10", Scenario))) == 4
)
cat("Sanity check passed.\n")

# ---- Standardize display to exactly 3 decimal places (2026-08-26) --
table04 <- table04 |>
  mutate(
    `H4 area (km²)` = sprintf("%.3f", `H4 area (km²)`),
    `Roads (km)`    = sprintf("%.3f", `Roads (km)`)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table04$`H4 area (km²)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table04$`Roads (km)`))
)
cat("Sanity check passed: H4 area / Roads display exactly 3 decimal places\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table04, "figures/table04_h4_exposure.csv", row.names = FALSE)
cat("Saved: figures/table04_h4_exposure.csv (", nrow(table04), "rows )\n")
