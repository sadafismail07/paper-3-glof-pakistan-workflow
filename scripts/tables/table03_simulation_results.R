# Simulation results

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

m <- read_csv("data/processed/table03_master_data.csv", show_col_types = FALSE)

lake_label <- c(L29 = "L29 Passu", L27 = "L27 Shisper")

scenario_label <- c(
  Low           = "Low",
  Mid           = "Mid (baseline)",
  High          = "High",
  Mid_n04       = "Mid (n = 0.04)",
  Mid_n06       = "Mid (n = 0.06)",
  Mid_b15       = "Mid (×1.5 bulk, n=0.05)",
  Mid_b20       = "Mid (×2.0 bulk, n=0.05)",
  Mid_b15_n010  = "Mid (×1.5 bulk, n=0.10)",
  Mid_b20_n010  = "Mid (×2.0 bulk, n=0.10)",
  p45           = "Partial drainage 45%",
  p28           = "Partial drainage 28%"
)

scenario_order <- names(scenario_label)

r3 <- function(x) round(x, 3)

table03 <- m |>
  mutate(
    Lake     = lake_label[lake_id],
    Scenario = factor(scenario_code, levels = scenario_order)
  ) |>
  arrange(factor(lake_id, levels = c("L29", "L27")), Scenario) |>
  transmute(
    Lake = Lake,
    Scenario = scenario_label[as.character(Scenario)],
    .bulked = !is.na(bulking_factor) & bulking_factor > 1,
    .partial = is.na(bulking_factor),
    `Volume (×10⁶ m³)` = r3(volume_m3 / 1e6),
    `Manning's n` = r3(manning_n),
    `Bulking factor` = r3(bulking_factor),
    `Peak Q (m³ s⁻¹)` = r3(peak_q_m3s),
    `Peak depth (m)` = r3(depth_max_m),
    `Peak velocity (m s⁻¹) [p95]` = r3(vel_p95_ms),
    `Peak velocity (m s⁻¹) [max]` = r3(vel_max_ms),
    `Wetted area (km²)` = r3(wetted_km2)
  )

stopifnot(
  nrow(table03) == 20,
  round(table03$`Peak Q (m³ s⁻¹)`[table03$Lake == "L29 Passu" & table03$Scenario == "Mid (baseline)"]) == 1258,
  round(table03$`Peak Q (m³ s⁻¹)`[table03$Lake == "L27 Shisper" & table03$Scenario == "Mid (baseline)"]) == 12546
)
cat("Check passed.\n")

table03 <- table03 |>
  mutate(
    `Volume (×10⁶ m³)`            = sprintf("%.3f", `Volume (×10⁶ m³)`),
    `Manning's n`                 = sprintf("%.3f", `Manning's n`),
    `Bulking factor`              = sprintf("%.3f", `Bulking factor`),
    `Peak Q (m³ s⁻¹)`             = sprintf("%.3f", `Peak Q (m³ s⁻¹)`),
    `Peak depth (m)`              = sprintf("%.3f", `Peak depth (m)`),
    `Peak velocity (m s⁻¹) [p95]` = sprintf("%.3f", `Peak velocity (m s⁻¹) [p95]`),
    `Peak velocity (m s⁻¹) [max]` = sprintf("%.3f", `Peak velocity (m s⁻¹) [max]`),
    `Wetted area (km²)`             = sprintf("%.3f", `Wetted area (km²)`),
    `Volume (×10⁶ m³)` = paste0(`Volume (×10⁶ m³)`, ifelse(.bulked, "¹", "")),
    Scenario            = paste0(Scenario, ifelse(.partial, "²", ""))
  ) |>
  select(-.bulked, -.partial)
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}¹?$", table03$`Volume (×10⁶ m³)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Manning's n`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Bulking factor`) | table03$`Bulking factor` == "NA"),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Peak Q (m³ s⁻¹)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Peak depth (m)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Peak velocity (m s⁻¹) [p95]`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Peak velocity (m s⁻¹) [max]`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", table03$`Wetted area (km²)`)),
  sum(grepl("¹$", table03$`Volume (×10⁶ m³)`)) == 8,
  sum(grepl("²$", table03$Scenario)) == 2
)
cat("Check passed.\n")
cat("Peak Q values (unrounded beyond 3 decimals):\n")
print(table03[, c("Lake", "Scenario", "Peak Q (m³ s⁻¹)")], row.names = FALSE)

dir.create("figures", showWarnings = FALSE)
write.csv(table03, "figures/table03_simulation_results.csv", row.names = FALSE)
cat("Saved: figures/table03_simulation_results.csv (", nrow(table03), "rows )\n")
