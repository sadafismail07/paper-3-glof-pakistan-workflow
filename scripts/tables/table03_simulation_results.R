# ============================================================
# table03_simulation_results.R
#
# TABLE SCRIPT: reads ONLY the canonical CSV written by
# scripts/analysis/23_table03_data_extraction.R. Does not open a
# raster, a hydrograph file, or any parameter table itself. Run
# 23_table03_data_extraction.R first whenever the underlying
# simulation data changes; this script only formats its output
# for the manuscript/repository.
#
# ROUNDING: values are rounded to at most 3 decimal places
# (round(x, 3), a no-op on a value that already has 3 or fewer
# decimals). Peak Q previously rounded to a whole number, which
# produced a 2,522-vs-2,523 ambiguity on the L29 x2.0 bulk row
# (1261.3 x 2 = 2522.6); it now displays as 2522.6, sidestepping
# that rounding-path judgment call entirely.
#
# FOOTNOTES: Table 3's caption carries two footnotes. Footnote 1
# explains that bulked-scenario Volume values are released volumes,
# not lake volumes (8 rows: the x1.5/x2.0 bulk rows for both lakes)
# -- "¹" is appended directly to the VOLUME cell for these rows
# (e.g. "3.231¹"). Footnote 2 explains the two partial-drainage
# scenarios (45%/28% released fractions, breach heights 60 m/37 m)
# as an alternative drainage assumption (2 rows: p45 and p28, both
# L27, the same rows where Bulking factor is NA) -- "²" is appended
# directly to the SCENARIO cell for these rows (e.g. "Partial
# drainage 45%²"). This makes the Volume column non-numeric in the
# written CSV for the 8 flagged rows, so
# table05_validation_envelopes.R -- the only other script
# that reads this Volume column back in for arithmetic -- strips a
# trailing ¹/² before converting to numeric. Any future script that
# reads figures/table03_simulation_results.csv's Volume or Scenario
# column expecting a clean number/label needs the same strip
# (grep -rn "table03_simulation_results.csv" scripts/ to find
# consumers).
#
# Source: data/processed/table03_master_data.csv
# Output: figures/table03_simulation_results.csv
# ============================================================

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

r3 <- function(x) round(x, 3)   # round-to-<=3-decimals helper; no-op if already <=3

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

# ---- Sanity checks (numeric, before display formatting) ---------------------
# Peak Q now carries decimals (e.g. 1261.3), so checks compare the rounded
# whole-number identity, not exact display value.
stopifnot(
  nrow(table03) == 20,
  round(table03$`Peak Q (m³ s⁻¹)`[table03$Lake == "L29 Passu" & table03$Scenario == "Mid (baseline)"]) == 1261,
  round(table03$`Peak Q (m³ s⁻¹)`[table03$Lake == "L27 Shisper" & table03$Scenario == "Mid (baseline)"]) == 12546
)
cat("Sanity check passed: 20 rows, formatted from data/processed/table03_master_data.csv.\n")

# ---- Standardize display to exactly 3 decimal places -----------------------
# Bulking factor is NA for the two partial-drainage scenarios (no bulking
# applies). sprintf("%.3f", NA_real_) returns the literal STRING "NA"
# (two characters), not a genuine R NA, so a downstream is.na() check on
# the formatted value is FALSE, not TRUE. The sanity check below tests
# directly for the literal string "NA" instead, which also matches the
# display already used for this column elsewhere in Table 3.
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
    # Footnote superscripts baked directly into the data cells -- no
    # separate flag column. Applied last, after every other column is
    # already its final display string, so nothing downstream in this
    # script re-parses these two columns as numbers.
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
cat("Sanity check passed: all numeric columns display exactly 3 decimal places; 8 Volume cells carry a trailing ¹, 2 Scenario cells carry a trailing ².\n")
cat("Peak Q values (unrounded beyond 3 decimals):\n")
print(table03[, c("Lake", "Scenario", "Peak Q (m³ s⁻¹)")], row.names = FALSE)

dir.create("figures", showWarnings = FALSE)
write.csv(table03, "figures/table03_simulation_results.csv", row.names = FALSE)
cat("Saved: figures/table03_simulation_results.csv (", nrow(table03), "rows )\n")
