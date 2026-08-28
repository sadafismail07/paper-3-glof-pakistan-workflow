# ============================================================
# tableS6_downstream_attenuation.R
#
# Purpose: build Table S6 (downstream attenuation of the mid-
# baseline scenario peak discharge from the breach inflow to the
# corridor outlet, for the two simulated lakes) and write it to a
# clean CSV for the GitHub repository.
#
# Sources (read-only, not modified by this script):
#   data/processed/routed_hydrograph_summary.csv
#     -- routed peak discharge at each downstream reference section
#        (does NOT include a breach-inflow / distance-0 row).
#   data/processed/all_simulations_combined_v2.csv
#     -- supplies the breach-inflow peak Q (Mid_baseline scenario)
#        used to synthesise the "Breach inflow" row at distance 0.
#        Same file used for Table 3 / Table 5 / Fig. 11 -- do NOT
#        substitute ALL_simulation_results_VERIFIED.csv or
#        all_simulations_combined.csv (without _v2), which carry a
#        stale L29 value (1,258 instead of 1,261).
#
# Output: figures/tableS6_downstream_attenuation.csv
# ============================================================

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

routed <- read_csv("data/processed/routed_hydrograph_summary.csv", show_col_types = FALSE)
sim    <- read_csv("data/processed/all_simulations_combined_v2.csv", show_col_types = FALSE)

lake_label <- c(L27 = "L27 Shisper", L29 = "L29 Passu")

# Section-label and distance lookup: the raw "section" text in
# routed_hydrograph_summary.csv doesn't match the published table's
# section names 1:1 (e.g. "Near-source (~2 km)" means "Hassanabad
# Nullah confluence" for L27 but "Upper reach" for L29) -- this table
# maps (lake, raw section) -> (display section, display distance).
section_lookup <- tribble(
  ~lake,          ~section_raw,              ~Section,                       ~Distance,
  "L27 Shisper",  "Near-source (~2 km)",     "Hassanabad Nullah confluence", "~2 km",
  "L27 Shisper",  "Corridor outlet",         "Corridor outlet",              "~50 km",
  "L29 Passu",    "Near-source (~2 km)",     "Upper reach",                  "~2 km",
  "L29 Passu",    "Inundation limit (~7 km)","Inundation limit",             "~7 km",
  "L29 Passu",    "Corridor outlet",         "Corridor outlet",              "~39 km"
)

# ---- 1. Routed sections (from routed_hydrograph_summary.csv) ---------------

routed_rows <- routed |>
  left_join(section_lookup, by = c("lake", "section" = "section_raw")) |>
  transmute(
    Lake = lake,
    Section,
    Distance,
    `Peak Q (m³ s⁻¹)` = round(peak_q),
    `% of breach peak` = sprintf("%.1f", pct_of_breach),
    `Time to peak (h)` = ifelse(is.na(t_peak_h), "-", sprintf("%.2f", t_peak_h))
  )

# ---- 2. Synthesised "Breach inflow" row per lake (distance 0, 100%) --------

breach_q <- sim |>
  filter(scenario == "Mid_baseline") |>
  transmute(
    Lake = lake_label[lake],
    Section = "Breach inflow",
    Distance = "0",
    `Peak Q (m³ s⁻¹)` = round(peak_Q_input_m3s),
    `% of breach peak` = "100.0",
    `Time to peak (h)` = "-"
  )

# ---- 3. Combine, order rows: breach inflow first, then downstream ----------

lake_order <- c("L27 Shisper", "L29 Passu")

tableS6 <- bind_rows(breach_q, routed_rows) |>
  mutate(
    Lake = factor(Lake, levels = lake_order),
    is_breach = Section == "Breach inflow"
  ) |>
  arrange(Lake, desc(is_breach)) |>
  select(-is_breach)

# ---- 4. Sanity check ---------------------------------------------------------
stopifnot(
  tableS6$`Peak Q (m³ s⁻¹)`[tableS6$Lake == "L29 Passu" & tableS6$Section == "Breach inflow"] == 1261,
  tableS6$`Peak Q (m³ s⁻¹)`[tableS6$Lake == "L27 Shisper" & tableS6$Section == "Breach inflow"] == 12546
)
cat("Sanity check passed: L29 breach inflow = 1261 m3/s, L27 = 12546 m3/s\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS6, "figures/tableS6_downstream_attenuation.csv", row.names = FALSE)
cat("Saved: figures/tableS6_downstream_attenuation.csv (", nrow(tableS6), "rows )\n")
