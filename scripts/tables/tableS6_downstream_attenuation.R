# Routed peaks at reference sections

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

routed <- read_csv("data/processed/routed_hydrograph_summary.csv", show_col_types = FALSE)
sim    <- read_csv("data/processed/all_simulations_combined_v2.csv", show_col_types = FALSE)

lake_label <- c(L27 = "L27 Shisper", L29 = "L29 Passu")

section_lookup <- tribble(
  ~lake,          ~section_raw,                                       ~Section,                                   ~Distance,
  "L27 Shisper",  "Karakoram Highway bridge, Hassanabad (~6.7 km)",   "Karakoram Highway bridge, Hassanabad",     "~6.7 km",
  "L27 Shisper",  "Hunza confluence (~8.9 km)",                       "Hassanabad Nullah - Hunza confluence",     "~8.9 km",
  "L27 Shisper",  "Domain outlet (~62 km)",                           "Model domain outlet",                      "~62 km",
  "L29 Passu",    "Hunza confluence (~5.7 km)",                       "Hunza valley floor",         "~5.7 km",
  "L29 Passu",    "Hunza, below Passu (~8.0 km)",                     "Hunza River below Passu",                  "~8.0 km",
  "L29 Passu",    "Inundation limit (~14.4 km)",                      "Limit of inundation",                      "~14.4 km",
  "L29 Passu",    "Domain outlet (~58 km)",                           "Model domain outlet",                      "~58 km"
)

t3_peak <- read_csv("data/processed/table03_master_data.csv", show_col_types = FALSE) |>
  filter(scenario_code == "Mid") |>
  transmute(lake = lake_label[lake_id], breach_peak = peak_q_m3s)

routed_rows <- routed |>
  left_join(section_lookup, by = c("lake", "section" = "section_raw")) |>
  left_join(t3_peak, by = "lake") |>
  mutate(pct_of_breach = 100 * peak_q / breach_peak) |>
  transmute(
    Lake = lake,
    Section,
    Distance,
    `Peak Q (m³ s⁻¹)` = round(peak_q),
    `% of breach peak` = sprintf("%.1f", pct_of_breach),
    `Time to peak (h)` = ifelse(is.na(t_peak_h), "-", sprintf("%.2f", t_peak_h))
  )

t3 <- read_csv("data/processed/table03_master_data.csv", show_col_types = FALSE)
breach_q <- t3 |>
  filter(scenario_code == "Mid") |>
  transmute(
    Lake = lake_label[lake_id],
    Section = "Breach inflow",
    Distance = "0",
    `Peak Q (m³ s⁻¹)` = round(peak_q_m3s),
    `% of breach peak` = "100.0",
    `Time to peak (h)` = "-"
  )

lake_order <- c("L27 Shisper", "L29 Passu")

tableS6 <- bind_rows(breach_q, routed_rows) |>
  mutate(
    Lake = factor(Lake, levels = lake_order),
    is_breach = Section == "Breach inflow"
  ) |>
  arrange(Lake, desc(is_breach)) |>
  select(-is_breach)

stopifnot(
  tableS6$`Peak Q (m³ s⁻¹)`[tableS6$Lake == "L29 Passu" & tableS6$Section == "Breach inflow"] == 1261,
  tableS6$`Peak Q (m³ s⁻¹)`[tableS6$Lake == "L27 Shisper" & tableS6$Section == "Breach inflow"] == 12546
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS6, "figures/tableS6_downstream_attenuation.csv", row.names = FALSE)
cat("Saved: figures/tableS6_downstream_attenuation.csv (", nrow(tableS6), "rows )\n")
