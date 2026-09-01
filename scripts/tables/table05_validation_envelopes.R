# ============================================================
# table05_validation_envelopes.R
#
# Purpose: build Table 5 (comparison of the 14 simulated HEC-RAS
# breach peak discharges against the Walder & O'Connor (1997) and
# Costa & Schuster (1988) empirical envelopes, and the Khan et al.
# (2021) published values for L27 Shisper) and write it to a clean
# CSV for the GitHub repository.
#
# Formulas (as stated in the manuscript, Section 3.11 / Fig. 11
# caption -- reproduced here, not re-derived):
#   Walder & O'Connor (1997), moraine-dammed lakes only:
#       Q_w = 0.045 * V0^0.66                (V0 in m^3, Q_w in m3/s)
#       Applicable to L29 Passu only (moraine-dammed). L27 Shisper is
#       ice-dammed, so this column is "--" for L27 rows.
#   Costa & Schuster (1988), potential-energy envelope, both lakes:
#       Q_c = 0.063 * PE^0.42
#       PE  = V * Hb * rho * g   (rho = 1000 kg/m3, g = 9.81 m/s2)
#
# Khan et al. (2021) values are a literature constant (not derived
# here): reported modelled peak discharges for the 2019-2020
# hypothetical L27 Shisper outburst scenarios, applicable to L27 only.
#
# This table reports the 14 core sensitivity-matrix scenarios only
# (matching the currently submitted Table 5); the partial-drainage
# and n=0.10 rows in Table 3 are not repeated here.
#
# Breach height (Hb) is read separately from breach_parameters.csv
# (constant per lake: 37.5 m for L29, 134 m for L27) since Table 3's
# output does not carry Hb as its own column.
#
# Data source: reads Peak Q, Vw_m3 and Hb_m from
# figures/table03_simulation_results.csv (the raster/hydrograph-
# derived output of table03_simulation_results.R) rather than
# data/processed/all_simulations_combined_v2.csv, so Table 3 and
# Table 5 cannot silently drift apart from each other -- run
# table03_simulation_results.R first, every time.
#
# Revision notes:
#  - Khan et al. (2021) L27 comparison range corrected to
#    "5348–6938" (matches manuscript Section 3.11/4.4 and the
#    submitted Table 5); a previous version had this hardcoded to
#    the wrong range, "4,500-6,937".
#  - core_scenarios points at the bulked-scenario rows run at n=0.10
#    (the roughness value adopted per R3.25), not the superseded
#    n=0.05 rows -- "Mid (×1.5 bulk)" no longer exists as a bare
#    label once table03 split the bulked scenarios into separate
#    n=0.05/n=0.10 columns, which had silently dropped nrow(table05)
#    from 14 to 10 until this filter was updated to match the new
#    labels.
#  - table03_simulation_results.R bakes footnote superscripts
#    directly into its Volume/Scenario cells rather than a separate
#    flag column, so "¹" is appended straight onto the Volume cell
#    for the 8 bulked-scenario rows. Four of those rows (the n=0.10
#    x1.5/x2.0 bulk scenarios, both lakes) are inside this script's
#    own core_scenarios list, so `t3`'s Volume column now reads in as
#    character rather than numeric once any value fails a clean
#    numeric parse. strip_superscript() below handles this everywhere
#    Volume is read numerically, instead of relying on read_csv's
#    automatic type inference.
#
# Output: figures/table05_validation_envelopes.csv
# ============================================================

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

# Strips a trailing footnote superscript (¹ or ²) before numeric conversion --
# see the revision notes above. Safe to apply to every row: rows with no
# superscript are returned unchanged.
strip_superscript <- function(x) as.numeric(gsub("[¹²]", "", x))

t3  <- read_csv("figures/table03_simulation_results.csv", show_col_types = FALSE)
brp <- read_csv("data/processed/breach_parameters.csv", show_col_types = FALSE)

RHO <- 1000   # kg/m3
G   <- 9.81   # m/s2

core_scenarios <- c("Low", "Mid (baseline)", "High", "Mid (n = 0.04)",
                     "Mid (n = 0.06)", "Mid (×1.5 bulk, n=0.10)", "Mid (×2.0 bulk, n=0.10)")   # updated for R3.25 roughness correction

hb_L29 <- brp$Hb_m[brp$lake_id == "L29" & brp$scenario == "Mid"]
hb_L27 <- brp$Hb_m[brp$lake_id == "L27" & brp$scenario == "Mid"]

# Khan et al. (2021) hypothetical 2019-2020 outburst range for L27 Shisper
# -- fixed literature value, the same for every L27 scenario row. Matches
# the manuscript text (Sections 3.11, 4.4) and the currently submitted
# Table 5; the previous script version had this wrong ("4,500-6,937").
khan_l27 <- "5348–6938"

fmt_num <- function(x) format(round(x), scientific = FALSE, trim = TRUE)  # no thousands separator -- required by the target journal's table formatting guidelines

table05 <- t3 |>
  filter(Scenario %in% core_scenarios) |>
  mutate(
    Lake_short = ifelse(Lake == "L29 Passu", "L29", "L27"),
    Scenario = factor(Scenario, levels = core_scenarios),
    Vw_m3 = strip_superscript(`Volume (×10⁶ m³)`) * 1e6,
    Hb_m  = ifelse(Lake_short == "L29", hb_L29, hb_L27),
    PE   = Vw_m3 * Hb_m * RHO * G,
    Q_w  = ifelse(Lake_short == "L29", 0.045 * Vw_m3^0.66, NA_real_),
    Q_c  = 0.063 * PE^0.42
  ) |>
  arrange(desc(Lake_short), Scenario) |>
  transmute(
    Lake = Lake,
    Scenario = as.character(Scenario),
    `HEC-RAS Q (m³ s⁻¹)` = fmt_num(`Peak Q (m³ s⁻¹)`),
    `Walder & O'Connor 1997 (m³ s⁻¹)` = ifelse(is.na(Q_w), "—", fmt_num(Q_w)),
    `Costa & Schuster 1988 (m³ s⁻¹)` = fmt_num(Q_c),
    `Khan et al. 2021 (m³ s⁻¹)` = ifelse(Lake == "L27 Shisper", khan_l27, "—")
  )

# ---- Sanity checks -----------------------------------------------------------
q_w_check <- 0.045 * (strip_superscript(t3$`Volume (×10⁶ m³)`[t3$Lake == "L29 Passu" & t3$Scenario == "Mid (baseline)"]) * 1e6)^0.66
stopifnot(
  nrow(table05) == 14,
  !anyNA(strip_superscript(t3$`Volume (×10⁶ m³)`[t3$Scenario %in% core_scenarios])),
  abs(q_w_check - 681) < 5,
  table05$`Khan et al. 2021 (m³ s⁻¹)`[table05$Lake == "L27 Shisper"][1] == "5348–6938"
)
cat("Sanity check passed: 14 rows, L29 Mid Walder & O'Connor ≈", round(q_w_check),
    "m3/s (expect 681), Khan et al. range 5348–6938 (no thousands separator).\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table05, "figures/table05_validation_envelopes.csv", row.names = FALSE)
cat("Saved: figures/table05_validation_envelopes.csv (", nrow(table05), "rows )\n")
