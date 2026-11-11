# Breach peaks against empirical relations

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

strip_superscript <- function(x) as.numeric(gsub("[¹²]", "", x))

t3  <- read_csv("figures/table03_simulation_results.csv", show_col_types = FALSE)
brp <- read_csv("data/processed/breach_parameters.csv", show_col_types = FALSE)

RHO <- 1000   # kg/m3
G   <- 9.81   # m/s2

core_scenarios <- c("Low", "Mid (baseline)", "High", "Mid (n = 0.04)",
                     "Mid (n = 0.06)", "Mid (×1.5 bulk, n=0.10)", "Mid (×2.0 bulk, n=0.10)")

hb_L29 <- brp$Hb_m[brp$lake_id == "L29" & brp$scenario == "Mid"]
hb_L27 <- brp$Hb_m[brp$lake_id == "L27" & brp$scenario == "Mid"]

khan_l27 <- "5348–6938"

fmt_num <- function(x) format(round(x), scientific = FALSE, trim = TRUE)

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

q_w_check <- 0.045 * (strip_superscript(t3$`Volume (×10⁶ m³)`[t3$Lake == "L29 Passu" & t3$Scenario == "Mid (baseline)"]) * 1e6)^0.66
stopifnot(
  nrow(table05) == 14,
  !anyNA(strip_superscript(t3$`Volume (×10⁶ m³)`[t3$Scenario %in% core_scenarios])),
  abs(q_w_check - 681) < 5,
  table05$`Khan et al. 2021 (m³ s⁻¹)`[table05$Lake == "L27 Shisper"][1] == "5348–6938"
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(table05, "figures/table05_validation_envelopes.csv", row.names = FALSE)
cat("Saved: figures/table05_validation_envelopes.csv (", nrow(table05), "rows )\n")
