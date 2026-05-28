# ============================================================
# fig08_v6_tornado.R
# ============================================================
setwd("C:/Users/sadaf/Documents/PPR3")
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
OUT_DIR   <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_W     <- 24.0
FIG_H     <- 14.0
FIG_DPI   <- 600
BASE_SIZE <- 7
# ── 1. Load & merge verified data ─────────────────────────────
ver <- read_csv("data/processed/ALL_simulation_results_VERIFIED.csv",
                show_col_types = FALSE) %>%
  mutate(scenario = recode(scenario,
    "Mid_b15" = "Mid_bulk1.5",
    "Mid_b20" = "Mid_bulk2.0"))
combined <- read_csv("data/processed/all_simulations_combined.csv",
                     show_col_types = FALSE) %>%
  rename(lake = lake_id) %>%
  mutate(scenario = recode(scenario,
    "Mid_bulk15" = "Mid_bulk1.5",
    "Mid_bulk20" = "Mid_bulk2.0",
    "Mid_b15"    = "Mid_bulk1.5",
    "Mid_b20"    = "Mid_bulk2.0")) %>%
  select(-any_of(c("peak_depth_m",
                   "wetted_area_km2",
                   "peak_velocity_p95",
                   "peak_velocity_max")))
sim <- combined %>%
  left_join(
    ver %>% select(lake, scenario, peak_depth_m, peak_vel_p95,
                   peak_vel_max, wetted_area_km2),
    by = c("lake", "scenario")
  )
cat("sim columns:", paste(names(sim), collapse = ", "), "\n")
cat("peak_depth_m NAs:", sum(is.na(sim$peak_depth_m)), "\n")
cat("peak_vel_p95 NAs:", sum(is.na(sim$peak_vel_p95)), "\n\n")
d29 <- sim %>% filter(lake == "L29")
d27 <- sim %>% filter(lake == "L27")
# ── 2. Compute percent change from Mid baseline ─────────────
compute_tornado <- function(df, lake_label) {
  mid       <- df %>% filter(scenario == "Mid_baseline")
  mid_depth <- mid$peak_depth_m
  mid_vel   <- mid$peak_vel_p95
  mid_area  <- mid$wetted_area_km2
  scens <- df %>%
    filter(scenario != "Mid_baseline") %>%
    mutate(
      pct_depth = (peak_depth_m - mid_depth) / mid_depth * 100,
      pct_vel   = (peak_vel_p95 - mid_vel) / mid_vel * 100,
      pct_area  = (wetted_area_km2 - mid_area) / mid_area * 100,
      param = case_when(
        scenario == "Low"         ~ "Volume (Low)",
        scenario == "High"        ~ "Volume (High)",
        scenario == "Mid_n04"     ~ "Manning n = 0.04",
        scenario == "Mid_n06"     ~ "Manning n = 0.06",
        scenario == "Mid_bulk1.5" ~ "Bulking f = 1.5",
        scenario == "Mid_bulk2.0" ~ "Bulking f = 2.0"
      )
    )
  long <- scens %>%
    select(param, pct_depth, pct_vel, pct_area) %>%
    pivot_longer(
      cols      = c(pct_depth, pct_vel, pct_area),
      names_to  = "metric",
      values_to = "pct_change"
    ) %>%
    mutate(
      metric = recode(metric,
        pct_depth = "Peak depth",
        pct_vel   = "Peak velocity",
        pct_area  = "Wetted area"
      ),
      lake = lake_label,
      show_label = abs(pct_change) >= 2.0
    )
  return(long)
}
long_l29 <- compute_tornado(d29, "L29 Passu")
long_l27 <- compute_tornado(d27, "L27 Shisper")
# ── 3. Baseline subtitles ───────────────────────────────────
get_mid <- function(df, col) {
  df %>% filter(scenario == "Mid_baseline") %>% pull({{ col }})
}
sub_l29 <- sprintf(
  "Mid baseline: depth %.2f m | velocity %.2f m s-1 | wetted area %.2f km2",
  get_mid(d29, peak_depth_m),
  get_mid(d29, peak_vel_p95),
  get_mid(d29, wetted_area_km2)
)
sub_l27 <- sprintf(
  "Mid baseline: depth %.2f m | velocity %.2f m s-1 | wetted area %.2f km2",
  get_mid(d27, peak_depth_m),
  get_mid(d27, peak_vel_p95),
  get_mid(d27, wetted_area_km2)
)
# ── 4. Factor ordering ──────────────────────────────────────
param_order <- c(
  "Manning n = 0.04",
  "Manning n = 0.06",
  "Volume (Low)",
  "Volume (High)",
  "Bulking f = 1.5",
  "Bulking f = 2.0"
)
metric_order <- c(
  "Peak depth",
  "Peak velocity",
  "Wetted area"
)
long_l29 <- long_l29 %>%
  mutate(
    param = factor(param, levels = param_order),
    metric = factor(metric, levels = metric_order)
  )
long_l27 <- long_l27 %>%
  mutate(
    param = factor(param, levels = param_order),
    metric = factor(metric, levels = metric_order)
  )
# ── 5. Colors ───────────────────────────────────────────────
metric_cols <- c(
  "Peak depth"    = "#2166AC",
  "Peak velocity" = "#762A83",
  "Wetted area"   = "#1B7837"
)
# ── 6. Save placeholder end ─────────────────────────────────
cat("Script loaded successfully.\n")
