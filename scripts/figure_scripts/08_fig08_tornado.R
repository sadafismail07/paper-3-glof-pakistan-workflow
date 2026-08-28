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
FIG_W     <- 22.0
FIG_H     <- 26.0
FIG_DPI   <- 800   # bumped from 600 to 800 dpi for repo/publication release
BASE_SIZE <- 9   # bumped from 7 for label legibility (R4.9)
# ── 1. Load & merge verified data ─────────────────────────────
ver <- read_csv("data/processed/ALL_simulation_results_VERIFIED.csv",
                show_col_types = FALSE) %>%
  mutate(scenario = recode(scenario,
    "Mid_b15" = "Mid_bulk1.5",
    "Mid_b20" = "Mid_bulk2.0"))
# NOTE: all_simulations_combined.csv (no suffix) no longer exists on
# disk -- reading all_simulations_combined_v2.csv instead. That file
# already uses a "lake" column (not "lake_id"), so no rename is needed.
combined <- read_csv("data/processed/all_simulations_combined_v2.csv",
                     show_col_types = FALSE) %>%
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

# ── 6. Group assignment and bold/pastel shading ──────────────
# Each of the 6 "param" levels belongs to one of three parameter
# groups, and to one of two rows within that group (the higher-
# magnitude / higher-value member of the pair rendered at full
# opacity, the other at reduced opacity).
group_of <- function(param) {
  case_when(
    param %in% c("Bulking f = 1.5", "Bulking f = 2.0")   ~ "Sed. bulking",
    param %in% c("Volume (Low)", "Volume (High)")        ~ "Volume",
    param %in% c("Manning n = 0.04", "Manning n = 0.06") ~ "Manning n"
  )
}
shade_of <- function(param) {
  ifelse(param %in% c("Bulking f = 2.0", "Volume (High)", "Manning n = 0.06"),
         "bold", "pastel")
}
group_cols <- c(
  "Sed. bulking" = "#D6604D",
  "Volume"       = "#1B7837",
  "Manning n"    = "#2166AC"
)
group_order <- c("Manning n", "Volume", "Sed. bulking")  # bottom-to-top, matches param_order

for (df_name in c("long_l29", "long_l27")) {
  df <- get(df_name)
  df$group <- factor(group_of(df$param), levels = group_order)
  df$shade <- shade_of(df$param)
  assign(df_name, df)
}

# ── 7. Plot builder ────────────────────────────────────────
build_panel <- function(df, panel_title, subtitle, x_limits) {
  ggplot(df, aes(x = param, y = pct_change, fill = metric, alpha = shade)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(
      data = df[df$show_label, ],
      aes(label = sprintf("%+.1f%%", pct_change),
          hjust = ifelse(pct_change >= 0, -0.15, 1.15)),
      position = position_dodge(width = 0.75),
      size = BASE_SIZE * 0.42, alpha = 1, color = "black",
      check_overlap = TRUE
    ) +
    geom_hline(yintercept = 0, color = "grey20", linewidth = 0.3) +
    # ylim (not a hard scale limit) zooms the view without dropping data
    # outside it -- the group-name labels below are placed just past
    # this range and rely on clip = "off" to still be drawn.
    coord_flip(ylim = x_limits, clip = "off") +
    scale_fill_manual(values = metric_cols, name = "Metric") +
    scale_alpha_manual(values = c(bold = 1, pastel = 0.45), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", ""), x, "%"),
                        expand = expansion(mult = 0.08)) +
    labs(title = panel_title, subtitle = subtitle,
         x = NULL, y = "% change from Mid baseline") +
    theme_minimal(base_size = BASE_SIZE * 1.5) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y   = element_text(size = rel(0.85)),
      plot.title    = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle = element_text(color = "grey30", size = rel(0.8)),
      legend.position = "bottom",
      plot.margin = margin(t = 5, r = 15, b = 5, l = 45)
    )
}

# Horizontal separators between the three parameter groups (drawn as
# dashed lines between params 2/3 and 4/5 of the 6-level y-axis).
add_group_separators <- function(p) {
  p + geom_hline(yintercept = 0) +
    annotate("segment", x = 2.5, xend = 2.5, y = -Inf, yend = Inf,
             linetype = "dashed", color = "grey50", linewidth = 0.3) +
    annotate("segment", x = 4.5, xend = 4.5, y = -Inf, yend = Inf,
             linetype = "dashed", color = "grey50", linewidth = 0.3)
}

# Group-name side labels with a coloured vertical tick, drawn outside
# the panel using coord_flip(clip = "off") plus expanded plot margin
# (set in build_panel() above).
add_group_labels <- function(p, df, y_min) {
  labs_df <- df %>%
    distinct(param, group) %>%
    mutate(x = as.numeric(factor(param, levels = param_order))) %>%
    group_by(group) %>%
    summarise(x_mid = mean(x), .groups = "drop")
  p +
    geom_text(
      data = labs_df,
      aes(x = x_mid, y = y_min, label = group, color = group),
      inherit.aes = FALSE, fontface = "bold", hjust = 1,
      size = BASE_SIZE * 0.35
    ) +
    scale_color_manual(values = group_cols, guide = "none")
}

p29 <- build_panel(long_l29, "(a) L29 Passu", sub_l29, c(-32, 32))
p29 <- add_group_separators(p29)
p29 <- add_group_labels(p29, long_l29, y_min = -40)

p27 <- build_panel(long_l27, "(b) L27 Shisper", sub_l27, c(-42, 42))
p27 <- add_group_separators(p27)
p27 <- add_group_labels(p27, long_l27, y_min = -50)

p_final <- plot_grid(p29, p27, ncol = 1, align = "v")

# ── 8. Save ────────────────────────────────────────────────
dir.create(OUT_DIR, showWarnings = FALSE)

ggsave(file.path(OUT_DIR, "fig08_v6_tornado.png"), p_final,
       width = FIG_W, height = FIG_H, units = "cm",
       dpi = FIG_DPI, bg = "white")

ggsave(file.path(OUT_DIR, "fig08_v6_tornado.tiff"), p_final,
       width = FIG_W, height = FIG_H, units = "cm",
       dpi = FIG_DPI, bg = "white", compression = "lzw")

cat("Saved: fig08_v6_tornado.png + .tiff\n")
