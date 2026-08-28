
# ════════════════════════════════════════════════════════════════
# 04_fig04_priority_scoring.R
# Figure 4 — Priority Scoring Dumbbell Plot
# PURPOSE: Hazard score vs exposure score per lake, sorted by composite
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
#
# UPDATED (2026-08-21): supersedes 04_fig04_priority_scoring.R (moved to
# scripts/OLD/). Fix for R3.13: the previous version read
# data/processed/lake_priority_scores.csv, which used the four-indicator
# (tier-included) hazard composite and credited surge-type status to
# moraine-dammed lakes regardless of their own damming mechanism -- both
# since corrected. This version reads the corrected
# data/processed/lake_priority_scores.csv (three-indicator hazard
# composite; surge credit restricted to lakes that are themselves
# ice-dammed) produced by 14_priority_scoring.R, which must be run
# first. Column names also changed: composite_score -> final_score, and
# the old boolean `selected` column doesn't exist in the new CSV -- it's
# derived here as lake_id %in% c("L27","L29"). Under the corrected scoring
# L27 Shisper now ranks first (0.758) and L29 Passu second (0.728),
# reversing the previous order.
# ════════════════════════════════════════════════════════════════

# =============================================================================
# Fig 4 — Priority Scoring Dumbbell Plot
#          Hazard score vs exposure score per lake, sorted by composite
# Output: figures/fig04o_priority_scores.pdf  (84 mm wide, single-column)
# Input:  data/processed/lake_priority_scores.csv
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# ── 1. Load + sort by composite score ────────────────────────────────────────

df <- read_csv("data/processed/lake_priority_scores.csv",
               show_col_types = FALSE) |>
  mutate(selected = lake_id %in% c("L27", "L29")) |>
  arrange(final_score) |>
  mutate(label = factor(lake_id, levels = lake_id))

# ── 2. Long form for dots ─────────────────────────────────────────────────────

df_long <- df |>
  pivot_longer(
    cols      = c(hazard_score, exposure_score),
    names_to  = "score_type",
    values_to = "score"
  ) |>
  mutate(
    score_type = recode(score_type,
                        "hazard_score"   = "Hazard",
                        "exposure_score" = "Exposure")
  )

# ── 3. Colours ────────────────────────────────────────────────────────────────

col_hazard    <- "#C03020"   # red    — hazard
col_exposure  <- "#2166AC"   # blue   — exposure
col_connector <- "#CCCCCC"   # light grey connector line
col_composite <- "#444444"   # dark grey — composite tick
col_sel_bg    <- "#FFF3E0"   # highlight band for selected lakes

# ── 4. Plot ───────────────────────────────────────────────────────────────────

# Highlight bands for selected lakes
selected_lakes <- df |> filter(selected) |> pull(label)

p <- ggplot() +

  # Highlight band for selected lakes
  geom_tile(
    data = df |> filter(selected),
    aes(x = 0.5, y = label, width = 1, height = 0.85),
    fill = col_sel_bg, colour = NA
  ) +

  # Dumbbell connector (hazard to exposure)
  geom_segment(
    data = df,
    aes(x = hazard_score, xend = exposure_score,
        y = label, yend = label),
    colour = col_connector, linewidth = 0.7
  ) +

  # Composite score tick mark
  geom_point(
    data = df,
    aes(x = final_score, y = label),
    shape = 124,   # vertical bar
    size  = 3.5,
    colour = col_composite
  ) +

  # Hazard + exposure dots
  geom_point(
    data = df_long,
    aes(x = score, y = label, colour = score_type),
    size = 2.2
  ) +

  # Selected lake labels
  geom_text(
    data = df |> filter(selected),
    aes(x = 1.01, y = label,
        label = paste0(lake_id, " ★")),
    hjust = 0, size = 2.1, fontface = "bold",
    colour = "#C05010"
  ) +

  scale_colour_manual(
    values = c("Hazard" = col_hazard, "Exposure" = col_exposure),
    name   = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 1.14),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0.01, 0))
  ) +

  labs(
    x       = "Score (0–1)",
    y       = NULL,
    caption = "Dots: hazard (red) and exposure (blue) scores. Bar: composite score.
Highlighted rows: selected lakes (L27 Shisper, L29 Passu)."
  ) +

  theme_classic(base_size = 9) +   # bumped from 7 for label legibility (R4.9)
  theme(
    legend.position      = c(0.01, 0.01),
    legend.justification = c(0, 0),
    legend.key.size      = unit(3, "mm"),
    legend.text          = element_text(size = 7.5),
    legend.background    = element_rect(fill = "white", colour = "#cccccc",
                                        linewidth = 0.3),
    legend.margin        = margin(2, 4, 2, 4),
    axis.text.y          = element_text(size = 7.5, colour = "#333333"),
    axis.text.x          = element_text(size = 7.5),
    axis.title.x         = element_text(size = 9),
    axis.line.y          = element_blank(),
    axis.ticks.y         = element_blank(),
    panel.grid.major.x   = element_line(colour = "#eeeeee", linewidth = 0.3),
    plot.caption         = element_text(size = 7, colour = "#666666",
                                        hjust = 0, margin = margin(t = 3)),
    plot.margin          = margin(4, 12, 4, 4, "mm"),
    plot.background      = element_rect(fill = "white", colour = NA),
    panel.background     = element_rect(fill = "white", colour = NA)
  )
OUT_DIR <- "C:/Users/sadaf/Documents/PPR3/figures"

# ── 5. Export ─────────────────────────────────────────────────────────────────

n_lakes <- nrow(df)
fig_h   <- max(60, n_lakes * 5.8)

ggsave(file.path(OUT_DIR, "fig04_v4_priority_scoring.png"), p,
       width = 84, height = fig_h, units = "mm",
       dpi = 800, bg = "white")

ggsave(file.path(OUT_DIR, "fig04_v4_priority_scoring.tiff"), p,
       width = 84, height = fig_h, units = "mm",
       dpi = 800, bg = "white", compression = "lzw")

cat("Done! Saved to", OUT_DIR, "\n")
