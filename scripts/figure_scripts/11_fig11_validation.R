# ════════════════════════════════════════════════════════════════
# 11_fig11_validation.R
# Figure 11 —  Validation Comparison
# PURPOSE: Two-panel dot plot (faceted by lake) on log-scale x-axis. Each dot is a
#          peak discharge estimate from a different source:
#          Blue circles: HEC-RAS runs (Low/Mid/High/bulk x2)
#          Gold triangles: empirical equations (Walder & O'Connor 1997, Costa & Schuster 1988)
#          Pink squares: published literature (Khan et al. 2021)
#          Shows the simulated results fall within/near the empirical envelope.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
#
# UPDATED (2026-08-21, rev 1): supersedes 11_fig11_validation.R (moved to
# scripts/OLD/). Two problems fixed:
#
# 1. WRONG EQUATIONS. The previous version's "Empirical" points used
#    "Froehlich (1995)" and "Hagen (1982)" -- neither appears anywhere in
#    the manuscript text or reference list, and Costa & Schuster (1988)
#    was missing from the figure entirely, despite being one of the two
#    equations the manuscript's Section 4.4 / Table 5 actually describe
#    and compare against. The Walder & O'Connor coefficient used (0.0347)
#    also didn't match the manuscript's own formula (0.045). This version
#    uses exactly the two equations Table 5 reports: Walder & O'Connor
#    (1997) Q = 0.045 * V^0.66 (L29 moraine-dammed scope only) and Costa &
#    Schuster (1988) Q = 0.063 * (rho*g*V*H)^0.42 (both lakes). Computed
#    values were cross-checked against Table 5's printed Mid-scenario
#    numbers before use: Walder L29 Mid = 681 (matches), Costa L29 Mid =
#    6265 (matches), Costa L27 Mid = 25216 (matches) -- all three agree
#    with the manuscript to the reported precision.
#
# 2. BROKEN DATA SOURCE. The previous version read
#    data/processed/L29_Passu_simulation_results.csv and
#    L27_Shisper_simulation_results.csv, which were archived to
#    data/processed/OLD/ this session as superseded by the _VERIFIED
#    versions. The _VERIFIED per-lake files don't carry a peak-discharge
#    column at all (only depth/velocity/wetted-area -- simulated outputs,
#    not the input boundary condition). This version reads
#    data/processed/ALL_simulation_results_VERIFIED.csv instead, which
#    has peak_Q_input_m3s directly and consistent scenario naming across
#    both lakes.
#
# UPDATED (2026-08-21, rev 2): fixed a second data-source problem found
# after re-checking the rendered figure against Table 3/Table 5's printed
# numbers. ALL_simulation_results_VERIFIED.csv's peak_Q_input_m3s column
# for L29 does NOT match the values actually used to build the published
# Table 3 / Table 5 (e.g. Mid_baseline: 1258.1 in that file vs. 1,261 in
# Table 3, Table 5, and the manuscript body text at Section 4.4 -- also
# off for High: 1547.2 vs 1,547, Mid_bulk1.5: 1887.1 vs 1,892, and
# Mid_bulk2.0: 2516.1 vs 2,522). L27's values in that file happened to
# match, which is why the mismatch wasn't caught earlier -- only L29 is
# affected. data/processed/all_simulations_combined_v2.csv carries the
# values that DO match Table 3 exactly (verified row-by-row against
# figures/table03_simulation_results.csv for both lakes, all 14
# scenarios, before switching). This version reads that file instead, so
# the figure is now numerically consistent with Tables 3 and 5. Scenario
# naming differs slightly between the two source files ("Mid_bulk2.0" in
# the old source vs. "Mid_bulk20" in this one) -- scens_show and the
# case_when() label mapping below were updated to match.
# ════════════════════════════════════════════════════════════════

setwd("C:/Users/sadaf/Documents/PPR3")

library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)

OUT_DIR   <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_W     <- 17.4
FIG_H     <- 9.0
FIG_DPI   <- 800   # bumped from 600 to 800 dpi for repo/publication release
BASE_SIZE <- 9   # bumped from 7 for label legibility (R4.9)

# ── 1. Load simulation results (matches Table 3 / Table 5 exactly) ──────────

sim <- read.csv("data/processed/all_simulations_combined_v2.csv")

scens_show <- c("Low", "Mid_baseline", "High", "Mid_bulk20")

make_hecras <- function(df, lake_code, lake_label) {
  df |>
    filter(lake == lake_code, scenario %in% scens_show) |>
    mutate(
      lake   = lake_label,
      Q      = peak_Q_input_m3s,
      label  = case_when(
        scenario == "Low"          ~ "HEC-RAS: Low",
        scenario == "Mid_baseline" ~ "HEC-RAS: Mid",
        scenario == "High"         ~ "HEC-RAS: High",
        scenario == "Mid_bulk20"   ~ "HEC-RAS: Bulk ×2.0"
      ),
      source = "HEC-RAS"
    )
}

hecras_l29 <- make_hecras(sim, "L29", "L29 Passu")
hecras_l27 <- make_hecras(sim, "L27", "L27 Shisper")

# ── 2. Empirical (Walder & O'Connor 1997; Costa & Schuster 1988) ────────────
# Mid-scenario volumes/heights, matching 15_volume_dam_breach_calculations.R
# and the manuscript's Table 5 exactly.

V29 <- 2154184;  H29 <- 37.5
V27 <- 16600000; H27 <- 134

q_walder <- function(V) 0.045 * V^0.66
q_costa  <- function(V, H) 0.063 * (1000 * 9.81 * V * H)^0.42

emp_l29 <- data.frame(
  lake   = "L29 Passu",
  source = "Empirical",
  label  = c("Walder & O'Connor (1997)", "Costa & Schuster (1988)"),
  Q      = c(q_walder(V29), q_costa(V29, H29))
)

emp_l27 <- data.frame(
  lake   = "L27 Shisper",
  source = "Empirical",
  # Walder & O'Connor (1997) is not applicable to L27 -- it is calibrated
  # on moraine-dammed outbursts and L27 is ice-dammed/piping (Section 4.4).
  label  = c("Costa & Schuster (1988)"),
  Q      = c(q_costa(V27, H27))
)

# ── 3. Literature ─────────────────────────────────────────────
lit_l27 <- data.frame(
  lake   = "L27 Shisper",
  source = "Literature",
  label  = c("Khan et al. (2021): low",
             "Khan et al. (2021): high"),
  Q      = c(5348, 6938)
)

# ── 4. Combine + sort by Q ────────────────────────────────────
cols <- c("lake","source","label","Q")

dat_l29 <- bind_rows(hecras_l29[,cols], emp_l29[,cols]) |>
  arrange(Q) |>
  mutate(label = factor(label, levels = unique(label)))

dat_l27 <- bind_rows(hecras_l27[,cols], emp_l27[,cols],
                     lit_l27[,cols]) |>
  arrange(Q) |>
  mutate(label = factor(label, levels = unique(label)))

# ── 5. Label x position = Q * offset (log-scale safe) ───
# This keeps text a fixed log-distance from the point regardless
# of where on the axis the point falls
label_mult <- 1.45   # place label ~16% of panel width to right of point

dat_l29 <- dat_l29 |> mutate(label_x = Q * label_mult)
dat_l27 <- dat_l27 |> mutate(label_x = Q * label_mult)

# ── 6. Source colours + shapes ────────────────────────────────
src_col <- c(
  "HEC-RAS"    = "#2166AC",
  "Empirical"  = "#D4AC0D",
  "Literature" = "#C77CFF"
)
src_shape <- c(
  "HEC-RAS"    = 21,
  "Empirical"  = 24,
  "Literature" = 22
)

# ── 7. Theme ──────────────────────────────────────────────────
theme_val <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.y = element_line(colour = "grey93",
                                        linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey88",
                                        linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(fill = NA, colour = "grey30",
                                        linewidth = 0.4),
      plot.background    = element_rect(fill = "white", colour = NA),
      axis.title.x       = element_text(size = base_size,
                                        margin = margin(t = 3)),
      axis.title.y       = element_blank(),
      axis.text.x        = element_text(size = base_size - 1.5,
                                        colour = "grey20"),
      axis.text.y        = element_text(size = base_size - 0.5,
                                        colour = "grey15"),
      legend.position    = "none",
      plot.title         = element_text(size = base_size + 0.5,
                                        face = "bold",
                                        margin = margin(b = 1)),
      plot.subtitle      = element_text(size = base_size - 1,
                                        colour = "grey40",
                                        margin = margin(b = 3)),
      plot.margin        = margin(3, 5, 3, 3, "mm")
    )
}

# ── 8. Panel builder ──────────────────────────────────────────
build_val <- function(dat, title, subtitle,
                      x_breaks, x_limits,
                      sep_y = NULL,
                      show_legend = FALSE) {

  hr <- dat |> filter(source == "HEC-RAS")

  p <- ggplot(dat,
              aes(x      = Q,
                  y      = label,
                  fill   = source,
                  shape  = source,
                  colour = source)) +

    # HEC-RAS envelope band
    annotate("rect",
             xmin = min(hr$Q) * 0.88,
             xmax = max(hr$Q) * 1.12,
             ymin = -Inf, ymax = Inf,
             fill = "#2166AC", alpha = 0.07) +

    # Group separator
    {if (!is.null(sep_y))
      geom_hline(yintercept = sep_y,
                 colour = "grey65", linewidth = 0.4,
                 linetype = "dashed")
    } +

    # Points
    geom_point(size = 2.4, stroke = 0.5) +

    # labels at label_x (= Q * 1.45), hjust = 0
    # so text starts at a fixed log-distance from point
geom_text(
  aes(x     = label_x,
      y     = label,
      label = scales::comma(round(Q))),
  hjust       = 0,
  vjust       = 0.45,
  size        = 1.9,
  colour      = "grey20",
  inherit.aes = FALSE
) +

    scale_fill_manual(
      name   = "Source",
      values = src_col,
      guide  = guide_legend(
        override.aes = list(size = 3),
        keywidth  = unit(0.4, "cm"),
        keyheight = unit(0.35, "cm")
      )
    ) +
    scale_colour_manual(name = "Source", values = src_col) +
    scale_shape_manual( name = "Source", values = src_shape) +

    scale_x_log10(
      name   = "Peak discharge (m³/s, log scale)",
      breaks = x_breaks,
      labels = scales::comma,
      limits = x_limits
    ) +

    labs(title = title, subtitle = subtitle) +
    theme_val()

  if (show_legend) {
    p <- p + theme(
      legend.position  = "bottom",
      legend.title     = element_text(size = BASE_SIZE,
                                      face = "bold"),
      legend.text      = element_text(size = BASE_SIZE - 1),
      legend.key.size  = unit(0.4, "cm"),
      legend.spacing.x = unit(0.4, "cm")
    )
  }

  return(p)
}

# ── 9. Build ──────────────────────────────────────────────────
cat("── Building panels ──\n")

p_l29 <- build_val(
  dat      = dat_l29,
  title    = "(a) L29 Passu",
  subtitle = "Moraine-dammed  |  Vw = 2.15 × 10⁶ m³",
  x_breaks = c(200, 500, 1000, 2000, 5000, 10000),
  x_limits = c(150, max(dat_l29$Q) * 4.5),
  sep_y    = 3
)

p_l27 <- build_val(
  dat      = dat_l27,
  title    = "(b) L27 Shisper",
  subtitle = "Ice-dammed  |  Vw = 1.66 × 10⁷ m³",
  x_breaks = c(1000, 10000, 100000),
  x_limits = c(800, max(dat_l27$Q) * 4.5),
  sep_y    = 3.5,
  show_legend = TRUE
)

# ── 10. Assemble ──────────────────────────────────────────────
cat("── Assembling ──\n")

leg      <- cowplot::get_legend(p_l27)
p_l27_nl <- p_l27 + theme(legend.position = "none")

p_maps <- cowplot::plot_grid(
  p_l29, p_l27_nl,
  ncol       = 2,
  rel_widths = c(1, 1),
  align      = "h",
  axis       = "tb"
)

p_final <- cowplot::plot_grid(
  p_maps, leg,
  ncol        = 1,
  rel_heights = c(1, 0.10)
)

# ── 11. Save ──────────────────────────────────────────────────
cat("── Saving ──\n")

ggsave(file.path(OUT_DIR, "fig11_validation_v4.png"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white")
cat("  Saved: fig11_validation_v4.png\n")

ggsave(file.path(OUT_DIR, "fig11_validation_v4.tiff"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white",
       compression = "lzw")
cat("  Saved: fig11_validation_v4.tiff\n")

cat("\nFig 11 v4 complete.\n")