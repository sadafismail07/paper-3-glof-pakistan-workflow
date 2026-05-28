
# ════════════════════════════════════════════════════════════════
# 11_fig11_validation.R
# Figure 11 —  Validation Comparison
# PURPOSE: Two-panel dot plot (faceted by lake) on log-scale x-axis. Each dot is a
#          peak discharge estimate from a different source:
#          Blue circles: HEC-RAS runs (Low/Mid/High/bulk x2)
#          Gold triangles: empirical equations (Walder & OConnor 1997, Costa & Schuster 1988)
#          Pink squares: published literature (Khan et al. 2021)
#          Shows your results fall within the empirical envelope.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
# ════════════════════════════════════════════════════════════════

# ============================================================
# fig11_validation_v3.R
# Fix: value labels positioned at Q * 1.15 (log-scale safe)
# x_limits extended right to accommodate labels
# ============================================================
setwd("C:/Users/sadaf/Documents/PPR3")

library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)

OUT_DIR   <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_W     <- 17.4
FIG_H     <- 9.0
FIG_DPI   <- 600
BASE_SIZE <- 7

# ── 1. Load simulation results ────────────────────────────────
d29 <- read.csv("data/processed/L29_Passu_simulation_results.csv")
d27 <- read.csv("data/processed/L27_Shisper_simulation_results.csv")

scens_show <- c("Low", "Mid_baseline", "High", "Mid_bulk20")

make_hecras <- function(df, lake) {
  df |>
    filter(scenario %in% scens_show) |>
    mutate(
      lake   = lake,
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

hecras_l29 <- make_hecras(d29, "L29 Passu")
hecras_l27 <- make_hecras(d27, "L27 Shisper")

# ── 2. Empirical ──────────────────────────────────────────────
V29 <- 2154184;  H29 <- 37.5
V27 <- 16600000; H27 <- 134

emp_l29 <- data.frame(
  lake   = "L29 Passu",
  source = "Empirical",
  label  = c("Walder & OConnor (1997)",
             "Froehlich (1995)",
             "Hagen (1982)"),
  Q      = c(0.0347 * V29^0.66,
             0.607  * V29^0.295 * H29^1.24,
             0.72   * V29^0.53)
)

emp_l27 <- data.frame(
  lake   = "L27 Shisper",
  source = "Empirical",
  label  = c("Walder & OConnor (1997)",
             "Froehlich (1995)",
             "Hagen (1982)"),
  Q      = c(1.94  * H27^1.0 * V27^0.34,
             0.607 * V27^0.295 * H27^1.24,
             0.72  * V27^0.53)
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

# ── 5. FIX: label x position = Q * offset (log-scale safe) ───
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

    # FIX: labels at label_x (= Q * 1.18), hjust = 0
    # so text starts at a fixed log-distance from point
geom_text(
  aes(x     = label_x,
      y     = label,          # FIX: pass y explicitly
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
cat("── Building panels ──
")

p_l29 <- build_val(
  dat      = dat_l29,
  title    = "(a) L29 Passu",
  subtitle = "Moraine-dammed  |  Vw = 2.15 × 10⁶ m³",
  x_breaks = c(200, 500, 1000, 2000, 5000, 10000),
  # FIX: extend right limit to 4× max Q so labels fit
  x_limits = c(150, max(dat_l29$Q) * 4.5),
  sep_y    = 3.5
)

p_l27 <- build_val(
  dat      = dat_l27,
  title    = "(b) L27 Shisper",
  subtitle = "Ice-dammed  |  Vw = 1.66 × 10⁷ m³",
  x_breaks = c(1000, 10000, 100000),
  # FIX: extend right limit to 4× max Q so labels fit
  x_limits = c(800, max(dat_l27$Q) * 4.5),
  sep_y    = 4.5,
  show_legend = TRUE
)

# ── 10. Assemble ──────────────────────────────────────────────
cat("── Assembling ──
")

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
cat("── Saving ──
")

ggsave(file.path(OUT_DIR, "fig11_validation_v3.png"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white")
cat("  Saved: fig11_validation_v3.png
")

ggsave(file.path(OUT_DIR, "fig11_validation_v3.tiff"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white",
       compression = "lzw")
cat("  Saved: fig11_validation_v3.tiff
")

cat("
Fig 11 v3 complete.
")


