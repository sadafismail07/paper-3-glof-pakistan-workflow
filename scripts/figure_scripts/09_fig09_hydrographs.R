
# ============================================================
# fig09_hydrographs_v6.R
# Fix: unified legend — coloured line segment + name + peak Q
# one row per scenario, single box per panel
# ============================================================
setwd("C:/Users/sadaf/Documents/PPR3")

library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)
library(colorspace)

OUT_DIR   <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_W     <- 20.0
FIG_H     <- 8.0
FIG_DPI   <- 600
BASE_SIZE <- 7

# ── 1. Load data ──────────────────────────────────────────────
read_hydro <- function(path, lake, scenario) {
  d <- read.csv(path)
  d$lake     <- lake
  d$scenario <- scenario
  d
}

scen_order <- c("Mid baseline", "Bulk ×1.5", "Bulk ×2.0")

dat <- bind_rows(
  read_hydro("data/processed/L29_breach_hydrograph_Mid.csv",
             "L29 Passu", "Mid baseline"),
  read_hydro("data/processed/L29_breach_hydrograph_Mid_bulk15.csv",
             "L29 Passu", "Bulk ×1.5"),
  read_hydro("data/processed/L29_breach_hydrograph_Mid_bulk20.csv",
             "L29 Passu", "Bulk ×2.0"),
  read_hydro("data/processed/L27_breach_hydrograph_Mid.csv",
             "L27 Shisper", "Mid baseline"),
  read_hydro("data/processed/L27_breach_hydrograph_Mid_bulk15.csv",
             "L27 Shisper", "Bulk ×1.5"),
  read_hydro("data/processed/L27_breach_hydrograph_Mid_bulk20.csv",
             "L27 Shisper", "Bulk ×2.0")
)

dat$scenario <- factor(dat$scenario, levels = scen_order)
dat$time_h   <- dat$time_hours

# ── 2. Peaks ──────────────────────────────────────────────────
# Locate the peak ROW for plotting the time marker (uses curve max)
peaks <- dat |>
  group_by(lake, scenario) |>
  slice(which.max(Q_m3_s)) |>
  ungroup()

# Canonical peak-Q values from Table 3 (verified HEC-RAS inputs).
# The discrete hydrograph CSV maxima can fall a few m³/s below these
# because the time sampling does not land exactly on the analytical
# peak; we display the Table 3 values to keep the figure and table
# perfectly consistent.
canonical_peaks <- tribble(
  ~lake,         ~scenario,        ~Q_label,
  "L29 Passu",   "Mid baseline",   1261,
  "L29 Passu",   "Bulk ×1.5", 1892,
  "L29 Passu",   "Bulk ×2.0", 2522,
  "L27 Shisper", "Mid baseline",   12546,
  "L27 Shisper", "Bulk ×1.5", 18819,
  "L27 Shisper", "Bulk ×2.0", 25092
)

# ── 3. Style maps ─────────────────────────────────────────────
scen_lty <- c(
  "Mid baseline"   = "solid",
  "Bulk ×1.5" = "dashed",
  "Bulk ×2.0" = "dotted"
)
scen_lwd <- c(
  "Mid baseline"   = 0.80,
  "Bulk ×1.5" = 0.65,
  "Bulk ×2.0" = 0.60
)

# ── 4. Theme ──────────────────────────────────────────────────
theme_hydro <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "grey92",
                                        linewidth = 0.3),
      panel.border       = element_rect(fill = NA, colour = "grey30",
                                        linewidth = 0.4),
      plot.background    = element_rect(fill = "white", colour = NA),
      axis.title         = element_text(size = base_size),
      axis.text          = element_text(size = base_size - 1,
                                        colour = "grey20"),
      legend.position    = "none",
      plot.title         = element_text(size = base_size + 0.5,
                                        face = "bold",
                                        margin = margin(b = 1)),
      plot.subtitle      = element_text(size = base_size - 1,
                                        colour = "grey40",
                                        margin = margin(b = 3)),
      plot.margin        = margin(3, 4, 3, 3, "mm")
    )
}

# ── 5. Panel builder ──────────────────────────────────────────
build_hydro <- function(lake_name, col, title, subtitle, y_breaks) {

  d    <- dat   |> filter(lake == lake_name)
  d_pk <- peaks |> filter(lake == lake_name)

  # X limit — trim flat tail
  max_t <- d |>
    group_by(scenario) |>
    mutate(thresh = max(Q_m3_s) * 0.01) |>
    filter(Q_m3_s > thresh) |>
    summarise(t_end = max(time_h)) |>
    pull(t_end) |> max()
  max_t <- ceiling(max_t * 10) / 10 + 0.1

  y_max <- max(y_breaks)

  # Scenario colours
  col_mid <- col
  col_15  <- colorspace::lighten(col, 0.28)
  col_20  <- colorspace::lighten(col, 0.50)
  scen_cols <- setNames(c(col_mid, col_15, col_20), scen_order)
  cols_vec  <- c(col_mid, col_15, col_20)

# Peak values — use canonical Table 3 values for the legend labels
  pk <- canonical_peaks |>
    filter(lake == lake_name) |>
    arrange(factor(scenario, levels = scen_order)) |>
    pull(Q_label)

  # ── Unified legend geometry ───────────────────────────────
  # Box sits in the recession area: x from 45% to 100% of max_t
  # y from 70% to 100% of y_max
  bx0 <- max_t * 0.45; bx1 <- max_t * 1.00
  by0 <- y_max * 0.68; by1 <- y_max * 1.01

  # Row spacing inside box
  n_rows <- 4  # header + 3 scenarios
  row_h  <- (by1 - by0) / (n_rows + 0.5)

  # Segment x positions (left side of box + small margin)
  seg_x0 <- bx0 + (bx1 - bx0) * 0.03
  seg_x1 <- bx0 + (bx1 - bx0) * 0.22
  txt_x  <- bx0 + (bx1 - bx0) * 0.25   # text starts after segment

  # Y positions for each row (top to bottom)
  y_hdr  <- by1 - row_h * 0.7
  y_r    <- c(by1 - row_h * 1.7,
              by1 - row_h * 2.7,
              by1 - row_h * 3.7)

  # Labels: "Scenario name — Peak Q m³/s"
  row_labels <- sprintf("%s  —  %s m³/s",
                        scen_order,
                        formatC(round(pk), format="d", big.mark=","))

  p <- ggplot(d, aes(x = time_h, y = Q_m3_s,
                     colour    = scenario,
                     linetype  = scenario,
                     linewidth = scenario,
                     group     = scenario)) +

    geom_line(alpha = 0.92) +

    # Peak time reference (Mid baseline only)
    geom_segment(
      data = filter(d_pk, scenario == "Mid baseline"),
      aes(x = time_h, xend = time_h,
          y = 0,      yend = Q_m3_s * 0.90),
      linetype = "dotted", linewidth = 0.3,
      colour = "grey55", inherit.aes = FALSE
    ) +

    # ── Legend box background ─────────────────────────────
    annotate("rect",
             xmin = bx0, xmax = bx1,
             ymin = by0, ymax = by1,
             fill = "white", colour = "grey65",
             alpha = 0.93, linewidth = 0.35) +

    # Header
    annotate("text",
             x = seg_x0, y = y_hdr,
             label = "Scenario  —  Peak Q",
             hjust = 0, vjust = 0.5,
             size = 2.0, colour = "grey20",
             fontface = "bold") +

    # ── Row 1: Mid baseline ───────────────────────────────
    annotate("segment",
             x = seg_x0, xend = seg_x1,
             y = y_r[1],  yend = y_r[1],
             colour = col_mid, linewidth = 0.80,
             linetype = "solid") +
    annotate("text",
             x = txt_x, y = y_r[1],
             label = row_labels[1],
             hjust = 0, vjust = 0.5,
             size = 1.95, colour = col_mid) +

    # ── Row 2: Bulk x1.5 ─────────────────────────────────
    annotate("segment",
             x = seg_x0, xend = seg_x1,
             y = y_r[2],  yend = y_r[2],
             colour = col_15, linewidth = 0.65,
             linetype = "dashed") +
    annotate("text",
             x = txt_x, y = y_r[2],
             label = row_labels[2],
             hjust = 0, vjust = 0.5,
             size = 1.95, colour = col_15) +

    # ── Row 3: Bulk x2.0 ─────────────────────────────────
    annotate("segment",
             x = seg_x0, xend = seg_x1,
             y = y_r[3],  yend = y_r[3],
             colour = col_20, linewidth = 0.60,
             linetype = "dotted") +
    annotate("text",
             x = txt_x, y = y_r[3],
             label = row_labels[3],
             hjust = 0, vjust = 0.5,
             size = 1.95, colour = col_20) +

    scale_colour_manual(values = scen_cols) +
    scale_linetype_manual(values = scen_lty) +
    scale_linewidth_manual(values = scen_lwd) +

    scale_x_continuous(
      name   = "Time since breach initiation (hours)",
      limits = c(0, max_t),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(
      name   = "Discharge (m³/s)",
      breaks = y_breaks,
      labels = scales::comma,
      limits = c(0, y_max * 1.04),
      expand = expansion(mult = c(0, 0))
    ) +

    labs(title = title, subtitle = subtitle) +
    theme_hydro()

  return(p)
}

# ── 6. Build ──────────────────────────────────────────────────
cat("── Building panels ──
")

p_l29 <- build_hydro(
  lake_name = "L29 Passu",
  col       = "#2166AC",
  title     = "(a) L29 Passu",
  subtitle  = "Moraine-dammed  |  Vw = 2.15 × 10⁶ m³",
  y_breaks  = seq(0, 3000, by = 500)
)

p_l27 <- build_hydro(
  lake_name = "L27 Shisper",
  col       = "#B2182B",
  title     = "(b) L27 Shisper",
  subtitle  = "Ice-dammed  |  Vw = 1.66 × 10⁷ m³",
  y_breaks  = seq(0, 30000, by = 5000)
)

# ── 7. Assemble ───────────────────────────────────────────────
cat("── Assembling ──
")

p_final <- cowplot::plot_grid(
  p_l29, p_l27,
  ncol       = 2,
  rel_widths = c(1, 1),
  align      = "h",
  axis       = "tb"
)

# ── 8. Save ───────────────────────────────────────────────────
cat("── Saving ──
")

ggsave(file.path(OUT_DIR, "fig09_hydrographs_v6.png"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white")
cat("  Saved: fig09_hydrographs_v6.png
")

ggsave(file.path(OUT_DIR, "fig09_hydrographs_v6.tiff"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white",
       compression = "lzw")
cat("  Saved: fig09_hydrographs_v6.tiff
")

cat("
Fig 9 v6 complete.
")


