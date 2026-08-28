
# ════════════════════════════════════════════════════════════════
# 02_fig2_workflow.R
# Figure 2 — Reproducible Workflow Diagram
# PURPOSE: Flowchart of the entire methodology from data inputs to manuscript output.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
#
# UPDATED (2026-08-21): supersedes 02_fig02_workflow.R (moved to scripts/OLD/).
# Fix for R1.software: the previous version colour-coded step boxes BY
# SOFTWARE TOOL (QGIS/GEE/R/HEC-RAS), with its own 5-entry colour legend,
# even though the boxes were already grouped into labelled phase panels
# (Acquisition/Delineation/Simulation/Assessment). That made "which
# software" the dominant visual read instead of "what stage." Step boxes
# are now coloured by phase (matching their panel's colour family) and
# the tool-based colour legend is removed -- the phase panels are already
# labelled, so a separate key isn't needed. The tool name is unchanged as
# an italic subtitle inside each box, so no information is lost.
# ════════════════════════════════════════════════════════════════

# =============================================================================
# Fig 2 — Reproducible Workflow Diagram (publication version)
# Output: figures/fig02_workflow.pdf  (83 mm wide, single-column)
# Packages: ggplot2, dplyr, tibble
# natural 4-phase structure
# =============================================================================

library(ggplot2)
library(dplyr)
library(tibble)

# ── 1. Steps ──────────────────────────────────────────────────────────────────

steps <- tribble(
  ~phase,        ~phase_id, ~step_id, ~label,                      ~tool,
  "Acquisition",  1,         1,        "Input data",                "NDMA · GLOF-II · DEM · OSM · RGI",
  "Acquisition",  1,         2,        "Filter lakes",              "QGIS",
  "Acquisition",  1,         3,        "Compile base layers",       "QGIS",
  "Acquisition",  1,         4,        "Acquire DEM + imagery",     "GEE",
  "Delineation",  2,         5,        "Delineate lakes (NDWI)",    "GEE + QGIS",
  "Delineation",  2,         6,        "Map corridors + exposure",  "QGIS + R",
  "Delineation",  2,         7,        "Score + rank lakes",        "R",
  "Simulation",   3,         8,        "Parameterise breach model", "R",
  "Simulation",   3,         9,        "Run flood simulations",     "HEC-RAS 2D",
  "Assessment",   4,         10,       "Map hazard + exposure",     "QGIS + R",
  "Assessment",   4,         11,       "Validate peak discharges",  "R + literature"
)

# ── 2. Layout constants ───────────────────────────────────────────────────────

box_w    <- 38
box_h    <- 11
gap_x    <- 5
gap_y    <- 14    # vertical space between phase panels
pad      <- 3     # padding inside phase panel around boxes
canvas_w <- 174   # mm — well centre each phase row to this

steps <- steps |>
  group_by(phase_id) |>
  mutate(
    pos_in_phase = row_number(),
    n_in_phase   = n()
  ) |>
  ungroup() |>
  mutate(
    # width of each phase row
    row_w  = n_in_phase * box_w + (n_in_phase - 1) * gap_x,
    # offset to centre each row within canvas_w
    x_offset = (canvas_w - row_w) / 2,
    x_mid    = x_offset + (pos_in_phase - 1) * (box_w + gap_x) + box_w / 2,
    x_left   = x_mid - box_w / 2,
    x_right  = x_mid + box_w / 2,
    y_top    = -(phase_id - 1) * (box_h + gap_y),
    y_bottom = y_top - box_h,
    y_mid    = y_top - box_h / 2
  )

# ── 3. Phase panels ───────────────────────────────────────────────────────────

phase_cols   <- c("Acquisition" = "#E8F4FA", "Delineation" = "#E8F7E8",
                  "Simulation"  = "#FDF3E8", "Assessment"  = "#F5EAF5")
phase_border <- c("Acquisition" = "#2A6EAA", "Delineation" = "#2A7A2A",
                  "Simulation"  = "#B06010", "Assessment"  = "#7A3A9A")

phase_panels <- steps |>
  group_by(phase, phase_id) |>
  summarise(
    xmin = min(x_left)   - pad,
    xmax = max(x_right)  + pad,
    ymax = max(y_top)    + pad + 3,   # +3 for phase label room
    ymin = min(y_bottom) - pad,
    .groups = "drop"
  ) |>
  mutate(
    fill   = phase_cols[phase],
    border = phase_border[phase]
  )

# ── 4. Within-phase arrows ────────────────────────────────────────────────────

within_arrows <- steps |>
  group_by(phase_id) |>
  arrange(pos_in_phase, .by_group = TRUE) |>
  mutate(
    x_from = x_right,
    x_to   = lead(x_left),
    y_arr  = y_mid
  ) |>
  filter(!is.na(x_to)) |>
  ungroup()

# ── 5. Between-phase arrows ───────────────────────────────────────────────────
# From bottom of phase panel → top of next phase panel (not into boxes)

between_arrows <- phase_panels |>
  arrange(phase_id) |>
  mutate(
    x_arr  = (xmin + xmax) / 2,   # centre of panel
    y_from = ymin,                 # bottom edge of current panel
    y_to   = lead(ymax)            # top edge of next panel
  ) |>
  filter(!is.na(y_to)) |>
  select(phase, x_arr, y_from, y_to)

# ── 6. Step-box colours (BY PHASE, not by software tool) ─────────────────────
# A deeper tint of each phase's colour family, so boxes read clearly against
# their own (paler) panel background while keeping the phase grouping as the
# single colour-coded dimension in the figure.

step_fill <- c("Acquisition" = "#CFE8F5", "Delineation" = "#CFEECF",
               "Simulation"  = "#FCE3C8", "Assessment"  = "#E8D2EE")

steps <- steps |>
  mutate(
    fill   = step_fill[phase],
    border = phase_border[phase]
  )

# ── 7. Canvas ─────────────────────────────────────────────────────────────────

x_range <- c(0, canvas_w)
y_range <- c(min(phase_panels$ymin) - 4, max(phase_panels$ymax) + 2)

# ── 8. Plot ───────────────────────────────────────────────────────────────────

p <- ggplot() +

  # Phase panels
  geom_rect(
    data = phase_panels,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill      = phase_panels$fill,
    colour    = phase_panels$border,
    linewidth = 0.4, linetype = "dashed"
  ) +

  # Phase labels
  geom_text(
    data = phase_panels,
    aes(x = (xmin + xmax) / 2, y = ymax - 1.5,
        label = toupper(phase), colour = phase),
    hjust = 0.5, vjust = 1, size = 2.6, fontface = "bold"
  ) +
  scale_colour_manual(values = phase_border, guide = "none") +

  # Between-phase arrows (panel bottom → panel top, no overlap with boxes)
  geom_segment(
    data = between_arrows,
    aes(x = x_arr, xend = x_arr, y = y_from, yend = y_to),
    arrow     = arrow(length = unit(1.4, "mm"), type = "closed"),
    colour    = "#777777", linewidth = 0.35
  ) +

  # Within-phase arrows
  geom_segment(
    data = within_arrows,
    aes(x = x_from, xend = x_to, y = y_arr, yend = y_arr),
    arrow     = arrow(length = unit(1.3, "mm"), type = "closed"),
    colour    = "#777777", linewidth = 0.3
  ) +

  # Step boxes (coloured by phase)
  geom_rect(
    data = steps,
    aes(xmin = x_left, xmax = x_right, ymin = y_bottom, ymax = y_top),
    fill      = steps$fill,
    colour    = steps$border,
    linewidth = 0.35
  ) +

  # Step label
  geom_text(
    data = steps,
    aes(x = x_mid, y = y_mid + 1.7, label = label),
    size = 2.5, fontface = "bold", hjust = 0.5, colour = "#111111"
  ) +

  # Tool subtitle (unchanged -- still names the software used, just not colour-coded)
  geom_text(
    data = steps,
    aes(x = x_mid, y = y_mid - 1.7, label = tool),
    size = 2.1, fontface = "italic", hjust = 0.5, colour = "#444444"
  ) +

  coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE) +
  theme_void() +
  theme(
    plot.margin     = margin(3, 3, 3, 3, "mm"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# ── 9. Export ────────────────────────────────────────────────────────────────

dir.create("figures", showWarnings = FALSE)

# dpi bumped from 600 to 800 for GitHub/publication release
ggsave("figures/fig02_workflow.tiff",
       width = 174, height = 115, units = "mm",
       dpi = 800, bg = "white", compression = "lzw")

ggsave("figures/fig02_workflow.png",
  plot = p, width = 174, height = 115, units = "mm", dpi = 800)

message("Saved: figures/fig02_workflow.tiff  +  .png")
