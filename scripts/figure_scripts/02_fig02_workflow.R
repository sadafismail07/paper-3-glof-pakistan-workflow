
# ════════════════════════════════════════════════════════════════
# 02_fig2_workflow.R
# Figure 2 — Reproducible Workflow Diagram
# PURPOSE: Flowchart of the entire methodology from data inputs to manuscript output.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
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
  ~phase,        ~phase_id, ~step_id, ~label,                      ~tool,                   ~tool_cat,
  "Acquisition",  1,         1,        "Input data",                "NDMA · GLOF-II · DEM · OSM · RGI", "input",
  "Acquisition",  1,         2,        "Filter lakes",              "QGIS",                  "qgis",
  "Acquisition",  1,         3,        "Compile base layers",       "QGIS",                  "qgis",
  "Acquisition",  1,         4,        "Acquire DEM + imagery",     "GEE",                   "gee",
  "Delineation",  2,         5,        "Delineate lakes (NDWI)",    "GEE + QGIS",            "gee",
  "Delineation",  2,         6,        "Map corridors + exposure",  "QGIS + R",              "qgis",
  "Delineation",  2,         7,        "Score + rank lakes",        "R",                     "r",
  "Simulation",   3,         8,        "Parameterise breach model", "R",                     "r",
  "Simulation",   3,         9,        "Run flood simulations",     "HEC-RAS 1D",            "hecras",
  "Assessment",   4,         10,       "Map hazard + exposure",     "QGIS + R",              "qgis",
  "Assessment",   4,         11,       "Validate peak discharges",  "R + literature",        "r"
)

# ── 2. Layout constants ───────────────────────────────────────────────────────

box_w    <- 38
box_h    <- 11
gap_x    <- 5
gap_y    <- 14    # vertical space between phase panels
pad      <- 3     # padding inside phase panel around boxes
canvas_w <- 174   # mm — we will centre each phase row to this

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

# ── 6. Tool colours ───────────────────────────────────────────────────────────

tool_fill <- c(
  input  = "#F0F0F0",
  qgis   = "#DAEAF7",
  r      = "#DAF0DA",
  gee    = "#FEF6D8",
  hecras = "#FAE2D8"
)
tool_border <- c(
  input  = "#999999",
  qgis   = "#2A6EAA",
  r      = "#2A7A2A",
  gee    = "#B08A00",
  hecras = "#B04010"
)
tool_label <- c(
  input  = "Input data",
  qgis   = "QGIS",
  r      = "R",
  gee    = "Google Earth Engine",
  hecras = "HEC-RAS"
)

steps <- steps |>
  mutate(
    fill   = tool_fill[tool_cat],
    border = tool_border[tool_cat]
  )

# ── 7. Canvas ─────────────────────────────────────────────────────────────────

x_range <- c(0, canvas_w)
y_range <- c(min(phase_panels$ymin) - 10, max(phase_panels$ymax) + 2)

# ── 8. Legend data ────────────────────────────────────────────────────────────

leg <- tibble(
  tool_cat = names(tool_label),
  lab      = unname(tool_label),
  x0       = 1, x1 = 6,
  y_mid    = seq(y_range[1] + 8, by = 4, length.out = 5),
  y0       = y_mid - 1.5,
  y1       = y_mid + 1.5
)

# ── 9. Plot ───────────────────────────────────────────────────────────────────

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
    hjust = 0.5, vjust = 1, size = 2.0, fontface = "bold"
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

  # Step boxes
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
    size = 1.9, fontface = "bold", hjust = 0.5, colour = "#111111"
  ) +

  # Tool subtitle
  geom_text(
    data = steps,
    aes(x = x_mid, y = y_mid - 1.7, label = tool),
    size = 1.6, fontface = "italic", hjust = 0.5, colour = "#444444"
  ) +

  # Legend boxes
  geom_rect(
    data = leg,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
    fill      = tool_fill[leg$tool_cat],
    colour    = tool_border[leg$tool_cat],
    linewidth = 0.3
  ) +
  geom_text(
    data = leg,
    aes(x = x1 + 1.5, y = y_mid, label = lab),
    hjust = 0, size = 1.7, colour = "#333333"
  ) +

  coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE) +
  theme_void() +
  theme(
    plot.margin     = margin(3, 3, 3, 3, "mm"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# ── 10. Export ────────────────────────────────────────────────────────────────

dir.create("figures", showWarnings = FALSE)

ggsave("figures/fig02_workflow.tiff",
       width = 174, height = 115, units = "mm",
       dpi = 600, bg = "white", compression = "lzw")


ggsave("figures/fig02_workflow.png",
  plot = p, width = 174, height = 115, units = "mm", dpi = 600)

message("Saved: figures/fig02_workflow.tiff  +  .png")


