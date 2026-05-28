
# ════════════════════════════════════════════════════════════════
# 10_fig10_h4_hazard.R
# Figure 10 — Hazard Class Map with Infrastructure Overlay 
# PURPOSE: THE HEADLINE FIGURE. Westoby H4 hazard zones for both lakes (mid scenario)
#          with population and infrastructure overlaid. Tells the key story:
#          "residential buildings are mostly above the floodplain; transportation
#          infrastructure (KKH, bridges) is the real exposure.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
# ════════════════════════════════════════════════════════════════
# ============================================================
# fig10_v4_fix_h4_hazard.R
# Fig 10 — H4 Hazard Class Map with Infrastructure Overlay
# v4 with terrain fix: hillshade cropped to map extent
# ============================================================
setwd("C:/Users/sadaf/Documents/PPR3")

library(terra)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(sf)
library(dplyr)
library(cowplot)
library(ggnewscale)

OUT_DIR    <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_WIDTH  <- 17.4
FIG_HEIGHT <- 11.0
FIG_DPI    <- 600
BASE_SIZE  <- 7
P          <- "data/processed/fig010o_layers"

# ── 1. Load layers ────────────────────────────────────────────
cat("── Loading layers ──
")

# FIX: load as _full so we can re-crop after extent is computed
hs_l29_full <- rast(file.path(P, "L29_hillshade_wgs84.tif"))
hs_l27      <- rast("data/processed/fig07_layers/L27_hillshade_wgs84.tif")

h4_l29_raw  <- rast(file.path(P, "L29_Mid_h4_wgs84_x4.tif"))
h4_l27_raw  <- rast(file.path(P, "L27_Mid_h4_wgs84_x4.tif"))

h4_l29 <- as.factor(round(h4_l29_raw))
h4_l27 <- as.factor(round(h4_l27_raw))
levels(h4_l29) <- data.frame(id = 1, label = "H4")
levels(h4_l27) <- data.frame(id = 1, label = "H4")
cat("  H4 L29 levels:", levels(h4_l29)[[1]]$label, "
")
cat("  H4 L27 levels:", levels(h4_l27)[[1]]$label, "
")

pop_l29 <- rast(file.path(P, "L29_worldpop_wgs84.tif"))
pop_l27 <- rast(file.path(P, "L27_worldpop_wgs84.tif"))

roads_l29   <- st_read(file.path(P, "L29_roads.gpkg"),   quiet = TRUE)
roads_l27   <- st_read(file.path(P, "L27_roads.gpkg"),   quiet = TRUE)
bridges_l29 <- st_read(file.path(P, "L29_bridges.gpkg"), quiet = TRUE)
bridges_l27 <- st_read(file.path(P, "L27_bridges.gpkg"), quiet = TRUE)
ann         <- read.csv(file.path(P, "fig010o_annotations.csv"))

cat("  All layers loaded

")

# ── 2. Map extents ────────────────────────────────────────────
get_lims <- function(r) {
  e <- ext(r)
  list(xlim = c(xmin(e), xmax(e)),
       ylim = c(ymin(e), ymax(e)))
}

lim_l27 <- get_lims(hs_l27)

# L29: 8% padding around H4 extent (same as v4)
e29   <- ext(h4_l29_raw)
x_pad <- (xmax(e29) - xmin(e29)) * 0.08
y_pad <- (ymax(e29) - ymin(e29)) * 0.08
lim_l29 <- list(
  xlim = c(xmin(e29) - x_pad, xmax(e29) + x_pad),
  ylim = c(ymin(e29) - y_pad, ymax(e29) + y_pad)
)

cat("  L29 map extent: lon", round(lim_l29$xlim, 4),
    " lat", round(lim_l29$ylim, 4), "
")
cat("  L27 map extent: lon", round(lim_l27$xlim, 4),
    " lat", round(lim_l27$ylim, 4), "

")

# FIX: crop hillshade to the FINAL map extent (not H4 extent)
# This ensures terrain fills the whole L29 panel
hs_l29 <- crop(hs_l29_full,
               ext(lim_l29$xlim[1], lim_l29$xlim[2],
                   lim_l29$ylim[1], lim_l29$ylim[2]))
cat("  L29 hillshade re-cropped to map extent:",
    sum(!is.na(values(hs_l29))), "valid cells

")

# ── 3. Population: log-scale, masked inside H4 ───────────────
prep_pop <- function(pop, h4_raw) {
  h4_r <- resample(h4_raw, pop, method = "near")
  out  <- pop
  out[!is.na(h4_r)] <- NA
  out[out <= 0]     <- NA
  log1p(out)
}

pop_l29 <- prep_pop(pop_l29, h4_l29_raw)
pop_l27 <- prep_pop(pop_l27, h4_l27_raw)

# ── 4. Annotation text ────────────────────────────────────────
fmt_ann <- function(lake_id) {
  r <- ann[ann$lake == lake_id, ]
  sprintf(
    "H4 area: %.2f sq.km
Road exp: %.2f km
Bridges: %d at risk
Built-up: %.0f sq.m
Pop (WP): %d",
    r$h4_km2, r$roads_km, r$bridges, r$built_m2, r$pop_wp
  )
}

ann_l29       <- fmt_ann("L29")
ann_l27       <- fmt_ann("L27")
n_bridges_l29 <- ann[ann$lake == "L29", "bridges"]
n_bridges_l27 <- ann[ann$lake == "L27", "bridges"]

# ── 5. Theme ──────────────────────────────────────────────────
theme_fig <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey20",
                                       linewidth = 0.5),
      plot.background   = element_rect(fill = "white", colour = NA),
      axis.title        = element_blank(),
      axis.text         = element_text(size = base_size - 1.5,
                                       colour = "grey30"),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      legend.title      = element_text(size = base_size, face = "bold"),
      legend.text       = element_text(size = base_size - 1),
      legend.key        = element_blank(),
      legend.key.size   = unit(0.30, "cm"),
      legend.spacing.y  = unit(0.10, "cm"),
      legend.margin     = margin(3, 3, 3, 3),
      legend.background = element_rect(fill      = alpha("white", 0.88),
                                       colour    = "grey70",
                                       linewidth = 0.3),
      plot.margin = margin(1, 1, 1, 1, "mm"),
      plot.title  = element_text(size   = base_size + 0.5,
                                 face   = "bold",
                                 margin = margin(0, 0, 2, 0))
    )
}

# ── 6. Panel builder ──────────────────────────────────────────
build_panel <- function(hs, h4, h4_raw, pop,
                        roads, bridges,
                        xlim, ylim,
                        title, ann_text,
                        n_bridges_csv,
                        show_legend   = FALSE,
                        show_scalebar = FALSE,
                        show_north    = FALSE,
                        show_yaxis    = TRUE) {

  xr <- xlim[2] - xlim[1]
  yr <- ylim[2] - ylim[1]

  # Annotation box: bottom-right, 36% wide, 30% tall, text centred
  ann_xmin <- xlim[2] - 0.38 * xr
  ann_xmax <- xlim[2] - 0.004
  ann_ymin <- ylim[1] + 0.003
  ann_ymax <- ylim[1] + 0.30 * yr
  ann_tx   <- ann_xmin + 0.012 * xr
  ann_ty   <- (ann_ymin + ann_ymax) / 2

  p <- ggplot() +

    # Layer 1 — Hillshade
    # na.value = "white" fills any edge pixels outside raster coverage
    geom_spatraster(data = hs, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey5",
                        high     = "white",
                        na.value = "white",
                        guide    = "none") +

    # Layer 2 — Population density outside H4 (blue palette)
    ggnewscale::new_scale_fill() +
    geom_spatraster(data = pop, show.legend = show_legend,
                    alpha = 0.70) +
    scale_fill_gradient(
      low      = "#EFF3FF",
      high     = "#084594",
      name     = "Population
density
(outside H4,
log scale)",
      na.value = "transparent",
      guide    = guide_colourbar(
        order          = 2,
        barwidth       = unit(0.28, "cm"),
        barheight      = unit(1.8,  "cm"),
        ticks          = FALSE,
        title.position = "top"
      )
    ) +

    # Layer 3 — H4 hazard zone (red — unambiguous against blue pop)
    ggnewscale::new_scale_fill() +
    geom_spatraster(data = h4, show.legend = show_legend,
                    alpha = 0.78) +
    scale_fill_manual(
      values       = c("H4" = "#D7191C"),
      labels       = c("H4" = "H4 hazard zone
(depth >1.5 m or
DxV >0.7 sq.m/s)"),
      name         = "",
      na.value     = "transparent",
      na.translate = FALSE,
      guide        = guide_legend(
        order        = 1,
        override.aes = list(alpha = 0.78, linetype = 0)
      )
    ) +

    # Layer 4 — Roads (key_glyph = "path" gives line in legend)
    geom_sf(data        = roads |> mutate(lbl = "KKH / Roads"),
            aes(colour  = lbl),
            linewidth   = 0.40,
            show.legend = show_legend,
            key_glyph   = "path") +
    scale_colour_manual(
      name   = NULL,
      values = c("KKH / Roads" = "grey10"),
      guide  = guide_legend(
        order        = 3,
        override.aes = list(fill      = NA,
                            shape     = NA,
                            linewidth = 0.6,
                            linetype  = "solid")
      )
    ) +

    # Layer 5 — Bridges (red triangles)
    geom_sf(data        = bridges,
            colour      = "#E31A1C",
            shape       = 17,
            size        = 2.2,
            show.legend = FALSE) +

    # Bridge count — top-right corner
    annotate("text",
             x        = xlim[2] - 0.004,
             y        = ylim[2] - 0.006,
             label    = sprintf("%d bridge%s at risk",
                                n_bridges_csv,
                                ifelse(n_bridges_csv == 1, "", "s")),
             hjust    = 1, vjust = 1,
             size     = 2.1, colour = "#C0392B",
             fontface = "bold") +

    # Annotation box background
    annotate("rect",
             xmin  = ann_xmin, xmax = ann_xmax,
             ymin  = ann_ymin, ymax = ann_ymax,
             fill  = "white", colour = "grey40",
             alpha = 0.93,    linewidth = 0.4) +

    # Annotation text — tight line spacing, all 5 lines visible
    annotate("text",
             x          = ann_tx,
             y          = ann_ty,
             label      = ann_text,
             hjust      = 0, vjust = 0.5,
             size       = 2.15,
             colour     = "grey10",
             lineheight = 1.15) +

    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggtitle(title) +
    theme_fig()

  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!show_yaxis)  p <- p + theme(axis.text.y = element_blank())

  # Scale bar — top-left, clear of annotation box
  if (show_scalebar) {
    p <- p + annotation_scale(
      location   = "tl",
      width_hint = 0.18,
      pad_x      = unit(0.20, "cm"),
      pad_y      = unit(0.20, "cm"),
      text_cex   = 0.55,
      line_width = 0.40,
      height     = unit(0.12, "cm")
    )
  }

  if (show_north) {
    p <- p + annotation_north_arrow(
      location    = "tr", which_north = "true",
      height      = unit(0.50, "cm"), width = unit(0.40, "cm"),
      pad_x       = unit(0.15, "cm"), pad_y = unit(0.55, "cm"),
      style       = north_arrow_fancy_orienteering(text_size = 4)
    )
  }

  return(p)
}

# ── 7. Build panels ───────────────────────────────────────────
cat("── Building panels ──
")

p_l29 <- build_panel(
  hs            = hs_l29,        # cropped to map extent — terrain fills panel
  h4            = h4_l29,
  h4_raw        = h4_l29_raw,
  pop           = pop_l29,
  roads         = roads_l29,
  bridges       = bridges_l29,
  xlim          = lim_l29$xlim,
  ylim          = lim_l29$ylim,
  title         = "(a) L29 Passu - Mid scenario H4 hazard zone",
  ann_text      = ann_l29,
  n_bridges_csv = n_bridges_l29,
  show_legend   = TRUE,
  show_scalebar = TRUE,
  show_north    = TRUE,
  show_yaxis    = TRUE
)

p_l27 <- build_panel(
  hs            = hs_l27,
  h4            = h4_l27,
  h4_raw        = h4_l27_raw,
  pop           = pop_l27,
  roads         = roads_l27,
  bridges       = bridges_l27,
  xlim          = lim_l27$xlim,
  ylim          = lim_l27$ylim,
  title         = "(b) L27 Shisper - Mid scenario H4 hazard zone",
  ann_text      = ann_l27,
  n_bridges_csv = n_bridges_l27,
  show_legend   = FALSE,
  show_scalebar = TRUE,
  show_north    = FALSE,
  show_yaxis    = FALSE
)

# ── 8. Assemble — legend in own narrow column ─────────────────
cat("── Assembling ──
")

legend_grob <- cowplot::get_legend(p_l29)
p_l29_noleg <- p_l29 + theme(legend.position = "none")

p_maps  <- plot_grid(p_l29_noleg, p_l27,
                     ncol = 2, rel_widths = c(1, 1))
p_final <- plot_grid(p_maps, legend_grob,
                     ncol = 2, rel_widths = c(1, 0.18))

# ── 9. Save ───────────────────────────────────────────────────
cat("── Saving ──
")

ggsave(file.path(OUT_DIR, "fig10_v4_fix_h4_hazard.png"),
       p_final, width = FIG_WIDTH, height = FIG_HEIGHT,
       units = "cm", dpi = FIG_DPI, bg = "white")
cat("  Saved: fig10_v4_fix_h4_hazard.png
")

ggsave(file.path(OUT_DIR, "fig10_v4_fix_h4_hazard.tiff"),
       p_final, width = FIG_WIDTH, height = FIG_HEIGHT,
       units = "cm", dpi = FIG_DPI, bg = "white",
       compression = "lzw")
cat("  Saved: fig10_v4_fix_h4_hazard.tiff
")

cat("
Fig 10 v4 fix complete.
")


