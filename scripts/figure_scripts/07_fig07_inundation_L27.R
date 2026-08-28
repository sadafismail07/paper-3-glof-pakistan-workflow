
# ════════════════════════════════════════════════════════════════
# 07_fig07_inundation_L27.R
# Figure 7 — Inundation Maps: L27 Shisper (Low / Mid / High)
# PURPOSE: Identical layout to Fig 6 but for L27 Shisper (ice-dammed).
# Shisper has a much larger footprint (up to 10.66 km²) so the map
# extent will be wider.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
# ════════════════════════════════════════════════════════════════
# =============================================================================
# fig07_v5_inundation_L27.R
# =============================================================================
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
FIG_HEIGHT <- 16.0
FIG_DPI    <- 800   # bumped from 600 to 800 dpi for repo/publication release
BASE_SIZE  <- 9   # bumped from 7 for label legibility (R4.9)
L <- "data/processed/fig07_layers"

# --- 1. Load layers -----------------------------------------------------------

cat("--- Loading layers ---
")
hs      <- rast(file.path(L, "L27_hillshade_wgs84.tif"))
d_low   <- rast(file.path(L, "L27_Low_depth_wgs84.tif"))
d_mid   <- rast(file.path(L, "L27_Mid_depth_wgs84.tif"))
d_high  <- rast(file.path(L, "L27_High_depth_wgs84.tif"))

roads_full   <- st_read("data/processed/fig01_layers/osm_roads.gpkg",   quiet = TRUE)
bridges_full <- st_read("data/processed/fig01_layers/osm_bridges.gpkg", quiet = TRUE)

if (file.exists(file.path(L, "L27_roads.gpkg"))) {
  roads_l27   <- st_read(file.path(L, "L27_roads.gpkg"),   quiet = TRUE)
  bridges_l27 <- st_read(file.path(L, "L27_bridges.gpkg"), quiet = TRUE)
  roads <- rbind(roads_full[,   attr(roads_full,   "sf_column")],
                 roads_l27[,    attr(roads_l27,    "sf_column")]) |>
    st_make_valid() |> distinct(.keep_all = TRUE)
  bridges <- rbind(bridges_full[, attr(bridges_full, "sf_column")],
                   bridges_l27[,  attr(bridges_l27,  "sf_column")]) |>
    st_make_valid() |> distinct(.keep_all = TRUE)
} else {
  roads   <- roads_full
  bridges <- bridges_full
}
cat("  Roads:", nrow(roads), " Bridges:", nrow(bridges), "
")
cat("  All loaded
")

# --- 2. Disaggregate to reduce pixelation -------------------------------------

cat("  Disaggregating depth rasters...
")
d_low  <- disagg(d_low,  fact = 5, method = "bilinear")
d_mid  <- disagg(d_mid,  fact = 5, method = "bilinear")
d_high <- disagg(d_high, fact = 5, method = "bilinear")
d_low[d_low   < 0.01] <- NA
d_mid[d_mid   < 0.01] <- NA
d_high[d_high < 0.01] <- NA
cat("  Done
")

# --- 3. Classify depth --------------------------------------------------------

classify_depth <- function(r) {
  m <- matrix(c(
     0,   3,  1,
     3,   7,  2,
     7,  15,  3,
    15, 100,  4
  ), ncol = 3, byrow = TRUE)
  classify(r, m, right = TRUE)
}

d_low_cls  <- classify_depth(d_low)
d_mid_cls  <- classify_depth(d_mid)
d_high_cls <- classify_depth(d_high)

levels(d_low_cls)  <- data.frame(id = 1:4, depth = c("1","2","3","4"))
levels(d_mid_cls)  <- data.frame(id = 1:4, depth = c("1","2","3","4"))
levels(d_high_cls) <- data.frame(id = 1:4, depth = c("1","2","3","4"))

# --- 4. Extent — strictly within hillshade ------------------------------------

hs_ext <- ext(hs)
xlim <- c(as.numeric(hs_ext[1]), as.numeric(hs_ext[2]))
ylim <- c(as.numeric(hs_ext[3]), as.numeric(hs_ext[4]))

map_bbox <- st_as_sfc(st_bbox(c(
  xmin = xlim[1], ymin = ylim[1],
  xmax = xlim[2], ymax = ylim[2]
), crs = st_crs(4326)))

roads   <- suppressWarnings(st_intersection(roads,   map_bbox))
bridges <- suppressWarnings(st_intersection(bridges, map_bbox))
cat("  Roads after clip:", nrow(roads), "
")
cat("  Bridges after clip:", nrow(bridges), "
")
cat("  Map extent: lon", round(xlim, 3), " lat", round(ylim, 3), "
")

# --- 5. Depth colours ---------------------------------------------------------

depth_cols <- c(
  "1" = "#7FC1DE",   # darkened from #A8D8EA for contrast against basemap (R4.9)
  "2" = "#4A90C4",
  "3" = "#1B4F8A",
  "4" = "#1A1147"
)

depth_labels <- c(
  "1" = "0 - 3 m",
  "2" = "3 - 7 m",
  "3" = "7 - 15 m",
  "4" = "> 15 m"
)

# --- 6. Theme -----------------------------------------------------------------

theme_fig <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey20", linewidth = 0.4),
      plot.background   = element_rect(fill = "white", colour = NA),
      axis.title        = element_blank(),
      axis.text.y       = element_text(size = base_size - 1.5),
      axis.text.x       = element_text(size = base_size - 1.5, angle = 45, hjust = 1),
      legend.title      = element_text(size = base_size, face = "bold"),
      legend.text       = element_text(size = base_size - 1),
      legend.key        = element_blank(),
      legend.key.size   = unit(0.3, "cm"),
      legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
      plot.margin       = margin(1, 1, 1, 1, "mm"),
      plot.title        = element_text(size = base_size, face = "bold",
                                       margin = margin(0, 0, 2, 0))
    )
}

# --- 7. Panel builder ---------------------------------------------------------

build_inundation_panel <- function(depth_cls, title, show_legend = FALSE,
                                   show_scalebar = FALSE, show_north = FALSE,
                                   show_xaxis = TRUE, show_yaxis = TRUE) {
  p <- ggplot() +

    # Hillshade base
    geom_spatraster(data = hs, show.legend = FALSE) +
    scale_fill_gradient(low = "grey10", high = "white",
                        na.value = "transparent", guide = "none") +

    ggnewscale::new_scale_fill() +

    # Depth classes
    geom_spatraster(data = depth_cls, show.legend = show_legend) +
    scale_fill_manual(
      values       = depth_cols,
      labels       = depth_labels,
      name         = "Max depth",
      na.value     = "transparent",
      na.translate = FALSE,
      drop         = FALSE,
      guide        = guide_legend(override.aes = list(linetype = 0))
    ) +

    # Roads — key_glyph = "path" forces a line swatch in the legend
    geom_sf(data      = roads |> mutate(lbl = "KKH / Roads"),
            aes(colour = lbl),
            linewidth  = 0.3,
            show.legend = show_legend,
            key_glyph  = "path") +
    scale_colour_manual(
      name   = NULL,
      values = c("KKH / Roads" = "grey15"),
      guide  = guide_legend(
        override.aes = list(
          fill      = NA,
          shape     = NA,
          linewidth = 0.5,
          linetype  = "solid"
        )
      )
    ) +

    # Bridges
    geom_sf(data = bridges, colour = "#E31A1C", shape = 17, size = 1.2) +

    # Shisper Lake origin marker
    annotate("point", x = 74.56, y = 36.305, shape = 21,
             size = 2, fill = "white", colour = "black", stroke = 0.5) +
    annotate("text", x = 74.56, y = 36.302, label = "Shisper Lake",
             size = 1.6, fontface = "bold", vjust = 1) +

    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggtitle(title) +
    theme_fig()

  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!show_xaxis)  p <- p + theme(axis.text.x = element_blank())
  if (!show_yaxis)  p <- p + theme(axis.text.y = element_blank())

  if (show_scalebar) {
    p <- p + annotation_scale(
      location   = "bl", width_hint = 0.15,
      pad_x      = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
      text_cex   = 0.5, line_width = 0.4, height = unit(0.1, "cm")
    )
  }

  if (show_north) {
    p <- p + annotation_north_arrow(
      location    = "tr", which_north = "true",
      height      = unit(0.5, "cm"), width = unit(0.4, "cm"),
      pad_x       = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
      style       = north_arrow_fancy_orienteering(text_size = 4)
    )
  }

  return(p)
}

# --- 8. Build panels ----------------------------------------------------------

cat("--- Building panels ---
")

# Top row: Low (left — north arrow, y-axis, no x-axis)
#          Mid (right — no y-axis, no x-axis)
p_low <- build_inundation_panel(
  d_low_cls, "(a) Low (10.2 M m³)",
  show_north = TRUE, show_xaxis = FALSE, show_yaxis = TRUE
)

p_mid <- build_inundation_panel(
  d_mid_cls, "(b) Mid (16.6 M m³)",
  show_xaxis = FALSE, show_yaxis = FALSE
)

# Bottom row: High (left — scale bar, x-axis, y-axis)
p_high <- build_inundation_panel(
  d_high_cls, "(c) High (23.0 M m³)",
  show_scalebar = TRUE, show_xaxis = TRUE, show_yaxis = TRUE
)

# Extract legend from a hidden render of the High panel
p_legend_source <- build_inundation_panel(
  d_high_cls, "",
  show_legend = TRUE, show_xaxis = FALSE, show_yaxis = FALSE
)
legend_grob <- cowplot::get_legend(p_legend_source)

# --- 9. Assemble layout -------------------------------------------------------

p_top <- plot_grid(
  p_low, p_mid,
  ncol       = 2,
  rel_widths = c(1, 0.88)   # left panel slightly wider: has y-axis labels
)

p_bot <- plot_grid(
  p_high, legend_grob,
  ncol       = 2,
  rel_widths = c(1, 0.35)   # High panel dominates; legend floats right
)

p_final <- plot_grid(
  p_top, p_bot,
  ncol        = 1,
  rel_heights = c(1, 1)
)

# --- 10. Save -----------------------------------------------------------------

ggsave(file.path(OUT_DIR, "fig07_v5_inundation_L27.png"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white")

ggsave(file.path(OUT_DIR, "fig07_v5_inundation_L27.tiff"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white", compression = "lzw")

cat("Done! Saved to", OUT_DIR, "
")


