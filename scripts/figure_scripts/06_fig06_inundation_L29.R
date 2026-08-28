
# ════════════════════════════════════════════════════════════════
# 06_fig06_inundation_L29.R
# Figure 6 — Inundation Maps: L29 Passu (Low / Mid / High)
# PURPOSE: Three-panel map showing how flood extent grows across volume scenarios
#          for the moraine-dammed lake. Each panel shows one scenario.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
# ════════════════════════════════════════════════════════════════
# =============================================================================
# fig06_v4_inundation_L29.R
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
FIG_HEIGHT <- 12.0
FIG_DPI    <- 800   # bumped from 600 to 800 dpi for repo/publication release
BASE_SIZE  <- 9   # bumped from 7 for label legibility (R4.9)
L <- "data/processed/fig06_layers"

# --- 1. Load layers -----------------------------------------------------------

cat("--- Loading layers ---
")
hs      <- rast(file.path(L, "L29_hillshade_wgs84.tif"))
d_low   <- rast(file.path(L, "L29_Low_depth_wgs84.tif"))
d_mid   <- rast(file.path(L, "L29_Mid_depth_wgs84.tif"))
d_high  <- rast(file.path(L, "L29_High_depth_wgs84.tif"))
roads   <- st_read(file.path(L, "L29_roads.gpkg"), quiet = TRUE)
bridges <- st_read(file.path(L, "L29_bridges.gpkg"), quiet = TRUE)
cat("  All loaded
")

# --- 2. Disaggregate to reduce pixelation -------------------------------------

cat("  Disaggregating depth rasters...
")
d_low  <- disagg(d_low,  fact = 7, method = "bilinear")
d_mid  <- disagg(d_mid,  fact = 7, method = "bilinear")
d_high <- disagg(d_high, fact = 7, method = "bilinear")
d_low[d_low < 0.01]   <- NA
d_mid[d_mid < 0.01]   <- NA
d_high[d_high < 0.01] <- NA
cat("  Done
")

# --- 3. Classify depth into 4 classes -----------------------------------------

classify_depth <- function(r) {
  m <- matrix(c(
    0,   1,  1,
    1,   3,  2,
    3,   7,  3,
    7, 100,  4
  ), ncol = 3, byrow = TRUE)
  classify(r, m, right = TRUE)
}

d_low_cls  <- classify_depth(d_low)
d_mid_cls  <- classify_depth(d_mid)
d_high_cls <- classify_depth(d_high)

levels(d_low_cls)  <- data.frame(id = 1:4, depth = c("1","2","3","4"))
levels(d_mid_cls)  <- data.frame(id = 1:4, depth = c("1","2","3","4"))
levels(d_high_cls) <- data.frame(id = 1:4, depth = c("1","2","3","4"))

# --- 4. Extent tight to actual flood -----------------------------------------

flood_ext <- function(r) {
  cells <- which(!is.na(values(r)))
  xy <- xyFromCell(r, cells)
  c(min(xy[,1]), max(xy[,1]), min(xy[,2]), max(xy[,2]))
}

e_low  <- flood_ext(d_low)
e_mid  <- flood_ext(d_mid)
e_high <- flood_ext(d_high)

xmin_f <- min(e_low[1], e_mid[1], e_high[1])
xmax_f <- max(e_low[2], e_mid[2], e_high[2])
ymin_f <- min(e_low[3], e_mid[3], e_high[3])
ymax_f <- max(e_low[4], e_mid[4], e_high[4])

xpad <- (xmax_f - xmin_f) * 0.15
ypad <- (ymax_f - ymin_f) * 0.05

xlim <- c(xmin_f - xpad, xmax_f + xpad)
ylim <- c(ymin_f - ypad, ymax_f + ypad)

cat("  Flood extent: lon", round(xlim, 3), " lat", round(ylim, 3), "
")

# --- 5. Depth colours ---------------------------------------------------------

depth_cols <- c(
  "1" = "#7FC1DE",   # darkened from #A8D8EA for contrast against basemap (R4.9)
  "2" = "#4A90C4",
  "3" = "#1B4F8A",
  "4" = "#1A1147"
)

depth_labels <- c(
  "1" = "0 - 1 m",
  "2" = "1 - 3 m",
  "3" = "3 - 7 m",
  "4" = "> 7 m"
)

# --- 6. Theme -----------------------------------------------------------------

theme_fig <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey20", linewidth = 0.4),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.title       = element_blank(),
      axis.text.y      = element_text(size = base_size - 1.5),
      axis.text.x      = element_text(size = base_size - 1.5, angle = 45, hjust = 1),
      legend.title     = element_text(size = base_size, face = "bold"),
      legend.text      = element_text(size = base_size - 1),
      legend.key       = element_blank(),
      legend.key.size  = unit(0.3, "cm"),
      legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
      plot.margin      = margin(1, 1, 1, 1, "mm"),
      plot.title       = element_text(size = base_size, face = "bold",
                                       margin = margin(0, 0, 2, 0))
    )
}

# --- 7. Panel builder ---------------------------------------------------------

build_inundation_panel <- function(depth_cls, title, show_legend = FALSE,
                                    show_scalebar = FALSE, show_north = FALSE,
                                    show_yaxis = TRUE) {

  p <- ggplot() +

    # Hillshade
    geom_spatraster(data = hs, show.legend = FALSE) +
    scale_fill_gradient(low = "grey10", high = "white",
                        na.value = "transparent", guide = "none") +

    ggnewscale::new_scale_fill() +

    # Depth classes
    geom_spatraster(data = depth_cls, show.legend = show_legend) +
    scale_fill_manual(
      values = depth_cols,
      labels = depth_labels,
      name = "Max depth",
      na.value = "transparent",
      na.translate = FALSE,
      drop = FALSE,
      guide = guide_legend(
        override.aes = list(linetype = 0)
      )
    ) +

    # Roads — use colour aesthetic for legend, not linetype
    geom_sf(data = roads |> mutate(lbl = "KKH / Roads"),
            aes(colour = lbl),
            linewidth = 0.3, show.legend = show_legend) +
    scale_colour_manual(
      name = NULL,
      values = c("KKH / Roads" = "grey15"),
      guide = guide_legend(
        override.aes = list(
          fill = NA,
          shape = NA,
          linewidth = 0.5,
          linetype = "solid"
        )
      )
    ) +

    # Bridges
    geom_sf(data = bridges, colour = "#E31A1C", shape = 17, size = 1.2) +

    # KKH label
    annotate("text", x = 74.875, y = 36.44, label = "KKH",
             size = 1.8, fontface = "italic", colour = "grey25", angle = -60) +

    # Passu Lake origin
    annotate("point", x = 74.893, y = 36.455, shape = 21,
             size = 2, fill = "white", colour = "black", stroke = 0.5) +
    annotate("text", x = 74.893, y = 36.453, label = "Passu Lake",
             size = 1.6, fontface = "bold", vjust = 1) +

    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggtitle(title) +
    theme_fig()

  if (!show_legend) p <- p + theme(legend.position = "none")
  if (!show_yaxis)  p <- p + theme(axis.text.y = element_blank())

  if (show_scalebar) {
    p <- p + annotation_scale(
      location = "bl", width_hint = 0.25,
      pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
      text_cex = 0.5, line_width = 0.4, height = unit(0.1, "cm")
    )
  }

  if (show_north) {
    p <- p + annotation_north_arrow(
      location = "tr", which_north = "true",
      height = unit(0.5, "cm"), width = unit(0.4, "cm"),
      pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
      style = north_arrow_fancy_orienteering(text_size = 4)
    )
  }

  return(p)
}

# --- 8. Build panels ----------------------------------------------------------

cat("--- Building panels ---
")

p_low  <- build_inundation_panel(d_low_cls,
           "(a) Low (1.08 M m³)",
           show_scalebar = TRUE, show_yaxis = TRUE)

p_mid  <- build_inundation_panel(d_mid_cls,
           "(b) Mid (2.15 M m³)",
           show_yaxis = FALSE)

p_high <- build_inundation_panel(d_high_cls,
           "(c) High (3.23 M m³)",
           show_legend = TRUE, show_north = TRUE, show_yaxis = FALSE)

# --- 9. Combine + save --------------------------------------------------------

cat("--- Combining ---
")

p_final <- plot_grid(p_low, p_mid, p_high, ncol = 3, rel_widths = c(1, 1, 1.3))

ggsave(file.path(OUT_DIR, "fig06_v4_inundation_L29.png"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white")

ggsave(file.path(OUT_DIR, "fig06_v4_inundation_L29.tiff"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white", compression = "lzw")

cat("Done! Saved to", OUT_DIR, "
")


