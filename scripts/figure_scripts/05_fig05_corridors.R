# ════════════════════════════════════════════════════════════════
# 05_fig05_corridors.R
# Figure 5 — Downstream Corridors with Exposure Scores
# PURPOSE: Map of all filtered lakes with their 50 km downstream corridors along the
#           river network, colour-coded by composite exposure score. This is the
#          "before simulation" overview — shows spatial pattern of exposure and
#           justifies why L27 and L29 were selected.
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
#
# UPDATED (2026-08-21): supersedes 05_fig05_corridors.R (moved to
# scripts/OLD/). Fixes one problem found during manuscript review:
#
# ON-IMAGE TITLE FALSELY CLAIMED A SECOND EXCLUSION. The panel (a) title
# read "16 corridors (L21 Karambar, L30 Khurdopin excluded)". Two things
# were wrong with this: (1) there is no lake "L30" anywhere in the
# working set -- the real lake named Khurdupin is L15, and (2) L15 was
# never actually excluded by the filter below in the first place: the
# old filter was `!lake_id %in% c("L21", "L30")`, and since no row has
# lake_id "L30", that filter only ever removed L21. The resulting count
# (17 - 1 = 16) was arithmetically correct, but the title's explanation
# of *why* it was 16 was fabricated -- confirmed against the manuscript
# body text (Section 4.1: "The L21 Karamber corridor ... is retained in
# the working set but excluded from the spatial display in Fig. 5.
# Across the remaining 16 corridors..." -- only ONE exclusion is
# described there, matching the code's actual behaviour, not the old
# title's claim of two). Confirmed visually too: L15's corridor IS
# present and coloured in the panel (a) overview (it just isn't
# labelled, and its longitude ~73.2°E falls in the gap between the
# Chitral cluster box and the Hunza-Karakoram cluster box, which is why
# it doesn't appear in panels (b) or (c) -- that is a real cartographic
# gap between the two inset extents, not a data exclusion, and the
# manuscript text does not claim otherwise).
#
# Fix: removed "L30" from the exclusion filter (it was dead code -- matched
# nothing, changed no output) and corrected the on-image title to state
# only the real exclusion (L21). No other logic changed; panel (a)'s
# plotted corridors and the overview map are otherwise pixel-identical
# to before this fix -- only the title text changes.
# ════════════════════════════════════════════════════════════════

setwd("C:/Users/sadaf/Documents/PPR3")

library(sf)
library(terra)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(dplyr)
library(tidyterra)
library(scico)
library(ggnewscale)
library(cowplot)

OUT_DIR    <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_WIDTH  <- 17.4
FIG_HEIGHT <- 18.0
FIG_DPI    <- 800   # bumped from 600 to 800 dpi for repo/publication release
BASE_SIZE  <- 9   # bumped from 7 for label legibility (R4.9)
L <- "data/processed/fig01_layers"

# --- 1. Load layers -----------------------------------------------------------

cat("--- Loading layers ---
")
corr_all   <- st_read("data/processed/lake_corridors_scored.gpkg", quiet = TRUE) |>
  st_transform(4326)
lakes      <- st_read(file.path(L, "lakes.gpkg"), quiet = TRUE)
gb_chitral <- st_read(file.path(L, "gb_chitral.gpkg"), quiet = TRUE)
rivers     <- st_read(file.path(L, "rivers_clipped.gpkg"), quiet = TRUE)
hs_rast    <- rast(file.path(L, "hillshade_clipped.tif"))
roads      <- st_read(file.path(L, "osm_roads.gpkg"), quiet = TRUE)
bridges    <- st_read(file.path(L, "osm_bridges.gpkg"), quiet = TRUE)
cat("  All layers loaded
")

# --- 2. Exclude L21 (spatial-display exclusion only; retained in the working
#        set and in Table 2 -- see Section 4.1) --------------------------------

corr <- corr_all |>
  filter(!lake_id %in% c("L21")) |>
  arrange(final_score)

corr$display_label <- case_when(
  corr$lake_id == "L27" ~ "L27 Shisper",
  corr$lake_id == "L29" ~ "L29 Passu",
  TRUE ~ corr$lake_id
)

lakes_plot <- lakes |> filter(!grepl("Karambar|L21", glacier_name, ignore.case = TRUE))
gb_dissolved <- st_union(gb_chitral)

ord_col <- intersect(names(rivers), c("ORD_STRA", "strahler", "ord_stra"))
if (length(ord_col) > 0) rivers$stream_wt <- pmin(rivers[[ord_col[1]]], 6)

score_min <- floor(min(corr$final_score, na.rm = TRUE) * 10) / 10
score_max <- ceiling(max(corr$final_score, na.rm = TRUE) * 10) / 10

# --- 3. Clusters and extents --------------------------------------------------

chitral_ids <- c("L06", "L01", "L02", "L03", "L04", "L05")
hunza_ids   <- c("L27", "L28", "L26", "L25", "L24", "L29", "L19")

chitral_corr <- corr |> filter(lake_id %in% chitral_ids)
hunza_corr   <- corr |> filter(lake_id %in% hunza_ids)

ch_pad <- 0.15
hu_pad <- 0.1
chitral_bbox <- st_bbox(chitral_corr)
hunza_bbox   <- st_bbox(hunza_corr)

chitral_xlim <- c(chitral_bbox[["xmin"]] - ch_pad, chitral_bbox[["xmax"]] + ch_pad)
chitral_ylim <- c(chitral_bbox[["ymin"]] - ch_pad, chitral_bbox[["ymax"]] + ch_pad)
hunza_xlim   <- c(hunza_bbox[["xmin"]] - hu_pad, hunza_bbox[["xmax"]] + hu_pad)
hunza_ylim   <- c(hunza_bbox[["ymin"]] - hu_pad, hunza_bbox[["ymax"]] + hu_pad)

chitral_lakes <- lakes_plot |>
  st_crop(st_bbox(c(xmin = chitral_xlim[1], ymin = chitral_ylim[1],
                     xmax = chitral_xlim[2], ymax = chitral_ylim[2]), crs = 4326))
hunza_lakes <- lakes_plot |>
  st_crop(st_bbox(c(xmin = hunza_xlim[1], ymin = hunza_ylim[1],
                     xmax = hunza_xlim[2], ymax = hunza_ylim[2]), crs = 4326))

lake_bbox <- st_bbox(lakes_plot)
lk_xpad <- (lake_bbox[["xmax"]] - lake_bbox[["xmin"]]) * 0.15
lk_ypad <- (lake_bbox[["ymax"]] - lake_bbox[["ymin"]]) * 0.15
ov_xlim <- c(lake_bbox[["xmin"]] - lk_xpad, lake_bbox[["xmax"]] + lk_xpad)
ov_ylim <- c(lake_bbox[["ymin"]] - lk_ypad, lake_bbox[["ymax"]] + lk_ypad)

box_chitral <- st_as_sfc(st_bbox(c(xmin = chitral_xlim[1], ymin = chitral_ylim[1],
                                    xmax = chitral_xlim[2], ymax = chitral_ylim[2]),
                                  crs = st_crs(4326)))
box_hunza <- st_as_sfc(st_bbox(c(xmin = hunza_xlim[1], ymin = hunza_ylim[1],
                                   xmax = hunza_xlim[2], ymax = hunza_ylim[2]),
                                 crs = st_crs(4326)))
box_labels <- data.frame(
  label = c("b", "c"),
  x = c(chitral_xlim[1] + 0.05, hunza_xlim[1] + 0.05),
  y = c(chitral_ylim[2] - 0.05, hunza_ylim[2] - 0.05)
)

# --- 4. Theme -----------------------------------------------------------------

theme_fig <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey20", linewidth = 0.4),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.title       = element_blank(),
      axis.text        = element_text(size = base_size - 1),
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

# --- 5. Panel builder ---------------------------------------------------------
# v9 FIX: roads legend now uses key_glyph = "path" on the geom_sf layer
# and override.aes explicitly sets linetype=1, linewidth=0.5, fill=NA, shape=NA
# This forces ggplot2 to draw a line swatch instead of a polygon/point box.

build_panel <- function(xlim, ylim, corr_sub, lakes_sub, label_ids = NULL,
                        show_legend = FALSE, title = NULL,
                        show_scalebar = FALSE, show_boundary = TRUE) {

  panel_bbox <- st_as_sfc(st_bbox(c(xmin = xlim[1], ymin = ylim[1],
                                     xmax = xlim[2], ymax = ylim[2]),
                                   crs = st_crs(4326)))
  riv_sub <- suppressWarnings(st_intersection(rivers, panel_bbox))
  corr_sub_union <- st_union(corr_sub) |> st_make_valid()  # FIX: repair degenerate vertices before intersection
  roads_sub   <- suppressWarnings(st_intersection(roads, corr_sub_union))
  bridges_sub <- suppressWarnings(st_intersection(bridges, corr_sub_union))

  if (!is.null(label_ids)) {
    label_data <- corr_sub |> filter(lake_id %in% label_ids)
  } else {
    label_data <- corr_sub[0, ]
  }

  p <- ggplot() +
    geom_spatraster(data = hs_rast, show.legend = FALSE) +
    scale_fill_gradient(low = "grey10", high = "white",
                        na.value = "transparent", guide = "none") +
    ggnewscale::new_scale_fill()

  if (show_boundary) {
    p <- p +
      geom_sf(data = gb_dissolved, fill = NA,
              colour = "#D95F02", linewidth = 0.4, linetype = "dashed")
  }

  if (length(ord_col) > 0 && nrow(riv_sub) > 0) {
    riv_sub$stream_wt <- pmin(riv_sub[[ord_col[1]]], 6)
    p <- p +
      geom_sf(data = riv_sub, aes(linewidth = stream_wt),
              colour = alpha("#4393C3", 0.3), show.legend = FALSE) +
      scale_linewidth_continuous(range = c(0.08, 0.4), guide = "none")
  }

  p <- p +
    geom_sf(data = corr_sub, aes(fill = final_score),
            colour = alpha("grey30", 0.5), linewidth = 0.3) +
    scico::scale_fill_scico(
      palette = "lajolla", name = "Hazard-exposure
score",
      limits = c(score_min, score_max),
      direction = 1, breaks = seq(0.3, 1.0, 0.1)
    )

  # ── v9 FIX: roads legend ────────────────────────────────────────────────────
  # Problem in v8: geom_sf() for line features was picking up the polygon
  # key glyph (hollow rectangle) from the surrounding fill scales.
  # Fix: set key_glyph = "path" explicitly on the roads geom_sf() call so
  # ggplot2 draws a line segment in the legend key, then use override.aes to
  # remove any residual fill/shape artefacts.
  if (nrow(roads_sub) > 0) {
    p <- p +
      geom_sf(
        data       = roads_sub |> mutate(lbl = "KKH / Roads"),
        aes(colour = lbl),
        linewidth  = 0.35,
        show.legend = show_legend,
        # Force a line-segment key glyph (not the default polygon rectangle)
        key_glyph  = "path"
      ) +
      scale_colour_manual(
        name   = NULL,
        values = c("KKH / Roads" = "grey15"),
        guide  = guide_legend(
          override.aes = list(
            # Explicitly blank out fill and shape so only the line shows
            fill      = NA,
            shape     = NA,
            linetype  = 1,
            linewidth = 0.6
          )
        )
      )
  }

  if (nrow(bridges_sub) > 0) {
    p <- p + geom_sf(data = bridges_sub, colour = "#E31A1C",
                     shape = 17, size = 1.5)
  }

  p <- p +
    geom_sf(data = lakes_sub, colour = "black", fill = "white",
            shape = 21, size = 1.8, stroke = 0.5)

  if (nrow(label_data) > 0) {
    p <- p +
      ggrepel::geom_label_repel(
        data = label_data,
        aes(geometry = geom, label = display_label),
        stat = "sf_coordinates",
        size = 2.0, fontface = "bold",
        fill = alpha("white", 0.9),
        label.size = 0.2,
        label.padding = unit(0.1, "lines"),
        box.padding   = unit(0.5, "lines"),
        point.padding = unit(0.3, "lines"),
        segment.size  = 0.5,
        segment.colour = "grey20",
        min.segment.length = 0,
        max.overlaps = 20, seed = 42
      )
  }

  p <- p +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_fig()

  if (!is.null(title)) p <- p + ggtitle(title)
  if (!show_legend)    p <- p + theme(legend.position = "none")

  if (show_scalebar) {
    p <- p + annotation_scale(
      location   = "bl", width_hint = 0.2,
      pad_x      = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
      text_cex   = 0.55, line_width = 0.4, height = unit(0.12, "cm")
    )
  }

  return(p)
}

# --- 6. Build panels ----------------------------------------------------------

cat("--- Building panels ---
")

p_overview <- build_panel(
  xlim = ov_xlim, ylim = ov_ylim,
  corr_sub = corr, lakes_sub = lakes_plot,
  label_ids = c("L27", "L29"),
  show_legend   = TRUE,
  show_boundary = FALSE,
  title         = "(a) Overview -- 16 corridors (L21 Karambar excluded)",
  show_scalebar = TRUE
) +
  geom_sf(data = box_chitral, fill = NA, colour = "#0072B2",
          linewidth = 0.7, linetype = "solid") +
  geom_sf(data = box_hunza, fill = NA, colour = "#009E73",
          linewidth = 0.7, linetype = "solid") +
  geom_text(data = box_labels, aes(x = x, y = y, label = label),
            size = 2.8, fontface = "bold",
            colour = c("#0072B2", "#009E73"))

p_chitral <- build_panel(
  xlim = chitral_xlim, ylim = chitral_ylim,
  corr_sub  = chitral_corr, lakes_sub = chitral_lakes,
  label_ids = chitral_ids,
  show_legend   = FALSE,
  show_boundary = TRUE,
  title         = "(b) Chitral cluster",
  show_scalebar = TRUE
) +
  theme(panel.border = element_rect(fill = NA, colour = "#0072B2", linewidth = 0.7))

p_hunza <- build_panel(
  xlim = hunza_xlim, ylim = hunza_ylim,
  corr_sub  = hunza_corr, lakes_sub = hunza_lakes,
  label_ids = c("L27", "L29", "L19"),
  show_legend   = FALSE,
  show_boundary = TRUE,
  title         = "(c) Hunza-Karakoram cluster",
  show_scalebar = TRUE
) +
  scale_x_continuous(breaks = seq(74.2, 75.0, by = 0.4)) +  # FIX: thin x-axis breaks (was 0.1 degree, too dense)
  theme(panel.border = element_rect(fill = NA, colour = "#009E73", linewidth = 0.7))

# --- 7. Combine + save --------------------------------------------------------

cat("--- Combining panels ---
")

p_bottom <- plot_grid(p_chitral, p_hunza, ncol = 2, rel_widths = c(1, 1))
p_final  <- plot_grid(p_overview, p_bottom, ncol = 1, rel_heights = c(1, 1))

ggsave(file.path(OUT_DIR, "fig05_v9_corridors.png"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white")

ggsave(file.path(OUT_DIR, "fig05_v9_corridors.tiff"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white", compression = "lzw")

cat("Done! Saved to", OUT_DIR, "
")