
# ════════════════════════════════════════════════════════════════
# 01_fig01_study_area.R
# Figure 1 — Study Area Map
# PURPOSE: Overview map of northern Pakistan showing the study region. 
# AUTHOR:  Ismail, Sadaf — GSAIS, Kyoto University
# DATE:    2026
# ════════════════════════════════════════════════════════════════

setwd("C:/Users/sadaf/Documents/PPR3")

library(sf)
library(terra)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(ggrepel)
library(dplyr)
library(tidyterra)

OUT_DIR    <- "C:/Users/sadaf/Documents/PPR3/figures"
FIG_WIDTH  <- 17.4
FIG_HEIGHT <- 12.0
FIG_DPI    <- 600
BASE_SIZE  <- 7
L <- "data/processed/fig01_layers"

# --- 1. Load pre-clipped layers -----------------------------------------------

cat("--- Loading layers ---
")
lakes      <- st_read(file.path(L, "lakes.gpkg"), quiet = TRUE)
pak_L0     <- st_read(file.path(L, "pak_L0.gpkg"), quiet = TRUE)
gb_chitral <- st_read(file.path(L, "gb_chitral.gpkg"), quiet = TRUE)
afg_L0     <- st_read(file.path(L, "afg_L0.gpkg"), quiet = TRUE)
chn_L0     <- st_read(file.path(L, "chn_L0.gpkg"), quiet = TRUE)
glaciers   <- st_read(file.path(L, "glaciers_clipped.gpkg"), quiet = TRUE)
rivers     <- st_read(file.path(L, "rivers_clipped.gpkg"), quiet = TRUE)
hs_rast    <- rast(file.path(L, "hillshade_clipped.tif"))
cat("  All layers loaded
")

# --- 2. Prepare lake attributes -----------------------------------------------

lakes$vuln_display <- as.factor(lakes$vulnerability)

lakes$is_priority <- grepl("Shisper|Shishper|L27|Passu|L29",
                           lakes$glacier_name, ignore.case = TRUE)
lakes$display_label <- ifelse(lakes$is_priority, lakes$glacier_name, NA)
lakes$display_label <- gsub(".*Shis[hp].*", "L27 — Shisper", lakes$display_label)
lakes$display_label <- gsub(".*Passu.*",    "L29 — Passu",   lakes$display_label)
priority_lakes <- lakes |> filter(is_priority)

# --- 3. Map extent ------------------------------------------------------------

lake_bbox <- st_bbox(lakes)
x_pad <- (lake_bbox[["xmax"]] - lake_bbox[["xmin"]]) * 0.15
y_pad <- (lake_bbox[["ymax"]] - lake_bbox[["ymin"]]) * 0.15

xlim <- c(lake_bbox[["xmin"]] - x_pad, lake_bbox[["xmax"]] + x_pad)
ylim <- c(lake_bbox[["ymin"]] - y_pad, lake_bbox[["ymax"]] + y_pad)

# --- 4. Derived objects -------------------------------------------------------

all_borders <- bind_rows(
  pak_L0[, attr(pak_L0, "sf_column")],
  afg_L0[, attr(afg_L0, "sf_column")],
  chn_L0[, attr(chn_L0, "sf_column")]
)

gb_dissolved <- st_union(gb_chitral) |> st_as_sf()
gb_dissolved$label <- "Gilgit-Baltistan & Chitral"

# River Strahler weight
ord_col <- intersect(names(rivers), c("ORD_STRA", "strahler", "ord_stra"))
if (length(ord_col) > 0) rivers$stream_wt <- pmin(rivers[[ord_col[1]]], 6)

# Country label positions
country_labels <- data.frame(
  label = c("Pakistan", "Afghanistan", "China"),
  x = c(mean(xlim), xlim[1] + diff(xlim) * 0.08, xlim[2] - diff(xlim) * 0.12),
  y = c(ylim[1] + diff(ylim) * 0.08, mean(ylim), ylim[2] - diff(ylim) * 0.08)
)

# Manual label positions — L27 fixed at 36.2N/74E, L29 uses ggrepel
priority_coords <- st_coordinates(priority_lakes)
priority_labels_df <- data.frame(
  label = priority_lakes$display_label,
  pt_x  = priority_coords[, 1],
  pt_y  = priority_coords[, 2]
)

l27_label <- priority_labels_df |> filter(grepl("L27", label))
l27_label$lbl_x <- 74.0
l27_label$lbl_y <- 36.2

l29_lakes <- priority_lakes |> filter(grepl("Passu", display_label))

# --- 5. Theme -----------------------------------------------------------------

theme_fig <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey20", linewidth = 0.4),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1),
      legend.title     = element_text(size = base_size, face = "bold"),
      legend.text      = element_text(size = base_size - 1),
      legend.key       = element_blank(),
      legend.key.size  = unit(0.3, "cm"),
      legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
      plot.margin      = margin(2, 2, 2, 2, "mm")
    )
}

# --- 6. Main map --------------------------------------------------------------

cat("--- Building main map ---
")

p_main <- ggplot() +

  # 1. Hillshade
  geom_spatraster(data = hs_rast, show.legend = FALSE) +
  scale_fill_gradient(low = "grey10", high = "white",
                      na.value = "transparent", guide = "none") +

  # 2. Country borders
  geom_sf(data = all_borders, fill = NA, colour = "grey40",
          linewidth = 0.5, linetype = "dashed") +

  # 3. Glaciers
  geom_sf(data = glaciers, fill = alpha("#B3DDF2", 0.5),
          colour = alpha("#7FB5D4", 0.6), linewidth = 0.15)

# 4. Rivers
if (length(ord_col) > 0) {
  p_main <- p_main +
    geom_sf(data = rivers, aes(linewidth = stream_wt),
            colour = "#4393C3", show.legend = FALSE) +
    scale_linewidth_continuous(range = c(0.1, 0.6), guide = "none")
} else {
  p_main <- p_main +
    geom_sf(data = rivers, colour = "#4393C3", linewidth = 0.2)
}

p_main <- p_main +

# 5. GB + Chitral ON TOP of rivers, with legend
  geom_sf(data = gb_dissolved,
          aes(linetype = label),
          fill = alpha("#D95F02", 0.08),
          colour = "#D95F02",
          linewidth = 0.8) +
  scale_linetype_manual(
    name = "Study area",
    values = c("Gilgit-Baltistan & Chitral" = "solid")
  ) +

  # 6. Lake points
  geom_sf(data = lakes, aes(size = vuln_display),
          colour = "#B2182B", fill = alpha("#B2182B", 0.6),
          shape = 21, stroke = 0.4) +
  scale_size_manual(
    values = c("2" = 1.0, "3" = 1.5, "4" = 2.2, "5" = 3.0),
    name = "Vulnerability", drop = TRUE
  ) +

  # 7a. L27 label — manual position
  geom_segment(data = l27_label,
               aes(x = lbl_x, y = lbl_y, xend = pt_x, yend = pt_y),
               linewidth = 0.6, colour = "grey20") +
  geom_label(data = l27_label,
             aes(x = lbl_x, y = lbl_y, label = label),
             size = 2.2, fontface = "bold",
             fill = alpha("white", 0.85),
             label.size = 0.2,
             label.padding = unit(0.12, "lines")) +

  # 7b. L29 label — ggrepel
  ggrepel::geom_label_repel(
    data = l29_lakes,
    aes(geometry = geom, label = display_label),
    stat = "sf_coordinates",
    size = 2.2, fontface = "bold",
    fill = alpha("white", 0.85),
    label.size = 0.2,
    label.padding = unit(0.12, "lines"),
    box.padding   = unit(0.4, "lines"),
    point.padding = unit(0.3, "lines"),
    segment.size  = 0.6,
    segment.colour = "grey20",
    min.segment.length = 0,
    max.overlaps = 20,
    seed = 42
  ) +

  # 8. Country labels
  geom_label(data = country_labels, aes(x = x, y = y, label = label),
             size = 2.8, colour = "grey25", fontface = "bold.italic",
             fill = alpha("white", 0.5), label.size = 0,
             label.padding = unit(0.1, "lines")) +

  # Coord + decorations
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  annotation_scale(
    location = "bl", width_hint = 0.2,
    pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm"),
    text_cex = 0.6, line_width = 0.5, height = unit(0.15, "cm")
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = unit(0.7, "cm"), width = unit(0.5, "cm"),
    pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 5)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_fig() +
  guides(
    linetype = guide_legend(
      override.aes = list(colour = "#D95F02", linewidth = 0.8)
    )
  )

# --- 7. Inset map -------------------------------------------------------------

cat("--- Building inset ---
")

study_box <- st_as_sfc(st_bbox(c(xmin = xlim[1], ymin = ylim[1],
                                  xmax = xlim[2], ymax = ylim[2]),
                                crs = st_crs(4326)))

p_inset <- ggplot() +
  geom_sf(data = all_borders, fill = "grey90", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = pak_L0, fill = alpha("grey80", 0.5),
          colour = "grey30", linewidth = 0.4) +
  geom_sf(data = study_box, fill = NA, colour = "#B2182B", linewidth = 0.6) +
  coord_sf(xlim = c(58, 82), ylim = c(23, 40), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "grey30",
                                         linewidth = 0.4),
        plot.margin = margin(0, 0, 0, 0))

# --- 8. Combine + save --------------------------------------------------------

cat("--- Saving ---
")

p_final <- ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_inset, x = 0.02, y = 0.02, width = 0.25, height = 0.30)

ggsave(file.path(OUT_DIR, "fig01_v4_study_area.png"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white")

ggsave(file.path(OUT_DIR, "fig01_v4_study_area.tiff"), p_final,
       width = FIG_WIDTH, height = FIG_HEIGHT, units = "cm",
       dpi = FIG_DPI, bg = "white", compression = "lzw")

cat("Done! Saved to", OUT_DIR, "
")


