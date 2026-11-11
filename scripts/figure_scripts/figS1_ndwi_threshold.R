# Figure S1: NDWI threshold

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

df <- read_csv(
  "data/processed/Threshold_sensitivity.csv",
  show_col_types = FALSE
) |>
  pivot_longer(
    cols      = starts_with("area_t"),
    names_to  = "threshold_code",
    values_to = "area_km2"
  ) |>
  mutate(
    threshold = case_when(
      threshold_code == "area_t00" ~ 0.0,
      threshold_code == "area_t01" ~ 0.1,
      threshold_code == "area_t02" ~ 0.2,
      threshold_code == "area_t03" ~ 0.3,
      threshold_code == "area_t04" ~ 0.4
    )
  )

df <- df |>
  mutate(
    name = recode(name, "Khurdopin" = "Khurdupin", "Karambar" = "Karamber")
  )

chosen_threshold <- 0.1

lake_colours <- c(
  "Khurdupin" = "#2166AC",
  "Shisper"   = "#E08020", 
  "Passu"     = "#1A9850",
  "Karamber"  = "#984EA3"
)

circle_r <- 0.022

shishper_y <- df |>
  filter(
    threshold == chosen_threshold,
    name == "Shisper"
  ) |>
  pull(area_km2)

passu_y <- df |>
  filter(
    threshold == chosen_threshold,
    name == "Passu"
  ) |>
  pull(area_km2)

x_label <- chosen_threshold + 0.045

labels_df <- tibble(

  # Label text identities stay same
  name = c("Shisper", "Passu"),

  # Text shown
  lab = sprintf("%.3f km\u00b2", c(passu_y, shishper_y)),

  # Text locations stay unchanged
  x_label = x_label,
  y_label = c(0.58, 0.32),

  pt_y = c(passu_y, shishper_y)

) |>
  mutate(
    dx   = chosen_threshold - x_label,
    dy   = pt_y - y_label,
    dist = sqrt(dx^2 + dy^2),

    # Arrow heads stop at circle edge
    x_end = chosen_threshold - (dx / dist) * circle_r,
    y_end = pt_y             - (dy / dist) * circle_r,

    # Arrow origins unchanged
    x_start = x_label,
    y_start = y_label - 0.025
  )

circles_df <- df |>
  filter(
    threshold == chosen_threshold,
    name %in% c("Shisper", "Passu")
  )

khurdupin_t0 <- df |>
  filter(
    threshold == 0.0,
    name == "Khurdupin"
  )

annot_x      <- 0.018
annot_y_top  <- khurdupin_t0$area_km2 + 0.07
arrow_x_end  <- khurdupin_t0$threshold + (circle_r * 0.5)

p <- ggplot(
  df,
  aes(
    x = threshold,
    y = area_km2,
    colour = name,
    group = name
  )
) +

  geom_vline(
    xintercept = chosen_threshold,
    linetype   = "longdash",
    linewidth  = 0.45,
    colour     = "#AAAAAA"
  ) +

  # Main lines
  geom_line(
    linewidth = 0.65,
    lineend   = "round"
  ) +

  # Main filled points (keeps legend unchanged)
  geom_point(
    size  = 2.0,
    shape = 16
  ) +

  annotate(
    "text",
    x = chosen_threshold + 0.008,
    y = 0.80,
    label = "Selected
threshold",
    hjust = 0,
    vjust = 0.5,
    size = 1.8,
    fontface = "italic",
    colour = "#999999",
    lineheight = 1.1
  ) +

  annotate(
    "text",
    x = annot_x,
    y = annot_y_top,
    label = "Shadow & snow
artefacts",
    hjust = 0,
    vjust = 0,
    size = 1.75,
    colour = "#2166AC",
    lineheight = 1.1,
    fontface = "italic"
  ) +

  annotate(
    "segment",
    x    = annot_x + 0.01,
    xend = arrow_x_end,
    y    = annot_y_top - 0.01,
    yend = khurdupin_t0$area_km2 + circle_r,
    arrow = arrow(
      length = unit(1.1, "mm"),
      type   = "closed"
    ),
    colour = "#2166AC",
    linewidth = 0.3
  ) +

  geom_point(
    data = circles_df,
    aes(
      x = threshold,
      y = area_km2,
      colour = name
    ),
    shape = 21,
    size = 3.2,
    stroke = 0.6,
    fill = "white",
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +

  # Arrows
  geom_segment(
    data = labels_df,
    aes(
      x    = x_start,
      xend = x_end,
      y    = y_start,
      yend = y_end
    ),
    arrow = arrow(
      length = unit(1.1, "mm"),
      type   = "closed"
    ),
    colour = "#333333",
    linewidth = 0.28,
    inherit.aes = FALSE
  ) +

  # Labels
  geom_text(
    data = labels_df,
    aes(
      x = x_label,
      y = y_label,
      label = lab
    ),
    hjust = 0,
    vjust = 0.5,
    size = 1.85,
    fontface = "bold",
    colour = "#333333",
    inherit.aes = FALSE
  ) +

  scale_colour_manual(
    values = lake_colours,
    name = NULL
  ) +

  scale_x_continuous(
    breaks = c(0.0, 0.1, 0.2, 0.3, 0.4),
    labels = c("0.0", "0.1", "0.2", "0.3", "0.4"),
    expand = expansion(add = c(0.02, 0.10))
  ) +

  scale_y_continuous(
    limits = c(0, 1.72),
    expand = expansion(mult = c(0.02, 0))
  ) +

 labs(
    x = "NDWI threshold",
    y = expression("Detected lake area (km"^2*")")
  ) +

  theme_classic(base_size = 9) +

  theme(
    legend.position      = "top",
    legend.justification = "right",
    legend.key.size      = unit(3.5, "mm"),
    legend.text          = element_text(size = 7.5),
    legend.background    = element_blank(),
    legend.margin        = margin(0, 0, 2, 0),

    axis.text            = element_text(size = 7.5),
    axis.title           = element_text(size = 9),

    plot.margin          = margin(2, 4, 4, 4, "mm"),

    plot.background      = element_rect(
      fill = "white",
      colour = NA
    ),

    panel.background     = element_rect(
      fill = "white",
      colour = NA
    )
  )

dir.create("figures", showWarnings = FALSE)

dir.create("figures", showWarnings = FALSE)
ggsave("figures/Figure_S1.tiff",
       plot = p, width = 83, height = 80, units = "mm",
       dpi = 800, bg = "white", compression = "lzw")

ggsave("figures/Figure_S1.png",
       plot = p, width = 83, height = 80, units = "mm",
       dpi = 800, bg = "white")

message(
  "Saved: figures/Figure_S1.tiff  +  .png"
)
