# 06_quick_map.R
# Quick visualization of vulnerable GLOF sites colored by vulnerability.
# Working-set lakes (vulnerability 4-5) are labeled.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)

inv <- sf::st_read(
  "data/processed/lakes_vulnerable_GB_Chitral.gpkg",
  quiet = TRUE
)

working_set <- inv |> dplyr::filter(vulnerability >= 4)

p <- ggplot() +
  geom_sf(
    data = inv,
    aes(color = factor(vulnerability),
        size  = factor(vulnerability))
  ) +
  scale_color_manual(
    values = c("2" = "grey70", "3" = "orange",
               "4" = "red",    "5" = "darkred"),
    name = "Vulnerability"
  ) +
  scale_size_manual(
    values = c("2" = 1.5, "3" = 2, "4" = 3, "5" = 3.5),
    name = "Vulnerability"
  ) +
  ggrepel::geom_label_repel(
    data         = working_set,
    aes(geometry = geom, label = glacier_name),
    stat         = "sf_coordinates",
    size         = 2.5,
    max.overlaps = 20
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title    = "Vulnerable GLOF Sites - Gilgit-Baltistan and Chitral",
    subtitle = paste0(nrow(working_set),
                      " lakes at vulnerability 4 or 5; ",
                      nrow(inv), " total"),
    x = "Longitude", y = "Latitude"
  )

ggsave(
  "figures/fig01_quick_lake_map.png",
  plot = p, width = 10, height = 7, dpi = 200
)

cat("Saved: figures/fig01_quick_lake_map.png\n")

