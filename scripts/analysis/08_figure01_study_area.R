# 08_figure01_study_area.R
# Generate Figure 1 (study area map) with all base layers.

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)

library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)

study_area  <- sf::st_read("data/processed/study_area_districts.gpkg",
                           quiet = TRUE)
glaciers    <- sf::st_read("data/processed/RGI7_glaciers_GB_Chitral.gpkg",
                           quiet = TRUE)
rivers_main <- sf::st_read("data/processed/HydroRIVERS_main_GB_Chitral.gpkg",
                           quiet = TRUE)
lakes       <- sf::st_read("data/processed/lakes_analysis_set.gpkg",
                           quiet = TRUE)

working_set <- lakes |> dplyr::filter(vulnerability >= 4)

p <- ggplot() +
  geom_sf(data = study_area, fill = "white",
          color = "grey60", linewidth = 0.3) +
  geom_sf(data = glaciers, fill = "#cce5ff",
          color = NA, alpha = 0.6) +
  geom_sf(data = rivers_main, color = "#1f78b4",
          linewidth = 0.3, aes(alpha = ORD_STRA)) +
  scale_alpha_continuous(range = c(0.3, 0.9), guide = "none") +
  geom_sf(data = lakes,
          aes(color = factor(vulnerability),
              size = factor(vulnerability))) +
  scale_color_manual(
    values = c("2" = "grey60", "3" = "orange",
               "4" = "red", "5" = "darkred"),
    name = "Vulnerability"
  ) +
  scale_size_manual(
    values = c("2" = 1.5, "3" = 2, "4" = 3, "5" = 3.5),
    name = "Vulnerability"
  ) +
  ggrepel::geom_label_repel(
    data = working_set,
    aes(geometry = geom, label = glacier_name),
    stat = "sf_coordinates", size = 2.2,
    max.overlaps = 25, label.padding = 0.15
  ) +
  coord_sf() +
  theme_minimal(base_size = 11) +
  labs(
    title = "Vulnerable GLOF Sites - Gilgit-Baltistan and Chitral",
    x = "Longitude", y = "Latitude",
    caption = "Boundaries: OCHA. Glaciers: RGI 7.0. Rivers: HydroRIVERS v1.0."
  )

ggsave("figures/fig01_study_area_v2.png",
       plot = p, width = 12, height = 8, dpi = 200)

cat("Saved: figures/fig01_study_area_v2.png\n")

