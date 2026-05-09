# 15_volume_dam_breach_calculations.R
# Compute lake volumes, dam heights, and Froehlich 2008 breach parameters
# for the two selected lakes (L29 Passu, L27 Shisper).

setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
library(sf)
library(terra)
library(dplyr)

# --- VOLUMES ---

volume_huggel <- function(A_m2) 0.104 * A_m2^1.42

# L29 Passu (Huggel since Cook & Quincey moraine fails for small lakes)
A_passu <- 0.142 * 1e6
V_passu <- volume_huggel(A_passu)

# L27 Shisper from Bhambri 2020 direct measurement
V_shisper <- 1.66e7
V_shisper_unc <- 6.4e6  # ± uncertainty per Bhambri 2020

volume_table <- data.frame(
  lake_id = c("L29","L29","L29","L27","L27","L27"),
  scenario = rep(c("Low","Mid","High"), 2),
  volume_m3 = c(V_passu * 0.5, V_passu, V_passu * 1.5,
                V_shisper - V_shisper_unc, V_shisper, V_shisper + V_shisper_unc),
  source = c(rep("Huggel 2002 ±50%", 3),
             rep("Bhambri 2020 ± uncertainty", 3))
)

write.csv(volume_table, "data/processed/lake_volume_scenarios.csv",
          row.names = FALSE)

# --- DAM HEIGHTS (Passu hillshade method) ---

lakes <- sf::st_read("data/processed/lakes_polygons_verified.gpkg",
                     quiet = TRUE)
passu <- lakes |> dplyr::filter(lake_id == "L29")
passu <- sf::st_transform(passu, 32642)
dem <- terra::rast("data/processed/DEM_UTM42N.tif")

water_elev <- terra::extract(dem, terra::vect(sf::st_centroid(passu)))[, 2]

ring_inner <- sf::st_buffer(passu, dist = 50)
ring_outer <- sf::st_buffer(passu, dist = 200)
moraine_ring <- sf::st_difference(ring_outer, ring_inner)

elev_in_ring <- terra::extract(dem, terra::vect(moraine_ring), raw = TRUE)
elev_values <- elev_in_ring[, 2]
elev_above_water <- elev_values[!is.na(elev_values) & elev_values > water_elev]

moraine_p25 <- quantile(elev_above_water, 0.25)
freeboard_passu <- as.numeric(moraine_p25 - water_elev)
mean_depth_passu <- V_passu / A_passu
Hb_passu <- freeboard_passu + mean_depth_passu

Hb_shisper <- 134  # Bhambri 2020 max depth

# --- FROEHLICH 2008 BREACH PARAMETERS ---

froehlich_breach <- function(Vw, Hb, mode = c("overtopping","piping")) {
  mode <- match.arg(mode)
  k <- ifelse(mode == "overtopping", 1.3, 1.0)
  z <- ifelse(mode == "overtopping", 1.0, 0.7)
  Bavg <- 0.27 * k * Vw^0.32 * Hb^0.04
  tf <- 63.2 * sqrt(Vw / (9.81 * Hb^2))
  data.frame(mode = mode, Hb_m = Hb, Vw_m3 = Vw,
             breach_width_m = round(Bavg, 1),
             formation_time_sec = round(tf, 0),
             formation_time_min = round(tf/60, 1),
             side_slope_z = z, k_factor = k)
}

passu_breach <- bind_rows(
  cbind(scenario = "Low",  froehlich_breach(V_passu * 0.5, Hb_passu, "overtopping")),
  cbind(scenario = "Mid",  froehlich_breach(V_passu,       Hb_passu, "overtopping")),
  cbind(scenario = "High", froehlich_breach(V_passu * 1.5, Hb_passu, "overtopping"))
)
passu_breach$lake_id <- "L29"

shisper_breach <- bind_rows(
  cbind(scenario = "Low",  froehlich_breach(V_shisper - V_shisper_unc, Hb_shisper, "piping")),
  cbind(scenario = "Mid",  froehlich_breach(V_shisper,                 Hb_shisper, "piping")),
  cbind(scenario = "High", froehlich_breach(V_shisper + V_shisper_unc, Hb_shisper, "piping"))
)
shisper_breach$lake_id <- "L27"

breach_table <- rbind(passu_breach, shisper_breach)
breach_table <- breach_table[, c("lake_id","scenario","mode","Vw_m3","Hb_m",
                                  "breach_width_m","formation_time_min",
                                  "side_slope_z","k_factor")]
write.csv(breach_table, "data/processed/breach_parameters.csv",
          row.names = FALSE)

cat("Computed and saved volume + dam height + breach parameters\n")

