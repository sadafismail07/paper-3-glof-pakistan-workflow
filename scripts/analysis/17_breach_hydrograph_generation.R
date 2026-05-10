# 17_breach_hydrograph_generation.R
# Generate breach inflow hydrographs for HEC-RAS using Froehlich 2008
# breach parameters and Walder 1996 / Singh 1996 hydrograph shape.
# Peak Q estimated as geometric mean of Walder & O'Connor 1997 (lower)
# and Costa & Schuster 1988 (upper) empirical bounds. Total integrated
# volume rescaled to match Vw exactly.

setwd("C:/Users/sadaf/Documents/PPR3")
library(dplyr)

build_hydrograph <- function(Vw, Hb, tf_sec, lake_id, scenario,
                              total_duration_hr = 6, dt = 5,
                              shape_param = 1.5) {
  Q_walder <- 0.045 * Vw^0.66
  PE <- 1000 * 9.81 * Vw * Hb
  Q_costa <- 0.063 * PE^0.42
  Q_peak_init <- sqrt(Q_walder * Q_costa)

  t_seq <- seq(0, total_duration_hr * 3600, by = dt)
  Q_t <- Q_peak_init * (t_seq / tf_sec)^shape_param *
         exp(shape_param * (1 - t_seq / tf_sec))
  Q_t[1] <- 0

  V_init <- sum(Q_t * dt)
  Q_t <- Q_t * (Vw / V_init)

  hydrograph <- data.frame(
    time_seconds = t_seq,
    time_minutes = t_seq / 60,
    time_hours = t_seq / 3600,
    Q_m3_s = round(Q_t, 2)
  )

  filename <- sprintf("data/processed/%s_breach_hydrograph_%s.csv",
                      lake_id, scenario)
  write.csv(hydrograph, filename, row.names = FALSE)

  list(peak_Q = max(Q_t),
       Q_walder_lower = Q_walder,
       Q_costa_upper = Q_costa)
}

# Generate all 6 base hydrographs (3 scenarios x 2 lakes)
bp <- read.csv("data/processed/breach_parameters.csv")

cat("Generating base hydrographs:\n")
for (i in 1:nrow(bp)) {
  res <- build_hydrograph(Vw = bp$Vw_m3[i],
                          Hb = bp$Hb_m[i],
                          tf_sec = bp$formation_time_min[i] * 60,
                          lake_id = bp$lake_id[i],
                          scenario = bp$scenario[i])
  cat(sprintf("%s %s: peak Q = %5.0f m3/s [validation: %.0f - %.0f]\n",
              bp$lake_id[i], bp$scenario[i], res$peak_Q,
              res$Q_walder_lower, res$Q_costa_upper))
}

# Generate sediment-bulked hydrographs for both lakes (Mid scenario only)
cat("\nGenerating sediment-bulked hydrographs (Mid scenario):\n")
for (lake_id in c("L29", "L27")) {
  mid_file <- sprintf("data/processed/%s_breach_hydrograph_Mid.csv", lake_id)
  if (!file.exists(mid_file)) next
  mid <- read.csv(mid_file)

  for (factor in c(1.5, 2.0)) {
    bulked <- mid
    bulked$Q_m3_s <- bulked$Q_m3_s * factor
    suffix <- sprintf("bulk%02d", round(factor * 10))
    out_file <- sprintf("data/processed/%s_breach_hydrograph_Mid_%s.csv",
                        lake_id, suffix)
    write.csv(bulked, out_file, row.names = FALSE)
    cat(sprintf("  %s Mid x%.1f: peak Q = %.0f m3/s\n",
                lake_id, factor, max(bulked$Q_m3_s)))
  }
}

