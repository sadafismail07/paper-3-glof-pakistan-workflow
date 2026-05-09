# 17_breach_hydrograph_generation.R
# Generate breach inflow hydrographs for HEC-RAS based on Froehlich 2008
# breach parameters and Clague & Mathews 1973 / Walder 1996 hydrograph shape.

setwd("C:/Users/sadaf/Documents/PPR3")
library(dplyr)

build_hydrograph <- function(Vw, Hb, tf_sec, lake_id, scenario,
                              total_duration_hr = 6, dt = 5,
                              shape_param = 1.5,
                              method = "geometric_mean") {

  # Estimate peak Q from empirical bounds
  Q_walder <- 0.045 * Vw^0.66
  PE <- 1000 * 9.81 * Vw * Hb
  Q_costa <- 0.063 * PE^0.42
  Q_peak_init <- sqrt(Q_walder * Q_costa)

  # Build time series
  t_seq <- seq(0, total_duration_hr * 3600, by = dt)
  Q_t <- Q_peak_init * (t_seq / tf_sec)^shape_param *
         exp(shape_param * (1 - t_seq / tf_sec))
  Q_t[1] <- 0

  # Scale so integrated volume matches Vw
  V_init <- sum(Q_t * dt)
  Q_t <- Q_t * (Vw / V_init)
  Q_peak <- max(Q_t)

  hydrograph <- data.frame(
    time_seconds = t_seq,
    time_minutes = t_seq / 60,
    time_hours = t_seq / 3600,
    Q_m3_s = round(Q_t, 2)
  )

  filename <- sprintf("data/processed/%s_breach_hydrograph_%s.csv",
                      lake_id, scenario)
  write.csv(hydrograph, filename, row.names = FALSE)

  list(peak_Q = Q_peak,
       Q_walder_lower = Q_walder,
       Q_costa_upper = Q_costa,
       file = filename)
}

# === L29 Passu, all 3 scenarios ===
breach_params <- read.csv("data/processed/breach_parameters.csv")

for (i in 1:nrow(breach_params)) {
  bp <- breach_params[i, ]
  res <- build_hydrograph(Vw = bp$Vw_m3,
                          Hb = bp$Hb_m,
                          tf_sec = bp$formation_time_min * 60,
                          lake_id = bp$lake_id,
                          scenario = bp$scenario)
  cat(sprintf("%s %s: Peak Q = %.0f m3/s [validation: %.0f - %.0f]\n",
              bp$lake_id, bp$scenario, res$peak_Q,
              res$Q_walder_lower, res$Q_costa_upper))
}

