# Run scoring, tables and figures

setwd("C:/Users/sadaf/Documents/PPR3")
steps <- c(
  "scripts/analysis/14_priority_scoring.R",
  "scripts/analysis/14b_scoring_diagnostics.R",
  "scripts/analysis/25_composite_weighting_sensitivity.R",
  "scripts/analysis/19_h4_hazard_mapping.R",
  "scripts/analysis/20_simulation_results.R",
  "scripts/analysis/21_h4_exposure_and_volume_conservation_audit.R",
  "scripts/analysis/23_table03_data_extraction.R",
  "scripts/analysis/24_crs_clip_validation.R",
  "scripts/tables/table02_priority_ranking.R",
  "scripts/tables/table03_simulation_results.R",
  "scripts/tables/table04_h4_exposure.R",
  "scripts/tables/table05_validation_envelopes.R",
  "scripts/tables/tableS1_corridor_overlap.R",
  "scripts/tables/tableS2_ranking_stability.R",
  "scripts/tables/tableS3_hazard_classes.R",
  "scripts/tables/tableS5_corridor_exposure.R",
  "scripts/tables/tableS6_downstream_attenuation.R",
  "scripts/tables/tableS7_weighting_sensitivity.R",
  "scripts/figure_scripts/fig1_study_area.R",
  "scripts/figure_scripts/fig2_workflow.R",
  "scripts/figure_scripts/fig4_priority_scores.R",
  "scripts/figure_scripts/fig5_breach_hydrographs.R",
  "scripts/figure_scripts/figS1_ndwi_threshold.R",
  "scripts/figure_scripts/fig3_corridors.R",
  "scripts/figure_scripts/fig6_inundation_passu.R",
  "scripts/figure_scripts/fig7_inundation_shisper.R",
  "scripts/figure_scripts/fig8_sensitivity.R",
  "scripts/figure_scripts/fig9_hazard_zones.R",
  "scripts/figure_scripts/figS2_breach_peaks.R"
)
log <- data.frame(step = steps, status = NA_character_, message = NA_character_)
for (i in seq_along(steps)) {
  cat("\n\n==================== ", steps[i], " ====================\n")
  res <- tryCatch({
    source(steps[i], local = new.env(), echo = FALSE)
    "OK"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  setwd("C:/Users/sadaf/Documents/PPR3")
  log$status[i] <- ifelse(res == "OK", "OK", "ERROR")
  log$message[i] <- ifelse(res == "OK", "", res)
}
cat("\n\n==================== SUMMARY ====================\n")
print(log[, c("step", "status")], row.names = FALSE)
write.csv(log, "data/processed/pipeline_run_log.csv", row.names = FALSE)
cat("\nLog saved to data/processed/pipeline_run_log.csv\n")
