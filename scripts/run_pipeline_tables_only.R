# Run extraction and tables

setwd("C:/Users/sadaf/Documents/PPR3")
steps <- c(
  "scripts/analysis/20_simulation_results.R",
  "scripts/analysis/23_table03_data_extraction.R",
  "scripts/tables/table02_priority_ranking.R",
  "scripts/tables/table03_simulation_results.R",
  "scripts/tables/table04_h4_exposure.R",
  "scripts/tables/table05_validation_envelopes.R",
  "scripts/tables/tableS1_corridor_overlap.R",
  "scripts/tables/tableS2_ranking_stability.R",
  "scripts/tables/tableS3_hazard_classes.R",
  "scripts/tables/tableS5_corridor_exposure.R",
  "scripts/tables/tableS6_downstream_attenuation.R",
  "scripts/tables/tableS7_weighting_sensitivity.R"
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
print(log, row.names = FALSE, right = FALSE)
write.csv(log, "data/processed/pipeline_run_log_tables.csv", row.names = FALSE)
