# 00_setup_project.R
# Create folder structure for the GLOF Pakistan workflow project.
# Run once at project initialization.

setwd("C:/Users/sadaf/Documents/PPR3")

folders <- c(
  "data/raw",
  "data/processed",
  "scripts/analysis",
  "scripts/gee",
  "scripts/qgis",
  "figures",
  "docs",
  "hecras",
  "outputs"
)

for (f in folders) {
  if (!dir.exists(f)) {
    dir.create(f, recursive = TRUE)
    cat("Created:", f, "\n")
  } else {
    cat("Already exists:", f, "\n")
  }
}

cat("\nFolder structure ready inside:", getwd(), "\n")

