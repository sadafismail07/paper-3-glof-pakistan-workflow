# GLOF Pakistan Workflow

A reproducible, open-data workflow for GLOF (Glacial Lake Outburst Flood)
exposure assessment and downstream simulation in Gilgit-Baltistan and
Chitral districts of northern Pakistan.

## Status

Work in progress. Steps 0-2 complete (project setup, lake inventory
cleaning, study area definition, RGI glacier integration). Step 3 (DEM
acquisition) and beyond pending.

## Repository structure
data/
raw/         - Source data (not tracked in git; see data/raw/README.md)
processed/   - Outputs from each script step (tracked)
scripts/
analysis/    - R scripts, runnable in numerical order
gee/         - Google Earth Engine scripts
qgis/        - QGIS scripts and project files
docs/
data_sources.csv - Tracked dataset versions, URLs, citations
figures/
fig01_*.png  - Generated figures
hecras/        - HEC-RAS project files (added in later steps)
outputs/       - Final tables and deliverables

## How to reproduce

1. Clone this repo.
2. Set up R with packages: `sf`, `dplyr`, `readr`, `ggplot2`, `ggrepel`.
3. Download raw datasets per `data/raw/README.md` and place in `data/raw/`.
4. Run scripts in order: `scripts/analysis/00_setup_project.R`,
   then `01`, `02`, etc.
5. Each script is independent and can be re-run.

## Datasets used

See `docs/data_sources.csv` for full versioning, URLs, and citations.
Key inputs:

- NDMA Vulnerable GLOF Sites of Pakistan (29 lakes, vulnerability 2-5)
- OCHA Pakistan administrative boundaries (Common Operational Dataset
  v_01, Sep 2022)
- RGI 7.0 Region 14 (South Asia West) glacier outlines
- Shrestha et al. 2023 HMA GLOF database

## Working set

After filtering, 17 lakes at vulnerability 4 or 5 distributed across 7
districts:

- Chitral Upper: 5 lakes
- Hunza: 5 lakes
- Ghizer: 3 lakes
- Chitral Lower, Gilgit, Gupis-Yasin, Nagar: 1 lake each

## License

To be determined.

## Contact

Sadaf Ismail (`sadafismail07@gmail.com`)

