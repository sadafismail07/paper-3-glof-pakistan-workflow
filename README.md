# GLOF Pakistan Workflow

A reproducible, open-data workflow for glacial lake outburst flood (GLOF)
prioritisation and hydrodynamic simulation in the Gilgit-Baltistan and
Chitral region of northern Pakistan. This repository accompanies the
manuscript:

> Ismail, S. and Yamashiki, Y. (2026). A Reproducible Open-Data Workflow for
> Glacial Lake Outburst Flood Prioritization and Hydrodynamic Simulation
> in Northern Pakistan. [Journal, volume, pages, DOI at publication.]

## Overview

The workflow takes a national inventory of vulnerable glacial lakes as
input, performs systematic exposure prioritisation across a multi-lake
working set, selects high-priority candidates for detailed simulation,
and runs scenario-based HEC-RAS 2D hydrodynamic modelling with H4 hazard
zone delineation. All inputs are open-access satellite, demographic, and
topographic datasets; all analytical steps are scripted in R or
JavaScript and documented for replication.

## Status

Manuscript drafted; figures and tables generated through Figure 8.
Figures 9-11 and final submission deposit pending.

## Repository structure
data/
raw/                # source data (gitignored; see data/raw/README.md)
processed/          # outputs from each pipeline stage (tracked)
scripts/
gee/                # Google Earth Engine JavaScript scripts
analysis/           # R analytical scripts (run in numerical order)
figure_scripts/     # R scripts that render manuscript figures
hecras_models/
L29_Passu/          # HEC-RAS project for L29 Passu
L27_Shisper/        # HEC-RAS project for L27 Shisper
figures/              # final figures and tables
docs/
data_sources.csv    # dataset versioning, URLs, citations
PAPER_NOTES.md      # verified bibliography and methodological notes
PROJECT_NOTES.md    # operational decisions log
SELECTION_CRITERIA.md  # pre-registered lake selection criteria
sessionInfo.txt     # R session info at the time of analysis
REPRODUCE.md          # step-by-step replication guide
LICENSE               # MIT License

## How to reproduce

See `REPRODUCE.md` for the complete step-by-step replication guide,
including software requirements, data acquisition, pipeline order, and
expected outputs.

## Datasets used

See `docs/data_sources.csv` for full versioning, URLs, and citations.
Principal inputs:

- Federal Flood Commission inventory of vulnerable glacial lakes (29
  lakes nationwide; 17-lake working set after vulnerability filtering)
- OCHA Pakistan administrative boundaries (WFP SDI Common Operational
  Dataset v_01, 2022)
- RGI 7.0 glacier outlines (Central Asia subset)
- HydroSHEDS HydroBASINS Levels 8 and 9, HydroRIVERS v1.0
- Copernicus GLO-30 Digital Elevation Model
- Sentinel-2 Level-2A surface reflectance imagery
- GHS-POP R2023A epoch 2025 population grid
- WorldPop Global 100 m Constrained 2020 (Pakistan)
- GHS-BUILT-S R2023A epoch 2030 built-up surface grid
- OpenStreetMap Pakistan extract (Geofabrik snapshot, 29 April 2026)

## Working set

After filtering the Federal Flood Commission inventory to NDMA
vulnerability rating >= 4 and to lakes within Gilgit-Baltistan or
Chitral, 17 lakes constitute the working set. Two top-ranked lakes were
selected for detailed hydrodynamic simulation:

- **L29 Passu** - moraine-dammed proglacial lake, Hunza Valley
- **L27 Shisper** - ice-dammed surge-driven lake, Hassanabad Nullah

Full working set documented in `figures/table01_working_set_lakes.csv`.

## License

This project is released under the MIT License. See `LICENSE` for
details.

## Citation

If you use this workflow or any part of it, please cite the manuscript
(reference above) and the archived Zenodo snapshot:

> [DOI to be inserted at submission]

## Contact

Sadaf Ismail - sadafismail07@gmail.com

