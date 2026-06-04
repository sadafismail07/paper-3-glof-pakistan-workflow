# REPRODUCE.md - Step-by-Step Replication Guide

This document walks through reproducing the analysis presented in the
manuscript:

> Ismail, S. and Yamashiki, Y. (2026). GLOF prioritisation and hydrodynamic
> simulation in Gilgit-Baltistan and Chitral, northern Pakistan: a
> reproducible open-data workflow. [Journal name to be inserted at submission.]

The full pipeline runs from raw data acquisition through to the figures
and tables in the manuscript. Most steps are scripted; two require
manual operations in QGIS or the HEC-RAS desktop GUI.

---

## Software requirements

- R 4.5.0 with the packages listed in `docs/sessionInfo.txt`
- QGIS 3.34.1 with the QuickMapServices plugin
- HEC-RAS 7.0 (April 2026 release), free from
  https://www.hec.usace.army.mil/software/hec-ras/
- Google Earth Engine account (free, at https://earthengine.google.com)
- Git (any recent version)

---

## Repository structure

```
PPR3/
  data/
    raw/                # original inputs (gitignored; see data/raw/README.md)
    processed/          # intermediate and final processed datasets
  scripts/
    gee/                # JavaScript scripts for Google Earth Engine
    analysis/           # R analytical scripts (run in numerical order)
    figure_scripts/     # R scripts that render figures
  hecras_models/
    L29_Passu/          # HEC-RAS project files for L29 Passu (inputs only)
    L27_Shisper/        # HEC-RAS project files for L27 Shisper (inputs only)
  figures/              # final figures (PNG) and tables (CSV, MD, TEX)
  docs/                 # documentation, citation files, session info
```

---

## Data sources (manual download required)

These inputs must be downloaded separately and placed in `data/raw/`:

| Dataset | Source | Filename expected |
|---|---|---|
| Federal Flood Commission inventory | Institutional source (see docs/data_sources.csv) | ndma_vulnerable_glof_sites.csv |
| Pakistan administrative boundaries | WFP SDI / UN OCHA | pak_admin2.shp and associated files |
| RGI 7.0 glacier outlines | https://rgitools.org | RGI 7.0 Central Asia subset |
| HydroBASINS Levels 8 and 9 | https://www.hydrosheds.org | Asia region |
| HydroRIVERS v1.0 | https://www.hydrosheds.org | Asia region |
| OpenStreetMap Pakistan | https://download.geofabrik.de | pakistan-latest.osm.pbf |

The Copernicus DEM, Sentinel-2 imagery, GHS-POP, WorldPop, and GHS-BUILT-S
are accessed via Google Earth Engine through the scripts in `scripts/gee/`
and do not need to be manually downloaded to local storage in advance.

See `docs/data_sources.csv` for full versioning, URLs, and citations.

---

## Pipeline

### Stage 1 - Project setup and inventory

1. `scripts/analysis/00_setup_project.R` - create folder structure and
   confirm working directory.
2. `scripts/analysis/01_clean_inventory.R` - clean and filter the Federal
   Flood Commission inventory; produce
   `data/processed/lakes_vulnerable_GB_Chitral.gpkg` (29 lakes).
3. `scripts/analysis/02_define_study_area.R` - load OCHA admin
   boundaries; spatially restrict to Gilgit-Baltistan plus Chitral.
4. `scripts/analysis/03_assign_lakes_to_districts.R` - join lake points
   to districts; filter working set to 17 lakes at vulnerability >= 4.

### Stage 2 - Glacier integration

5. `scripts/analysis/04_clip_rgi_glaciers.R` - clip RGI 7.0 glacier
   outlines to the study area.
6. `scripts/analysis/05_match_lakes_to_glaciers.R` - associate each
   lake with the source glacier and extract surge-type classifications.
7. `scripts/analysis/06_quick_map.R` - quick visual verification map.

### Stage 3 - Hydrography

8. `scripts/analysis/07_clip_hydrosheds.R` - clip HydroBASINS Levels 8
   and 9 and HydroRIVERS to the study area.
9. `scripts/analysis/08_figure01_study_area.R` - prepare Figure 1 layers.

### Stage 4 - DEM and lake delineation

10. `scripts/gee/01_export_dem.js` - run in Google Earth Engine Code
    Editor to export the Copernicus GLO-30 DEM clipped to the study
    area. Download from Google Drive to `data/raw/`.
11. `scripts/analysis/09_process_dem.R` - reproject DEM to UTM Zone 42N
    and clip to the working area.
12. `scripts/gee/02_water_mask.js` - run in GEE to produce regional NDWI
    water mask from Sentinel-2 imagery.
13. **Manual step in QGIS**: load the NDWI mask and digitise lake
    polygons for visible lakes. Save as
    `data/processed/lakes_polygons_verified.gpkg`. See Section 3.4 of
    the manuscript for the digitisation protocol.

### Stage 5 - Corridor delineation

14. `scripts/analysis/10_lake_pour_points.R` - identify the downstream
    pour point for each lake from the DEM.
15. `scripts/analysis/11_extend_river_network.R` - extend HydroRIVERS
    upstream into headwaters where needed.
16. `scripts/analysis/12_downstream_corridors.R` - delineate 17
    downstream corridors via HydroBASINS NEXT_DOWN routing, with a 50 km
    cut-off and 1 km lateral buffer.

### Stage 6 - Exposure quantification and priority scoring

17. `scripts/gee/03_population_export.js` and
    `scripts/gee/04_built_export.js` - run in GEE to export GHS-POP
    (R2023A epoch 2025), WorldPop (Global 100 m Constrained 2020), and
    GHS-BUILT-S (R2023A epoch 2030) clipped to the study area.
18. `scripts/analysis/13_extract_population.R` - extract per-corridor
    population, building, road, bridge, power-feature, and built-up
    area exposure indicators from the OSM and GEE-derived inputs.
19. `scripts/analysis/14_priority_scoring.R` - apply the composite
    hazard-exposure score described in Section 3.6; produce
    `data/processed/lake_corridors_scored.gpkg` and select the top-2
    lakes for detailed simulation.

### Stage 7 - Hydrodynamic simulation

20. `scripts/analysis/15_volume_dam_breach_calculations.R` - compute
    volume scenarios (low/mid/high) and Froehlich breach parameters for
    L29 Passu and L27 Shisper.
21. `scripts/analysis/16_prepare_hecras_terrain.R` - prepare DEM and
    auxiliary geometry inputs for HEC-RAS.
22. `scripts/analysis/17_breach_hydrograph_generation.R` - generate
    upstream breach hydrographs for all 14 lake-scenario combinations.
23. **Manual step in HEC-RAS**: open the project file in
    `hecras_models/L29_Passu/` and run all simulation plans. Repeat
    for `hecras_models/L27_Shisper/`. Export max-depth and max-velocity
    rasters to `data/processed/`.
24. `scripts/analysis/18_velocity_distribution_diagnostic.R` -
    diagnostic on velocity distributions for quality control.
25. `scripts/analysis/20_simulation_results.R` - compile the 14-run
    simulation results matrix (peak depth, velocity, wetted area) used
    in Table 3 and the sensitivity figures.

### Stage 8 - Hazard zones and refined exposure

26. `scripts/analysis/19_h4_hazard_mapping.R` - delineate H4 hazard
    zones using the Westoby (2015) threshold (depth > 1.5 m or
    depth-velocity product > 0.7 m^2/s); compute per-zone exposure
    indicators; produce `data/processed/h4_exposure_summary.csv`.

### Stage 9 - Figures and tables

27. Run scripts in `scripts/figure_scripts/` in numerical order to
    regenerate Figures 1-11:
    - `01_fig01_study_area.R`
    - `02_fig02_workflow.R`
    - `03_fig03_ndwi_sensitivity.R`
    - `04_fig04_priority_scoring.R`
    - `05_fig05_corridors.R`
    - `06_fig06_inundation_L29.R`
    - `07_fig07_inundation_L27.R`
    - `08_fig08_tornado.R`
    - `09_fig09_hydrographs.R`
    - `10_fig10_h4_hazard.R`
    - `11_fig11_validation.R`
28. Tables 1-5 are generated by the table scripts in
    `scripts/analysis/` (see Section 3 of the manuscript); outputs are
    saved to `figures/` as CSV, Markdown, and LaTeX.

---

## Manual steps

Two stages require human judgement that is not automated:

- **Stage 4, Step 13** - manual lake digitisation in QGIS for the lakes
  with visible water surfaces. The Sentinel-2 NDWI water mask produced
  by `scripts/gee/02_water_mask.js` is the starting point; final
  polygons are drawn by hand from Sentinel-2 and ESRI World Imagery at
  scales appropriate to each lake (typically 1:2,000 to 1:10,000; finer
  for very small features, e.g. ~1:900 for L01 Reshun). See Section 3.4
  of the manuscript for the full protocol.
- **Stage 7, Step 23** - HEC-RAS plan execution from the HEC-RAS desktop
  GUI. Project files contain all geometry, mesh, boundary conditions,
  and unsteady-flow data; the simulation plans are pre-configured and
  can be batch-run from the HEC-RAS Run menu.

---

## Expected outputs

After running the full pipeline, the following are regenerated:

- Figures 1-11 as `figures/fig01_*.png` through `figures/fig11_*.png`
- Tables 1-5 as `figures/table0[1-5]_*.{csv,md,tex}`
- All intermediate datasets in `data/processed/`

---

## Citation

If you use this workflow or any part of it, please cite:

> Ismail, S. and Yamashiki, Y. (2026). GLOF prioritisation and hydrodynamic
> simulation in Gilgit-Baltistan and Chitral, northern Pakistan: a
> reproducible open-data workflow. [Journal, volume, pages, DOI at publication.]

A snapshot of this repository is archived on Zenodo at
[DOI to be inserted at submission].

---

## Contact

Questions about the workflow or its application to other regions:
Sadaf Ismail, sadafismail07@gmail.com
