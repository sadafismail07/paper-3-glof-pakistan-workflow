# REPRODUCE.md - Step-by-Step Replication Guide

This document walks through reproducing the analysis presented in the
manuscript:

> Ismail, S. and Yamashiki, Y. (2026). GLOF prioritisation and hydrodynamic
> simulation in Gilgit-Baltistan and Chitral, northern Pakistan: a
> reproducible open-data workflow. [Journal, volume, pages, DOI at
> publication.]

This is the revised (post-peer-review) version of the pipeline. It
reflects the composite hazard scoring and additional validation steps
introduced during revision — see `docs/SELECTION_CRITERIA.md` for the
dated amendment record.

The full pipeline runs from raw data acquisition through to the figures
and tables in the manuscript. Most steps are scripted; two require
manual operations in QGIS or the HEC-RAS desktop GUI.

---

## Software requirements

- R 4.5.0 with the packages used by the scripts in `scripts/`
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
    tables/             # R scripts that render Table 1-5 and S1-S7 CSVs
    figure_scripts/     # R scripts that render Figures 1-11
  hecras_models/
    L29_Passu/          # HEC-RAS project files for L29 Passu
    L27_Shisper/        # HEC-RAS project files for L27 Shisper
  figures/              # final figures (PNG/TIFF) and tables (CSV)
  docs/                 # documentation and citation/provenance files
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
| District population totals for corridor cross-check (Chitral: 2023 census; Gilgit-Baltistan: 2017 P&DD estimate via citypopulation.de -- two different sources, see docs/data_sources.csv) | see docs/data_sources.csv | used for corridor_census_check.csv |

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
9. `scripts/analysis/08_figure01_study_area.R` - render Figure 1.

### Stage 4 - DEM and lake delineation

10. `scripts/gee/01_export_dem.js` - run in Google Earth Engine Code
    Editor to export the Copernicus GLO-30 DEM clipped to the study
    area. Download from Google Drive to `data/raw/`.
11. `scripts/analysis/09_process_dem.R` - reproject DEM to UTM Zone 42N
    and clip to the working area.
12. `scripts/gee/02_water_mask.js` - run in GEE to produce regional NDWI
    water mask from Sentinel-2 imagery.
13. **Manual step in QGIS**: load the NDWI mask and digitise lake
    polygons for visible lakes (L01, L02, L04, L15, L29). Save as
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
    `data/processed/lake_corridors_scored.gpkg` and
    `data/processed/lake_priority_scores.csv`. As of the post-peer-review
    revision, the hazard composite is the mean of three physical
    indicators only (NDMA vulnerability rating, lake surface area, RGI
    surge-type), with surge-type credit restricted to lakes that are
    themselves ice-dammed — see the 2026-08-28 amendment in
    `docs/SELECTION_CRITERIA.md` for the full rationale.
20. `scripts/analysis/14b_scoring_diagnostics.R` - sensitivity
    diagnostics for the composite score (leave-one-out indicator
    removal, baseline/no-tier/no-surge ranking comparisons); produces
    `data/processed/TableS2_ranking_stability.csv`, read by
    `scripts/tables/tableS2_ranking_stability.R` (Table S2). Table S1
    (corridor overlap) is computed independently inside
    `scripts/tables/tableS1_corridor_overlap.R` and does not depend on
    this script.
21. `scripts/analysis/25_composite_weighting_sensitivity.R` - test
    alternative indicator weighting schemes against the baseline
    ranking; produces `data/processed/composite_weighting_sensitivity.csv`
    (Table S7).

### Stage 7 - Hydrodynamic simulation

22. `scripts/analysis/15_volume_dam_breach_calculations.R` - compute
    volume scenarios (low/mid/high) and Froehlich breach parameters for
    L29 Passu and L27 Shisper.
23. `scripts/analysis/16_prepare_hecras_terrain.R` - prepare DEM and
    auxiliary geometry inputs for HEC-RAS.
24. `scripts/analysis/17_breach_hydrograph_generation.R` - generate
    upstream breach hydrographs for all 14 lake-scenario combinations
    under the full-drainage assumption.
25. **Manual step in HEC-RAS**: open the project file in
    `hecras_models/L29_Passu/` and run all simulation plans. Repeat
    for `hecras_models/L27_Shisper/`. Export max-depth and max-velocity
    rasters to `data/processed/`.
26. `scripts/analysis/18_velocity_distribution_diagnostic.R` -
    diagnostic on velocity distributions for quality control.
27. `scripts/analysis/24_crs_clip_validation.R` - diagnostic check
    confirming the H4 hazard raster and the OSM building layer share a
    coordinate reference system and that the Hassanabad corridor clip
    returns a non-empty intersection (run in response to Reviewer 3's
    challenge to the L27 near-zero building count, R3.4); produces
    `data/processed/crs_clip_validation.csv`. Not itself a numbered
    manuscript table — see `docs/data_sources.csv` for detail.
28. `scripts/analysis/20_simulation_results.R` - consolidate per-scenario
    HEC-RAS results into `data/processed/ALL_simulation_results_VERIFIED.csv`
    and the per-lake `*_simulation_results_VERIFIED.csv` files (Table 3).
29. `scripts/analysis/21_h4_exposure_and_volume_conservation_audit.R` -
    audits per-scenario volume-conservation error read directly from
    each HEC-RAS plan's HDF5 file (`data/processed/volume_conservation_FINAL.csv`,
    R3.33); extracts H4 exposure for the four n=0.10 sediment-bulked
    scenarios that had not yet been extracted
    (`data/processed/h4_exposure_n010_scenarios.csv`); extracts routed
    discharge at downstream reference sections
    (`data/processed/L27_routed_hydrographs.csv`,
    `data/processed/routed_hydrograph_summary.csv`, R3, Table S6); and
    re-simulates L27 Shisper under field-observed partial-drainage
    fractions from Muhammad et al. (2021) instead of full drainage
    (`data/processed/L27_hyd_partial28.csv`,
    `data/processed/L27_hyd_partial45.csv`).

### Stage 8 - Hazard zones and refined exposure

30. `scripts/analysis/19_h4_hazard_mapping.R` - delineate H4 hazard
    zones using the Westoby (2015) threshold (depth > 1.5 m or
    depth-velocity product > 0.7 m^2/s); compute per-zone exposure
    indicators; produce `data/processed/h4_exposure_summary.csv`.
31. `scripts/analysis/22_identification_distances.R` - cross-checks
    each of the 29 NDMA inventory coordinates against the nearest
    independently published location for a glacier/lake of the same
    name, to catch lake-identification errors systematically (R1.02);
    produces `data/processed/identification_distances.csv` (Table S4).
    Flagged discrepancies in 5 of 29 sites, including the L15
    "Khurdupin" entry ~208 km from the real Khurdopin Glacier — see the
    2026-04-29 amendment in `docs/SELECTION_CRITERIA.md`.
32. `scripts/analysis/23_table03_data_extraction.R` - re-extracts the
    raw values behind Table 3 (peak discharge and volume from the raw
    breach hydrograph time series; depth/velocity/wetted-area via
    raster extraction) after two earlier raster-extraction approaches
    were found to give incorrect values for 16 of 20 scenarios.
    `data/processed/table03_master_data.csv` is now the sole canonical
    source `scripts/tables/table03_simulation_results.R` reads;
    `data/processed/table3_extracted.csv` is kept as the independent
    audit-trail reference used to confirm the fix.

### Stage 9 - Figures and tables

33. Run scripts in `scripts/figure_scripts/` in numerical order to
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
34. Run scripts in `scripts/tables/` to regenerate Tables 1-5 (main
    text) and Tables S1-S7 (supplementary), each written as a single
    CSV to `figures/`:
    - `table01_working_set_lakes.R` -> Table 1
    - `table02_priority_ranking.R` -> Table 2
    - `table03_simulation_results.R` -> Table 3
    - `table04_h4_exposure.R` -> Table 4
    - `table05_validation_envelopes.R` -> Table 5
    - `tableS1_corridor_overlap.R` -> Table S1
    - `tableS2_ranking_stability.R` -> Table S2
    - `tableS3_hazard_classes.R` -> Table S3
    - `tableS4_lake_identification.R` -> Table S4
    - `tableS5_corridor_exposure.R` -> Table S5
    - `tableS6_downstream_attenuation.R` -> Table S6
    - `tableS7_weighting_sensitivity.R` -> Table S7

---

## Manual steps

Two stages require human judgement that is not automated:

- **Stage 4, Step 13** - manual lake digitisation in QGIS for the five
  lakes with visible water surfaces. The Sentinel-2 NDWI water mask
  produced by `scripts/gee/02_water_mask.js` is the starting point;
  final polygons are drawn by hand at scales between 1:2,000 and
  1:10,000 to capture the lake shape accurately. See Section 3.4 of the
  manuscript for the full protocol.
- **Stage 7, Step 25** - HEC-RAS plan execution from the HEC-RAS desktop
  GUI. Project files contain all geometry, mesh, boundary conditions,
  and unsteady-flow data; the simulation plans are pre-configured and
  can be batch-run from the HEC-RAS Run menu.

---

## Expected outputs

After running the full pipeline, the following are regenerated:

- Figures 1-11 as `figures/fig01_*.png`/`.tiff` through `figures/fig11_*.png`/`.tiff`
- Tables 1-5 (main text) as `figures/table0[1-5]_*.csv`
- Tables S1-S7 (supplementary) as `figures/tableS[1-7]_*.csv`
- All intermediate and audit datasets in `data/processed/`

---

## Citation

If you use this workflow or any part of it, please cite:

> Ismail, S. and Yamashiki, Y. (2026). GLOF prioritisation and hydrodynamic
> simulation in Gilgit-Baltistan and Chitral, northern Pakistan: a
> reproducible open-data workflow. [Journal, volume, pages, DOI at
> publication.]

A snapshot of this repository is archived on Zenodo:
Ismail, S. and Yamashiki, Y. (2026). GLOF prioritisation and hydrodynamic simulation in Gilgit-Baltistan and Chitral, northern Pakistan: a reproducible open-data workflow (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.20534604

> **Note:** the `v1.0.0` Zenodo snapshot was cut before this revision's
> changes (composite scoring amendment, additional validation scripts
> and tables) were pushed. If you need the archived version to match the
> current `main` branch exactly, check whether a newer Zenodo version has
> been minted, or contact the corresponding author.

---

## Contact

Questions about the workflow or its application to other regions:
Sadaf Ismail, sadafismail07@gmail.com
