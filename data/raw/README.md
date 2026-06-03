# Raw Data Sources

This folder contains raw input datasets. These files are **not tracked in git**
because of size constraints. Anyone reproducing this analysis must download
the source data using the URLs and access dates documented in
`docs/data_sources.csv`.

## Files expected in this folder

- `ndma_vulnerable_glof_sites.csv` — NDMA Vulnerable GLOF Sites of Pakistan
  (29 lakes, vulnerability rated 2-5). Institutional source; see docs/data_sources.csv for citation.

- `gadm41_PAK.gpkg` — GADM v4.1 Pakistan administrative boundaries.
  https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_PAK.gpkg

- `pak_admin_boundaries/` — OCHA Pakistan Common Operational Dataset v_01,
  Sep 2022. https://data.humdata.org/dataset/cod-ab-pak

- `RGI2000-v7.0-G-14_south_asia_west/` — Randolph Glacier Inventory v7.0,
  Region 14 (South Asia West). https://nsidc.org/data/nsidc-0770/versions/7
  (requires Earthdata login)

- `shrestha_2023_HMA_GLOF.csv` — Shrestha et al. 2023 HMA GLOF database.
  https://doi.org/10.5194/essd-15-3941-2023

See `docs/data_sources.csv` for full citation details.

