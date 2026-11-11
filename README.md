# GLOF Pakistan Workflow

Scripts, HEC-RAS 2D model inputs and figures for a glacial lake outburst
flood (GLOF) study in Gilgit-Baltistan and Chitral, northern Pakistan.

## Contents

- `scripts/` - R, Python and Google Earth Engine scripts
- `hecras_models/` - HEC-RAS project files (geometry, plans, unsteady flow) and input shapefiles
- `figures/` - figures

HEC-RAS results, terrain rasters and input datasets are not included.

## Figures

Each figure has its own script in `scripts/figure_scripts/` (`fig1_*` to `fig9_*`,
`figS1_*` to `figS3_*`). Run from the project root, for example
`Rscript scripts/figure_scripts/fig3_corridors.R`. Output goes to `figures/`.
The map figures use layers built by `00_prepare_figure_layers.py`, which the
R scripts run automatically if the layers are missing.

## Software

R 4.5, Python 3 (numpy, pandas, geopandas, rasterio, shapely, matplotlib, rdata),
QGIS 3.34, HEC-RAS 7.0, Google Earth Engine.

## License

MIT (see `LICENSE`).

## Contact

Sadaf Ismail - sadafismail07@gmail.com
