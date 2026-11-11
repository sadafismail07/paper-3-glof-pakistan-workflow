# Exposure per corridor

import os, tempfile, shutil, numpy as np, geopandas as gpd, rasterio
from rasterio.mask import mask
P = 'data/processed/'; RAW = 'data/raw/'
C = gpd.read_file(P + 'lake_downstream_corridors.gpkg').to_crs(32642)
def rsum(path, geoms):
    with rasterio.open(path) as r:
        out = []
        for g in geoms.to_crs(r.crs):
            a, _ = mask(r, [g], crop=True, filled=False, all_touched=False)
            v = a[0].astype('float64').filled(np.nan)
            if r.nodata is not None and not np.isnan(r.nodata): v[v == r.nodata] = np.nan
            out.append(float(np.nansum(v)))
        return out
C['pop_ghs2025'] = rsum(RAW + 'GHS_POP_2025_GB_Chitral.tif', C.geometry)
C['pop_worldpop2020'] = rsum(RAW + 'WorldPop_2020_GB_Chitral.tif', C.geometry)
bb = tuple(C.to_crs(4326).total_bounds)
lay = {k: gpd.read_file(P + f'OSM_{k}_GB_Chitral.gpkg', bbox=bb).to_crs(32642) for k in ['roads', 'buildings', 'bridges', 'power']}
C['road_length_km'] = [lay['roads'][lay['roads'].intersects(g)].geometry.intersection(g).length.sum() / 1000 for g in C.geometry]
C['building_count'] = [int(lay['buildings'].intersects(g).sum()) for g in C.geometry]
C['bridge_count'] = [int(lay['bridges'].intersects(g).sum()) for g in C.geometry]
C['power_features_count'] = [int(lay['power'].intersects(g).sum()) for g in C.geometry]
C['built_area_m2'] = rsum(RAW + 'GHS_BUILT_S_2030_GB_Chitral.tif', C.geometry)
C['pop_mean'] = (C.pop_ghs2025 + C.pop_worldpop2020) / 2
C['n_segments'] = np.nan
C = C[['lake_id', 'n_segments', 'distance_km', 'pop_ghs2025', 'pop_worldpop2020', 'road_length_km', 'building_count',
       'bridge_count', 'power_features_count', 'built_area_m2', 'pop_mean', 'geometry']]
tmp = os.path.join(tempfile.mkdtemp(), 'e.gpkg'); C.to_file(tmp, driver='GPKG')
shutil.copyfile(tmp, P + 'lake_corridors_with_exposure.gpkg')
print(C.drop(columns='geometry').round(1).to_string())
