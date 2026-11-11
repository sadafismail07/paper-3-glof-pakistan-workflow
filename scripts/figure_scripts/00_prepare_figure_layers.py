# Map layers for the figures
# Run from the project root: python scripts/figure_scripts/00_prepare_figure_layers.py
# Requires: numpy, pandas, geopandas, rasterio, shapely, rdata

import os, glob, csv, shutil, tempfile
import numpy as np, pandas as pd, geopandas as gpd, rasterio
from rasterio.mask import mask
from rasterio.features import shapes
from rasterio.warp import calculate_default_transform, reproject, Resampling
from shapely.geometry import box, shape, Polygon, MultiPolygon
from shapely.ops import unary_union

PROC = os.path.join('data', 'processed'); RAW = os.path.join('data', 'raw')
OUT = os.path.join(PROC, 'figure_layers')
TMP = tempfile.mkdtemp(prefix='figlayers_')
MODELS = {'L27': os.path.join('hecras_models', 'L27_Shisper_v2'),
          'L29': os.path.join('hecras_models', 'L29_Passu_v2')}
WET = 0.05


def rd(path, **kw):
    if path.endswith('.gpkg'):
        path = shutil.copy(path, tempfile.mkdtemp())
    return gpd.read_file(path, **kw)


def save(g, sub, name):
    d = os.path.join(TMP, sub); os.makedirs(d, exist_ok=True)
    g.to_file(os.path.join(d, name), driver='GPKG')


# ---- regional maps (Figs. 1 and 3) ----
with rasterio.open(os.path.join(PROC, 'DEM_hillshade_WGS84.tif')) as r:
    ext = box(*r.bounds)
bb = ext.buffer(0.25).bounds

save(rd(os.path.join(PROC, 'lakes_vulnerable_GB_Chitral.gpkg')), 'maps', 'lakes.gpkg')
save(rd(os.path.join(PROC, 'study_area_districts.gpkg')).to_crs(4326), 'maps', 'gb_chitral.gpkg')
save(rd(os.path.join(RAW, 'pak_admin_boundaries', 'pak_admin0.shp')).to_crs(4326), 'maps', 'pak_L0.gpkg')


def gadm(path):
    import rdata
    x = rdata.conversion.convert(rdata.parser.parse_file(path))
    xy = np.asarray(x.coordinates); ix = np.asarray(x.index).astype(int)
    starts = list(ix[:, 3]) + [len(xy)]
    polys = {}
    for k, (g, p, hole, s) in enumerate(ix):
        ring = xy[starts[k]:starts[k + 1]]
        if len(ring) < 3: continue
        polys.setdefault((g, p), [None, []])
        if hole == 0: polys[(g, p)][0] = ring
        else: polys[(g, p)][1].append(ring)
    geoms = {}
    for (g, p), (shell, holes) in polys.items():
        if shell is not None:
            geoms.setdefault(g, []).append(Polygon(shell, holes).buffer(0))
    attr = x.attributes.reset_index(drop=True)
    return gpd.GeoDataFrame(attr, geometry=[unary_union(geoms[g]) for g in sorted(geoms)], crs=4326)


for iso in ['AFG', 'CHN']:
    save(gadm(os.path.join(RAW, 'gadm', f'gadm41_{iso}_0_pk.rds')), 'maps', f'{iso.lower()}_L0.gpkg')

rgi = glob.glob(os.path.join(RAW, 'RGI2000-v7.0-G-14_south_asia_west', '*.shp'))[0]
save(rd(rgi, bbox=bb).to_crs(4326), 'maps', 'glaciers_clipped.gpkg')

riv = rd(os.path.join(PROC, 'HydroRIVERS_extended_GB_Chitral.gpkg')).to_crs(4326)
save(riv[riv.intersects(box(*bb))], 'maps', 'rivers_clipped.gpkg')

MAJOR = ['trunk', 'trunk_link', 'primary', 'secondary', 'tertiary']
corr = rd(os.path.join(PROC, 'lake_corridors_scored.gpkg')).to_crs(4326).union_all()
roads = rd(os.path.join(PROC, 'OSM_roads_GB_Chitral.gpkg')).to_crs(4326)
save(roads[roads.highway.isin(MAJOR) & roads.intersects(corr)], 'maps', 'osm_roads.gpkg')
br = rd(os.path.join(PROC, 'OSM_bridges_GB_Chitral.gpkg')).to_crs(4326)
br = br[br.to_crs(32642).length < 1000]   # drops mis-tagged ways
save(br[br.highway.isin(MAJOR) & br.intersects(corr)], 'maps', 'osm_bridges.gpkg')
print('maps done')


# ---- HEC-RAS maps (Figs. 6, 7 and 9) ----
def rasters(lake, folder):
    t = sorted(glob.glob(os.path.join(MODELS[lake], folder, '*.tif')))
    return ([x for x in t if 'Depth' in os.path.basename(x)][0],
            [x for x in t if 'Velocity' in os.path.basename(x)][0])


def read(path):
    with rasterio.open(path) as r:
        a = r.read(1).astype('float64'); nd = r.nodata
        if nd is not None: a[a == nd] = np.nan
        return a, r.transform, r.crs


def to_wgs84(arr, T, crs, out, scale=1, nodata=np.nan, resampling=Resampling.nearest):
    h, w = arr.shape
    left, top = T * (0, 0); right, bottom = T * (w, h)
    dT, dw, dh = calculate_default_transform(crs, 'EPSG:4326', w, h, left, bottom, right, top)
    if scale > 1:
        dT = rasterio.Affine(dT.a / scale, dT.b, dT.c, dT.d, dT.e / scale, dT.f); dw *= scale; dh *= scale
    dst = np.full((dh, dw), nodata, dtype='float32')
    reproject(arr.astype('float32'), dst, src_transform=T, src_crs=crs, dst_transform=dT, dst_crs='EPSG:4326',
              resampling=resampling, src_nodata=nodata, dst_nodata=nodata)
    with rasterio.open(out, 'w', driver='GTiff', height=dh, width=dw, count=1, dtype='float32', crs='EPSG:4326',
                       transform=dT, nodata=nodata, compress='deflate') as o:
        o.write(dst, 1)


def hillshade(bounds, out):
    with rasterio.open(os.path.join(PROC, 'DEM_UTM42N.tif')) as r:
        win = rasterio.windows.from_bounds(*bounds, r.transform).round_offsets().round_lengths()
        z = r.read(1, window=win).astype('float64'); T = r.window_transform(win); crs = r.crs
    z = np.where(np.isfinite(z), z, np.nanmean(z))
    dy, dx = np.gradient(z, 30, 30)
    slope = np.pi / 2 - np.arctan(np.hypot(dx, dy)); aspect = np.arctan2(-dx, dy)
    az, alt = np.radians(315), np.radians(45)
    hs = np.sin(alt) * np.sin(slope) + np.cos(alt) * np.cos(slope) * np.cos(az - aspect)
    to_wgs84(np.clip(hs, 0, 1) * 255, T, crs, out, resampling=Resampling.bilinear)


osm_all = {'roads': rd(os.path.join(PROC, 'OSM_roads_GB_Chitral.gpkg')),
           'bridges': rd(os.path.join(PROC, 'OSM_bridges_GB_Chitral.gpkg'))}


def clip_osm(kind, bounds, out):
    poly = gpd.GeoSeries([box(*bounds)], crs=32642).to_crs(4326).iloc[0]
    g = osm_all[kind]; g[g.intersects(poly)].to_file(out, driver='GPKG')


for lake, sub in [('L29', 'fig6'), ('L27', 'fig7')]:
    od = os.path.join(TMP, sub); os.makedirs(od, exist_ok=True); allb = None
    for scen in ['Low', 'Mid', 'High']:
        d, T, crs = read(rasters(lake, f'{lake}_{scen}')[0]); d[d <= WET] = np.nan
        to_wgs84(d, T, crs, os.path.join(od, f'{lake}_{scen}_depth_wgs84.tif'))
        r_, c_ = np.where(np.isfinite(d))
        x0, y0 = T * (c_.min(), r_.max() + 1); x1, y1 = T * (c_.max() + 1, r_.min())
        allb = (x0, y0, x1, y1) if allb is None else (min(allb[0], x0), min(allb[1], y0), max(allb[2], x1), max(allb[3], y1))
    B = (allb[0] - 1500, allb[1] - 1500, allb[2] + 1500, allb[3] + 1500)
    hillshade(B, os.path.join(od, f'{lake}_hillshade_wgs84.tif'))
    for k in ['roads', 'bridges']: clip_osm(k, B, os.path.join(od, f'{lake}_{k}.gpkg'))
    print(sub, 'done')

od = os.path.join(TMP, 'fig9'); os.makedirs(od, exist_ok=True)
for lake in ['L29', 'L27']:
    dp, vp = rasters(lake, f'{lake}_Mid'); d, T, crs = read(dp); v, *_ = read(vp)
    d[d <= 0] = np.nan; wet = np.isfinite(d)
    h4 = wet & ((d > 1.5) | (d * np.nan_to_num(v) > 0.7))
    poly = unary_union([shape(g) for g, val in shapes(h4.astype('uint8'), mask=h4, transform=T) if val == 1])
    to_wgs84(np.where(h4, 1.0, np.nan), T, crs, os.path.join(od, f'{lake}_Mid_h4_wgs84_x4.tif'), scale=4)
    x0, y0, x1, y1 = poly.bounds; B = (x0 - 1500, y0 - 1500, x1 + 1500, y1 + 1500)
    for k in ['roads', 'bridges']: clip_osm(k, B, os.path.join(od, f'{lake}_{k}.gpkg'))
    if lake == 'L29': hillshade(B, os.path.join(od, f'{lake}_hillshade_wgs84.tif'))
    with rasterio.open(os.path.join(RAW, 'WorldPop_2020_GB_Chitral.tif')) as r:
        g = gpd.GeoSeries([box(*B)], crs=32642).to_crs(r.crs)
        a, tr = mask(r, list(g), crop=True, filled=True, nodata=0)
        to_wgs84(a[0].astype('float64'), tr, r.crs, os.path.join(od, f'{lake}_worldpop_wgs84.tif'), nodata=0)

ex = pd.read_csv(os.path.join(PROC, 'h4_exposure_summary.csv'))
with open(os.path.join(od, 'annotations.csv'), 'w', newline='') as f:
    w = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC)
    w.writerow(['lake', 'label', 'h4_km2', 'roads_km', 'bridges', 'built_m2', 'pop_wp', 'pop_ghs'])
    for lake, lab in [('L29', 'L29 Passu'), ('L27', 'L27 Shisper')]:
        e = ex[ex.scenario == f'{lake}_Mid'].iloc[0]
        w.writerow([lake, lab, e.h4_area_km2, e.roads_h4_km, int(e.bridges_h4), int(e.built_area_h4_m2),
                    int(e.pop_wp_h4), int(e.pop_ghs_h4)])
print('fig9 done')

for root, _, files in os.walk(TMP):
    for fn in files:
        dst = os.path.join(OUT, os.path.relpath(os.path.join(root, fn), TMP))
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copyfile(os.path.join(root, fn), dst)
print('Layers written to', OUT)
