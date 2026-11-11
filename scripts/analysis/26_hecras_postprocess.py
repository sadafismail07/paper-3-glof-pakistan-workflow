# Post-process HEC-RAS results

import os, glob, re, csv
import numpy as np, h5py, rasterio, geopandas as gpd
from rasterio.features import shapes
from rasterio.mask import mask
from rasterio.warp import calculate_default_transform, reproject, Resampling
from shapely.geometry import shape, box
from shapely.ops import unary_union

import shutil, tempfile
ROOT = os.getcwd(); PROC = os.path.join(ROOT, 'data', 'processed'); RAW = os.path.join(ROOT, 'data', 'raw')
P_OUT = tempfile.mkdtemp(prefix='hecras_pp_')
P = PROC  # inputs
MODELS = {'L27': os.path.join(ROOT, 'hecras_models', 'L27_Shisper_v2'),
          'L29': os.path.join(ROOT, 'hecras_models', 'L29_Passu_v2')}
WET = 0.05

SCEN = {
 'L29': [('Low','L29_Low',.05,1),('Mid_baseline','L29_Mid',.05,1),('High','L29_High',.05,1),
         ('Mid_n04','L29_Mid_n04',.04,1),('Mid_n06','L29_Mid_n06',.06,1),
         ('Mid_bulk15','L29_Mid_b15',.05,1.5),('Mid_bulk20','L29_Mid_b20',.05,2)],
 'L27': [('Low','L27_Low',.05,1),('Mid_baseline','L27_Mid',.05,1),('High','L27_High',.05,1),
         ('Mid_n04','L27_Mid_n04',.04,1),('Mid_n06','L27_Mid_n06',.06,1),
         ('Mid_bulk15','L27_Mid_bulk1.5',.05,1.5),('Mid_bulk20','L27_Mid_bulk2.0',.05,2)]}
HB = {'L27': 134, 'L29': 37.5}

def plan_hdf(lake, short_id):
    for p in glob.glob(os.path.join(MODELS[lake], '*.p[0-9][0-9]')):
        t = open(p, errors='ignore').read()
        m = re.search(r'^Short Identifier=(.*)$', t, re.M)
        if m and m.group(1).strip() == short_id and os.path.exists(p + '.hdf'):
            return p + '.hdf'
    raise FileNotFoundError(short_id)

def rasters(lake, folder):
    d = os.path.join(MODELS[lake], folder); tifs = sorted(glob.glob(os.path.join(d, '*.tif')))
    return ([t for t in tifs if 'Depth' in os.path.basename(t)][0], [t for t in tifs if 'Velocity' in os.path.basename(t)][0])

def read(path):
    with rasterio.open(path) as r:
        a = r.read(1).astype('float64'); nd = r.nodata
        if nd is not None: a[a == nd] = np.nan
        return a, r.transform, r.crs, r.profile

def hdf_info(path):
    h = h5py.File(path, 'r'); ve = {}
    def f(n, o):
        if 'Error Percent' in o.attrs and not ve: ve.update(err=float(o.attrs['Error']), pct=float(o.attrs['Error Percent']))
    h['Results/Unsteady/Summary'].visititems(f)
    base = 'Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series'
    t = h[base + '/Time'][:] * 24
    bc = h[base + '/2D Flow Areas/Perimeter 1/Boundary Conditions']
    qin = bc[[k for k in bc if k.lower() == 'breach_inflow'][0]][:, 1]
    rl = h[base + '/2D Flow Areas/Perimeter 1/Reference Lines']
    names = [n.decode().split('|')[0].strip() for n in rl['Name'][:]]
    q = np.abs(rl['Flow'][:])
    peaks = {n: (float(q[:, j].max()), float(t[np.argmax(q[:, j])]) if q[:, j].max() > 0 else np.nan) for j, n in enumerate(names)}
    return ve, float(qin.max()), peaks

def h4_poly(lake, folder):
    dp, vp = rasters(lake, folder); d, T, crs, prof = read(dp); v, *_ = read(vp)
    d[d <= 0] = np.nan; wet = np.isfinite(d)
    h4 = wet & ((d > 1.5) | (d * np.nan_to_num(v) > 0.7))
    geoms = [shape(g) for g, val in shapes(h4.astype('uint8'), mask=h4, transform=T) if val == 1]
    return unary_union(geoms), h4, d, v, T, crs, prof

rows = []
for lake in ['L29', 'L27']:
    for scen, folder, n, bulk in SCEN[lake]:
        dp, vp = rasters(lake, folder); d, *_ = read(dp); v, *_ = read(vp)
        wet = np.isfinite(d) & (d > WET); px = 30 * 30
        vw = v[wet & np.isfinite(v) & (v > 0)]
        ve, qin, _ = hdf_info(plan_hdf(lake, folder))
        rows.append(dict(lake=lake, scenario=scen, manning_n=n, bulking_factor=bulk, Hb_m=HB[lake],
                         peak_Q_input_m3s=round(qin), peak_depth_m=round(np.nanmax(d[wet]), 2),
                         wetted_area_km2=round(wet.sum() * px / 1e6, 3),
                         peak_velocity_p95=round(np.percentile(vw, 95), 2), peak_velocity_max=round(vw.max(), 2),
                         vol_error_1000m3=round(ve['err'], 4), vol_error_pct=round(ve['pct'], 5)))
old = {}
src = os.path.join(P, '_archive_pre_submission', 'all_simulations_combined_v2.csv')
if os.path.exists(src):
    for r in csv.DictReader(open(src)): old[(r['lake'], r['scenario'])] = r['Vw_m3']
for r in rows: r['Vw_m3'] = old.get((r['lake'], r['scenario']), '')
cols = ['lake','scenario','Vw_m3','Hb_m','manning_n','bulking_factor','peak_Q_input_m3s','peak_depth_m','wetted_area_km2',
        'peak_velocity_p95','peak_velocity_max','vol_error_1000m3','vol_error_pct']
with open(os.path.join(P_OUT, 'all_simulations_combined_v2.csv'), 'w', newline='') as f:
    w = csv.DictWriter(f, cols); w.writeheader(); [w.writerow({k: r[k] for k in cols}) for r in rows]
print('wrote all_simulations_combined_v2.csv')

SECT = {'L27': ('L27_Mid_24h', 'L27 Shisper', [('RL_KKH_Hassanabad', 'Karakoram Highway bridge, Hassanabad (~6.7 km)'),
                                              ('RL_confluence', 'Hunza confluence (~8.9 km)'),
                                              ('RL_outlet', 'Domain outlet (~62 km)')]),
        'L29': ('L29_Mid_24h', 'L29 Passu', [('RL_confluence', 'Hunza confluence (~5.7 km)'),
                                            ('RL_upper', 'Hunza, below Passu (~8.0 km)'),
                                            ('RL_arrest', 'Inundation limit (~14.4 km)'),
                                            ('RL_outlet', 'Domain outlet (~58 km)')])}
with open(os.path.join(P_OUT, 'routed_hydrograph_summary.csv'), 'w', newline='') as f:
    w = csv.writer(f); w.writerow(['lake', 'section', 'peak_q', 't_peak_h', 'breach_q', 'pct_of_breach'])
    for lake, (folder, lab, secs) in SECT.items():
        ve, qin, pk = hdf_info(plan_hdf(lake, folder))
        for rl, s in secs:
            q, t = pk[rl]
            w.writerow([lab, s, round(q, 1), '' if np.isnan(t) else round(t, 2), round(qin), round(100 * q / qin, 1)])
print('wrote routed_hydrograph_summary.csv')

def rsum(path, geom):
    with rasterio.open(path) as r:
        g = gpd.GeoSeries([geom], crs=32642).to_crs(r.crs)
        a, _ = mask(r, list(g), crop=True, filled=False)
        v = a[0].astype('float64').filled(np.nan)
        if r.nodata is not None and not np.isnan(r.nodata): v[v == r.nodata] = np.nan
        return float(np.nansum(v))
osm = {}
def osm_layer(name, bb):
    return gpd.read_file(os.path.join(P, f'OSM_{name}_GB_Chitral.gpkg'), bbox=bb).to_crs(32642)
ex_rows = []; polys = {}
for lake in ['L27', 'L29']:
    for scen, folder, n, bulk in SCEN[lake]:
        poly, *_ = h4_poly(lake, folder); polys[folder] = poly
        if lake not in osm:
            with rasterio.open(rasters(lake, folder)[0]) as rr: rb = rr.bounds
            bb = tuple(gpd.GeoSeries([box(*rb)], crs=32642).to_crs(4326).total_bounds)  # whole model domain
            osm[lake] = {k: osm_layer(k, bb) for k in ['buildings', 'roads', 'bridges', 'power']}
        o = osm[lake]
        ghs = rsum(os.path.join(RAW, 'GHS_POP_2025_GB_Chitral.tif'), poly)
        wp = rsum(os.path.join(RAW, 'WorldPop_2020_GB_Chitral.tif'), poly)
        ex_rows.append(dict(scenario=folder.replace('Mid_bulk1.5', 'Mid_bulk15').replace('Mid_bulk2.0', 'Mid_bulk20').replace('Mid_b15', 'Mid_bulk15').replace('Mid_b20', 'Mid_bulk20'),
            pop_ghs_h4=round(ghs), pop_wp_h4=round(wp), buildings_h4=int(o['buildings'].intersects(poly).sum()),
            roads_h4_km=round(o['roads'][o['roads'].intersects(poly)].geometry.intersection(poly).length.sum() / 1000, 2),
            bridges_h4=int(o['bridges'].intersects(poly).sum()), power_h4=int(o['power'].intersects(poly).sum()),
            built_area_h4_m2=round(rsum(os.path.join(RAW, 'GHS_BUILT_S_2030_GB_Chitral.tif'), poly)),
            h4_area_km2=round(poly.area / 1e6, 3), lake_id=lake, pop_mean_h4=round((ghs + wp) / 2)))
with open(os.path.join(P_OUT, 'h4_exposure_summary.csv'), 'w', newline='') as f:
    w = csv.DictWriter(f, list(ex_rows[0])); w.writeheader(); w.writerows(ex_rows)
print('wrote h4_exposure_summary.csv')

def to_wgs84(src_arr, T, crs, out, nodata=np.nan, resampling=Resampling.nearest, scale=1):
    h, w = src_arr.shape
    left, top = T.c, T.f; right, bottom = left + T.a * w, top + T.e * h
    dT, dw, dh = calculate_default_transform(crs, 'EPSG:4326', w, h, left, bottom, right, top)
    if scale > 1:
        dT = dT * dT.scale(1 / scale, 1 / scale); dw *= scale; dh *= scale
    dst = np.full((dh, dw), nodata, dtype='float32')
    reproject(src_arr.astype('float32'), dst, src_transform=T, src_crs=crs, dst_transform=dT, dst_crs='EPSG:4326',
              resampling=resampling, src_nodata=nodata, dst_nodata=nodata)
    with rasterio.open(out, 'w', driver='GTiff', height=dh, width=dw, count=1, dtype='float32', crs='EPSG:4326',
                       transform=dT, nodata=nodata, compress='deflate') as o: o.write(dst, 1)

def write_utm(arr, T, crs, out, nodata=np.nan):
    with rasterio.open(out, 'w', driver='GTiff', height=arr.shape[0], width=arr.shape[1], count=1, dtype='float32',
                       crs=crs, transform=T, nodata=nodata, compress='deflate') as o: o.write(arr.astype('float32'), 1)

def hillshade(bounds, out_utm, out_wgs):
    with rasterio.open(os.path.join(P, 'DEM_UTM42N.tif')) as r:
        win = rasterio.windows.from_bounds(*bounds, r.transform).round_offsets().round_lengths()
        z = r.read(1, window=win).astype('float64'); T = r.window_transform(win); crs = r.crs
    z = np.where(np.isfinite(z), z, np.nanmean(z))
    dy, dx = np.gradient(z, 30, 30); slope = np.pi / 2 - np.arctan(np.hypot(dx, dy)); aspect = np.arctan2(-dx, dy)
    az, alt = np.radians(315), np.radians(45)
    hs = np.sin(alt) * np.sin(slope) + np.cos(alt) * np.cos(slope) * np.cos(az - aspect)
    hs = (np.clip(hs, 0, 1) * 255)
    write_utm(hs, T, crs, out_utm); to_wgs84(hs, T, crs, out_wgs, resampling=Resampling.bilinear)

def clip_osm(name, bounds, out):
    bb = tuple(gpd.GeoSeries([box(*bounds)], crs=32642).to_crs(4326).total_bounds)
    g = gpd.read_file(os.path.join(P, f'OSM_{name}_GB_Chitral.gpkg'), bbox=bb)
    g = g[g.intersects(gpd.GeoSeries([box(*bounds)], crs=32642).to_crs(4326).iloc[0])]
    g.to_file(out, driver='GPKG')

for lake, fdir, fol in [('L29', 'fig06_layers', 'L29'), ('L27', 'fig07_layers', 'L27')]:
    od = os.path.join(P_OUT, fdir); os.makedirs(od, exist_ok=True)
    allb = None
    for scen in ['Low', 'Mid', 'High']:
        dp, _ = rasters(lake, f'{lake}_{scen}'); d, T, crs, _ = read(dp); d[d <= WET] = np.nan
        write_utm(d, T, crs, os.path.join(od, f'{lake}_{scen}_depth.tif'))
        to_wgs84(d, T, crs, os.path.join(od, f'{lake}_{scen}_depth_wgs84.tif'))
        rows_, cols_ = np.where(np.isfinite(d))
        x0, y0 = T * (cols_.min(), rows_.max() + 1); x1, y1 = T * (cols_.max() + 1, rows_.min())
        b = (x0, y0, x1, y1); allb = b if allb is None else (min(allb[0], b[0]), min(allb[1], b[1]), max(allb[2], b[2]), max(allb[3], b[3]))
    pad = 1500; B = (allb[0] - pad, allb[1] - pad, allb[2] + pad, allb[3] + pad)
    hillshade(B, os.path.join(od, f'{lake}_hillshade.tif'), os.path.join(od, f'{lake}_hillshade_wgs84.tif'))
    for k in ['roads', 'bridges']:
        clip_osm(k, B, os.path.join(od, f'{lake}_{k}.gpkg'))
    print('wrote', fdir)

od = os.path.join(P_OUT, 'fig010o_layers'); os.makedirs(od, exist_ok=True); ann = []
for lake in ['L29', 'L27']:
    poly, h4, d, v, T, crs, prof = h4_poly(lake, f'{lake}_Mid')
    hh = np.where(h4, 1.0, np.nan)
    to_wgs84(hh, T, crs, os.path.join(od, f'{lake}_Mid_h4_wgs84.tif'))
    to_wgs84(hh, T, crs, os.path.join(od, f'{lake}_Mid_h4_wgs84_x4.tif'), scale=4)
    gpd.GeoDataFrame({'lake_id': [lake]}, geometry=[poly], crs=32642).to_crs(4326).to_file(os.path.join(od, f'{lake}_h4_outline.gpkg'), driver='GPKG')
    x0, y0, x1, y1 = poly.bounds; B = (x0 - 1500, y0 - 1500, x1 + 1500, y1 + 1500)
    for k in ['roads', 'bridges']: clip_osm(k, B, os.path.join(od, f'{lake}_{k}.gpkg'))
    if lake == 'L29': hillshade(B, os.path.join(P_OUT, '_hs_tmp.tif'), os.path.join(od, 'L29_hillshade_wgs84.tif'))
    for name, fn in [('worldpop', 'WorldPop_2020_GB_Chitral.tif'), ('ghspop', 'GHS_POP_2025_GB_Chitral.tif'), ('built', 'GHS_BUILT_S_2030_GB_Chitral.tif')]:
        with rasterio.open(os.path.join(RAW, fn)) as r:
            g = gpd.GeoSeries([box(*B)], crs=32642).to_crs(r.crs)
            a, tr = mask(r, list(g), crop=True, filled=True, nodata=0)
            to_wgs84(a[0].astype('float64'), tr, r.crs, os.path.join(od, f'{lake}_{name}_wgs84.tif'), nodata=0)
    e = [r for r in ex_rows if r['scenario'] == f'{lake}_Mid'][0]
    ann.append([lake, {'L27': 'L27 Shisper', 'L29': 'L29 Passu'}[lake], e['h4_area_km2'], e['roads_h4_km'], e['bridges_h4'],
                e['built_area_h4_m2'], e['pop_wp_h4'], e['pop_ghs_h4']])
with open(os.path.join(od, 'fig010o_annotations.csv'), 'w', newline='') as f:
    w = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC); w.writerow(['lake', 'label', 'h4_km2', 'roads_km', 'bridges', 'built_m2', 'pop_wp', 'pop_ghs']); w.writerows(ann)
print('wrote fig010o_layers')

for root, dirs, files in os.walk(P_OUT):
    for fn in files:
        if fn.startswith('_hs_tmp'): continue
        rel = os.path.relpath(os.path.join(root, fn), P_OUT); dst = os.path.join(PROC, rel)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        with open(os.path.join(root, fn), 'rb') as a, open(dst, 'wb') as b: shutil.copyfileobj(a, b)
print('copied outputs to', PROC)
