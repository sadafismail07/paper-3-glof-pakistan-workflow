# Shisper drainage-mode comparison

import os, glob, re, csv, shutil, tempfile, numpy as np, h5py, rasterio, geopandas as gpd, warnings; warnings.filterwarnings('ignore')
from rasterio.features import shapes
from shapely.geometry import shape, box, Point
from shapely.ops import unary_union, substring
P = 'data/processed/'; M = 'hecras_models/L27_Shisper_v2/'
INFLOW = Point(1000614, 4037416)
SCEN = [('Abrupt breach, full lake (mid baseline)', 'L27_Mid', 16.6, ''),
        ('Abrupt breach, 45% of full lake', 'L27_p45', 7.5, ''),
        ('Abrupt breach, 28% of full lake', 'L27_p28', 4.6, ''),
        ('Ice-dam failure, full lake', 'L27_ntfull', 16.6, ''),
        ('Tunnel drainage, full lake', 'L27_tunfull', 16.6, ''),
        ('Tunnel drainage, 2019 volume', 'L27_tun2019', 9.7, 142),
        ('Tunnel drainage, 2020 volume', 'L27_tun2020', 4.8, 85)]
tmp = tempfile.mkdtemp()
path = gpd.read_file(shutil.copy(P + 'lake_flowpaths.gpkg', tmp)).to_crs(32642).set_index('lake_id').geometry['L27']
s0 = path.project(INFLOW); ravine = substring(path, s0, s0 + 8900).buffer(1000)

def plan_hdf(short_id):
    for p in glob.glob(M + '*.p[0-9][0-9]'):
        m = re.search(r'^Short Identifier=(.*)$', open(p, errors='ignore').read(), re.M)
        if m and m.group(1).strip() == short_id and os.path.exists(p + '.hdf'): return p + '.hdf'
    raise FileNotFoundError(short_id)

def routed(short_id):
    h = h5py.File(plan_hdf(short_id), 'r')
    a = h['Results/Unsteady/Output/Output Blocks/Base Output/Unsteady Time Series/2D Flow Areas/Perimeter 1']
    bc = a['Boundary Conditions']; qin = bc[[k for k in bc if k.lower() == 'breach_inflow'][0]][:, 1]
    rl = a['Reference Lines']; names = [n.decode().split('|')[0] for n in rl['Name'][:]]
    q = np.abs(rl['Flow'][:]); v = np.abs(rl['Velocity'][:])
    jb = names.index('RL_KKH_Hassanabad'); jc = names.index('RL_confluence')
    return qin.max(), q[:, jb].max(), q[:, jc].max(), np.nanmax(v[:, jb])

def ravine_h4(folder):
    t = sorted(glob.glob(M + folder + '/*.tif'))
    dp = [x for x in t if 'Depth' in os.path.basename(x)][0]; vp = [x for x in t if 'Velocity' in os.path.basename(x)][0]
    with rasterio.open(dp) as r: d = r.read(1).astype(float); T = r.transform; b = r.bounds; nd = r.nodata
    with rasterio.open(vp) as r: v = r.read(1).astype(float)
    d[(d == nd) | (d <= 0)] = np.nan
    m = np.isfinite(d) & ((d > 1.5) | (d * np.nan_to_num(v) > 0.7))
    poly = unary_union([shape(g) for g, val in shapes(m.astype('uint8'), mask=m, transform=T) if val == 1])
    return poly.intersection(ravine), b

rows = [['Scenario', 'Released volume (10^6 m3)', 'Inflow peak incl. base flow (m3/s)', 'Peak at Hassanabad bridge (m3/s)',
         'Peak at Hunza confluence (m3/s)', 'Section-mean velocity at bridge (m/s)',
         'H4-H6 area, terminus to confluence (km2)', 'Buildings in H4-H6', 'Measured peak at bridge (m3/s)']]
bld = None
for label, sid, vol, meas in SCEN:
    qin, qb, qc, vb = routed(sid); poly, b = ravine_h4(sid)
    if bld is None:
        bb = tuple(gpd.GeoSeries([box(*b)], crs=32642).to_crs(4326).total_bounds)
        bld = gpd.read_file(shutil.copy(P + 'OSM_buildings_GB_Chitral.gpkg', tmp), bbox=bb).to_crs(32642)
    r0 = lambda x: int(np.floor(round(float(x), 1) + 0.5))
    rows.append([label, vol, r0(qin), r0(qb), r0(qc), f'{vb:.1f}', f'{poly.area / 1e6:.2f}', int(bld.intersects(poly).sum()), meas])
with open('figures/table05_shisper_drainage_modes.csv', 'w', newline='', encoding='utf-8') as f: csv.writer(f).writerows(rows)
for r in rows: print(r)
