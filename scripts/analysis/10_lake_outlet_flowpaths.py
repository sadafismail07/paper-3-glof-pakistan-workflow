# Flow path from each lake outlet (50 km)

import os, tempfile, shutil, numpy as np
if not hasattr(np, 'in1d'): np.in1d = np.isin
import geopandas as gpd, rasterio
from rasterio.windows import from_bounds
from rasterio.features import geometry_mask
from pysheds.grid import Grid
from shapely.geometry import LineString, Point
from shapely.ops import substring, linemerge, unary_union

P = 'data/processed'; MAXLEN = 50000; HALF = 32000
L = gpd.read_file(f'{P}/lakes_polygons_verified.gpkg').to_crs(32642)
R = gpd.read_file(f'{P}/HydroRIVERS_extended_GB_Chitral.gpkg').to_crs(32642).set_index('HYRIV_ID')
dirmap = (64, 128, 1, 2, 4, 8, 16, 32)
step = {64:(-1,0),128:(-1,1),1:(0,1),2:(1,1),4:(1,0),8:(1,-1),16:(0,-1),32:(-1,-1)}
tmp = tempfile.mkdtemp()

def lake_geom(g):
    if g.area < 1000:  # no mappable polygon -> inventory point
        x0, y0, x1, y1 = g.bounds; return Point((x0+x1)/2, (y0+y1)/2).buffer(300)
    return g.buffer(150)

def hydro_continue(pt, remaining):
    hid = R.distance(pt).idxmin(); seg = R.geometry[hid]
    pieces = [substring(seg, seg.project(pt), seg.length)]; tot = pieces[0].length; nxt = R.NEXT_DOWN[hid]
    while tot < remaining and nxt in R.index:
        pieces.append(R.geometry[nxt]); tot += R.geometry[nxt].length; nxt = R.NEXT_DOWN[nxt]
    return pieces

rows = []
with rasterio.open(f'{P}/DEM_UTM42N.tif') as dem:
    for _, lk in L.iterrows():
        g = lake_geom(lk.geometry); c = g.centroid
        win = from_bounds(c.x-HALF, c.y-HALF, c.x+HALF, c.y+HALF, dem.transform).round_offsets().round_lengths()
        a = dem.read(1, window=win, boundless=True, fill_value=np.nan); T = dem.window_transform(win)
        prof = dem.profile.copy(); prof.update(width=a.shape[1], height=a.shape[0], transform=T, compress='deflate')
        f = os.path.join(tmp, f'{lk.lake_id}.tif')
        with rasterio.open(f, 'w', **prof) as o: o.write(a, 1)
        grid = Grid.from_raster(f); z = grid.read_raster(f)
        z = grid.resolve_flats(grid.fill_depressions(grid.fill_pits(z)))
        fdr = grid.flowdir(z, dirmap=dirmap); fd = np.asarray(fdr); ac = np.asarray(grid.accumulation(fdr, dirmap=dirmap))
        m = ~geometry_mask([g], out_shape=fd.shape, transform=T)
        r, cc = np.unravel_index(np.argmax(np.where(m, ac, -1)), ac.shape)
        pts, length, seen = [], 0.0, set()
        while 0 <= r < fd.shape[0] and 0 <= cc < fd.shape[1] and length < MAXLEN and (r, cc) not in seen:
            seen.add((r, cc)); x, y = T * (cc + 0.5, r + 0.5)
            if pts: length += Point(pts[-1]).distance(Point(x, y))
            pts.append((x, y)); dr, dc = step.get(int(fd[r, cc]), (0, 0))
            if (dr, dc) == (0, 0): break
            r += dr; cc += dc
        line = LineString(pts); method = 'D8'
        if length < MAXLEN - 100:
            extra = hydro_continue(Point(pts[-1]), MAXLEN - length)
            line = linemerge(unary_union([line, LineString([pts[-1], extra[0].coords[0]])] + extra))
            method = f'D8 {length/1000:.1f} km + HydroRIVERS'
        rows.append(dict(lake_id=lk.lake_id, outlet_x=pts[0][0], outlet_y=pts[0][1], d8_km=round(length/1000, 1),
                         method=method, path_km=round(line.length/1000, 1), geometry=line))
        print(lk.lake_id, method)
out = os.path.join(tmp, 'lake_flowpaths.gpkg')
gpd.GeoDataFrame(rows, crs=32642).to_file(out, driver='GPKG')
shutil.copyfile(out, f'{P}/lake_flowpaths.gpkg'); print('Saved data/processed/lake_flowpaths.gpkg')
