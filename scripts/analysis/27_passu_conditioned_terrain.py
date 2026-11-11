# Conditioned terrain for Passu

import os, shutil, tempfile, numpy as np, rasterio, geopandas as gpd, warnings; warnings.filterwarnings('ignore')
from rasterio.features import rasterize
from shapely.geometry import Point
from shapely.ops import substring
from pysheds.grid import Grid
R=''; src='data/processed/hecras_terrain/L29_terrain_v2.tif'; tmpd=tempfile.mkdtemp()
with rasterio.open(src) as r: z=r.read(1).astype('float64'); prof=r.profile; T=r.transform
F=gpd.read_file(shutil.copy('data/processed/lake_flowpaths.gpkg',tmpd)).to_crs(32642).set_index('lake_id'); g=F.geometry['L29']; s0=g.project(Point(1027696,4050997))
w=gpd.read_file(R+'hecras_models/v2_inputs/L29_outlet_stream_osm.geojson').to_crs(32642); s=w.geometry.iloc[0]
burn=rasterize([(s.buffer(45),1)],out_shape=z.shape,transform=T,all_touched=True).astype(bool)
zb=z.copy(); zb[burn]-=3.0
zt=zb.copy(); run=None
for k in np.arange(s0+24000, min(s0+34000,g.length), 15):
    p=g.interpolate(k); rr,cc=rasterio.transform.rowcol(T,p.x,p.y)
    win=zt[rr-1:rr+2,cc-1:cc+2]; cur=win.min(); run=cur if run is None else min(run,cur)
    zt[rr-1:rr+2,cc-1:cc+2]=np.minimum(win,run)
tmp=os.path.join(tmpd,'l29_t.tif'); p=prof.copy(); p.update(dtype='float64',nodata=-9999.0,compress='deflate')
with rasterio.open(tmp,'w',**p) as o: o.write(zt,1)
G=Grid.from_raster(tmp); dem=G.read_raster(tmp); c=G.resolve_flats(G.fill_depressions(G.fill_pits(dem)),eps=1e-4)
fill=np.array(c,dtype='float64')-zt; fill[fill<0]=0
m=rasterize([(substring(g,s0,s0+16000).buffer(1000),1)],out_shape=z.shape,transform=T).astype(bool)
zo=zb.copy(); zo[m]+=fill[m]
d=zo-z; print('pit-fill volume Mm3',round(d[d>0].sum()*900/1e6,2),'max',round(d.max(),1),'burn cells',int((d<-0.05).sum()))
out=R+'data/processed/hecras_terrain/L29_terrain_v5_conditioned.tif'; p2=prof.copy(); p2.update(dtype='float32',nodata=-9999.0,compress='deflate',predictor=3)
t2=os.path.join(tmpd,'out.tif')
with rasterio.open(t2,'w',**p2) as o: o.write(zo.astype('float32'),1)
shutil.copyfile(t2,out); print('Saved',out)
