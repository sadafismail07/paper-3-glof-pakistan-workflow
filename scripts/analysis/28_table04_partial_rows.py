# Hazard-zone exposure for the partial-drainage runs

import os, glob, csv, numpy as np, rasterio, geopandas as gpd, warnings; warnings.filterwarnings('ignore')
from rasterio.features import shapes
from rasterio.mask import mask
from shapely.geometry import shape, box
from shapely.ops import unary_union
ROOT=os.getcwd()
P=ROOT+'/data/processed/'; RAW=ROOT+'/data/raw/'; M=ROOT+'/hecras_models/L27_Shisper_v2/'
def h4(folder):
    t=sorted(glob.glob(M+folder+'/*.tif')); dp=[x for x in t if 'Depth' in os.path.basename(x)][0]; vp=[x for x in t if 'Velocity' in os.path.basename(x)][0]
    with rasterio.open(dp) as r: d=r.read(1).astype(float); T=r.transform; b=r.bounds; nd=r.nodata
    with rasterio.open(vp) as r: v=r.read(1).astype(float)
    d[(d==nd)|(d<=0)]=np.nan; m=np.isfinite(d)&((d>1.5)|(d*np.nan_to_num(v)>0.7))
    return unary_union([shape(g) for g,val in shapes(m.astype('uint8'),mask=m,transform=T) if val==1]), b
def rsum(path,geom):
    with rasterio.open(path) as r:
        g=gpd.GeoSeries([geom],crs=32642).to_crs(r.crs); a,_=mask(r,list(g),crop=True,filled=False); v=a[0].astype(float).filled(np.nan)
        if r.nodata is not None and not np.isnan(r.nodata): v[v==r.nodata]=np.nan
        return float(np.nansum(v))
rows=[['Lake','Scenario','H4 area (km²)','Population (GHS)','Population (WP)','Population (mean)','Buildings (n)','Roads (km)','Bridges (n)','Built-up area (m²)']]
osm=None
for folder,lab in [('L27_p45','Partial drainage 45%'),('L27_p28','Partial drainage 28%')]:
    poly,b=h4(folder)
    if osm is None:
        bb=tuple(gpd.GeoSeries([box(*b)],crs=32642).to_crs(4326).total_bounds); osm={k:gpd.read_file(P+f'OSM_{k}_GB_Chitral.gpkg',bbox=bb).to_crs(32642) for k in ['buildings','roads','bridges']}
    ghs=rsum(RAW+'GHS_POP_2025_GB_Chitral.tif',poly); wp=rsum(RAW+'WorldPop_2020_GB_Chitral.tif',poly)
    rows.append(['L27 Shisper',lab,f'{poly.area/1e6:.3f}',round(ghs),round(wp),round((ghs+wp)/2),int(osm['buildings'].intersects(poly).sum()),
                 f"{osm['roads'][osm['roads'].intersects(poly)].geometry.intersection(poly).length.sum()/1000:.3f}",int(osm['bridges'].intersects(poly).sum()),round(rsum(RAW+'GHS_BUILT_S_2030_GB_Chitral.tif',poly))])
out='figures/table04_partial_rows.csv'
csv.writer(open(out,'w',newline='',encoding='utf-8')).writerows(rows); print(open(out,encoding='utf-8').read())
