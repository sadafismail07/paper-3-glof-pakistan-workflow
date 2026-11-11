# 1 km buffer of each flow path

import os, tempfile, shutil, geopandas as gpd
P = 'data/processed'
F = gpd.read_file(f'{P}/lake_flowpaths.gpkg').to_crs(32642)
C = gpd.GeoDataFrame({'lake_id': F.lake_id, 'distance_km': (F.geometry.length / 1000).round(1)},
                     geometry=F.geometry.buffer(1000), crs=32642)
tmp = os.path.join(tempfile.mkdtemp(), 'c.gpkg'); C.to_file(tmp, driver='GPKG')
shutil.copyfile(tmp, f'{P}/lake_downstream_corridors.gpkg'); print('Saved', len(C), 'corridors')
