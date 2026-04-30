// ===================================================================
// Script: 01_export_dem.js
// Purpose: Export Copernicus 30m DEM clipped to GB + Chitral study area
// Repository: glof-pakistan-ppr3
//
// Run via Google Earth Engine Code Editor:
//   https://code.earthengine.google.com/
//
// Output: GeoTIFF exported to Drive folder GLOF_Pakistan_PPR3
// ===================================================================

var studyArea = ee.Geometry.Rectangle({
  coords: [70.8, 34.8, 77.5, 37.7],
  geodesic: false
});

Map.centerObject(studyArea, 7);
Map.addLayer(studyArea, {color: 'red'}, 'Study area bbox');

var demCollection = ee.ImageCollection("COPERNICUS/DEM/GLO30");

var dem = demCollection
            .select("DEM")
            .mosaic()
            .clip(studyArea);

var demVis = {
  min: 0,
  max: 8000,
  palette: ['006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5']
};
Map.addLayer(dem, demVis, 'Copernicus DEM');

print('DEM projection:', dem.projection());
print('DEM scale (m):', dem.projection().nominalScale());
print('DEM bounds:', dem.geometry().bounds());

Export.image.toDrive({
  image: dem,
  description: 'Copernicus_DEM_GB_Chitral',
  folder: 'GLOF_Pakistan_PPR3',
  fileNamePrefix: 'Copernicus_DEM_GB_Chitral',
  scale: 30,
  region: studyArea,
  crs: 'EPSG:4326',
  maxPixels: 1e10,
  fileFormat: 'GeoTIFF'
});

print('Export task created. Click Tasks tab and Run.');

