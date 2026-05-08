// 03_population_export.js
// Export GHS-POP 2025 and WorldPop 2020 for GB + Chitral study area
// Repository: glof-pakistan-ppr3

var studyArea = ee.Geometry.Rectangle({
  coords: [70.8, 34.8, 77.5, 37.7],
  geodesic: false
});

Map.centerObject(studyArea, 7);

// GHS-POP R2023A epoch 2025
var ghs2025 = ee.Image('JRC/GHSL/P2023A/GHS_POP/2025').clip(studyArea);

Export.image.toDrive({
  image: ghs2025, description: 'GHS_POP_2025_GB_Chitral',
  folder: 'GLOF_Pakistan_PPR3',
  fileNamePrefix: 'GHS_POP_2025_GB_Chitral',
  scale: 100, region: studyArea, crs: 'EPSG:4326',
  maxPixels: 1e10, fileFormat: 'GeoTIFF'
});

// WorldPop 2020 Pakistan constrained
var worldpop2020 = ee.ImageCollection('WorldPop/GP/100m/pop')
                     .filter(ee.Filter.eq('country', 'PAK'))
                     .filter(ee.Filter.eq('year', 2020))
                     .first()
                     .clip(studyArea);

Export.image.toDrive({
  image: worldpop2020, description: 'WorldPop_2020_GB_Chitral',
  folder: 'GLOF_Pakistan_PPR3',
  fileNamePrefix: 'WorldPop_2020_GB_Chitral',
  scale: 100, region: studyArea, crs: 'EPSG:4326',
  maxPixels: 1e10, fileFormat: 'GeoTIFF'
});

