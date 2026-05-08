// 04_built_export.js
// Export GHS-BUILT-S R2023A epoch 2030 for GB + Chitral study area

var studyArea = ee.Geometry.Rectangle({
  coords: [70.8, 34.8, 77.5, 37.7],
  geodesic: false
});

var built2030 = ee.Image('JRC/GHSL/P2023A/GHS_BUILT_S/2030').clip(studyArea);

Export.image.toDrive({
  image: built2030,
  description: 'GHS_BUILT_S_2030_GB_Chitral',
  folder: 'GLOF_Pakistan_PPR3',
  fileNamePrefix: 'GHS_BUILT_S_2030_GB_Chitral',
  scale: 100, region: studyArea, crs: 'EPSG:4326',
  maxPixels: 1e10, fileFormat: 'GeoTIFF'
});

