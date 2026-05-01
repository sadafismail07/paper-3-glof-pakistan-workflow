// ===================================================================
// Script: 02_water_mask.js
// Purpose: Multi-index water mask for vulnerable GLOF lakes in
//          GB + Chitral, Pakistan
// Repository: glof-pakistan-ppr3
//
// Combined NDWI + MNDWI + NDSI rule:
//   water = NDWI > 0.2 AND MNDWI > 0.0 AND NDSI < 0.6
//
// Exports NDWI, MNDWI, and combined water mask at 10m resolution
// Sentinel-2 SR Harmonized, August-September 2024 composite
// ===================================================================

var studyArea = ee.Geometry.Rectangle({
  coords: [70.8, 34.8, 77.5, 37.7],
  geodesic: false
});

Map.centerObject(studyArea, 7);

function maskS2clouds(image) {
  var qa = image.select('QA60');
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
              .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
  return image.updateMask(mask).divide(10000);
}

var s2 = ee.ImageCollection("COPERNICUS/S2_SR_HARMONIZED")
           .filterBounds(studyArea)
           .filterDate("2024-08-01", "2024-09-30")
           .filter(ee.Filter.lt("CLOUDY_PIXEL_PERCENTAGE", 20))
           .map(maskS2clouds);

var s2_composite = s2.median().clip(studyArea);

var ndwi  = s2_composite.normalizedDifference(['B3', 'B8']).rename('NDWI');
var mndwi = s2_composite.normalizedDifference(['B3', 'B11']).rename('MNDWI');
var ndsi  = s2_composite.normalizedDifference(['B3', 'B11']).rename('NDSI');

var water = ndwi.gt(0.2)
              .and(mndwi.gt(0.0))
              .and(ndsi.lt(0.6))
              .rename('water');

Export.image.toDrive({
  image: ndwi, description: 'S2_NDWI_GB_Chitral_2024',
  folder: 'GLOF_Pakistan_PPR3', scale: 10, region: studyArea,
  crs: 'EPSG:4326', maxPixels: 1e10, fileFormat: 'GeoTIFF'
});

Export.image.toDrive({
  image: mndwi, description: 'S2_MNDWI_GB_Chitral_2024',
  folder: 'GLOF_Pakistan_PPR3', scale: 10, region: studyArea,
  crs: 'EPSG:4326', maxPixels: 1e10, fileFormat: 'GeoTIFF'
});

Export.image.toDrive({
  image: water.toByte(), description: 'S2_WaterMask_GB_Chitral_2024',
  folder: 'GLOF_Pakistan_PPR3', scale: 10, region: studyArea,
  crs: 'EPSG:4326', maxPixels: 1e10, fileFormat: 'GeoTIFF'
});

