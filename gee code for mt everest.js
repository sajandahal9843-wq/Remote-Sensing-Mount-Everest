var everestPoint = ee.Geometry.Point([86.9250, 27.9881]);
var watersheds = ee.FeatureCollection('WWF/HydroSHEDS/v1/Basins/hybas_8');
var aoi = watersheds.filterBounds(everestPoint).geometry();

Map.centerObject(aoi, 11);
Map.addLayer(aoi, {color: 'red'}, 'Dudh Koshi Natural Watershed', false);

var dem = ee.Image('USGS/SRTMGL1_003').select('elevation').clip(aoi);

var elevBands = dem.gte(3000).and(dem.lt(4000)).multiply(1)
  .add(dem.gte(4000).and(dem.lt(5000)).multiply(2))
  .add(dem.gte(5000).and(dem.lt(6000)).multiply(3))
  .add(dem.gte(6000).multiply(4))
  .rename('elevation_zone');

Map.addLayer(dem, {min: 3000, max: 8000, palette: ['blue', 'green', 'orange', 'white']}, 'DEM Elevation', false);

var startDate = '2000-01-01';
var endDate = '2025-12-31';

var modisSnow = ee.ImageCollection('MODIS/061/MOD10A1')
                  .filterBounds(aoi)
                  .filterDate(startDate, endDate)
                  .select('NDSI_Snow_Cover');

var era5 = ee.ImageCollection('ECMWF/ERA5_LAND/MONTHLY')
             .filterBounds(aoi)
             .filterDate(startDate, endDate)
             .select(['temperature_2m', 'surface_net_solar_radiation', 'total_precipitation']);

var years = ee.List.sequence(2000, 2025);
var months = ee.List.sequence(1, 12);

var monthlyData = years.map(function(y) {
  return months.map(function(m) {
    var start = ee.Date.fromYMD(y, m, 1);
    var end = start.advance(1, 'month');
    
    var snowMonth = modisSnow.filterDate(start, end);
    var era5MonthColl = era5.filterDate(start, end);
    
        var hasEra5 = ee.Algorithms.If(era5MonthColl.size().gt(0),
      era5MonthColl.mean().clip(aoi),
      ee.Image.constant([273.15, 0, 0]).rename(['temperature_2m', 'surface_net_solar_radiation', 'total_precipitation']).clip(aoi)
    );
    var era5Month = ee.Image(hasEra5);
    
        var hasSnow = ee.Algorithms.If(snowMonth.size().gt(0), 
      snowMonth.map(function(img) {
        return img.gte(40).and(img.lte(100));
      }).mean(), 
      ee.Image.constant(0).rename('NDSI_Snow_Cover')
    );
    
    var snowCoverImage = ee.Image(hasSnow).rename('snow_cover_prob');
    var pixelArea = ee.Image.pixelArea().divide(1e6); // km2
    
    var zonalAnalysisImage = snowCoverImage.multiply(pixelArea).rename('snow_area')
                              .addBands(elevBands);
                              
    var zonalStats = zonalAnalysisImage.reduceRegion({
      reducer: ee.Reducer.sum().group({
        groupField: 1,
        groupName: 'zone_id'
      }),
      geometry: aoi,
      scale: 500,
      maxPixels: 1e9
    });
    
        var tempVal = era5Month.select(['temperature_2m']).reduceRegion({reducer: ee.Reducer.mean(), geometry: aoi, scale: 11132, maxPixels: 1e9}).get('temperature_2m');
    var radVal = era5Month.select(['surface_net_solar_radiation']).reduceRegion({reducer: ee.Reducer.mean(), geometry: aoi, scale: 11132, maxPixels: 1e9}).get('surface_net_solar_radiation');
    var precipVal = era5Month.select(['total_precipitation']).reduceRegion({reducer: ee.Reducer.mean(), geometry: aoi, scale: 11132, maxPixels: 1e9}).get('total_precipitation');

    return ee.Feature(null, {
      'year': y,
      'month': m,
      'date': start.format('YYYY-MM'),
      'zonal_breakdown': zonalStats.get('groups'),
      'basin_mean_temp_k': tempVal,
      'basin_mean_solar_rad': radVal,
      'basin_mean_precip': precipVal
    });
  });
}).flatten();

var timeSeriesFC = ee.FeatureCollection(monthlyData);

Export.table.toDrive({
  collection: timeSeriesFC,
  description: 'Everest_Elevation_Zoned_Snow_Climate_2000_2025',
  fileFormat: 'CSV',
  folder: 'EarthEngine_Exports'
});