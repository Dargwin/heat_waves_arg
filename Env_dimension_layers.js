var dataset = ee.ImageCollection('MODIS/006/MOD44B');

var visualization = {
  bands: ['Percent_Tree_Cover'],
  min: 0.0,
  max: 100.0,
  palette: ['bbe029', '0a9501', '074b03']
};

var visualization2 = {
  bands: ['Percent_NonVegetated'],
  min: 0.0,
  max: 100.0,
  palette: ['ffff00', 'ffaa00', 'aa5500']
};

// Agregar identificación de áreas urbanas
var landCover = ee.Image('MODIS/006/MCD12Q1/2010_01_01')
  .select('LC_Type1');

var urbanMask = landCover.eq(13);

// Definir la región de interés (Argentina)
var CuadroARG = ee.Geometry.Rectangle(-76, -57, -52, -21);

// Filtrar y mosaiquear la colección de imágenes
var clippedImage = dataset
  .filterDate('2010-01-01', '2010-12-31')
  .filterBounds(CuadroARG)
  .mosaic()
  .clip(CuadroARG);

// Configurar los parámetros de exportación para Percent Tree Cover
var exportParams1 = {
  image: clippedImage.select('Percent_Tree_Cover'),
  description: 'Percent_tree_cover',
  folder: 'EE_exports',
  region: CuadroARG,
  scale: 1000,
  maxPixels: 200000000, // Ajusta este valor según sea necesario
  crs: 'EPSG:4326', // Proyección WGS84
};

// Configurar los parámetros de exportación para Percent NonTree Vegetation (suelo desnudo)
var exportParams2 = {
  image: clippedImage.select('Percent_NonVegetated'),
  description: 'Percent_non_vegetated',
  folder: 'EE_exports',
  region: CuadroARG,
  scale: 1000,
  maxPixels: 200000000, // Ajusta este valor según sea necesario
  crs: 'EPSG:4326', // Proyección WGS84
};

// Configurar los parámetros de exportación para áreas urbanas
var exportUrbanParams = {
  image: urbanMask,
  description: 'Urban_Areas',
  folder: 'EE_exports',
  region: CuadroARG,
  scale: 1000,
  maxPixels: 200000000, // Ajusta este valor según sea necesario
  crs: 'EPSG:4326', // Proyección WGS84
  
};

// Exportar la banda de Percent Tree Cover a Google Drive
Export.image.toDrive(exportParams1);

// Exportar la banda de Percent NonTree Vegetation a Google Drive
Export.image.toDrive(exportParams2);

// Exportar áreas urbanas a Google Drive
Export.image.toDrive(exportUrbanParams);

