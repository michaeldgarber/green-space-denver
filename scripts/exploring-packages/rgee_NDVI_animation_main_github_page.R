#https://github.com/r-spatial/rgee

#Following along with the third example: Create an NDVI animation, but then modifying
library(tidyverse)
library(sf)
library(mapview)
library(rgee)

mask <- system.file("shp/arequipa.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee() #converts it to an ee object

region <- mask$geometry()$bounds()

modis_ndvi = ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI') #https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MOD13A2
landsat_ndvi = ee$ImageCollection('LANDSAT/LC08/C01')$select('T1_8DAY_NDVI')

class(landsat_ndvi)

ee.ImageCollection("LANDSAT/LC08/C01/T1_8DAY_NDVI") #https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_8DAY_NDVI
class(col)

