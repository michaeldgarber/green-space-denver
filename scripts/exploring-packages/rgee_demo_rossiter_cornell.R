#following here: http://www.css.cornell.edu/faculty/dgr2/_static/files/R_PDF/ex_rgee.pdf
#Goal to learn rgee for pulling landsat data

#Revised 12/27/21


library(mapview)
library(tidyverse)
library(sf)
library(raster)
library(here)
library(googleCloudStorageR)
library(googledrive) #worked after I updated google drive
library(rgee)
library(reticulate)
library(terra)
packageVersion("googledrive")

#for some reason, I have to run all of this, not just ee_initialize, so:
source(here("scripts", "configure-rgee-short-lenovo.R"))

#The intro stuff is worth a read, but I alredy completed successfulyly in
#the above install-rgee-r-google-earth-engine script

#---1. Example: Imagery-------#######
#1.1. Following their example-----######
#task: specify an image collection, filter it for a date range, and select one product
#my addition: 
#he's using https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_8DAY_EVI
#MG comment: I'm using more descriptive object names.
landsat_8_evi_image_coll<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_EVI')$
  #MG limiting date
  filterDate('2017-01-01', '2017-02-01') #note it works to put this on the second line below the dollar sign

class(landsat_8_evi_image_coll)
ee_print(landsat_8_evi_image_coll)


#just grab EVI from the dataset / image collection
landsat_evi = landsat_8_evi_image_coll$select('EVI')
class(landsat_evi)  #so they're the same? what am I selecting?
                    #maybe the reason for this is sometimes there are multiple products?
ee_print(landsat_evi)

#task: convert ImageCollection to a multi-band Image and display the band names
#this is very important, as the ee_as_raster function takes an image as an input
evi <- landsat_evi$select('EVI')$toBands()
class(evi)


#load denver county for your region of interest
load(here("data-processed", "den_county.RData"))

#convert denver county to an ee file
#see here for ref: https://csaybar.github.io/blog/2020/06/15/rgee_02_io/
den_county_ee = den_county %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>% 
  rgee::sf_as_ee() 

evi_raster_den_county = evi %>% 
  ee_as_raster(
    region = den_county_ee,
    scale = 1000,
    via = 'drive' #workeda after I updated the googledrive package.
)

plot(evi_raster_den_county)
class(evi_raster_den_county)
evi_raster_den_county_X20170101_EVI = evi_raster_den_county$X20170101_EVI
test = terra::rast(evi_raster_den_county)
evi_raster_den_county_X20170101_EVI %>% mapview()
test %>% raster::raster() %>% mapview()
summary(test$`20170101_EVI`)

#1.2. Trying myself with NDVi----######
#alternate: try NDVI
#https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_8DAY_NDVI
landsat_8_ndvi_image_coll<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$
  filterDate('2017-01-01', '2017-12-31') 

ee_print(landsat_8_ndvi_image_coll)