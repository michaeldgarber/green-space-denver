#testing landsat collection 2
#I'm having issues getting the same values for each day of year
library(rgee)
library(sf)
setwd(here("data-processed"))

# small reprex for the repeating-values issues-------

## load packages
library(sf)
library(terra)
library(raster)
library(reticulate)
library(rgee)
library(raster)
library(mapview)
## rgee configuration steps-----
rgee::ee_Initialize(drive = T) #must run every time.
reticulate::py_config()
rgee::ee_check() #worked.
rgee::ee_check_credentials()

## define bounding box-------
den_bbox_small = st_bbox(
  c(
    xmin = -105.01063, #westmost
    ymin = 39.7069, 
    xmax = -104.949165, #eastmost
    ymax = 39.7557
  ),
  crs=st_crs(4326)
)
den_bbox_small %>% mapview()
den_bbox_small_ee  = den_bbox_small %>% 
  sf::st_as_sfc() %>% 
  rgee::sf_as_ee() 


## define image collections (product, area, and date)-------
l8_test_image_2016_jun = ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2016-06-01", "2016-06-30")$     
  select("NDVI")$   
  toBands()  
l8_test_image_2017_jun = ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2017-06-01", "2017-06-30")$     
  select("NDVI")$   
  toBands()  
l8_test_image_2018_jun = ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2018-06-01", "2018-06-30")$     
  select("NDVI")$   
  toBands()  

identical(l8_test_image_2016_jun, l8_test_image_2017_jun)
## convert image collections to raster------
n_m_per_pixel = 30
l8_test_rast_2016_jun = l8_test_image_2016_jun %>% 
  ee_as_raster(
#    via = "drive" ,
#    via = "getInfo" ,
    dsn = "l8_test_rast_2016_jun",
    region = den_bbox_small_ee,  
    scale = n_m_per_pixel 
  )

plot(l8_test_rast_2016_jun)

l8_test_rast_2017_jun = l8_test_image_2017_jun %>% 
  ee_as_raster(
     via = "drive" ,
 #        via = "getInfo" , #throws an error even though default..
         dsn = "l8_test_rast_2017_jun",
    region = den_bbox_small_ee,  
    scale = n_m_per_pixel 
  )
plot(l8_test_rast_2017_jun)
identical(l8_test_rast_2016_jun, l8_test_rast_2017_jun )

# landsat 8 collection 2----------
l8c2_image_col_sr_b4_2016_jun<- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$ 
  filterDate("2016-06-01", "2016-06-30")$    #1/15/22 update to include 5 years per meeting
  select("SR_B4")$  #surface reflectance band 4 (red)
  toBands() #from imagecollection to image

#ndvi
#https://eos.com/blog/ndvi-faq-all-you-need-to-know-about-ndvi/
#In most cases, NDVI values between 0.2 and 0.4 
#correspond to areas with sparse vegetation; 
#moderate vegetation tends to vary between 0.4 and 0.6; 
#anything above 0.6 indicates the highest possible density of green leaves.
ee_print(l8c2_image_col_sr_b4_2016_jun) #takes a long time.

#load larger bounding box
load("den_metro_bbox_custom.RData")
den_metro_ee = den_metro_bbox_custom %>% 
  sf::st_as_sfc() %>% 
  rgee::sf_as_ee() 

n_m_per_pixel = 30
l8c2_image_col_sr_b4_rast_test = l8c2_image_col_sr_b4_2016_jun  %>% 
  ee_as_raster(
    region = den_bbox_small_ee,  
    scale = n_m_per_pixel,   
    via = "drive",
    dsn = "l8c2_image_col_sr_b4_rast_test"
  )