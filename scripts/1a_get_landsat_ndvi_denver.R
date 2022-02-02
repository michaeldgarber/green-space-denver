#filename: 1_get_landsat_ndvi_denver
#Revised 12/27/21
#Revised 1/17/22 on Mac (switched OS)
#Revised 2/1/22
#Planning a pipeline:

#0 -download denver region data
#1  - download landsat data using rgee, restrict to denver area, and convert to raster
#2 - raster wrangling, e.g.
    #-extract values at the botanic gardens and elsewhere
    # perform some summaries, e.g. summarizing over areas and time

library(tidyverse)
library(sf)
library(mapview)
library(terra)
library(raster)
library(stars)
library(here)
library(viridisLite)
library(viridis)
library(lubridate)
library(scales)
# library(googledrive)
# library(googleCloudStorageR)
library(reticulate)
library(rgee)
setwd(here("data-processed"))

reticulate::py_config()
rgee::ee_check() #worked.
rgee::ee_check_credentials()
#source(here("scripts", "configure  _rgee_lenovo.R")) #initialize rgee for Windows
rgee::ee_Initialize(drive = T) #must run every time.

#Load denver area data for your region of interest
setwd(here("data-processed"))
load("den_metro_co_geo.RData")
load("den_jeff_co_geo.RData")

# create bounding box------------
#1/25/22 I considered using all counties, including Adams and Arapahoe,
#but I decided that a bounding box around Denver County and Jefferson county is sufficient.

den_jeff_co_geo %>% mapview()
den_metro_bbox = den_jeff_co_geo %>% 
  st_bbox() 
save(den_metro_bbox, file = "den_metro_bbox.RData")

mapview(den_metro_bbox, col.regions = "red")+ mapview(den_metro_co_geo)

#this is also too big. let's go even smaller for speed.
#go only as far south as Castle Rock, which is at 39.354023
den_metro_bbox_custom = st_bbox(
  c(
  den_metro_bbox[1],
  ymin=39.354023, #this is south to castle rock...so not all of jeff co
  den_metro_bbox[3],
  den_metro_bbox[4]
  ),
  crs=st_crs(4326)
)
save(den_metro_bbox_custom, file = "den_metro_bbox_custom.RData")
mapview(den_metro_bbox_custom, col.regions="red")+mapview(den_metro_bbox)#great
#convert bbox to ee image so it can be used as the region of interest
den_metro_ee = den_metro_bbox_custom %>% 
  sf::st_as_sfc() %>% 
  rgee::sf_as_ee() 

#  Get LANDSAT image from rgee------------
#note this is the best demo I've found yet on this:
#http://www.css.cornell.edu/faculty/dgr2/_static/files/R_PDF/ex_rgee.pdf

#use the dollar sign analogously to a pipe. selector makes sense,
#as, at this stage, we're just specifying the data to be gathered, 
#not really modifying it, as you would in a tidyverse pipe

#first, specify the image collections (landsat 8-day)
#https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C01_T1_8DAY_NDVI
#update: I'm separating by one-year increments to make sure it runs.
landsat_8_ndvi_image_coll_2016<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2016-01-01", "2016-12-31")$    #1/15/22 update to include 5 years per meeting
  select("NDVI")$  #select the NDVI band. note it works with single or double quotes. I prefer dbl
  toBands() #from imagecollection to image
landsat_8_ndvi_image_coll_2017<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2017-01-01", "2017-12-31")$    
  select("NDVI")$   
  toBands()  
landsat_8_ndvi_image_coll_2018<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2018-01-01", "2018-12-31")$    
  select("NDVI")$   
  toBands()  
landsat_8_ndvi_image_coll_2019<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2019-01-01", "2019-12-31")$    
  select("NDVI")$   
  toBands()  
landsat_8_ndvi_image_coll_2020<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2020-01-01", "2020-12-31")$    
  select("NDVI")$   
  toBands()  
landsat_8_ndvi_image_coll_2021<- ee$ImageCollection('LANDSAT/LC08/C01/T1_8DAY_NDVI')$ 
  filterDate("2021-01-01", "2021-12-31")$    
  select("NDVI")$   
  toBands()  


#get information about the image collections
ee_print(landsat_8_ndvi_image_coll_2017) #12 bands. about 2.4 mb
ee_print(landsat_8_ndvi_image_coll_2019)
ee_print(landsat_8_ndvi_image_coll_2020)
#  convert LANDSAT image collection to raster --------
n_m_per_pixel = 30#30 is the lowest you can go. 
#try this with better understanding of terra read/write
#note renaming...removing the lsat...was too long.
ndvi_den_metro_rast_2016 = landsat_8_ndvi_image_coll_2016 %>% 
  ee_as_raster(
    dsn = "ndvi_den_metro_rast_2016", #very important to specify; otherwise will overwrite and duplicate info
    region = den_metro_ee, #use metro to ensure coverage throughout region
    scale = n_m_per_pixel,  #number of meters per pixel. smaller number...slower speed
    via = "drive" #worked after I updated the googledrive package.
  )

ndvi_den_metro_rast_2017 = landsat_8_ndvi_image_coll_2017 %>% 
  ee_as_raster(
    dsn = "ndvi_den_metro_rast_2017",
    region = den_metro_ee,  
    scale = n_m_per_pixel,   
    via = "drive"
  )

ndvi_den_metro_rast_2018 = landsat_8_ndvi_image_coll_2018 %>% 
  ee_as_raster(
    dsn = "ndvi_den_metro_rast_2018",
    region = den_metro_ee,  
    scale = n_m_per_pixel,   
    via = 'drive' 
  )

ndvi_den_metro_rast_2019 = landsat_8_ndvi_image_coll_2019 %>% 
  ee_as_raster(
    dsn = "ndvi_den_metro_rast_2019",
    region = den_metro_ee,  
    scale = n_m_per_pixel,   
    via = "drive"
  )

ndvi_den_metro_rast_2020 = landsat_8_ndvi_image_coll_2020 %>% 
  ee_as_raster(
    dsn = "ndvi_den_metro_rast_2020",
    region = den_metro_ee,  
    scale = n_m_per_pixel,   
    via = "drive" 
  )

ndvi_den_metro_rast_2021 = landsat_8_ndvi_image_coll_2021 %>% 
  ee_as_raster(
    dsn = "ndvi_den_metro_rast_2021",
    region = den_metro_ee,  
    scale = n_m_per_pixel,   
    via = "drive"
  )

## convert each to terra--------
ndvi_den_metro_terr_2016 = ndvi_den_metro_rast_2016 %>% 
  terra::rast()
ndvi_den_metro_terr_2017 = ndvi_den_metro_rast_2017 %>% 
  terra::rast()
ndvi_den_metro_terr_2018 = ndvi_den_metro_rast_2018 %>% 
  terra::rast()
ndvi_den_metro_terr_2019 = ndvi_den_metro_rast_2019 %>% 
  terra::rast()
ndvi_den_metro_terr_2020 = ndvi_den_metro_rast_2020 %>% 
  terra::rast()
ndvi_den_metro_terr_2021 = ndvi_den_metro_rast_2021 %>% 
  terra::rast()

#combine them using c
ndvi_den_metro_terr_5_yr = c(
  ndvi_den_metro_terr_2016, 
  ndvi_den_metro_terr_2017,
  ndvi_den_metro_terr_2018,
  ndvi_den_metro_terr_2019,
  ndvi_den_metro_terr_2020,
  ndvi_den_metro_terr_2021
)
#  write raster files to disc--------------
#note this is not the usual way you'd save an R object.
library(here)
setwd(here("data-processed"))
#I tried several ways to save, including the suggestion here to save as .rds 
# https://tmieno2.github.io/R-as-GIS-for-Economists/read-write-stars.html
#What worked best for me was to convert to terra and then save using writeRaster 
#and then you can convert
#back if needed (round trip)

#https://geocompr.robinlovelace.net/read-write.html#file-formats
#this worked.
## multi-year terra raster----------
setwd(here("data-processed"))
terra::writeRaster(
  ndvi_den_metro_terr_5_yr,
  overwrite=TRUE,
  #  datatype = "INT1U", #see Robin's discussion of what this means. default OK
  filename = "ndvi_den_metro_terr_5_yr.tif" 
)


