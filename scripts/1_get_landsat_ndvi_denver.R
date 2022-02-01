#filename: 1_get_landsat_ndvi_denver
#Revised 12/27/21
#Revised 1/17/22 on Mac
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


#load denver area data for your region of interest
setwd(here("data-processed"))
load("den_metro_co_geo.RData")
load("den_jeff_co_geo.RData")

# create bounding box------------
#1/25/22 I considered using all counties, including Adams and Arapahoe,
#but I decided that a bounding box around Denver County and Jefferson county is sufficient.
#Look. It covers a lot.

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
#What seems to work the best is to convert to terra and then save using writeRaster 
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

## can I read it back?---------
# setwd(here("data-processed"))
# ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")
# test_write_raster = ndvi_den_metro_terr_2016
# terra::writeRaster(
#   test_write_raster,
#   overwrite=TRUE,
#   #  datatype = "INT1U", 
#   filename = "test_write_raster.tif" 
# )

# test_write_raster_read = terra::rast("test_write_raster.tif")
# test_write_raster_read %>% plot() #yay! worked.

# visualize dates using the raster version to check-------
#check on cloud cover, etc.
ndvi_den_metro_terr_5_yr$`20210805_NDVI` %>% raster::raster() %>% 
  mapview()
ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% raster::raster() %>% 
  mapview() #this is a good image and could be used as a guide
ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% values()
ndvi_den_metro_terr_5_yr$`20210626_NDVI` %>% raster::raster() %>% 
  mapview() #mostly cloudy. toss.
ndvi_den_metro_terr_5_yr$`20210712_NDVI` %>% raster::raster() %>% 
  mapview() #all very cloudy
ndvi_den_metro_terr_5_yr$`20210720_NDVI` %>% raster::raster() %>% 
  mapview() #partly cloudy. would probably toss.
ndvi_den_metro_terr_5_yr$`20210728_NDVI` %>% raster::raster() %>% 
  mapview() 


#note data are more missing in 2020...less frequent for some reason.
ndvi_den_metro_terr_5_yr$`20200101_NDVI`  %>% raster::raster() %>% mapview() #data available
ndvi_den_metro_terr_5_yr$`20200109_NDVI` %>% raster::raster() %>% mapview() #missing
ndvi_den_metro_terr_5_yr$`20200703_NDVI` %>% raster::raster() %>% mapview() 


#okay, so my workflow is going to be:
#1. download data from rgee as a rasterstack, i.e., using the raster package
#2. convert to terra using terra:rast()
#3. save to disk using the terra package's writeRaster function above
#4. read terra::rast()
#5. if you want to use mapview, convert back to raster using raster::raster()

# Determine how to remove bad images--------------
#I'm going to pick a few spots where I know the true NDVI should be pretty high, and then if it's
#low on that day, it means it's cloudy.
## Areas to pick------------
### Evergreen East----------------------
#The area east of Evergreen is consistently very green. Create a polygon and exclude days
#where the NDVI is low there.

#SE corner 39.598319, -105.243563
#NE corner 39.63442095463794, -105.2529061959742
#NW corner 39.63423901448882, -105.28947594683657
bbox_evergreen_east = st_bbox(
  c(
      xmin = -105.28947594683657, #westmost
      ymin = 39.598319, 
      xmax = -105.2529061959742, #eastmost
      ymax = 39.63423901448882
  ),
  crs=st_crs(4326)
  ) %>% 
  sf::st_as_sfc() 
  
mv_good_date =ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% raster::raster() %>%  
  mapview(layer.name = "NDVI") 
mv_bbox_evergreen_east = bbox_evergreen_east %>% mapview(layer.name = "evergreen_east")
mv_good_date+mv_bbox_evergreen_east

#test this method
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")
ndvi_den_metro_terr_5_yr_evergreen_east = ndvi_den_metro_terr_5_yr %>% 
  terra::crop(bbox_evergreen_east) %>%
  terra::values() %>%  #this just pulls the values out %>% 
  as_tibble() %>% 
  #make long form and summarize by date
  pivot_longer( 
    cols = contains("20"),#contains a year that begins with 20..flexible
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(
    date_fixed_char = substr(date_ndvi, 1,8), #extract the date
    date = lubridate::as_date(date_fixed_char),
    test_place_name = "evergreen_east"
  ) %>% 
  dplyr::select(-date_ndvi) %>% 
  arrange(date)

ndvi_den_metro_terr_5_yr_evergreen_east_day= ndvi_den_metro_terr_5_yr_evergreen_east %>% 
  group_by(date) %>% 
  summarise(ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(
    month = lubridate::month(date),
    year=lubridate::year(date)
  )

## 
library(scales)
ndvi_den_metro_terr_5_yr_evergreen_east_day %>% filter(year==2019) %>% View()
ndvi_den_metro_terr_5_yr_evergreen_east_day %>% 
  ggplot(aes(
    x=date, 
    y=ndvi_mean  
  ))+
  geom_line()+
  geom_point()+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ndvi_den_metro_terr_5_yr_evergreen_east_day %>% 
  filter(ndvi_mean < .2) %>% 
  View()

#test some that appear problematic per evergreen east
ndvi_den_metro_terr_5_yr$`20160422_NDVI`  %>% raster::raster() %>% mapview()#maybe fine?
ndvi_den_metro_terr_5_yr$`20160414_NDVI`  %>% raster::raster() %>% mapview() #definitely bad.
ndvi_den_metro_terr_5_yr$`20160406_NDVI`  %>% raster::raster() %>% mapview() #very cloudy.
ndvi_den_metro_terr_5_yr$`20160406_NDVI`  %>% raster::raster() %>% mapview()  
ndvi_den_metro_terr_5_yr$`20200508_NDVI` %>% raster::raster() %>% mapview() #bad.
#conclusion: can be pretty confident that anything below 0.1 is bad.
  
### Indian Tree Golf Club in Arvada---------------
#SE corner 39.832660820244314, -105.08522614674321
#SW corner 39.8321149314402, -105.08906107810273
#north coordinate 39.83484433207773, -105.08704072402064
bbox_indian_tree_golf= st_bbox(
  c(
    xmin = -105.08906107810273, #westmost
    ymin = 39.832660820244314, #southernmost
    xmax = -105.08522614674321, #eastmost
    ymax = 39.83484433207773 #northernmost
  ),
  crs=st_crs(4326)
) %>% 
  sf::st_as_sfc() 

bbox_indian_tree_golf %>% mapview()

ndvi_den_metro_terr_5_yr_indian_tree_golf = ndvi_den_metro_terr_5_yr %>% 
  terra::crop(bbox_indian_tree_golf) %>%
  terra::values() %>%  #this just pulls the values out %>% 
  as_tibble() %>% 
  #make long form and summarize by date
  pivot_longer( 
    cols = contains("20"),#contains a year that begins with 20..flexible
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(
    date_fixed_char = substr(date_ndvi, 1,8), #extract the date
    date = lubridate::as_date(date_fixed_char),
    test_place_name = "indian_tree_golf"
  ) %>% 
  arrange(date)

ndvi_den_metro_terr_5_yr_indian_tree_golf= ndvi_den_metro_terr_5_yr_indian_tree_golf %>% 
  group_by(date) %>% 
  summarise(ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(
    month = lubridate::month(date),
    year=lubridate::year(date)
  )

ndvi_den_metro_terr_5_yr_indian_tree_golf %>% 
  ggplot(aes(
    x=date, 
    y=ndvi_mean  
  ))+
  geom_line()+
  geom_point()+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#borderline cases between 0.3 and 0.5 and in summer
ndvi_den_metro_terr_5_yr_indian_tree_golf %>% 
  filter(ndvi_mean>0.3 & ndvi_mean<0.5) %>% 
  filter(month>=5&month<=8)

#check these 6. if they're all bad, then can confidently exclude.
ndvi_den_metro_terr_5_yr$`20160516_NDVI` %>% raster::raster() %>% mapview() #for sure cloudy...exclude
ndvi_den_metro_terr_5_yr$`20170805_NDVI` %>% raster::raster() %>% mapview()
      #denver itself v cloudy...maybe some areas are valid but can't trust the whole image
ndvi_den_metro_terr_5_yr$`20190602_NDVI` %>% raster::raster() %>% mapview()
    #east not bad but denver and golden very cloudy...exclude
ndvi_den_metro_terr_5_yr$`20190610_NDVI` %>% raster::raster() %>% mapview()
  #partly cloudy...maybe the most borderline of the bunch...but probably exclude
ndvi_den_metro_terr_5_yr$`20160406_NDVI`  %>% raster::raster() %>% mapview()  
