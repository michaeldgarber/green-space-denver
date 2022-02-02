#filename: 1b_wrangle_check_landsat_ndvi_denver

#The purpose of this is to wrangle and check the NDVI data loaded in this script:
library(tidyverse)
library(terra)
library(mapview)
library(sf)


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

ndvi_den_metro_terr_5_yr
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

# Determine how to remove bad (cloudy) images--------------
#I'm going to pick a few spots where I know the true NDVI should be pretty high, 
#and then if it's low on that day, it means it's cloudy.
#--------------Areas to pick:------------#

## Define a couple functions that are used throughout------
#create a function since this is repeated at least 3x
pivot_longer_fix_date = function(df){
  df %>% 
    terra::values() %>%  #this just pulls the values out from the SpatRaster file
    as_tibble() %>% 
    pivot_longer(     #make long form and summarize by date
      cols = contains("20"),#contains a year that begins with 20..flexible
      names_to = "date_ndvi",
      values_to = "ndvi"
    ) %>% 
    mutate(
      date_fixed_char = substr(date_ndvi, 1,8), #extract the date
      date = lubridate::as_date(date_fixed_char)
    ) %>% 
    dplyr::select(-date_ndvi) %>% 
    arrange(date)
}

group_by_date_summarise_ndvi = function(df) {
  df %>% 
    group_by(date) %>% 
    summarise(ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate(
      month = lubridate::month(date),
      year=lubridate::year(date)
    )
}

library(lubridate)
library(scales)
ggplot_ndvi_over_time = function(df){
  df %>% 
    ggplot(
      aes(
        x=date, 
        y=ndvi_mean  
      ))+
    geom_line()+
    geom_point()+
    scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}

## Evergreen East----------------------
#The area east of Evergreen is consistently very green.
#Create a polygon and exclude days
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
  pivot_longer_fix_date() %>% 
  mutate(test_place_name = "evergreen_east") 

ndvi_den_metro_terr_5_yr_evergreen_east_day= ndvi_den_metro_terr_5_yr_evergreen_east %>% 
  group_by_date_summarise_ndvi()

## 
library(scales)
ndvi_den_metro_terr_5_yr_evergreen_east_day %>% filter(year==2019) %>% View()

#ggplot over time
ndvi_den_metro_terr_5_yr_evergreen_east_day %>% 
  ggplot(
    aes(
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

## Indian Tree Golf Club in Arvada---------------
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
  pivot_longer_fix_date() %>% 
  mutate(test_place_name = "indian_tree_golf")

ndvi_den_metro_terr_5_yr_indian_tree_golf_day= ndvi_den_metro_terr_5_yr_indian_tree_golf %>% 
  group_by_date_summarise_ndvi()

ndvi_den_metro_terr_5_yr_indian_tree_golf_day %>% 
  ggplot_ndvi_over_time()

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

## City Park (downtown Denver)-------------
#using the southwest corner...not the golf course part or any of the fountains
#SW corner 39.74451423794387, -104.957913933541
#SE corner 39.74549373003514, -104.9561144255883
#NE corner 39.746296514307176, -104.95634095869859
#NW corner 39.74627741072699, -104.95626462928261

bbox_city_park= st_bbox(
  c(
    xmin = -104.957913933541, #westmost
    ymin = 39.74549373003514, #southernmost
    xmax = -104.95634095869859, #eastmost
    ymax = 39.74627741072699 #northernmost
  ),
  crs=st_crs(4326)
) %>% 
  sf::st_as_sfc() 

bbox_city_park %>% mapview()

ndvi_den_metro_terr_5_yr_city_park = ndvi_den_metro_terr_5_yr %>% 
  terra::crop(bbox_city_park) %>%
  pivot_longer_fix_date() %>% 
  mutate(test_place_name = "city_park")

ndvi_den_metro_terr_5_yr_city_park_day= ndvi_den_metro_terr_5_yr_city_park %>% 
  group_by_date_summarise_ndvi()


#test on a good day
#ndvi_den_metro_terr_5_yr_city_park$`20210704_NDVI` %>% raster::raster() %>% mapview() #all very high.
