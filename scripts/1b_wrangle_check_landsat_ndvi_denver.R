#filename: 1b_wrangle_check_landsat_ndvi_denver

#The purpose of this is to wrangle and check the NDVI data loaded in the previous script:
library(tidyverse)
library(terra)
library(raster)
library(mapview)
library(sf)
library(here)
library(scales)
library(lubridate)
# Read the 5-year terra raster file--------
# Note this is different from the usual way I'd load a file.
#You are loading it from disc using terra::rast()
setwd(here("data-processed"))
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")
ndvi_den_metro_terr_5_yr

# visualize dates using the raster version to check-------
#check on cloud cover, etc.
ndvi_den_metro_terr_5_yr$`20210805_NDVI` %>% 
  raster::raster() %>% 
  mapview()

ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  raster::raster() %>% 
  mapview() #this is a good image and could be used as a guide

#alternate color palette to more easily find zeros
pal = terrain.colors(100) %>% rev()#reverse the order of the palette
ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  raster::raster() %>% 
  mapview(layer.name = "NDVI",
          col.regions = pal, at = seq(-0.4, 1, 0.1)
          ) 


ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% values()
ndvi_den_metro_terr_5_yr$`20210626_NDVI` %>% 
  raster::raster() %>% 
  mapview() #mostly cloudy. toss.
ndvi_den_metro_terr_5_yr$`20210712_NDVI` %>% 
  raster::raster() %>% 
  mapview() #all very cloudy
ndvi_den_metro_terr_5_yr$`20210720_NDVI` %>% 
  raster::raster() %>% 
  mapview() #partly cloudy. would probably toss.
ndvi_den_metro_terr_5_yr$`20210728_NDVI` %>% 
  raster::raster() %>% 
  mapview() 

#Note data are more missing in 2020...less frequent for some reason.
ndvi_den_metro_terr_5_yr$`20200101_NDVI`  %>% 
  raster::raster() %>% 
  mapview() #data available
ndvi_den_metro_terr_5_yr$`20200109_NDVI` %>% 
  raster::raster() %>% 
  mapview() #missing
ndvi_den_metro_terr_5_yr$`20200703_NDVI` %>% 
  raster::raster() %>% 
  mapview() 


#okay, so workflow:
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
    group_by(date, test_place_name) %>% #include place name just to keep the var around
    summarise(
      ndvi_mean = mean(ndvi, na.rm=TRUE),
      ndvi_var = var(ndvi, na.rm=TRUE),
      ndvi_sd = sd(ndvi, na.rm=TRUE)
      ) %>% 
    ungroup() %>% 
    mutate(
      month = lubridate::month(date),
      year=lubridate::year(date)
    )
}

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

#save so can re-use
setwd(here("data-processed"))
save(bbox_evergreen_east, file = "bbox_evergreen_east.RData")
mv_good_date =ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  raster::raster() %>%  
  mapview(layer.name = "NDVI")
mv_bbox_evergreen_east = bbox_evergreen_east %>% 
  mapview(layer.name = "evergreen_east")
mv_good_date+mv_bbox_evergreen_east


#summarize NDVI in the Evergreen East area
ndvi_den_metro_terr_5_yr_evergreen_east = ndvi_den_metro_terr_5_yr %>% 
  terra::crop(bbox_evergreen_east) %>%
  pivot_longer_fix_date() %>% 
  mutate(test_place_name = "evergreen_east") 

ndvi_den_metro_terr_5_yr_evergreen_east_day= ndvi_den_metro_terr_5_yr_evergreen_east %>% 
  group_by_date_summarise_ndvi()

#ggplot over time
ndvi_den_metro_terr_5_yr_evergreen_east_day %>% 
  ggplot_ndvi_over_time()

# ndvi_den_metro_terr_5_yr_evergreen_east_day %>% 
#   filter(ndvi_mean < .2) %>% 
#   View()

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

setwd(here("data-processed"))
save(bbox_indian_tree_golf, file = "bbox_indian_tree_golf.RData")

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
ndvi_den_metro_terr_5_yr_indian_tree_golf_day %>% 
  filter(ndvi_mean>0.3 & ndvi_mean<0.5) %>% 
  filter(month>=5&month<=8)

#check these 6. if they're all bad, then can confidently exclude.
ndvi_den_metro_terr_5_yr$`20160516_NDVI` %>% raster::raster() %>% 
  mapview() #for sure cloudy...exclude
ndvi_den_metro_terr_5_yr$`20170805_NDVI` %>% raster::raster() %>% 
  mapview()
#denver itself v cloudy...maybe some areas are valid but can't trust the whole image
ndvi_den_metro_terr_5_yr$`20190602_NDVI` %>% raster::raster() %>% 
  mapview()
#east not bad but denver and golden very cloudy...exclude
ndvi_den_metro_terr_5_yr$`20190610_NDVI` %>% raster::raster() %>% 
  mapview()
#partly cloudy...maybe the most borderline of the bunch...but probably exclude
ndvi_den_metro_terr_5_yr$`20160406_NDVI`  %>% raster::raster() %>% 
  mapview()  

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

setwd(here("data-processed"))
save(bbox_city_park, file = "bbox_city_park.RData")

bbox_city_park %>% mapview()

ndvi_den_metro_terr_5_yr_city_park = ndvi_den_metro_terr_5_yr %>% 
  terra::crop(bbox_city_park) %>%
  pivot_longer_fix_date() %>% 
  mutate(test_place_name = "city_park")

ndvi_den_metro_terr_5_yr_city_park_day= ndvi_den_metro_terr_5_yr_city_park %>% 
  group_by_date_summarise_ndvi() 

ndvi_den_metro_terr_5_yr_city_park_day %>% 
  ggplot_ndvi_over_time()


#test on a good day per the raster above
ndvi_den_metro_terr_5_yr_city_park_day %>% 
  filter(date== "2021-07-04") #yup, very high.

## rbind test places together------
#so you can visualize them all to get a sense for the consistency
#of the weird measurements
ndvi_test_places_day = ndvi_den_metro_terr_5_yr_evergreen_east_day %>% 
  bind_rows(
    ndvi_den_metro_terr_5_yr_indian_tree_golf_day,
    ndvi_den_metro_terr_5_yr_city_park_day
  ) %>% 
  #note my tolerance for including bad data is lower than my tolerance
  #for excluding good data. That is, I prefer specificity.
  
  #0.35 seems reasonable for evergreen_east, but it's a tough call.
  mutate(
    month_summer = case_when(
      month>=6 & month <=8 ~1,
      TRUE ~0),
    month_summer_include_may = case_when(
      month>=5 & month <=8 ~1,
      TRUE ~0)    ,
    valid_city_park = case_when(
      month_summer_include_may == 1 &
        test_place_name == "city_park" & 
        ndvi_mean>0.6 ~1,
      TRUE ~0),
    
    valid_indian_tree_golf = case_when(
      month_summer_include_may == 1 &
        test_place_name == "indian_tree_golf" &
        ndvi_mean>0.6 ~1,
      TRUE ~0),
    
    valid_evergreen_east = case_when(
      month_summer_include_may == 1 &
        test_place_name == "evergreen_east" & 
        ndvi_mean>0.35 ~1,
      TRUE ~0)
  )

#check
table(ndvi_test_places_day$valid_indian_tree_golf)
table(ndvi_test_places_day$valid_city_park)
table(ndvi_test_places_day$valid_evergreen_east)
#make little datasets for each...the meaning of the var will change.
date_when_valid_evergreen_east = ndvi_test_places_day %>% 
  distinct(date, valid_evergreen_east) 
date_when_valid_city_park = ndvi_test_places_day %>% 
  distinct(date, valid_city_park) 
date_when_valid_indian_tree_golf = ndvi_test_places_day %>% 
  distinct(date, valid_indian_tree_golf)

## wrangle valid dates------
#now link these. there is probably a more elegant way...using pivot_wider maybe,
#but this will work. we are changing the meaning of the valid variables slightly
#so they apply at the date level
ndvi_test_places_day_wrangle = ndvi_test_places_day %>% 
  dplyr::select(-starts_with("valid_")) %>% 
  left_join(date_when_valid_evergreen_east, by = "date") %>% 
  left_join(date_when_valid_city_park, by = "date") %>% 
  left_join(date_when_valid_indian_tree_golf, by = "date") %>% 
  mutate(
    date_is_valid_all = case_when(
      valid_evergreen_east==1 & 
      valid_city_park==1 &
      valid_indian_tree_golf==1 ~ 1,
    TRUE ~0
  ))

setwd(here("data-processed"))
save(ndvi_test_places_day_wrangle, file = "ndvi_test_places_day_wrangle.RData")


ndvi_test_places_day_wrangle %>% 
  group_by(date_is_valid_all) %>% 
  summarise(n=n())

### Save valid dates--------------
lookup_date_is_valid_all =ndvi_test_places_day_wrangle %>% 
  distinct(date, date_is_valid_all)

save(lookup_date_is_valid_all, file = "lookup_date_is_valid_all.RData")
lookup_date_is_valid_all %>% 
  group_by(date_is_valid_all) %>% 
  summarise(n=n())
  #40 days over the five years.

#save this. this is an indicator for valid dates over the five years.
setwd(here("data-processed"))
save(date_when_valid_all, file = "date_when_valid_all.RData")

### visualize by site name-----------
ndvi_test_places_day_wrangle %>% 
  ggplot(
    aes(
      x=date, 
      y=ndvi_mean  
    ))+
  geom_line(aes(colour = test_place_name))+
  geom_point(aes(colour = test_place_name))+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

### visualize all three during summer months only (including may)----------
ndvi_test_places_day %>% 
  filter(month>=5 & month <=8) %>%
  ggplot(
    aes(
      x=date, 
      y=ndvi_mean  
    ))+
  geom_line(aes(colour = test_place_name))+
  geom_point(aes(colour = test_place_name))+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
  scale_y_continuous(n.breaks = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#it looks like anything below 0.6 for city park and indian tree is invalid and anything
#below .35 for evergreen east is invalid

#what happens if we restrict to city park above 0.6. Do we get invalid results for
#the other two zones?
dates_summer_when_ndvi_valid = ndvi_den_metro_terr_5_yr_city_park_day %>% 
  filter(month>=5 & month <=8) %>% 
  filter(ndvi_mean>0.60) %>% 
  mutate(city_park_above_06=1) %>% 
  distinct(date, city_park_above_06)

#link that to the dataset with all three, restrict to those dates, and visualize
ndvi_test_places_day %>% 
  left_join(dates_summer_when_ndvi_city_park_above_06, by = "date") %>% 
  filter(city_park_above_06==1) %>% 
  ggplot(
    aes(
      x=date, 
      y=ndvi_mean  
    ))+
  geom_line(aes(colour = test_place_name))+
  geom_point(aes(colour = test_place_name))+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#visualize using the new variables
ndvi_test_places_day_wrangle %>% 
  filter(date_is_valid_all==1) %>% 
  ggplot(
    aes(
      x=date, 
      y=ndvi_mean  
    ))+
  geom_line(aes(colour = test_place_name))+
  geom_point(aes(colour = test_place_name))+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 months")+
  scale_y_continuous(n.breaks = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


  
  

