#filename: 2_summarize_ndvi_specific_places
#the processing steps once data are downloaded and converted into raster in previous code
#note the previous code takes a while

#Revised 1/19/22
library(tidyverse)
library(sf)
library(mapview)
library(terra)
library(raster)
library(here)
library(viridisLite)
library(viridis)
library(shades)
library(ggmap)

# Load data--------------
## Load specific places provided by Michael Guidi----------
#Note these were imported as kml and converted to sf in a separate script:
#0_read_denver_native_zones.R
setwd(here("data-processed"))
load("places_native_geo.RData") #sent from Michael Guidi

places_native_geo %>% mapview(zcol = "place_name")
## Read raster data-------------
#i.e., load the data
setwd(here("data-processed"))
#read raster data. 
#see for additional discussion on the nuances of reading and writing raster data:
#1_get_landsat_ndvi_denver.R
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")


# Measure NDVI in the specific places-------------
load("places_native_nogeo.RData")
places_native_nogeo
library(lubridate)
library(stringr)
## create long-form dataset of NDVI by place over time--------
native_places_ndvi = ndvi_den_metro_terr_5_yr %>% 
  #note because it's a stack, this does it for many days
  terra::extract(vect(places_native_geo)) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = contains("20"),#contains a year that begins with 20..flexible 
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(   #now fix that date column using substr
    date_fixed_char = substr(date_ndvi, 1,8),
    date = lubridate::as_date(date_fixed_char)
  ) %>% 
  rename(
    place_id = ID) %>% #for left join below
  dplyr::select(place_id, ndvi, date) %>% 
  #link in the place names
  left_join(places_native_nogeo, by = "place_id")

native_places_ndvi %>% 
  filter(date_is_valid==1) %>% 
  dplyr::select(ndvi, date)

native_places_ndvi %>% 
  filter(date_is_valid==1) %>% 
  ggplot(aes(x=date, y=ndvi))+
  geom_line()+
  geom_point()+
  facet_grid(cols = vars(place_name))

lookup_date_is_valid = native_places_ndvi %>% 
  distinct(date_is_valid, date)

## Summarize NDVI (mean, min, max, etc.) over time for each place-------

native_places_ndvi_day_geo = native_places_ndvi %>% 
  group_by(place_id, date) %>% 
  summarise(
    ndvi_min = min(ndvi, na.rm=TRUE),
    ndvi_max = max(ndvi, na.rm=TRUE),
    ndvi_25 = quantile(ndvi, probs = c(0.25), na.rm=TRUE),
    ndvi_75 = quantile(ndvi, probs = c(0.75), na.rm=TRUE),
    ndvi_50 = median(ndvi, na.rm=TRUE),
    ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(places_native_geo, by = "place_id") %>%    #link in the place names
  left_join(lookup_date_is_valid, by = "date") %>% #valid dates
  st_as_sf()

#test
pal_viridis_trunc=viridis::viridis_pal(end=.9) #trunc for truncated
native_places_ndvi_day_geo %>% 
  filter(date == "2021-05-25") %>% 
  mapview(
    layer.name = "NDVI, Median",
    col.regions = pal_viridis_trunc,
    zcol = "ndvi_50" 
  )

## ggplot - line graph of NDVI over time with median, 25th, and 75th as ribbon by area-------
library(scales)
native_places_ndvi_day_geo %>% 
  filter(date_is_valid==1) %>% 
  ggplot(aes(
    x=date, 
    y=ndvi_50 #plot median
  ))+
  geom_ribbon( #ribbon around 25th and 75th percentile
    aes(ymin =ndvi_25, ymax = ndvi_75 ),
    alpha=.4
  )+
  ylab("NDVI, Median") +
  xlab("Date") +
  geom_line( size=.7 ) +#note size better than lwd
  geom_point()+
  scale_x_date(labels=date_format("%Y-%m-%d"), date_breaks = "2 weeks") +
  scale_y_continuous(
    limits = c(0, NA),  #force axis origin to be zero
    breaks= seq(0,0.8,by = 0.1))+ 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.border = element_rect(
          colour = "gray72", size = 0.5, fill=NA))+
  facet_grid(   #now facet them
    cols = vars(place_name_fac),
    labeller = label_wrap_gen() #wrap facet labels
  )
  
  

## ggplot - NDVI vs percent native over all dates---------
native_places_ndvi_day_geo %>% 
  filter(date_is_valid==1) %>% 
  ggplot(aes(
    x=native_percent, 
    y=ndvi_50 #plot median
  ))+
  geom_point(aes(colour=place_name_fac), size = 2) +
  xlab("Percent native") +
  ylab("NDVI, median") +
  scale_y_continuous(
    limits = c(0, NA),  #force axis origin to be zero
    breaks= seq(0,0.8,by = 0.1))+ 
  scale_color_hue(
    name = "Place name"
  )
  

den_jeff_co_green_space_ndvi_day_geo = den_jeff_co_green_space_ndvi_long %>% 
  group_by(park_id_row_number, date) %>% 
  summarise(
    ndvi_min = min(ndvi, na.rm=TRUE),
    ndvi_max = max(ndvi, na.rm=TRUE),
    ndvi_median = median(ndvi, na.rm=TRUE),
    ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #link the fips code. this also has tract-level geometry
  left_join(den_jeff_co_green_space_public_no_water_w_extract_id, by = "park_id_row_number") %>% 
  st_as_sf() %>% #it has geometry. just make it so.
  dplyr::select(contains("id_row_nu"), contains("ndvi"), date) 


## Map areas against one month of NDVI-----------
#valid dates include 5/25, 6/2, 6/10, 7/4
pal_viridis_trunc=viridis::viridis_pal(end=.9) #trunc for truncated
mv_ndvi_lsat_den_jeff_co_rast_20210610 =ndvi_lsat_den_jeff_co_rast$X20210610_NDVI %>% 
  mapview(
    maxpixels =  4863888,
    col.regions = pal_viridis_trunc,
    layer.name = "NDVI, 2021-06-10"
  )

mv_places_native = places_native_geo %>% 
  mapview(
    color = "black",
    lwd=2,
    col.regions = "red",
    alpha.regions = .5,
    layer.name = "Places")

mv_ndvi_lsat_den_jeff_co_rast_20210610 + mv_places_native
# Buffer areas...not current focus-----------

## around botanic gardens point of various sizes ----------------
den_botanic = as_tibble("Denver Botanic Gardens") %>% 
  rename(address=value)

den_botanic_sf = mutate_geocode(den_botanic, address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

#add a buffer of various sizes.
den_botanic_buff_30_m = den_botanic_sf %>% 
  st_buffer(30)

den_botanic_buff_100_m = den_botanic_sf %>% 
  st_buffer(100)

den_botanic_buff_500_m = den_botanic_sf %>% 
  st_buffer(500)

den_botanic_buff_1000_m = den_botanic_sf %>% 
  st_buffer(1000)

mapview(den_botanic_buff_1000_m)

#create a mapview of the border
mv_den_botanic_native_100 = den_botanic_native_100 %>% 
  mapview(
    layer.name = "100% native zone, Botanic Gardens",
    color = "black",
    col.regions = "black",
    alpha.regions = 0,
    lwd=2
    
  )
mv_den_botanic_native_100

## north table mountain-------------
north_table = as_tibble("North Table Mountain, Golden CO") %>% 
  rename(address=value)

north_table_sf = mutate_geocode(north_table, address, force = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 

north_table_1000_m = north_table_sf %>% 
  st_buffer(1000)

north_table_2000_m = north_table_sf %>% 
  st_buffer(2000)

ndvi_north_table_1000_m_cropped_rast = ndvi_lsat_den_jeff_co_rast %>% 
  raster::crop(north_table_1000_m)

ndvi_north_table_2000_m_cropped_rast = ndvi_lsat_den_jeff_co_rast %>% 
  raster::crop(north_table_2000_m)

mv_ndvi_north_table_2000_m_cropped_rast_20210610 = ndvi_north_table_2000_m_cropped_rast$X20210610_NDVI %>% 
  mapview(
    col.regions = pal_terrain_col ,
    layer.name = "NDVI"
  )

#visualize
mv_ndvi_north_table_2000_m_cropped_rast_20210610 


