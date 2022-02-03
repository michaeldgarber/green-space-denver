#filename: 2_summarize_ndvi_tracts_parks_den

#The goal of this script is to link the landsat raster file with the ACS census-tract data to begin to characterize
#census tracts by NDVI
#And do the same for the parks and public green spaces

library(here)
library(tidyverse)
library(terra)
library(raster)
library(mapview)
library(sf)

# Read raster data------------
setwd(here("data-processed"))
getwd()
#read raster data. see for additional discussion on the nuances of reading and writing raster data
#1_get_landsat_ndvi_denver.R
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")


# Read census tract and OSM data---------####
load("den_jeff_co_tracts_no_water_geo.RData")

# census tract NDVI----------
## Summarize NDVI by census tract-------------
den_metro_ndvi_long_int =ndvi_den_metro_terr_5_yr %>% 
  #this creates a data frame with a row id for each intersecting polygon, 
  #presumably sorted by their order of appearance
  #note have to convert to vector first
  #note I am using the den and jeff co county version only and I removed water
  terra::extract(terra::vect(den_jeff_co_tracts_no_water_geo)) %>% #have to convert to vector first
  as_tibble() 

#takes a while (about 10 mins) so might as well save

save(den_metro_ndvi_long_int, file = "den_metro_ndvi_long_int.RData") #saving also takes about 5 mins.
object.size(den_metro_ndvi_long_int)

#for interactive coding, separate because the first step takes so long. eventually combine
den_metro_tracts_ndvi_long = den_metro_ndvi_long_int %>% 
  pivot_longer( #make long form
    cols = contains("20"),#contains a year that begins with 20..flexible
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(
    date_fixed_char = substr(date_ndvi, 1,8), #extract the date
    date = lubridate::as_date(date_fixed_char)
  ) %>% 
  rename(
    tract_id_row_number = ID, #make it less ambiguous
    ) %>% 
  dplyr::select(tract_id_row_number, ndvi, date) 

den_metro_tracts_ndvi_long
nrow(den_metro_tracts_ndvi_long)
#create an ID that will link to the extracted values (row number)
den_jeff_co_tracts_geo_w_extract_id = den_jeff_co_tracts_no_water_geo %>% 
  st_transform(4326) %>% 
  mutate( tract_id_row_number=row_number()) 


den_jeff_co_tracts_geo_w_extract_id 
load("den_jeff_co_nogeo.RData")
den_metro_tracts_ndvi_day_geo = den_metro_tracts_ndvi_long %>% 
  #naming is hard. but calling this _day to indicate a daily summary.
  group_by(tract_id_row_number, date) %>% 
  summarise(
    ndvi_min = min(ndvi, na.rm=TRUE),
    ndvi_max = max(ndvi, na.rm=TRUE),
    ndvi_median = median(ndvi, na.rm=TRUE),
    ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #link the fips code. this also has tract-level geometry
  left_join(den_jeff_co_tracts_geo_w_extract_id, by = "tract_id_row_number") %>% 
  #link the county information
  left_join(den_jeff_co_nogeo, by = "county_fips") %>% 
  st_as_sf() #it has geometry. just make it so.

den_metro_tracts_ndvi_day_geo 
## visualize census tracts NDVI ---------
### define color palettes------------------
pal_terrain_col = rev(terrain.colors(568)) #mapview below has 586 colors
pal_terrain_col #it just interpolates by repeating.
#use viridis instead. more flexible. annoying to pick an exact number.
pal_viridis_trunc=viridis::viridis_pal(end=.9) #trunc for truncated
table(den_metro_tracts_ndvi_day_geo$county_name_short)
### may 25, 2021-------
den_metro_tracts_ndvi_day_geo %>% 
  filter(date == "2021-05-25") %>% 
  filter(
    county_name_short == "Jefferson"| 
      county_name_short == "Denver") %>% 
  mapview(
    layer.name = "NDVI, Median",
     col.regions = pal_viridis_trunc,
    zcol = "ndvi_median" 
    )

#valid dates include 5/25, 6/2, 6/10, 7/4
### june 10 2021----------
den_metro_tracts_ndvi_day_geo %>% 
  filter(date == "2021-06-10") %>% 
  filter(
    county_name_short == "Jefferson"| 
      county_name_short == "Denver") %>% 
  mapview(
    layer.name = "NDVI, Median",
    col.regions = pal_viridis_trunc,
    zcol = "ndvi_median" 
  )

# green space NDVI----
## Summarize NDVI of parks and public green space------
load("den_jeff_co_green_space_public_no_water.RData")
den_jeff_co_green_space_public_no_water
den_metro_green_space_ndvi_long_int =ndvi_den_metro_terr_5_yr %>% 
  terra::extract(terra::vect(den_jeff_co_green_space_public_no_water)) %>% #have to convert to vector first
  as_tibble() 

#takes a while so save.
save(den_metro_green_space_ndvi_long_int, 
     file = "den_metro_green_space_ndvi_long_int.RData")
#for interactive coding, separate because the first step takes so long. eventually combine
den_metro_green_space_ndvi_long = den_metro_green_space_ndvi_long_int %>% 
  pivot_longer( #long form
    cols = contains("20"),#contains a year that begins with 20..flexible enough as other code doesn't have the X
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(
    date_fixed_char = substr(date_ndvi, 1,8), #different from the other one because this one begins with the year
    date_fixed_date = lubridate::as_date(date_fixed_char)
  ) %>% 
  rename(
    date = date_fixed_date,
    park_id_row_number = ID, #make it less ambiguous
  ) %>% 
  dplyr::select(park_id_row_number, ndvi, date) 

#create an ID that will link to the extracted values (row number)
den_jeff_co_green_space_public_no_water_w_extract_id = den_jeff_co_green_space_public_no_water %>% 
  st_transform(4326) %>% 
  mutate(park_id_row_number=row_number()) 

den_metro_green_space_ndvi_day_geo = den_metro_green_space_ndvi_long %>% 
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

## visualize NDVI of parks and green space------------
load("den_jeff_co_geo.RData")
mv_den_jeff_co_geo = den_jeff_co_geo %>%  
  mapview(
    layer.name = "County name",
    color = c("red", "orange"),
    col.regions = c("red", "orange"),
    lwd=2,
    zcol = "county_name_short", alpha.regions = 0)
mv_den_metro_green_space_ndvi_day_geo=den_metro_green_space_ndvi_day_geo %>% 
  filter(date == "2021-05-25") %>% 
  mapview(
    layer.name = "NDVI, Median",
    col.regions = pal_viridis_trunc,
    zcol = "ndvi_median" 
  )
mv_den_metro_green_space_ndvi_day_geo+mv_den_jeff_co_geo

# Filter out cloudy images--------
#Some people use 0.1 as a threshold, but what about water?
#https://www.neonscience.org/resources/learning-hub/tutorials/dc-ndvi-calc-raster-time-series