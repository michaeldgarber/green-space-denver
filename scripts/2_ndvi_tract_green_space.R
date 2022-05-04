#filename: 2_summarize_ndvi_tract_parks_den

#The goal of this script is to link the landsat raster file with the ACS census-tract data to 
#begin to characterize census tracts by NDVI
#And do the same for the parks and public green spaces

#I had previously included the water-removal code in this script, but I've moved it to:
#~/scripts/1_remove_water_tract_bg_park.R

# Revised and re-ran May 3, 2022
#changed from characterizing NDVI of full metro area tracts to just that of Denver County tracts,
#as it took too long.

library(tidyverse)
library(terra)
library(mapview)
library(sf)
library(here)
setwd(here("data-processed"))
load("den_metro_tract_no_wtr_geo.RData")

# Read in (load) data raster-------

setwd(here("data-processed"))
getwd()
#read raster data. see for additional discussion on the nuances of reading and writing raster data
#created and managed in these two scripts: 
#~/1a_get_landsat_ndvi_denver.R
#~/1b_wrangle_check_landsat_ndvi_denver.R
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")

# Compare raster bounding box with administrative boundaries
load("den_metro_bbox_custom_sf.RData") #load bounding box
names(den_metro_tract_no_wtr_geo)
mv_den_metro_tract_no_wtr_geo  = den_metro_tract_no_wtr_geo   %>% 
  mapview(zcol = "county_fips", layer.name = "tracts")

#check coverage of these census tracts compared with the bounding box
mv_den_metro_bbox_custom = den_metro_bbox_custom_sf %>% 
  mapview(col.regions = "coral", layer.name = "bbox")
mv_den_metro_tract_no_wtr_geo + mv_den_metro_bbox_custom


#from scripts/1b_wrangle_check_landsat_ndvi_denver.R
load("lookup_date_is_valid_all.RData")
lookup_date_is_valid_all


# Summarize NDVI by census tract for 5 years
## Extract NDVI from census tracts
#convert to 4326 because the raster is in 4326
st_crs(ndvi_den_metro_terr_5_yr)
st_crs(den_metro_tract_no_wtr_geo)
#update May 3 2022
#Because the full metro area takes forever and sometimes doesn't run, I'm limiting
#this to Denver County only.

den_co_tract_no_wtr_4326 = den_metro_tract_no_wtr_geo %>% 
  #filter to denver county only
  filter(county_fips == "031") %>% 
  st_transform(4326) 

mv_ndvi_layer = ndvi_den_metro_terr_5_yr$`20200101_NDVI`%>% 
  raster::raster() %>% 
  mapview(layer.name = "ndvi") 

mv_den_co_tract_no_wtr_4326 =  den_co_tract_no_wtr_4326 %>% 
  mapview(layer.name = "tracts")
mv_ndvi_layer + mv_den_co_tract_no_wtr_4326


#note this step is super slow because it's 5 years of data
den_co_tract_ndvi_long =ndvi_den_metro_terr_5_yr  %>% 
  #this creates a data frame with a row id for each intersecting polygon, 
  #presumably sorted by their order of appearance
  #note have to convert to vector first
  #must convert to the terra package's version of vector first
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    y =  terra::vect(den_co_tract_no_wtr_4326)) %>% 
  as_tibble() %>% 
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

#don't save this. too huge.
nrow(den_co_tract_ndvi_long)# wow. almost a billion observations!
nrow(den_co_tract_ndvi_long)

#create an ID that will link to the extracted values (row number)
den_co_tract_geo_w_extract_id = den_co_tract_no_wtr_4326 %>% 
  st_transform(4326) %>% 
  mutate(tract_id_row_number=row_number()) 

den_co_tract_geo_w_extract_id 
load("den_metro_co_nogeo.RData")

## Census tract NDVI by day------------
den_co_tract_ndvi_day_geo = den_co_tract_ndvi_long %>% 
  #naming is difficult, but let's call this _day to indicate a daily summary.
  group_by(tract_id_row_number, date) %>% 
  summarise(
    ndvi_min = min(ndvi, na.rm=TRUE),
    ndvi_max = max(ndvi, na.rm=TRUE),
    ndvi_25 = quantile(ndvi, probs = c(0.25), na.rm=TRUE),
    ndvi_75 = quantile(ndvi, probs = c(0.75), na.rm=TRUE),
    ndvi_med = median(ndvi, na.rm=TRUE),
    ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #link the fips code. this also has tract-level geometry
  left_join(den_co_tract_geo_w_extract_id , by = "tract_id_row_number") %>% 
  #link the county information
  left_join(den_metro_co_nogeo, by = "county_fips") %>% 
  st_as_sf() #it has geometry. just make it so.

save(den_co_tract_ndvi_day_geo, file = "den_co_tract_ndvi_day_geo.RData")
load("den_co_tract_ndvi_day_geo.RData")


## wrangle day-level census tract NDVI---------
#continue wrangling from this saved dataset so we don't have to load the long-form data, as it's 
#800 million observations.

#link valid dates
den_co_tract_ndvi_day_wrangle_geo = den_co_tract_ndvi_day_geo %>% 
  left_join(lookup_date_is_valid_all, by = "date")

setwd(here("data-processed"))
save(den_co_tract_ndvi_day_wrangle_geo, file = "den_co_tract_ndvi_day_wrangle_geo.RData")

#a no-geo version
den_co_tract_ndvi_day_wrangle_nogeo = den_co_tract_ndvi_day_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

save(den_co_tract_ndvi_day_wrangle_nogeo, file = "den_co_tract_ndvi_day_wrangle_nogeo.RData")

#what are the valid dates, again?
lookup_date_is_valid_all %>% 
  filter(date_is_valid_all==1) %>% 
  dplyr::select(date) %>% 
  pull()


## visualize census tracts NDVI ---------
den_metro_bbox_custom_sf %>% mapview()
### define color palettes------------------
pal_terrain_col = rev(terrain.colors(568)) #mapview below has 586 colors
pal_terrain_col #it just interpolates by repeating.
#use viridis instead. more flexible. annoying to pick an exact number.
pal_viridis_trunc=viridis::viridis_pal(end=.9) #trunc for truncated
table(den_co_tract_ndvi_day_geo$county_name_short)

#overall
den_co_tract_ndvi_day_wrangle_geo %>% 
  filter(date == "2021-05-25") %>% 
  mapview()
### may 25, 2021-------
pal_terrain_col = rev(terrain.colors(100)) 
den_co_tract_ndvi_day_wrangle_geo %>% 
  filter(date == "2021-05-25") %>% 
  mapview(
    layer.name = "NDVI, Median",
     col.regions = pal_terrain_col,
    zcol = "ndvi_med" 
    )

#valid dates include 5/25, 6/2, 6/10, 7/4
### june 10 2021----------
den_co_tract_ndvi_day_geo %>% 
  filter(date == "2021-06-10") %>% 
  mapview(
    layer.name = "NDVI, Median",
    col.regions = pal_terrain_col,
    zcol = "ndvi_med" 
  )

# green space NDVI over 5 years----
## Summarize NDVI of parks and public green space------
# Comment March 16 2022: leave this restricted to Denver and Jefferson counties
setwd(here("data-processed"))
load("den_jeff_co_green_space_no_wtr.RData")
names(den_jeff_co_green_space_no_wtr)
st_crs(den_jeff_co_green_space_no_wtr) #2876
den_jeff_co_green_space_no_wtr_4326 = den_jeff_co_green_space_no_wtr %>% 
  st_transform(4326)
den_jeff_co_green_space_no_wtr_4326 %>% mapview(zcol = "osm_value")

den_metro_green_space_ndvi_long_int =ndvi_den_metro_terr_5_yr %>% 
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    #just use denver and jefferson counties
    y =  terra::vect(den_jeff_co_green_space_no_wtr_4326)) %>% 
  as_tibble() 

nrow(den_metro_green_space_ndvi_long_int)
#do not save this; it takes 1 GB of space.

#for interactive coding, separate because the first step takes so long. eventually combine
den_metro_green_space_ndvi_long = den_metro_green_space_ndvi_long_int %>% 
  pivot_longer( #long form
    cols = contains("20"),#contains a year that begins with 20..
    names_to = "date_ndvi",
    values_to = "ndvi"
  ) %>% 
  mutate(
    date_fixed_char = substr(date_ndvi, 1,8), #extract the date
    date = lubridate::as_date(date_fixed_char) #rename
  ) %>% 
  rename(
    park_id_row_number = ID, #make it less ambiguous
  ) %>% 
  dplyr::select(park_id_row_number, ndvi, date) 

#do not save. it's 1 GB+
#create an ID that will link to the extracted values (row number)
den_jeff_co_green_space_no_wtr_w_extract_id = den_jeff_co_green_space_no_wtr %>% 
  st_transform(4326) %>% 
  mutate(park_id_row_number=row_number()) 
names(den_jeff_co_green_space_no_wtr_w_extract_id)

den_metro_green_space_ndvi_day_geo = den_metro_green_space_ndvi_long %>% 
  group_by(park_id_row_number, date) %>% 
  summarise(
    ndvi_min = min(ndvi, na.rm=TRUE),
    ndvi_max = max(ndvi, na.rm=TRUE),
    ndvi_25 = quantile(ndvi, probs = c(0.25), na.rm=TRUE),
    ndvi_75 = quantile(ndvi, probs = c(0.75), na.rm=TRUE),
    ndvi_med = median(ndvi, na.rm=TRUE),
    ndvi_mean = mean(ndvi, na.rm=TRUE)) %>% 
  ungroup() %>% 
  #link the look-up. this also has geometry.
  left_join(den_jeff_co_green_space_no_wtr_w_extract_id, by = "park_id_row_number") %>% 
  st_as_sf()  #it has geometry. just make it so.

save(den_metro_green_space_ndvi_day_geo, file = "den_metro_green_space_ndvi_day_geo.RData")

## wrangle day-level green space NDVI---------
setwd(here("data-processed"))
# link valid dates
load("den_metro_green_space_ndvi_day_geo.RData")
load("lookup_date_is_valid_all.RData")
den_metro_green_space_ndvi_day_wrangle_geo = den_metro_green_space_ndvi_day_geo %>% 
  left_join(lookup_date_is_valid_all, by = "date")

save(den_metro_green_space_ndvi_day_wrangle_geo,
     file = "den_metro_green_space_ndvi_day_wrangle_geo.RData")
#a no-geo version
den_metro_green_space_ndvi_day_wrangle_nogeo = den_metro_green_space_ndvi_day_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
save(den_metro_green_space_ndvi_day_wrangle_nogeo, 
     file = "den_metro_green_space_ndvi_day_wrangle_nogeo.RData")


## visualize NDVI of parks and green space------------
load("den_jeff_co_geo.RData")
pal_viridis_trunc=viridis::viridis_pal(end=.9) #trunc for truncated
mv_den_jeff_co_geo = den_jeff_co_geo %>%  
  mapview(
    layer.name = "County name",
    color = c("red", "orange"),
    col.regions = c("red", "orange"),
    lwd=2,
    zcol = "county_name_short", alpha.regions = 0)
mv_den_metro_green_space_ndvi_day_geo = den_metro_green_space_ndvi_day_wrangle_geo %>% 
  filter(date == "2021-05-25") %>% 
  mapview(
    layer.name = "NDVI, Median",
    col.regions = pal_viridis_trunc,
    zcol = "ndvi_med" 
  )
mv_den_metro_green_space_ndvi_day_geo+mv_den_jeff_co_geo


den_metro_green_space_ndvi_day_wrangle_geo %>% 
  filter(date == "2021-05-25") %>% 
  mapview(
    layer.name = "type",
    col.regions = pal_viridis_trunc,
    zcol = "osm_value" 
  )




