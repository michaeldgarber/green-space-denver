#Load OSM park data for Denver and Jeff Co--
#1/19/22 add bodies of water as well
#filename: 0_load_denver_osm_parks_water
library(tidyverse)
library(sf)
library(mapview)
library(here)
library(osmdata)

# osm_features_list = osmdata::available_features()
# osm_features_list
#-----------useful links:------------------------------------#
#https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html#1_Introduction
#https://wiki.openstreetmap.org/wiki/Key:park:type
#https://wiki.openstreetmap.org/wiki/Tag:leisure%3Dpark
#https://wiki.openstreetmap.org/wiki/Category:Key_descriptions

#load bounding box for den county and jeff co that was created in another script in this project
setwd(here("data-processed"))
load("den_jeff_co_bbox.RData")
den_jeff_co_bbox %>% mapview()

#another idea for a bounding box, similar to what I did in atlanta (i-285)
#is to use the
# Parks--------------
## Download park data-----------
#Commenting out because it takes a while
# den_area_park = opq(bbox = den_jeff_co_bbox) %>%
#   add_osm_feature(key = "leisure" , value="park") %>%
#   osmdata_sf()
# 
# den_area_nature_reserve = opq(bbox = den_jeff_co_bbox) %>%
#   add_osm_feature(key = "leisure" , value="nature_reserve") %>%
#   osmdata_sf()
# 
# den_area_protected_area = opq(bbox = den_jeff_co_bbox) %>%
#   add_osm_feature(key = "boundary" , value="protected_area") %>%
#   osmdata_sf()


## save park data because it takes a bit---------
# save(den_area_park, file = "den_area_park.RData")
# save(den_area_nature_reserve, file = "den_area_nature_reserve.RData")
# save(den_area_protected_area, file = "den_area_protected_area.RData")

load("den_area_park.RData")
load("den_area_nature_reserve.RData")
load("den_area_protected_area.RData")


#----------wrangle downloaded park data--------------------#
#select the polygons nad multipolygons from the OSM object
den_area_park_polygons = den_area_park$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_area_park_multipolygons = den_area_park$osm_multipolygons  %>% 
  mutate(osm_origin_feature_type = "multipolygon")

den_area_park_resolved = den_area_park_polygons %>% 
  bind_rows(den_area_park_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "leisure",
    osm_value = "park"
  )

#den_area_park_resolved %>% mapview()

#do the same for the nature reserve
den_area_nature_reserve_polygons = den_area_nature_reserve$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_area_nature_reserve_multipolygons = den_area_nature_reserve$osm_multipolygons %>% 
  mutate(osm_origin_feature_type = "multipolygon")


den_area_nature_reserve_resolved = den_area_nature_reserve_polygons %>% 
  bind_rows(den_area_nature_reserve_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "leisure",
    osm_value = "nature_reserve"
  )
#den_area_nature_reserve_resolved %>% mapview()

den_area_protected_area_polygons = den_area_protected_area$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_area_protected_area_multipolygons = den_area_protected_area$osm_multipolygons %>% 
  mutate(osm_origin_feature_type = "multipolygon")

den_area_protected_area_resolved = den_area_protected_area_polygons %>% 
  bind_rows(den_area_protected_area_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "boundary",
    osm_value = "protected_area"
  )

## combine all of them and agin restrict to the denver and jefferson counties ---------
#note this may not be comprehensive, but it's a start.
#load this file from the other code
load("den_jeff_co_geo.RData")
#note name change to specify that it's just these two counties
den_jeff_co_green_space_public =den_area_park_resolved %>% 
  bind_rows(
    den_area_nature_reserve_resolved,
    den_area_protected_area_resolved
  ) %>% 
  st_make_valid() %>% #needed because there are some duplicate vertices
  #restrict to just the bounding box, as it was picking up many features
  #that extend far beyond the metro area
  st_intersection(
    den_jeff_co_geo
  ) %>% 
  #just select a few fields...can add more later as needed
  dplyr::select(
    starts_with("osm"),
    contains("fips"),
    contains("name"),
    contains("owner"),
    contains("protect"),
    contains("admin"),
    contains("boundary")
                )
    
names(den_jeff_co_green_space_public)
save(den_jeff_co_green_space_public, file = "den_jeff_co_green_space_public.RData")
den_jeff_co_green_space_public %>% mapview(zcol = "osm_key")

# Bodies of water------------
## Download bodies of water---------------
# den_area_water = opq(bbox = den_jeff_co_bbox) %>%
#   add_osm_feature(key = "natural" , value="water") %>%
#   osmdata_sf()
# save(den_area_water, file = "den_area_water.RData")

load("den_area_water.RData")
den_area_water_polygons = den_area_water$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_area_water_multipolygons = den_area_water$osm_multipolygons %>% 
  mutate(osm_origin_feature_type = "multipolygon")
den_area_water_lines = den_area_water$osm_lines %>% 
  mutate(osm_origin_feature_type = "lines") #no area by definition so doesn't matter


den_area_water_resolved = den_area_water_polygons %>% 
  bind_rows(den_area_water_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "natural",
    osm_value = "water"
  )

#den_area_water_resolved %>% mapview(zcol = "osm_origin_feature_type")
save(den_area_water_resolved, file = "den_area_water_resolved.RData")

## create a union of all of this for an easy int-----------
#warning: this takes forever.
# union_method_1_t_start = Sys.time()
# den_area_water_resolved_union_method_1 = den_area_water_resolved %>%
#   st_union() 
# union_method_1_t_end = Sys.time()
# union_method_1_t_elapsed = union_method_1_t_end-union_method_1_t_start #7 minutes
# save(den_area_water_resolved_union_method_1 , file = "den_area_water_resolved_union_method_1.RData")
# 
# class(den_area_water_resolved_union_method_1)
# union_method_2_t_start = Sys.time()
# den_area_water_resolved_union_method_2 = den_area_water_resolved %>%
#   mutate(dummy=1) %>% 
#   group_by(dummy) %>% 
#   summarise(n=n()) %>% 
#   ungroup() 
# union_method_2_t_end = Sys.time()
# union_method_2_t_elapsed = union_method_2_t_end-union_method_2_t_start #7 min
# save(den_area_water_resolved_union_method_2, file = "den_area_water_resolved_union_method_2.RData")
# class(den_area)
load("den_area_water_resolved_union_method_1.RData")
load("den_area_water_resolved_union_method_2.RData")
den_area_water_resolved_union_method_2 %>% mapview()
# Remove bodies of water from parks-----------
#use st_difference https://cran.r-project.org/web/packages/sf/vignettes/sf3.html

den_jeff_co_green_space_public_no_water = den_jeff_co_green_space_public %>% 
  st_difference(den_area_water_resolved_union_method_2) %>% 
  st_make_valid() #this did the trick!

save(den_jeff_co_green_space_public_no_water, file = "den_jeff_co_green_space_public_no_water.RData")
#I was going to use tmap because mapview was failing silently but all good after st_make_valid()
#I had issues getting mapview to render mixed geometries
#https://github.com/r-spatial/mapview/issues/342
#https://github.com/r-spatial/mapview/issues/85
names(den_jeff_co_green_space_public_no_water)
den_jeff_co_green_space_public_no_water %>% 
  mapview(
    zcol = "osm_key",
    layer.name = "hi")
