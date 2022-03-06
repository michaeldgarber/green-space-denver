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

#load bounding box for Metro Denver that was created here:
#~/Dropbox/CSU/green-space-denver/scripts/1a_get_landsat_ndvi_denver.R

setwd(here("data-processed"))
load("den_metro_bbox_custom.RData") #doesn't include all of southern JeffCo
load("den_jeff_co_bbox.RData") #all of Jeff Co
mapview(den_metro_bbox_custom) + mapview(den_jeff_co_bbox)
#update 3/6/22 for speed use den_metro_bbox_custom instead


# Parks--------------
## Download park data-----------
#Don't run unless you have to, as it takes about 1 min, and the IP address may have a quota
# den_metro_park = opq(bbox = den_metro_bbox_custom) %>%
#   add_osm_feature(key = "leisure" , value="park") %>%
#   osmdata_sf()
# 
# den_metro_nature_reserve = opq(bbox = den_metro_bbox_custom) %>%
#   add_osm_feature(key = "leisure" , value="nature_reserve") %>%
#   osmdata_sf()
# 
# den_metro_protected_area = opq(bbox = den_metro_bbox_custom) %>%
#   add_osm_feature(key = "boundary" , value="protected_area") %>%
#   osmdata_sf()


## save the park data from OSM ---------
# save(den_metro_park, file = "den_metro_park.RData")
# save(den_metro_nature_reserve, file = "den_metro_nature_reserve.RData")
# save(den_metro_protected_area, file = "den_metro_protected_area.RData")

#load rather than re-downloading unless the bounding box changes
load("den_metro_park.RData")
load("den_metro_nature_reserve.RData")
load("den_metro_protected_area.RData")


## wrangle park data before combining-----------

#select the polygons nad multipolygons from the OSM object
den_metro_park_polygons = den_metro_park$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_metro_park_multipolygons = den_metro_park$osm_multipolygons  %>% 
  mutate(osm_origin_feature_type = "multipolygon")

den_metro_park_resolved = den_metro_park_polygons %>% 
  bind_rows(den_metro_park_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "leisure",
    osm_value = "park"
  )

#den_metro_park_resolved %>% mapview()

#do the same for the nature reserve
den_metro_nature_reserve_polygons = den_metro_nature_reserve$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_metro_nature_reserve_multipolygons = den_metro_nature_reserve$osm_multipolygons %>% 
  mutate(osm_origin_feature_type = "multipolygon")


den_metro_nature_reserve_resolved = den_metro_nature_reserve_polygons %>% 
  bind_rows(den_metro_nature_reserve_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "leisure",
    osm_value = "nature_reserve"
  )
#den_metro_nature_reserve_resolved %>% mapview()

den_metro_protected_area_polygons = den_metro_protected_area$osm_polygons %>% 
  mutate(osm_origin_feature_type = "polygon")
den_metro_protected_area_multipolygons = den_metro_protected_area$osm_multipolygons %>% 
  mutate(osm_origin_feature_type = "multipolygon")

den_metro_protected_area_resolved = den_metro_protected_area_polygons %>% 
  bind_rows(den_metro_protected_area_multipolygons) %>% 
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
#update to change to green space (omit the word public for brevity)
den_jeff_co_green_space =den_metro_park_resolved %>% 
  bind_rows(
    den_metro_nature_reserve_resolved,
    den_metro_protected_area_resolved
  ) %>% 
  st_make_valid() %>% #needed because there are some duplicate vertices
  #restrict to just the bounding box, as it was picking up many features
  #that extend far beyond the metro area
  st_intersection(
    den_jeff_co_geo
  ) %>% 
#  rename name to osm_name (as you've done in other projects)
  rename(osm_name = name) %>% 
  #just select a few fields...can add more later as needed
  dplyr::select(
    starts_with("osm"),
    contains("fips"),
    contains("osm_name"),
#    contains("owner"),
    contains("protect"),
#    contains("admin"),
    contains("boundary")
                ) 
names(den_jeff_co_green_space)
save(den_jeff_co_green_space, file = "den_jeff_co_green_space.RData")
load("den_jeff_co_green_space.RData")

den_jeff_co_green_space %>% mapview(zcol = "osm_value")
# Bodies of water------------
## Download bodies of water---------------
#A few different ways:
### key natural value water ----------
#https://wiki.openstreetmap.org/wiki/Tag:natural%3Dwater
den_metro_natural_water= opq(bbox = den_metro_bbox_custom) %>%
  add_osm_feature(key = "natural" , value="water") %>%
  osmdata_sf()
setwd(here("data-processed"))
save(den_metro_natural_water, file = "den_metro_natural_water.RData")


### key waterway value river & stream ----------
#Another way to get waterways:
#https://wiki.openstreetmap.org/wiki/Key:waterway
den_metro_waterway_river= opq(bbox = den_metro_bbox_custom) %>%
  add_osm_feature(key = "waterway" , value="river") %>%
  osmdata_sf()
setwd(here("data-processed"))
save(den_metro_waterway_river, file = "den_metro_waterway_river.RData")

den_metro_waterway_stream= opq(bbox = den_metro_bbox_custom) %>%
  add_osm_feature(key = "waterway" , value="stream") %>%
  osmdata_sf()
save(den_metro_waterway_stream, file = "den_metro_waterway_stream.RData")

#Test the different sf feature types
den_metro_waterway_river$osm_lines %>% mapview() #yes, many
den_metro_waterway_river$osm_multilines %>% mapview() #yes, many
den_metro_waterway_river$osm_polygons %>% mapview() #none
den_metro_waterway_river$osm_multipolygons #null
#den_metro_waterway_river$osm_points %>% mapview()#there but don't use

### Mapview all of them together--------
#### mapview  natural water-----------
#I'm curious what's happening with the line-based features. I'd like to avoid using
#them if possible.
mv_natural_water_lines = den_metro_natural_water_lines %>% 
  mapview(layer.name = "lines, natural, water", color = "cadetblue1")
mv_natural_water_polygons = den_metro_natural_water_polygons %>% 
  mapview(layer.name = "polygons, natural, water", color = "blue", col.regions = "blue")
mv_natural_water_multipolygons = den_metro_natural_water_multipolygons %>% 
  mapview(layer.name = "multipolygons, natural, water", color = "green", col.regions = "green")

mv_natural_water_polygons+mv_natural_water_multipolygons+mv_natural_water_lines
#Great, all of the line-based features are also polygon-based features, so we can omit
#these lines. and are the polygons and multipolygons the same?
#they're almost the same, but there are a few features that are not represented
#by polygons but are represented by multipolygons. 
#otherwise, polygons is much more complete.

#### mapview waterway river------------------
#Do they overlap perfectly? or does one contain information the other does not?
mv_waterway_river_lines = den_metro_waterway_river$osm_lines %>% 
  mapview(layer.name = "lines, waterway, river", color = "red")
mv_waterway_river_multilines = den_metro_waterway_river$osm_multilines %>% 
  mapview(layer.name = "multilines, waterway, river", color = "green")

mv_waterway_river_multilines+mv_waterway_river_lines
#osm_lines is more complete. forget multilines

#### mapview waterway stream------
mv_waterway_stream_lines = den_metro_waterway_stream$osm_lines %>% 
  mapview(layer.name = "lines, waterway, stream", color = "purple")
mv_waterway_stream_multilines = den_metro_waterway_stream$osm_multilines %>% 
  mapview(layer.name = "multilines, waterway, stream", color = "green") #null

mv_waterway_stream_lines+mv_waterway_stream_multilines

#natural water
mv_natural_water_polygons+
  mv_natural_water_multipolygons+
  mv_natural_water_lines +
  #waterway river
  mv_waterway_river_lines +
  mv_waterway_stream_lines

#Summary of mapviews here:
#There are several streams that are not otherwise noted by polygons.
#Streams indicate a continuous flow, which sometimes include bodies of water as well.
#The data structure includes overlapping features, which makes sense. It's true
#that a reservoir may be part of a longer river, so both the river and the reservoir
#should be represented.

#For those rivers and streams that are only represented by lines, how should
#I indicate their area? Maybe say 10 feet?



### Create sf objects for each---------
#### sf objects for natural water-----------
den_metro_natural_water_polygons = den_metro_natural_water$osm_polygons %>% 
  mutate(
    osm_key = "natural",
    osm_value = "water",
    osm_origin_feature_type = "polygon")
den_metro_natural_water_multipolygons = den_metro_natural_water$osm_multipolygons %>% 
  mutate(
    osm_key = "natural",
    osm_value = "water",
    osm_origin_feature_type = "multipolygon"
  )
#don't create an sf object for den_metro_natural_water$osm_lines, per above checks

#

den_metro_natural_water_resolved = den_metro_water_polygons %>% 
  bind_rows(den_metro_water_multipolygons) %>% 
  st_as_sf() %>% 
  #add a couple variables to keep track of source
  mutate(
    osm_key = "natural",
    osm_value = "water"
  )

#den_metro_water_resolved %>% mapview(zcol = "osm_origin_feature_type")
save(den_metro_water_resolved, file = "den_metro_water_resolved.RData")

## create a union of all of this for an easy int-----------
#warning: this takes forever (about 10 minutes)
# union_method_1_t_start = Sys.time()
# den_metro_water_resolved_union_method_1 = den_metro_water_resolved %>%
#   st_union() 
# union_method_1_t_end = Sys.time()
# union_method_1_t_elapsed = union_method_1_t_end-union_method_1_t_start #7 minutes
# save(den_metro_water_resolved_union_method_1 , file = "den_metro_water_resolved_union_method_1.RData")
# 
# class(den_metro_water_resolved_union_method_1)
# union_method_2_t_start = Sys.time()
# den_metro_water_resolved_union_method_2 = den_metro_water_resolved %>%
#   mutate(dummy=1) %>% 
#   group_by(dummy) %>% 
#   summarise(n=n()) %>% 
#   ungroup() 
# union_method_2_t_end = Sys.time()
# union_method_2_t_elapsed = union_method_2_t_end-union_method_2_t_start #7 min
# save(den_metro_water_resolved_union_method_2, file = "den_metro_water_resolved_union_method_2.RData")
# class(den_area)
load("den_metro_water_resolved_union_method_1.RData")
load("den_metro_water_resolved_union_method_2.RData")
den_metro_water_resolved_union_method_2 %>% mapview()


# Remove bodies of water from parks-----------
#use st_difference https://cran.r-project.org/web/packages/sf/vignettes/sf3.html
den_jeff_co_green_space_no_water = den_jeff_co_green_space %>% 
  st_difference(den_metro_water_resolved_union_method_2) %>% 
  st_make_valid() #this did the trick!

save(den_jeff_co_green_space_no_water, file = "den_jeff_co_green_space_no_water.RData")
#I was going to use tmap because mapview was failing silently but all good after st_make_valid()
#I had issues getting mapview to render mixed geometries
#https://github.com/r-spatial/mapview/issues/342
#https://github.com/r-spatial/mapview/issues/85
names(den_jeff_co_green_space_no_water)
den_jeff_co_green_space_no_water %>% 
  mapview(
    zcol = "osm_value",
    layer.name = "Green space by type")
