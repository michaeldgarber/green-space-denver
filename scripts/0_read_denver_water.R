#filename: 0_read_denver_water
#Revised Mar 6 2022 7 pm. Github messed this one up as well
library(here)
library(tidyverse)
library(sf)
library(mapview)
# Note in this script (~/scripts/0_load_denver_osm_parks_water.R), 
# we load OpenStreetMap data with bodies of water.
setwd(here("data-processed"))
load("den_area_water_resolved.RData")

#useful to have this so I know where county boundaries are
#~/green-space-denver/scripts/0_import_manage_denver_acs.R
load("den_metro_co_geo.RData")
mapview(den_area_water_resolved)+mapview(den_metro_co_geo)

#This script will load that data as well as load the water data from the
#Denver Open Data page:
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-lakes-and-ponds
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-streams

# Load data from Denver Open Data Portal----------
## Load lakes and ponds------------

#dsn = #the folder where the shapefile is located
setwd(here("data-input", "city-of-denver-data", "bodies-of-water"))
den_lakes_ponds = st_read(dsn ="lakes_and_ponds") %>%
  st_transform(2876) %>% #says it's feet
  st_make_valid() %>% 
  rename(
    object_id = OBJECTID_1,
    object_name = LAKE_NAME #make it more general since we already know it's a lake
    ) %>% 
  #these are all L&P, i.e., Lakes and Ponds.
  #in the river version, they call it UNITTYPE.
  #let's resolve that all under the same variable.
  mutate(
    unit_type = "lake or pond",
    unit_type_broad = "lake or pond" #for consistency with below
    ) %>% 
  dplyr::select(starts_with("object"), contains("type"))

setwd(here("data-processed"))
save(den_lakes_ponds, file = "den_lakes_ponds.RData")
den_lakes_ponds %>% mapview()
## Load streams--------
setwd(here("data-input", "city-of-denver-data", "bodies-of-water"))
den_streams = st_read(dsn ="streams") %>%
  st_transform(2876) %>% #says it's feet
  st_make_valid() %>% 
  rename(
    object_id = OBJECTID_1,
    object_name = STREAM_NAM, #make it more general since we already know it's a stream
    unit_type = UNITTYPE,
    stream_type = STREAM_TYP
    ) %>% 
  mutate(unit_type_broad = "streams") %>% 
  dplyr::select(starts_with("object"), contains("type"))  

den_streams %>% mapview()
class(den_streams$geometry)
class(den_lakes_ponds$geometry)

## Combine the two into one--------
#note this won't work with mapview because it's of type mixed geometry, but still worth
#creating this so you can use st_buffer on object instead of multiple
den_streams_lakes_ponds = den_streams %>% 
  st_transform(2876) %>% #feet https://epsg.io/2876
  #make a small buffer around it so mapview works.
  st_buffer(.1) %>% 
  bind_rows(den_lakes_ponds) 
setwd(here("data-processed"))
save(den_streams_lakes_ponds, file = "den_streams_lakes_ponds.RData")

den_streams_lakes_ponds %>% mapview(zcol = "unit_type_broad")
table(den_streams_lakes_ponds$unit_type)
den_streams_lakes_ponds %>% 
  filter(unit_type_broad == "streams") %>% 
  mapview(layer.name = "test")



