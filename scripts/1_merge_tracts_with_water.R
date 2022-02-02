#filename: 1_merge_tracts_with_water
#began 1/20/22
#The purpose of this script is to remove bodies of water from ACS areas. It will be very short
#but I want to make sure that it is run in order, so creating a new script

#It relies on:
# 0_import_manage_denver_acs
# 0_load_denver_osm_parks_water
library(tidyverse)
library(here)
library(sf)
library(mapview)

load("den_jeff_co_geo.RData") 
load("den_jeff_co_tracts_geo.RData")
load("den_area_water_resolved_union_method_2.RData")#note this is just den and jeff co. should note that.
den_jeff_co_tracts_no_water_geo = den_jeff_co_tracts_geo %>% 
  st_difference(den_area_water_resolved_union_method_2) %>% 
  st_make_valid()  %>% 
  dplyr::select(contains("fips"), geometry)

setwd(here("data-processed"))
save(den_jeff_co_tracts_no_water_geo, file = "den_jeff_co_tracts_no_water_geo.RData")
