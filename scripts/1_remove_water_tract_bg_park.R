#filename: 1_remove_water_tract_bg_park

#This is a stand-alone script for removing water from administrative units
#and parks for good measure

library(here)
library(tidyverse)
library(terra)
library(raster)
library(mapview)
library(sf)

# Remove bodies of water from tracts and parks--------
## Remove bodies of water from tracts and block groups----------
setwd(here("data-processed"))
#previous script:
#~/green-space-denver/scripts/0_import_manage_denver_acs.R
load("den_metro_tract_geo.RData") #census tracts in metro area
load("den_metro_bg_geo.RData") #block groups in the metro area (defined via my custom bb)
den_metro_bg_geo %>% mapview(zcol = "area_mi2_bg")

#Update March 22: replace no_wtr with no_wtr to shorten the object names throughout
load("den_osm_water_poly_both_union.RData") #from 0_load_denver_osm_parks_water.R
### from census tracts----------
den_metro_tract_no_wtr_geo = den_metro_tract_geo %>% 
  st_transform(2876) %>% #convert to central colorado foot-based crs
  st_difference(den_osm_water_poly_both_union) %>% 
  st_make_valid()  %>% 
  dplyr::select(contains("fips"), geometry)

setwd(here("data-processed"))
save(den_metro_tract_no_wtr_geo, file = "den_metro_tract_no_wtr_geo.RData")
den_metro_tract_no_wtr_geo %>% mapview(zcol = "county_fips")

### from block groups-------------
den_metro_bg_no_wtr_geo = den_metro_bg_geo %>% 
  st_transform(2876) %>% #convert to central colorado foot-based crs
  st_difference(den_osm_water_poly_both_union) %>% 
  st_make_valid()  %>% 
  dplyr::select(contains("fips"), geometry)
save(den_metro_bg_no_wtr_geo, file = "den_metro_bg_no_wtr_geo.RData")
den_metro_bg_no_wtr_geo %>% mapview(zcol = "county_fips")



## area lookups for tracts and block groups without water--------
lookup_tract_no_wtr_area = den_metro_tract_no_wtr_geo %>% 
  mutate(
    area_ft2_no_wtr  = as.numeric(st_area(geometry)),
    area_m2_no_wtr = area_ft2_no_wtr/10.764, #meters squared 
    area_mi2_no_wtr = area_ft2_no_wtr/(5280**2) #miles squared
  ) %>% 
  st_set_geometry(NULL) %>% 
  distinct(tract_fips, 
           area_ft2_no_wtr,
           area_m2_no_wtr, 
           area_mi2_no_wtr )%>% 
  as_tibble()
save(lookup_tract_no_wtr_area, file = "lookup_tract_no_wtr_area.RData")
nrow(lookup_tract_no_wtr_area)
nrow(den_metro_tract_no_wtr_geo)
nrow(den_metro_tract_geo)

lookup_bg_no_wtr_area = den_metro_bg_no_wtr_geo %>% 
  mutate(
    area_ft2_no_wtr  = as.numeric(st_area(geometry)),
    area_m2_no_wtr = area_ft2_no_wtr/10.764, #meters squared, for informational purposes.
    area_mi2_no_wtr = area_ft2_no_wtr/(5280**2) #miles squared
  ) %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, 
           area_ft2_no_wtr, 
           area_m2_no_wtr, 
           area_mi2_no_wtr ) %>% 
  as_tibble()

setwd(here("data-processed"))
save(lookup_bg_no_wtr_area, file = "lookup_bg_no_wtr_area.RData")

#check to make sure it works around the lake, etc.
den_metro_bg_geo %>% 
  filter(county_fips == "031") %>% 
  left_join(lookup_bg_no_wtr_area, by = "bg_fips") %>% 
  mapview(zcol = "area_mi2_no_wtr") #working as expected in 08031012010



## Remove bodies of water from parks-----------
#from ~/scripts/0_load_denver_osm_parks_water.R
setwd(here("data-processed"))
load("den_jeff_co_green_space.RData")  
den_jeff_co_green_space %>% mapview(zcol = "osm_value")
#and this, which includes 10-foot buffers around rivers and streams 
#if they are not originally represented as polygons. Load the unioned version
load("den_osm_water_poly_both.RData")
load("den_osm_water_poly_both_union.RData")
#use st_difference https://cran.r-project.org/web/packages/sf/vignettes/sf3.html
sf::sf_use_s2(FALSE) #getting invalid spherical geo
#Update to shorten object names: no_wtr rather than no_water
den_jeff_co_green_space_no_wtr = den_jeff_co_green_space %>% 
  st_transform(2876) %>% 
  st_simplify(dTolerance = 5) %>% #makes the file smaller.
  st_make_valid() %>% 
  st_difference(den_osm_water_poly_both_union) %>% 
  st_make_valid() #this did the trick! I was having issues with mapview before 

save(den_jeff_co_green_space_no_wtr, file = "den_jeff_co_green_space_no_wtr.RData")
#I was going to use tmap because mapview was failing silently but all good after st_make_valid()
#I had issues getting mapview to render mixed geometries
#https://github.com/r-spatial/mapview/issues/342
#https://github.com/r-spatial/mapview/issues/85
load("den_jeff_co_green_space_no_wtr.RData")
names(den_jeff_co_green_space_no_wtr)
den_jeff_co_green_space_no_wtr %>% 
  mapview(
    zcol = "osm_value",
    layer.name = "Green space by type")

