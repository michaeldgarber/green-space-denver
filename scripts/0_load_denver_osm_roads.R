# Import Denver roads from OSM

#filename: 0_load_denver_osm_roads

library(osmdata)
library(sf)
library(tidyverse)
library(mapview)
library(here)

setwd(here("data-processed"))
load("den_co_geo.RData")
den_co_geo %>% mapview()
den_co_geo_4326 = den_co_geo %>% 
  st_transform(4326) %>% 
  dplyr::select(geometry)   #just keep geometry

#Comment: I used my old Atlanta OSM code as a guide but tried to simplify.

# Pull data from OSM-------
#motorways, trunk, primary, secondary, tertiary, residential
motorway = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf()

motorway$osm_lines %>% mapview()
motorway_line = motorway$osm_lines %>% 
  st_intersection(den_co_geo_4326)  #limit to Denver County
motorway_line %>% mapview() 

#note there are also "link" versions of each
#link roads for each of these -- see https://wiki.openstreetmap.org/wiki/Highways
motorway_link= opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'motorway_link') %>%
  osmdata_sf()

motorway_link_line = motorway_link$osm_lines %>% 
  st_intersection(den_co_geo_4326) 
motorway_link_line %>% mapview()


## trunk roads -- large state roads, etc. that aren't interstates / freeways--------
trunk = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'trunk') %>%
  osmdata_sf()
trunk_line = trunk$osm_lines %>% 
  st_intersection(den_co_geo_4326) 
trunk_line %>% mapview()

trunk_link = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'trunk_link') %>%
  osmdata_sf()

trunk_link_line = trunk_link$osm_lines %>% 
  st_intersection(den_co_geo_4326) 

## primary roads - next most important road, e.g. ponce------------
primary = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'primary') %>%
  osmdata_sf()

primary_line = primary$osm_lines %>% 
  st_intersection(den_co_geo_4326) 
primary_line %>% mapview()

primary_link = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'primary_link') %>%
  osmdata_sf()

primary_link_line = primary_link$osm_lines %>% 
  st_intersection(den_co_geo_4326) 

primary_link_line %>% mapview()

## secondary roads, e.g. north avenue (where it's not residential)--------------
secondary = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'secondary') %>%
  osmdata_sf()

secondary_line = secondary$osm_lines %>% 
  st_intersection(den_co_geo_4326) 

secondary_link = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'secondary_link') %>%
  osmdata_sf()

secondary_link_line = secondary_link$osm_lines %>% 
  st_intersection(den_co_geo_4326) 

## tertiary roads--------------
tertiary = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'tertiary') %>%
  osmdata_sf()

tertiary_line = tertiary$osm_lines %>% 
  st_intersection(den_co_geo_4326) 

tertiary_line %>% mapview()

#didn't work, so don't worry about it.
tertiary_link = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'tertiary_link') %>%
  osmdata_sf()

#none


## residential roads (this is the big one)-------------
residential = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'residential') %>%
  osmdata_sf()

residential_line = residential$osm_lines %>% 
  st_intersection(den_co_geo_4326)  
residential_line %>% mapview()

residential_link = opq(bbox = "Denver County, Colorado, USA") %>%
  add_osm_feature(key = 'highway', value = 'residential_link') %>%
  osmdata_sf()

residential_link #doesn't exist.

# Combine them--------
table(motorway_line$highway)
table(motorway_link_line$highway)
den_osm_roads = motorway_line %>% 
  bind_rows(
    motorway_link_line,
    trunk_line,
    trunk_link_line,
    primary_line,
    primary_link_line,
    secondary_line,
    secondary_link_line,
    tertiary_line,
    residential_line
  ) %>% 
  st_transform(2876) %>%   #convert to feet, as we've been doing elsewhere
  mutate(
    length_ft = as.numeric(st_length(geometry)),
    length_mi = length_ft/5280,
    highway_main_cat = case_when(
      highway == "motorway" ~ "motorway",
      highway == "motorway_link" ~ "motorway",
      highway == "trunk" ~ "trunk",
      highway == "trunk_link" ~ "trunk",
      highway == "primary" ~ "primary",
      highway == "primary_link" ~ "primary",
      highway == "secondary" ~ "secondary",
      highway == "secondary_link" ~ "secondary",
      TRUE ~ highway
    ),
    highway_primary_or_lower = case_when(
      highway_main_cat == "motorway" ~0,
      highway_main_cat == "trunk" ~0,
      TRUE ~ 1
    )
  )


setwd(here("data-processed"))
save(den_osm_roads, file = "den_osm_roads.RData")

den_osm_roads %>% 
  filter(highway == "primary") %>% 
  mapview(zcol = "length_ft")
den_osm_roads %>% mapview(
  color = rainbow(n=n_distinct(den_osm_roads$highway_main_cat)),
  zcol = "highway_main_cat")
den_osm_roads %>% mapview(zcol = "highway_primary_or_lower")