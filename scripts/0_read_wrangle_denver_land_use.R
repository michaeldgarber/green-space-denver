#filename: 0_read_denver_land_use

#https://r-spatial.github.io/sf/reference/st_read.html

library(tidyverse)
library(here)
library(sf)
library(mapview)
library(shades)

# Load parcel-based land-use data (2018)----------
# Only do this once, as it takes a while. Load it from R instead.
#source:
#https://www.denvergov.org/opendata/dataset/
#city-and-county-of-denver-existing-landuse-2018
#dsn = #the folder where the shapefile is located
# setwd(here("data-input", "city-of-denver-data"))
# den_landuse_2018 = st_read(dsn ="existing_landuse") %>% 
#   st_transform(4326) %>% 
#   st_make_valid()
# 
setwd(here("data-processed"))#save
# save(den_landuse_2018, file = "den_landuse_2018.RData")

#Examine a subset, since the data are pretty big.
#NW corner #39.760199, -104.977148
#SE corner 39.719007, -105.017163
load("den_landuse_2018.RData")
den_subset = st_bbox(
  c(
  ymin = 39.719007,
  ymax = 39.760199,
  xmin = -105.017163,
  xmax = -104.977148
  ),
  crs=st_crs(4326)
  ) %>% 
  st_as_sfc()

den_subset %>% mapview()

sf::sf_use_s2(FALSE)
den_landuse_2018_subset = den_landuse_2018 %>% 
  st_intersection(den_subset)

den_landuse_2018_subset %>% 
  mapview(zcol = "CPD_LANDUS")

# Load zoning data----------
#These data do not have quite the spatial resolution of the parcel-based data, which is fine.
#They are easier to work with.
setwd(here("data-input", "city-of-denver-data"))
den_zoning = st_read(dsn ="zoning") %>% 
  st_transform(4326) %>% 
  st_make_valid()

#how many categories? I want to visualize using rainbow
#21
den_zoning %>% 
  st_set_geometry(NULL) %>% 
  group_by(ZONE_DESCR) %>% 
  summarise(n=n())
pal_rainbow_21 = rainbow(21)
pal_rainbow_21 %>% swatch()
setwd(here("data-processed"))#save
save(den_zoning, file = "den_zoning.RData")
den_zoning %>% 
  st_set_geometry(NULL) %>% 
  group_by(ZONE_DESCR) %>% 
  summarise(n=n())

den_zoning %>% 
  mapview(
  zcol = "ZONE_DESCR",
  col.regions = pal_rainbow_21 )

#measure the area
den_zoning_wrangle = den_zoning %>% 
  mutate(
    area_m2 = as.numeric(st_area(geometry))
  )

summary(den_zoning_wrangle$area_m2)
zoning_area = den_zoning_wrangle %>% 
  st_set_geometry(NULL) %>% 
  group_by(ZONE_DESCR) %>% 
  summarise(area_m2 = sum(area_m2))

# Group and summarise geometry by zone code---------
nrow(den_zoning)
table(den_zoning$ZONE_DESCR)
den_zoning_no_airport_union = den_zoning %>% 
  #convert to the foot-based CRS we've been using
  filter(ZONE_DESCR != "Airport  (DIA)") %>% #note the weird spacing
  group_by(ZONE_DESCR) %>% 
  summarise(area_m2 = sum(area_m2))

