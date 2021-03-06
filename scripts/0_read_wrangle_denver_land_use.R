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
setwd(here("data-input", "city-of-denver-data"))
getwd()
den_landuse_2018 = st_read(dsn ="existing_landuse") %>%
  st_transform(2876) %>% #local feet
  st_simplify(dTolerance = 5) %>% #vertices every 5 feet for speed
  st_make_valid() %>% 
  st_as_sf() %>% 
  mutate(
    parcel_id = row_number(),
    area_ft2_parcel = as.numeric(st_area(geometry)),
    area_mi2_parcel = area_ft2_parcel/(5280**2) ,
    area_ac_parcel = area_ft2_parcel/43560, #acres
    
    #classify parcels by size per CB suggestions on stormwater regulation
    parcel_size_cat = factor(
        case_when(
          area_ac_parcel >= 1 ~ ">1.0 acre",
          area_ac_parcel < 1 & area_ac_parcel >=0.5 ~ "0.5-1.0 acre",
          area_ac_parcel < 0.5 ~ "<0.5 acre"
                    ),
    levels = c("<0.5 acre", "0.5-1.0 acre", ">1.0 acre")
    ),
    
    #per CCDOGI doc provided by CB
    # Stormwater footprint:impervious footprint
    stormwater_footprint_ratio = 
      case_when(
        parcel_size_cat==">1.0 acre" ~ 40,
        parcel_size_cat=="0.5-1.0 acre" ~ 25,
        parcel_size_cat=="<0.5 acre" ~ 10
        ),
    #Comment: come back to this, as I don't fully understand his suggestions
    #regarding the proportion of land that woudl be vegetated vs impervious
    #rough guess based on CB suggestion
    n_redevelop_per_y = 
        case_when(
          parcel_size_cat== ">1.0 acre" ~ 100,
          parcel_size_cat== "0.5-1.0 acre" ~ 25,
          parcel_size_cat== "<0.5 acre" ~ 400
                )
  )

object.size(den_landuse_2018)
setwd(here("data-processed"))
save(den_landuse_2018, file = "den_landuse_2018.RData")
table(den_landuse_2018$parcel_size_cat)
names(den_landuse_2018)
#these mapviews take a while
# den_landuse_2018 %>%
#   filter(parcel_size_cat == ">1.0 acre") %>%
#   mapview(zcol = "area_ac_parcel")
# den_landuse_2018 %>%
#   filter(parcel_size_cat == "<0.5 acre") %>%
#   mapview(zcol = "area_ac_parcel")

head(den_landuse_2018)
#Examine a subset, since the data are pretty big.
#NW corner #39.760199, -104.977148
#SE corner 39.719007, -105.017163
load("den_landuse_2018.RData")
den_subset = st_bbox(
  c(
    ymin = 39.719007,
    ymax = 39.760199,
    xmin = -105.017163,
    xmax = -104.977148),
  crs = st_crs(4326)
  ) %>% 
  st_as_sfc() %>% 
  st_transform(2876)

den_subset %>% mapview()

sf::sf_use_s2(FALSE)
den_landuse_2018_subset = den_landuse_2018 %>% 
  st_intersection(den_subset)

den_landuse_2018_subset %>% 
  mapview(
    zcol = "CPD_LANDUS",
    col.regions = rainbow(n=n_distinct(den_landuse_2018_subset$CPD_LANDUS)))

## another subset around union station (to parallel that for parking)-------
#load the union station 1-mi buffer
setwd(here("data-processed"))
load("union_station_1_mi.RData")
load("union_station_1_mi_4326.RData")
st_crs(den_landuse_2018)
st_crs(union_station_1_mi_2876)
den_landuse_union_sta = den_landuse_2018 %>% 
  st_intersection(union_station_1_mi_2876)
save(den_landuse_union_sta, file = "den_landuse_union_sta.RData")
den_landuse_union_sta %>% 
  mapview(
    zcol = "CPD_LANDUS",
    col.regions = rainbow(n=n_distinct(den_landuse_union_sta$CPD_LANDUS)))


# Load zoning data----------
#These data do not have quite the spatial resolution of the parcel-based data, which is fine.
#They are easier to work with.
setwd(here("data-input", "city-of-denver-data"))
den_zoning = st_read(dsn ="zoning") %>% 
  st_transform(2876) %>% 
  st_simplify() %>% 
  st_make_valid()

#how many categories? I want to visualize using rainbow
#21
den_zoning %>% 
  st_set_geometry(NULL) %>% 
  group_by(ZONE_DESCR) %>% 
  summarise(n=n())
setwd(here("data-processed"))#save
save(den_zoning, file = "den_zoning.RData")
den_zoning %>% 
  st_set_geometry(NULL) %>% 
  group_by(ZONE_DESCR) %>% 
  summarise(n=n())

# visualize  zoning using rainbow palette -----
den_zoning %>% 
  mapview(
  zcol = "ZONE_DESCR",
  col.regions = rainbow(n=n_distinct(den_zoning$ZONE_DESCR)) )


# Group by and summarise by zone code--------
den_zoning_grouped = den_zoning %>% 
  st_transform(2876) %>%  #convert to the foot-based CRS we've been using
  mutate(
    area_ft2 = as.numeric(st_area(geometry)),
    area_mi2 = area_ft2/(5280**2) 
  )%>% 
  group_by(ZONE_DESCR) %>% 
  summarise(
    area_ft2 = sum(area_ft2),
    area_mi2 = sum(area_mi2)
    ) %>% 
  ungroup() %>% 
  st_simplify()

setwd(here("data-processed"))#save
save(den_zoning_grouped, file = "den_zoning_grouped.RData")
object.size(den_zoning_grouped)

#visualize
den_zoning_grouped %>%   
  mapview(
    zcol = "ZONE_DESCR",
    col.regions = rainbow(n=n_distinct(den_zoning_grouped$ZONE_DESCR))
  )

#check to be sure your sum is correct. Denver should be about 155 square miles
den_zoning_grouped %>% 
  st_set_geometry(NULL) %>% 
  mutate(all = 1) %>% 
  group_by(all) %>% 
  summarise(
    area_mi2 = sum(area_mi2)
  )
#yup, great.

  
den_zoning_grouped_no_airport = den_zoning_grouped %>% 
  filter(ZONE_DESCR != "Airport  (DIA)")  #note the weird spacing

setwd(here("data-processed"))#save
save(den_zoning_grouped_no_airport, file = "den_zoning_grouped_no_airport.RData")

den_zoning_grouped_no_airport %>%   
  mapview(
    zcol = "ZONE_DESCR",
    col.regions = rainbow(
      n=n_distinct(den_zoning_grouped_no_airport$ZONE_DESCR))  
    )


den_zoning_grouped_no_airport_union = den_zoning_grouped_no_airport %>%
  mutate(all = 1) %>% 
  group_by(all) %>% 
  summarise(
    area_mi2 = sum(area_mi2)
  )

save(den_zoning_grouped_no_airport_union, 
     file = "den_zoning_grouped_no_airport_union.RData")
den_zoning_grouped_no_airport_union %>% mapview()
  

