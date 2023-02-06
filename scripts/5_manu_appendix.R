# Code to create the appendix
# Comment October 13, 2022: it's actually simpler
#for me to create this in a script rather than in an RMarkdown
#doc. 

#The RMarkdown version has been updated as well Feb 6 2023:
#green-space-denver/docs/tables-figs-appendix-web.Rmd

library(here)
library(tidyverse)
library(sf)
library(terra)
library(raster) 
library(mapview) 
library(viridis)
library(scales)
library(lubridate) #for ggplot

# eAppendix 1: native plant plots-------
setwd(here("data-processed"))
load("places_native_geo.RData")
load("places_native_nogeo.RData")
load("native_places_ndvi_day_nogeo.RData")
sf::sf_use_s2(FALSE) 
#  Okay, I'm just focusing on the three that we actually used. 
#The easiest way to plot this will
#be as 3 separate layers.
table(places_native_nogeo$place_name_short)
library(shades)
library(RColorBrewer)
pal_accent_3 = RColorBrewer::brewer.pal(3, name = "Accent")
pal_accent_3 %>% swatch()
mv_dbg_native_zone_point = places_native_geo %>% 
  filter(place_name_short == "Denver Botanic G. Native Zone") %>% 
  st_centroid() %>% #easier to see as ap oint
  mapview(
    homebutton=FALSE,
    layer.name = "Denver Botanic Gardens Native Plot",
    col.regions =pal_accent_3[1],
    color = "black")
mv_dbg_native_zone_point
mv_green_mountain_polygon = places_native_geo %>% 
  filter(place_name_short == "Green Mountain") %>% 
  mapview(
    homebutton=FALSE,
    layer.name = "Green Mountain",
    col.regions =pal_accent_3[2],
    color = "black")
mv_green_mountain_polygon

mv_dbg_green_roof_point = places_native_geo %>% 
  filter(place_name_short == "Denver Botanic G. Green Roof") %>% 
  st_centroid() %>% #easier to see as ap oint
  mapview(
    homebutton=FALSE,
    layer.name = "Denver Botanic Gardens Green Roof",
    col.regions = pal_accent_3[3],
    color = "black"
  )

mv_dbg_green_roof_point

#all three

mv_3_native_places = mv_dbg_native_zone_point+
  mv_green_mountain_polygon+
  mv_dbg_green_roof_point

mv_3_native_places
#Remove junk for putting in paper.
#layersControl is the other option
mv_3_native_places %>% 
  removeMapJunk(junk = c("zoomControl"))
mv_green_mountain_polygon
places_native_geo_points = places_native_geo %>% 
  st_make_valid() %>% 
  st_transform(2876) %>% 
  st_centroid()

mapviewOptions(
  basemaps = c("CartoDB.Positron",
               "Stamen.Toner", "Stamen.TonerBackground", "Stamen.TonerLite"
               ))

#eAppendix 2 - demographics-------
## Table-------
setwd(here("data-processed"))
load("den_bg_acs5_wrangle_geo.RData")
#see here for the table:
#green-space-denver/docs/tables_figs_appendix.Rmd

## Map of included census block groups
mapviewOptions(
  basemaps = c("CartoDB.Positron",
               "Stamen.Toner", "Stamen.TonerBackground", "Stamen.TonerLite"
  ))
den_bg_acs5_wrangle_geo %>% 
  filter(county_fips=="031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  mutate(
    nbhd_northeast_exclude_fac = as.factor(nbhd_northeast_exclude),
    included_in_study = case_when(
      nbhd_northeast_exclude_fac==1~"No",
      TRUE ~ "Yes"
    )) %>% 
  mapview(
    zcol = "included_in_study",
    layer.name = "Block group included in study")

## Map population density by block group------
setwd(here("data-processed"))
load("lookup_tract_nbhd_northeast_exclude.RData")
load("den_bg_acs5_wrangle_geo.RData")
den_bg_for_study_area_vis =den_bg_acs5_wrangle_geo %>% 
  filter(county_fips=="031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0)  #Yes, filter.
mapviewOptions(
  basemaps = c("CartoDB.Positron"
  ))
den_bg_for_study_area_vis %>% 
  dplyr::select(ends_with( "fips"), contains("pop_dens")) %>% 
  mapview(
    homebutton=FALSE,
    layer.name = "Population density per square mile",
    zcol = "pop_dens_mi2_all") %>% 
  removeMapJunk(junk = c("zoomControl","layersControl"))

## Map age by block group------
den_bg_for_study_area_vis %>% 
  mapview(
    homebutton=FALSE,
    layer.name = "Median age",
    zcol = "age_med") %>% 
  removeMapJunk(junk = c("zoomControl","layersControl"))

## Map race (proportion non-white) by census tract
den_bg_for_study_area_vis %>% 
  mapview(
    homebutton=FALSE,
    layer.name = "Proportion non-white race",
    zcol = "race_nw_prop") %>% 
  removeMapJunk(junk = c("zoomControl","layersControl"))

## Map poverty
#Note this is available at census tract level, not block group
load("lookup_tract_nbhd_northeast_exclude.RData")
load("den_tract_acs5_wrangle_geo.RData")
den_tract_acs5_wrangle_geo %>% 
  filter(county_fips=="031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>% 
  dplyr::select(ends_with( "fips"), contains("pov")) %>% 
  mapview(
    homebutton=FALSE,
    layer.name = "Proportion in poverty, last 12 months",
    zcol = "pov_last_12_prop") %>% 
  removeMapJunk(junk = c("zoomControl","layersControl"))
names(den_bg_for_study_area_vis)

## Map household income------
den_tract_acs5_wrangle_geo %>% 
  filter(county_fips=="031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>%  
  mapview(
    homebutton=FALSE,
    layer.name = "Median household income",
    zcol = "hh_inc_med") %>% 
  removeMapJunk(junk = c("zoomControl","layersControl"))

# eAppendix 3: more info about policy scenarios------
## Scenario 1: vis of block groups categorized by NDVI cutoffs--------
#This code is mostly copied from
#green-space-denver/docs/tables-figs-appendix-web.Rmd
setwd(here("data-processed"))
load("den_co_bg_ndvi_geo.RData")
#names(den_co_bg_ndvi_geo)
mv_bg_native_cat = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mutate(ndvi_native_cat = cut(ndvi_mean_wt, breaks= c(0, 0.34, 0.42, 0.54,1))) %>% 
  mapview(
    layer.name = "NDVI category",
    zcol = "ndvi_native_cat"
  )
mv_bg_native_cat

## Scenario 2: riparian areas scenario vis------
setwd(here("data-processed"))
load("den_bg_int_wtr_200ft_comp_ndvi.RData")
load("den_bg_int_wtr_200ft_tx_ndvi.RData")
load("den_co_osm_wtr.RData")
load("study_area_2876.RData")
pal_terrain_col = rev(terrain.colors(100)) 
mapviewOptions(
  basemaps = c("CartoDB.Positron",
               "Stamen.Toner", "Stamen.TonerBackground", "Stamen.TonerLite",
               "OpenStreetMap"))
mapviewGetOption("basemaps")
mv_den_bg_int_wtr_200ft_comp_ndvi= den_bg_int_wtr_200ft_comp_ndvi %>% 
  mapview(
#    col.regions = pal_terrain_col,
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft - 500 m",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_wtr_200ft_tx_ndvi= den_bg_int_wtr_200ft_tx_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft",
    zcol = "ndvi_mean_wt")

mv_den_co_osm_wtr = den_co_osm_wtr %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = turbo(n_distinct(den_co_osm_wtr$water_type)),
    zcol = "water_type")

mv_rip_all3 = mv_den_bg_int_wtr_200ft_tx_ndvi+ 
  mv_den_bg_int_wtr_200ft_comp_ndvi +
  mv_den_co_osm_wtr
mv_rip_all3

## Scenario 3.1. Large retention ponds---------
setwd(here("data-processed"))
load("den_bg_int_ogi_proj_comp_ndvi.RData")
load("den_bg_int_ogi_proj_tx_ndvi.RData")
load("den_co_osm_wtr.RData")
load("study_area_2876.RData")
pal_terrain_col = rev(terrain.colors(100)) 

#### 500 m Buffer excluding the project---------#
mv_den_bg_int_ogi_proj_comp_ndvi= den_bg_int_ogi_proj_comp_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 500m buffer, projects",
    zcol = "ndvi_mean_wt")

#### The project only----------#
mv_den_bg_int_ogi_proj_tx_ndvi= den_bg_int_ogi_proj_tx_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, projects only",
    zcol = "ndvi_mean_wt")

#### Bodies of water--------#
load("den_co_osm_water.RData")
mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = turbo(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")

#### Visualize all at once--------------#
mv_ogi_proj_all3 = mv_den_bg_int_ogi_proj_comp_ndvi+ 
  mv_den_bg_int_ogi_proj_tx_ndvi +mv_den_co_osm_wtr
mv_ogi_proj_all3

## Scenario 3.2. parcels-----------
load("den_landuse_union_sta.RData") #created scripts/0_read_wrangle_denver_land_use.R
load("den_landuse_2018.RData") #created scripts/0_read_wrangle_denver_land_use.R
nrow(den_landuse_2018)
mv_landuse_union_sta = den_landuse_union_sta %>% 
  dplyr::select(-starts_with("address")) %>% 
  mapview(
    zcol = "CPD_LANDUS",
    layer.name = "Land-use type",
    col.regions = viridis::turbo(n=n_distinct(den_landuse_union_sta$CPD_LANDUS)))
mv_landuse_union_sta

### Parcels map-------
#If I group it by CPD_LANDUS, I think it'll be faster to map.
den_landuse_2018_grouped = den_landuse_2018 %>% 
  group_by(CPD_LANDUS) %>% 
  summarise(n=n())
names(den_landuse_2018_grouped)
den_landuse_2018_grouped %>% 
  mapview(
    lwd=0,
    zcol = "CPD_LANDUS",
    layer.name = "Land-use type",
    col.regions = viridis::turbo(n=n_distinct(den_landuse_union_sta$CPD_LANDUS)))

### Map sample of parcels--------
library(here)
setwd(here("data-processed"))
load("den_landuse_sample.RData")
mv_den_landuse_sample = den_landuse_sample %>% 
  st_centroid() %>%   #easier to see if points:
  mapview(
    layer.name = "Parcel size category",
    zcol = "parcel_size_cat")
mv_den_landuse_sample

### Vis of NDVI near sampled parcels, etc.
setwd(here("data-processed"))
load("den_bg_int_parcel_comp_ndvi.RData")
load("den_bg_int_parcel_tx_ndvi.RData")
load("study_area_2876.RData")
load("union_station_1_mi_4326.RData") # we will limit to a 1-mi radius around union station
pal_terrain_col = rev(terrain.colors(100)) 
library(leaflet.extras)
library(mapview)
#actually not worrying about union station
#### Buffer excluding the parking---------#
sf::sf_use_s2(FALSE) #have to use this as I was getting an error that loop not valid
mv_den_bg_int_parcel_comp_ndvi= den_bg_int_parcel_comp_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    layer.name = "NDVI, 500 m buffer, parcel",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_parcel_comp_ndvi

#### The parking only----------#
mv_den_bg_int_parcel_tx_ndvi= den_bg_int_parcel_tx_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    layer.name = "NDVI, parcel only",
    zcol = "ndvi_mean_wt")

#### Visualize all at once--------------#
mv_parcel_both = mv_den_bg_int_parcel_tx_ndvi+ 
  mv_den_bg_int_parcel_comp_ndvi

mv_parcel_both

## Scenario 4 - Parking------
#### Buffer excluding the parking---------#
sf::sf_use_s2(FALSE) #have to use this as I was getting an error that loop not valid
#mapping everywhere, not just near union station
mv_den_bg_int_prkng_comp_ndvi= den_bg_int_prkng_comp_ndvi %>% 
  st_transform(2876) %>% 
#  st_simplify(dTolerance = 20) %>%  #make size much smaller for visual. intentionally ordered here
#  st_intersection(union_station_2_mi_2876) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    layer.name = "NDVI, 500 m buffer, parking",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi

#### The parking only----------#
mv_den_bg_int_prkng_tx_ndvi= den_bg_int_prkng_tx_ndvi %>% 
  st_transform(2876) %>% 
#  st_simplify(dTolerance = 20) %>%  #make size much smaller for visual. intentionally ordered here
#  st_intersection(union_station_2_mi_2876) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    layer.name = "NDVI, parking only",
    zcol = "ndvi_mean_wt")

#### Visualize all at once--------------#
mv_prkng_both = mv_den_bg_int_prkng_tx_ndvi+ 
  mv_den_bg_int_prkng_comp_ndvi

mv_prkng_both
#### Simple map of parking-------
load("den_bg_int_prkng_tx_union.RData")
load("den_bg_int_prkng_comp_union.RData")
#From 5_manu_appendix

den_bg_int_prkng_tx_union %>% mapview()

# eAppendix 4: equity indices-------
## map both equity indices---------
library(scales)
library(mapview)
library(leafsync)
library(leaflet.extras)
setwd(here("data-processed"))
load("den_equity_ind_2020.RData") #updated april 18 2022
load("cdphe_equity_geo.RData")
mv_cdphe_equity_den_tertile =  cdphe_equity_geo %>% 
  filter(county_fips == "031") %>% 
  mapview(
    layer.name = "Equity Index, CDPHE, tertile",
    zcol = "equity_bg_cdphe_tertile_den",
    col.regions = viridis_pal(direction = -1) #flip scale
  )

mv_den_equity_ind_tertile = den_equity_ind_2020 %>% 
  mapview(
    layer.name = "Equity Index, Denver, tertile",
    zcol = "equity_nbhd_denver_tertile",
    col.regions = viridis_pal(direction = 1) #don't flip scale
    
  )

mv_cdphe_equity_den_tertile
mv_den_equity_ind_tertile

#From leafsync
mv_equity_both = sync(mv_den_equity_ind_tertile,mv_cdphe_equity_den_tertile)

mv_equity_both
## scatterplot of NDVI and equity indices------
load("den_co_bg_pop_per_ndvi.RData") #made in scripts/4_pop_dens_per_ndvi.R
load("hia_all.RData")
library(shades)
ndvi_native_threshold_vector = hia_all %>% 
  distinct(ndvi_native_threshold) %>% 
  arrange(ndvi_native_threshold) %>% 
  pull()
terrain_colors_4= terrain.colors(4) %>% rev()
#to keep consistent, using exact same function used in 3_hia_for_each_scenario.
#Load functions and data
setwd(here())
getwd()
source(here("scripts", "0_read_equity_indices.R"))
den_co_bg_pop_per_ndvi %>% 
  dplyr::select(-tract_fips) %>% 
  link_equity_indices() %>% 
  ggplot(aes(x=equity_nbhd_denver, y=ndvi_mean_wt )) + 
  geom_point()+
  ylab("NDVI") +
  xlab("Denver Equity Index (lower is more disadvantaged)") +
  theme_bw(base_size = 14) +
  geom_hline(
    yintercept = ndvi_native_threshold_vector[1],
    linetype = "dashed", color = terrain_colors_4[4], lwd=1)+
  geom_hline(
    yintercept = ndvi_native_threshold_vector[2], lwd=1,
    linetype = "dashed", color = terrain_colors_4[4]) +
  geom_hline(
    yintercept = ndvi_native_threshold_vector[3], lwd=1,
    color = terrain_colors_4[4], #I thought about changing but I think one color is good
    linetype = "dashed")+
  geom_smooth(method = lm)

#eAppendix 5: supplemental tables-------
#located here
#green-space-denver/docs/tables-appendix.Rmd
# Pollinator summit extra figures--------
#  Nov 8, 2022: putting them here seems to make the most sense.
#here Nov 8, 2022: example for Native Plants SUmmit pres.
setwd(here("data-processed"))
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")
ndvi_den_metro_terr_5_yr$

pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
class(ndvi_den_metro_terr_5_yr$`20210704_NDVI`)
install.packages("tidyterra")
library(tidyterra)
names(ndvi_den_metro_terr_5_yr$`20210704_NDVI`)
## subset NDVI raster-------
#subset values above 0
#I guess I'm supposed to use clamp for this, per
#https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r
#filter would be better, as implemented recently in tidyterra
terra_by_pixel_20210704_NDVI_gt_0=ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  clamp(lower=0)  #strange, filter would be better.
  
terra_by_pixel_20210704_NDVI_gt_0
mv_by_pixel_20210704_NDVI_gt_0= terra_by_pixel_20210704_NDVI_gt_0 %>% 
  raster::raster() %>% 
  mapview(
    #Trying a few color palettes
    #    col.regions = pal_terrain, #bad for colorblindness.
    #I like Mako for this.
    col.regions= viridis_pal(option = "G", direction = 1),
#    col.regions = viridis_pal(direction=-1),
    layer.name = "NDVI") 
mv_by_pixel_20210704_NDVI_gt_0 %>% 
  mapview::removeMapJunk(
    junk = c(
      "zoomControl"
    ))

## Geocode Denver Botanic Gardnes so I can plot it for reference:
library(ggmap)
den_botanic_g_geocode  = as_tibble("Denver Botanic Gardens") %>% 
  rename(address = value) %>%  #rename the variable so it has a name
  mutate_geocode(address, force=TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"),crs = 4326) 


mv_den_botanic_g = den_botanic_g_geocode %>% 
  mapview(
    layer.name = "Denver Botanic Gardens",
    col.regions = "yellow",
    color="black")

## alternate mapviews for reference places---------
mv_dbg_native_zone_point_yellow = places_native_geo %>% 
  filter(place_name_short == "Denver Botanic G. Native Zone") %>% 
  st_centroid() %>% #easier to see as ap oint
  mapview(
    homebutton=FALSE,
    layer.name = "Denver Botanic Gardens Native Plot",
    col.regions ="Yellow",
    color = "black")
mv_dbg_native_zone_point_yellow
mv_green_mountain_polygon_green = places_native_geo %>% 
  filter(place_name_short == "Green Mountain") %>% 
  mapview(
    homebutton=FALSE,
    layer.name = "Green Mountain",
    col.regions ="Green",
    color = "Green")
mv_green_mountain_polygon_green

mv_dbg_green_roof_point_orange = places_native_geo %>% 
  filter(place_name_short == "Denver Botanic G. Green Roof") %>% 
  st_centroid() %>% #easier to see as ap oint
  mapview(
    homebutton=FALSE,
    layer.name = "Denver Botanic Gardens Green Roof",
    col.regions = "Orange",
    color = "black"
  )

mv_dbg_green_roof_point_orange


#combine them all
mv_den_co_bg_ndvi_w_junk+ #created in 5_manu_figures
  mv_green_mountain_polygon_green+
  mv_dbg_native_zone_point_yellow+
  mv_dbg_green_roof_point_orange



