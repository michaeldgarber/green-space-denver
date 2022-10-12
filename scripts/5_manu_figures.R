#filename: 5_manu_figures
library(here)
library(tidyverse)
library(mapview)
library(sf)

#David suggested we create a panel figure of all scenarios.
#Im not sure if we'll be able to create one single panel figure, as it will be a lot, but let's try.

#Similar code to reference:
#~green-space-denver/docs/tables_figs_main_text.Rmd
#~green-space-denver/docs/tables_figs_appendix.Rmd

# Visualizing NDVI of all scenarios-------
#This visualizes the NDVI of each, mostly following code here:
#green-space-denver/docs/tables_figs_appendix.Rmd

## NDVI of all block groups---------
#Note this is directly copied from the appendix version.
#The weighted block-group-level mean NDVI on July 4, 2021 is presented here.
setwd(here("data-processed"))
load("den_co_bg_ndvi_geo.RData") #updated april 18 2022
#names(den_co_bg_ndvi_geo)
pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
mv_den_co_bg_ndvi  = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain, 
    at = seq(-0.1, 1, 0.1)
  )

mv_den_co_bg_ndvi

#A version without boundaries so that the other layers show up more clearly
#when layered on top of this:
mv_den_co_bg_ndvi_no_lines =  den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    lwd=0,
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain, 
    at = seq(-0.1, 1, 0.1)
  )

mv_den_co_bg_ndvi_no_lines

## Study area boundaries----------
#For reference, you have a map of the study area boundaries here:
load("study_area.RData") #created here: scripts/3_HIA_for_each_scenario.R
mv_study_area = study_area %>% 
  mapview(
    color = "black",
    alpha.regions=0, legend=FALSE
  )

mv_study_area

## Riparian areas---------

setwd(here("data-processed"))
load("den_bg_int_wtr_200ft_comp_ndvi.RData")
load("den_bg_int_wtr_200ft_tx_ndvi.RData")
load("den_co_osm_wtr.RData")
load("study_area_2876.RData")
pal_terrain_col = rev(terrain.colors(100)) 
### Complement (residential buffer)----------
mv_den_bg_int_wtr_200ft_comp_ndvi= den_bg_int_wtr_200ft_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft - 500 m",
    zcol = "ndvi_mean_wt")

### Treatment--------
mv_den_bg_int_wtr_200ft_tx_ndvi= den_bg_int_wtr_200ft_tx_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
#    define the breaks so consistent between layers
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; 
    layer.name = "NDVI, 200 ft buffer", #changed name from other code here.
#    lwd=0, #changed from other code
    zcol = "ndvi_mean_wt")


###--------Waterways in the Denver area--------
mv_den_co_osm_wtr = den_co_osm_wtr %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_wtr$water_type)),
    zcol = "water_type")



## Retention ponds-------
setwd(here("data-processed"))
load("den_bg_int_ogi_proj_comp_ndvi.RData")
load("den_bg_int_ogi_proj_tx_ndvi.RData")
load("den_co_osm_wtr.RData")
load("study_area_2876.RData")
pal_terrain_col = rev(terrain.colors(100)) 

### Complement (residential buffer)----------
mv_den_bg_int_ogi_proj_comp_ndvi= den_bg_int_ogi_proj_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 500m buffer, projects",
    zcol = "ndvi_mean_wt")

### The project only----------
mv_den_bg_int_ogi_proj_tx_ndvi= den_bg_int_ogi_proj_tx_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1),
#    lwd=0, #changed from other code
    layer.name = "NDVI, projects only",
    zcol = "ndvi_mean_wt")
mv_den_bg_int_ogi_proj_tx_ndvi

mv_den_bg_int_ogi_proj_tx_ndvi + 
  mv_den_bg_int_ogi_proj_comp_ndvi+
    mv_study_area

## Parking (treatment areas)---------
setwd(here("data-processed"))
load("den_bg_int_prkng_comp_ndvi.RData")
load("den_bg_int_prkng_tx_ndvi.RData")
load("study_area_2876.RData")
load("union_station_1_mi_4326.RData")
load("union_station_1_mi_2876.RData")
load("union_station_2_mi_4326.RData") 
load("union_station_2_mi_2876.RData")

pal_terrain_col = rev(terrain.colors(100)) 
library(leaflet.extras)
library(mapview)
#### Buffer excluding the parking---------
sf::sf_use_s2(FALSE) #have to use this as I was getting an error that loop not valid
mv_den_bg_int_prkng_comp_ndvi= den_bg_int_prkng_comp_ndvi %>% 
  st_transform(2876) %>% 
  st_simplify(dTolerance = 20) %>%  #make size much smaller for visual. intentionally ordered here
  #on the website, I use this intersection. In the script, I don't need to.
#  st_intersection(union_station_2_mi_2876) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>% 
  mapview(
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; 
    col.regions = pal_terrain_col,
    layer.name = "NDVI, 500 m buffer, parking",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi

#alternate version for use below. Remove lines

mv_den_bg_int_prkng_comp_ndvi_no_lines= den_bg_int_prkng_comp_ndvi %>% 
  st_transform(2876) %>% 
  st_simplify(dTolerance = 20) %>%  #make size much smaller for visual. intentionally ordered here
  #on the website, I use this intersection. In the script, I don't need to.
  #  st_intersection(union_station_2_mi_2876) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>% 
  mapview(
    lwd=0,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; 
    col.regions = pal_terrain_col,
    layer.name = "NDVI, block group", #for figure below
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi_no_lines
#### The parking only----------
mv_den_bg_int_prkng_tx_ndvi= den_bg_int_prkng_tx_ndvi %>% 
  st_transform(2876) %>% 
  #make size much smaller for visual. intentionally ordered here
  st_simplify(dTolerance = 20) %>%  
  #don't need this on the script version. use on the rmarkdown version
#  st_intersection(union_station_2_mi_2876) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>% 
  mapview(
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; 
    col.regions = pal_terrain_col,
    lwd=0, #added on script version
    layer.name = "NDVI, parking only",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi_alt

#Try a single parking layer, all the same color.

mv_prkng_both = mv_den_bg_int_prkng_comp_ndvi+ 
  mv_den_bg_int_prkng_tx_ndvi
mv_prkng_both


# Simpler: visualize all intervention locations, no color-------
mv_den_co_bg_ndvi


## Riparian areas------
names(den_bg_int_wtr_200ft_tx_ndvi)
mv_den_bg_int_wtr_200ft_mono_chrome= den_bg_int_wtr_200ft_tx_ndvi %>% 
  group_by(scenario_sub) %>% #collapse into one thing
  summarise(n=n()) %>% 
  mapview(col.regions = "black",
          alpha.regions=1,
          layer.name = "Riparian areas")
mv_den_bg_int_wtr_200ft_mono_chrome

## Retention ponds------
names(den_bg_int_ogi_proj_tx_ndvi)
mv_den_bg_int_ogi_proj_mono_chrome= den_bg_int_ogi_proj_tx_ndvi %>% 
  group_by(scenario_sub) %>% #collapse into one thing
  summarise(n=n()) %>% 
  mapview(col.regions = "black",
          alpha.regions=1,
          legend=FALSE) #I'm manually adding a legend
mv_den_bg_int_ogi_proj_mono_chrome

## Parking lots-------
names(den_bg_int_prkng_tx_ndvi)
table(den_bg_int_prkng_tx_ndvi$buff_type)
mv_den_bg_int_prkng_mono_chrome= den_bg_int_prkng_tx_ndvi %>% 
  group_by(buff_type) %>% 
  #I had wanted to separate because it's slow, but this
  #strange this is happening, where if I define it like this,
  #it automatically goes to the back layer, so don't separate them, actually
  #separate because slow
  summarise(n=n()) %>% 
  mapview(
    layer.name = "Parking",
    alpha.regions=1,
    col.regions = "black")
mv_den_bg_int_prkng_mono_chrome

#Yea, I like this. So it will be a panel figure of four.
#1. block group with NDVI colored
#the others in monochrome layered on top of that.
mv_den_co_bg_ndvi_no_lines +
  mv_den_bg_int_wtr_200ft_mono_chrome+
  mv_den_bg_int_ogi_proj_mono_chrome+
  mv_den_bg_int_prkng_mono_chrome

#For some reason, the parking layer always goes to the back
#unless I use the complement version:
mv_den_bg_int_prkng_comp_ndvi_no_lines+ 
  mv_den_bg_int_prkng_mono_chrome

