#filename: 5_manu_figures
library(here)
library(tidyverse)
library(mapview)
library(sf)
library(RColorBrewer)
library(viridis)
library(shades)
#Revised Oct 12, 2022
#David suggested we create a panel figure of all scenarios.

#Update Oct 20, 2022:
#make figures color-blind safe; change palette from terrain.colors(100) to
#a colorbrewer palette that's colorblind safe.

#Here February 1, 2023 working on Environment International revision

#Similar code to reference:
#~green-space-denver/docs/tables_figs_main_text.Rmd
#~green-space-denver/docs/tables_figs_appendix.Rmd

# Visualizing NDVI of all scenarios-------#
#I was originally going to visualize the NDVI of each area, but I think that's too hard to see.
#See below - Oct 12, 2022

# NDVI of all block groups---------
#Note this is directly copied from the appendix version.
#The weighted block-group-level mean NDVI on July 4, 2021 is presented here.
setwd(here("data-processed"))
load("den_co_bg_ndvi_geo.RData") #updated april 18 2022
#names(den_co_bg_ndvi_geo)
pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
pal_terrain_10 = terrain.colors(10) %>% rev()#reverse the order of the palette
pal_terrain_10 %>% swatch()
#This version has boundaries. Try a few different background maps.
mapviewGetOption("basemaps")
#Usual defaults are:
## [1] "CartoDB.Positron"   "CartoDB.DarkMatter" "OpenStreetMap"     
## [4] "Esri.WorldImagery"  "OpenTopoMap"
#change default
#https://leaflet-extras.github.io/leaflet-providers/preview/
mapviewOptions(
  basemaps = c("CartoDB.Positron",
               "Stamen.Toner", "Stamen.TonerBackground", "Stamen.TonerLite",
                "OpenStreetMap"))
mapviewGetOption("basemaps")
summary(den_co_bg_ndvi_geo$ndvi_mean_wt)
## block groups with lines without junk--------
mv_den_co_bg_ndvi  = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    lwd=1.5,
    #Trying different palettes.  Don't use terrain as bad for colorblindness
#    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
#    col.regions = pal_terrain, 
    homebutton=FALSE, #this removes the small label on the bottom right
    at = seq(0, 1, 0.1)  #there are none that are -1, so why go to negative?
  )%>% 
  #remove junk
  mapview::removeMapJunk(
    junk = c(
      "layersControl",
      "zoomControl"
      ))

class(den_co_bg_ndvi_geo)
mv_den_co_bg_ndvi

## block group with lines with junk----
mv_den_co_bg_ndvi_w_junk  = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    lwd=1.5,
    #Trying different palettes.  Don't use terrain as bad for colorblindness
    #    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
    #    col.regions = pal_terrain, 
    homebutton=FALSE, #this removes the small label on the bottom right
    at = seq(0, 1, 0.1)  #there are none that are -1, so why go to negative?
  )

mv_den_co_bg_ndvi_w_junk 

## block group without boundary lines-------
#A version without boundaries so that the other layers show up more clearly
#when layered on top of this:
mv_den_co_bg_ndvi_no_lines =  den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    lwd=0,
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    #Trying different palettes.  Don't use terrain as bad for colorblindness
    #    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
    #    col.regions = pal_terrain,
    at = seq(-0.1, 1, 0.1)
  )

mv_den_co_bg_ndvi_no_lines

##  block group vis without legend--------
#as it's not always needed
mv_den_co_bg_ndvi_no_lines_no_legend =  den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    lwd=0,
    legend=FALSE,
    zcol = "ndvi_mean_wt",
    #Trying different palettes.  Don't use terrain as bad for colorblindness
    #    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
    #    col.regions = pal_terrain,
    at = seq(-0.1, 1, 0.1)
  ) 

mv_den_co_bg_ndvi_no_lines_no_legend

# one with a very opaque legend or with a different basemap
#just to screenshot the legend
mapviewGetOption("basemaps")
#change default
#https://leaflet-extras.github.io/leaflet-providers/preview/
#mapviewOptions(basemaps = c("Stamen.Toner", "Stamen.TonerBackground", "Stamen.TonerLite"))
den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    lwd=0,
    layer.name = "NDVI, block group",
    legend.opacity=1,
    zcol = "ndvi_mean_wt",
    col.regions = viridis_pal(direction=-1),
    #    col.regions = pal_terrain, #bad for colorblindness
    at = seq(-0.1, 1, 0.1)
  )

## Study area boundaries----------
#For reference, you have a map of the study area boundaries here:
load("study_area.RData") #created here: scripts/3_HIA_for_each_scenario.R
mv_study_area = study_area %>% 
  mapview(
    color = "black",
    alpha.regions=0, legend=FALSE
  )

mv_study_area


## filter below NDVI values-------
### highest--------
mv_den_co_bg_ndvi_filtered_hi  = den_co_bg_ndvi_geo %>%
  filter(ndvi_mean_wt <0.54) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    lwd=1.5,
    #Trying different palettes.  Don't use terrain as bad for colorblindness
    #    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
    #    col.regions = pal_terrain, 
    homebutton=FALSE, #this removes the small label on the bottom right
    at = seq(0, 1, 0.1)  #there are none that are -1, so why go to negative?
  )

mv_den_co_bg_ndvi_filtered_hi
### mid------
mv_den_co_bg_ndvi_filtered_mid = den_co_bg_ndvi_geo %>% 
  filter(ndvi_mean_wt<0.42) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    lwd=1.5,
    #Trying different palettes.  Don't use terrain as bad for colorblindness
    #    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
    #    col.regions = pal_terrain, 
    homebutton=FALSE, #this removes the small label on the bottom right
    at = seq(0, 1, 0.1)  #there are none that are -1, so why go to negative?
  )

mv_den_co_bg_ndvi_filtered_mid

### lo------
mv_den_co_bg_ndvi_filtered_lo = den_co_bg_ndvi_geo %>% 
  filter(ndvi_mean_wt<0.34) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    lwd=1.5,
    #Trying different palettes.  Don't use terrain as bad for colorblindness
    #    col.regions = viridis_pal(direction=-1),
    col.regions= viridis_pal(option = "G", direction = 1),
    #    col.regions = pal_terrain, 
    homebutton=FALSE, #this removes the small label on the bottom right
    at = seq(0, 1, 0.1)  #there are none that are -1, so why go to negative?
  )

mv_den_co_bg_ndvi_filtered_lo


# Riparian areas---------

setwd(here("data-processed"))
load("den_bg_int_wtr_200ft_comp_ndvi.RData")
load("den_bg_int_wtr_200ft_tx_ndvi.RData")
load("den_co_osm_wtr.RData")
load("study_area_2876.RData")
pal_terrain_col = rev(terrain.colors(100)) 


### Treatment--------
#Changing color palette 
mv_den_bg_int_wtr_200ft_tx_ndvi= den_bg_int_wtr_200ft_tx_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; 
    layer.name = "NDVI, 200 ft buffer", #changed name from other code here.
    #    lwd=0, #changed from other code
    zcol = "ndvi_mean_wt")

mv_den_bg_int_wtr_200ft_tx_ndvi

###--------Waterways in the Denver area--------
mv_den_co_osm_wtr = den_co_osm_wtr %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_wtr$water_type)),
    zcol = "water_type")

### Complement (residential buffer)----------
#Complement within residential buffer
mv_den_bg_int_wtr_200ft_comp_ndvi= den_bg_int_wtr_200ft_comp_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft - 500 m",
    zcol = "ndvi_mean_wt")
mv_den_bg_int_wtr_200ft_comp_ndvi

mv_den_bg_int_wtr_200ft_tx_ndvi +mv_den_bg_int_wtr_200ft_comp_ndvi

# Complement (negative space) of the whole study area. 
#Needed to get some mapviews to work, unfortunately.
#I checked, and I never made this exact spatial object here:
#green-space-denver/scripts/3_HIA_for_each_scenario.R
den_bg_int_wtr_200ft_tx_union = den_bg_int_wtr_200ft_tx_ndvi %>% 
  group_by(buff_type) %>% 
  summarise(n=n())
den_bg_int_wtr_200ft_comp_ndvi %>% mapview()

den_co_bg_ndvi_comp_wtr_200ft = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  st_difference(den_bg_int_wtr_200ft_tx_union)



###  Final vis-------
#Update October 12, 2022
#I'm plotting all of these on the st_difference() of the block groups
#because mapview wasn't letting me put the treatment area as the top layer
#for some of them, so this will ensure that the treatment layer stays on top.

# Oct 12, 2022 - 
#Do this procedure under each scenario rather than bringing them all together in the end.
#Easier to keep track of object names this way.

names(den_bg_int_wtr_200ft_tx_ndvi)
#just black.
mv_den_bg_int_wtr_200ft_mono_chrome= den_bg_int_wtr_200ft_tx_ndvi %>% 
  group_by(scenario_sub) %>% #collapse into one thing
  summarise(n=n()) %>% 
  mapview(
    col.regions = "black",
    alpha.regions=1,
    homebutton=FALSE,
    legend=FALSE) #I'm manually adding a legend
mv_den_bg_int_wtr_200ft_mono_chrome
  
#no borders between block groups
mv_den_co_bg_ndvi_comp_wtr_200ft_no_lines = den_co_bg_ndvi_comp_wtr_200ft %>% 
  mapview(
    lwd=0, #no borders
    legend=FALSE,
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    homebutton=FALSE,
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-0.1, 1, 0.1)
  )

#Combine and remove junk
mv_wtr_200ft_for_paper = mv_den_co_bg_ndvi_comp_wtr_200ft_no_lines +
  mv_den_bg_int_wtr_200ft_mono_chrome
mv_wtr_200ft_for_paper %>% #works
  removeMapJunk(junk = c("zoomControl", "layersControl"))


# Retention ponds-------
#Updating color palette February 6th, 2023
setwd(here("data-processed"))
load("den_bg_int_ogi_proj_comp_ndvi.RData")
load("den_bg_int_ogi_proj_tx_ndvi.RData")
load("den_co_osm_wtr.RData")
load("study_area_2876.RData")
pal_terrain_col = rev(terrain.colors(100)) 

### The project only----------
mv_den_bg_int_ogi_proj_tx_ndvi= den_bg_int_ogi_proj_tx_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1),
    #    lwd=0, #changed from other code
    layer.name = "NDVI, projects only",
    zcol = "ndvi_mean_wt")
mv_den_bg_int_ogi_proj_tx_ndvi



### Complement (residential buffer)----------
mv_den_bg_int_ogi_proj_comp_ndvi= den_bg_int_ogi_proj_comp_ndvi %>% 
  mapview(
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-.1, 1, 0.1), 
    layer.name = "NDVI, 500m buffer, projects",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_ogi_proj_tx_ndvi + 
  mv_den_bg_int_ogi_proj_comp_ndvi+
  mv_study_area

# Complement (negative space) of the whole study area. 
#Needed to get some mapviews to work, unfortunately.
#I checked, and I never made this exact spatial object here:
#green-space-denver/scripts/3_HIA_for_each_scenario.R
den_bg_int_ogi_proj_tx_union = den_bg_int_ogi_proj_tx_ndvi %>% 
  group_by(buff_type) %>% 
  summarise(n=n())
den_bg_int_ogi_proj_tx_union %>% mapview()

den_co_bg_ndvi_comp_ogi_proj = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  st_difference(den_bg_int_ogi_proj_tx_union)

## Final vis-------
mv_den_bg_int_ogi_proj_mono_chrome= den_bg_int_ogi_proj_tx_ndvi %>% 
  group_by(scenario_sub) %>% #collapse into one thing
  summarise(n=n()) %>% 
  mapview(
    col.regions = "black",
    alpha.regions=1,
    homebutton=FALSE,
    legend=FALSE) #I'm manually adding a legend
mv_den_bg_int_ogi_proj_mono_chrome

#no borders between block groups
mv_den_co_bg_ndvi_comp_ogi_proj_no_lines = den_co_bg_ndvi_comp_ogi_proj %>% 
  mapview(
    lwd=0, #no borders
    legend=FALSE,
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    homebutton=FALSE,
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-0.1, 1, 0.1)
  )

#Combine and remove junk
mv_ogi_proj_for_paper = mv_den_co_bg_ndvi_comp_ogi_proj_no_lines +
  mv_den_bg_int_ogi_proj_mono_chrome
mv_ogi_proj_for_paper %>% #works
  removeMapJunk(junk = c("zoomControl", "layersControl"))


# Parking (treatment areas)---------
#Revising palette February 6th, 2023
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
    col.regions= viridis_pal(option = "G", direction = 1),
    layer.name = "NDVI, 500 m buffer, parking",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi

## alternate version for use below. Remove lines
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
    col.regions= viridis_pal(option = "G", direction = 1),
    layer.name = "NDVI, block group", #for figure below
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi_no_lines

##### complement without legend for vis below--------
mv_den_bg_int_prkng_comp_ndvi_no_lines_no_legend= den_bg_int_prkng_comp_ndvi %>% 
  st_transform(2876) %>% 
  st_simplify(dTolerance = 20) %>%  #make size much smaller for visual. intentionally ordered here
  #on the website, I use this intersection. In the script, I don't need to.
  #  st_intersection(union_station_2_mi_2876) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>% 
  mapview(
    lwd=0,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; 
    col.regions= viridis_pal(option = "G", direction = 1),
    legend=FALSE,
    homebutton=FALSE,
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_comp_ndvi_no_lines_no_legend


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



#Try a single parking layer, all the same color.

mv_prkng_both = mv_den_bg_int_prkng_comp_ndvi+ 
  mv_den_bg_int_prkng_tx_ndvi
mv_prkng_both


# Complement (negative space) of the whole study area. 
#Needed to get some mapviews to work, unfortunately.
#I checked, and I never made this exact spatial object here:
#green-space-denver/scripts/3_HIA_for_each_scenario.R
den_bg_int_prkng_tx_union = den_bg_int_prkng_tx_ndvi %>% 
  group_by(buff_type) %>% 
  summarise(n=n())
den_bg_int_prkng_tx_union %>% mapview()

den_co_bg_ndvi_comp_prkng = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  st_difference(den_bg_int_prkng_tx_union)
#Takes a while for parking but not terrible (less than 30 s)

## Final vis-------
mv_den_bg_int_prkng_mono_chrome= den_bg_int_prkng_tx_union %>% 
  mapview(
    col.regions = "black",
    alpha.regions=1,
    homebutton=FALSE,
    legend=FALSE) #I'm manually adding a legend
mv_den_bg_int_prkng_mono_chrome

#no borders between block groups
mv_den_co_bg_ndvi_comp_prkng_no_lines = den_co_bg_ndvi_comp_prkng %>% 
  mapview(
    lwd=0, #no borders
    legend=FALSE,
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    homebutton=FALSE,
    col.regions= viridis_pal(option = "G", direction = 1),
    at = seq(-0.1, 1, 0.1)
  )

#Combine and remove junk
mv_prkng_for_paper = mv_den_co_bg_ndvi_comp_prkng_no_lines +
  mv_den_bg_int_prkng_mono_chrome
mv_prkng_for_paper %>% #works
  removeMapJunk(junk = c("zoomControl", "layersControl"))


# Simpler: visualize all intervention locations, no color-------
#Update October 12, 2022
#I'm plotting all of these on the st_difference() of the block groups
#because mapview wasn't letting me put the treatment area as the top layer
#for some of them, so this will ensure that the treatment layer stays on top.


  

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
mapviewOptions()
mv_den_bg_int_prkng_mono_chrome= den_bg_int_prkng_tx_ndvi %>% 
  group_by(buff_type) %>% 
  #I had wanted to separate because it's slow, but this
  #strange this is happening, where if I define it like this,
  #it automatically goes to the back layer, so don't separate them, actually
  #separate because slow
  summarise(n=n()) %>% 
  mapview(
    layer.name = "Parking",
    col.regions = "black",
    alpha.regions=1,
#    homebutton=FALSE, #DOESN'T work
    legend=FALSE) #I'm manually adding a legend
mv_den_bg_int_prkng_mono_chrome

#Yea, I like this. So it will be a panel figure of four.
#1. block group with NDVI colored
#the others in monochrome layered on top of that.
mv_den_co_bg_ndvi_no_lines +
  mv_den_bg_int_wtr_200ft_mono_chrome+
  mv_den_bg_int_ogi_proj_mono_chrome+
  mv_den_bg_int_prkng_mono_chrome

#For some reason, the parking layer always goes to the back
#unless I use the complement for the BG NDVI:
mv_den_bg_int_prkng_comp_ndvi_no_lines+ 
  mv_den_bg_int_prkng_mono_chrome

## One each without a legend, as I can load a legend another way-------
mv_den_co_bg_ndvi_no_lines_no_legend + mv_den_bg_int_wtr_200ft_mono_chrome
mv_den_co_bg_ndvi_no_lines_no_legend+ mv_den_bg_int_ogi_proj_mono_chrome
mv_den_bg_int_prkng_comp_ndvi_no_lines_no_legend+ mv_den_bg_int_prkng_mono_chrome 
hi = removeMapJunk(mv_den_co_bg_ndvi_no_lines_no_legend, junk=c("layersControl", "zoomControl"))
hi
# Try tmap


