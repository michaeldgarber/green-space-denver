#filename: 3_HIA_for_each_scenario
library(tidyverse)
library(sf)
library(mapview)
library(here)
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)
setwd(here("data-processed"))

#major revision april 14 2022 to make scenario datasets long form
# code is less repetitive and will be easier to bootstrap

#This code should
#-extract baseline NDVI for each polygon
#-calculate the total number of adults in each polygon as necessary
#-make the appropriate assumptions about how much NDVI would change in a given scenario
#-compute mortality and cases averted by age group

# Prep for all scenarios----------
## Summarize NDVI on one good day at the block-group level in the Denver area
#update 3/10/22
#Here, I want to pick one good day and summarize NDVI at the block-group level
#on that day.
## Load files, and limit geographical and temporal extent--------
# Here are the valid dates that you decided upon in 
# ~/scripts/1b_wrangle_check_landsat_ndvi_denver.R
### NDVI raster file ----------
load("lookup_date_is_valid_all.RData")
load("den_co_geo.RData") #Load Denver County's 
den_co_geo %>% mapview()
den_co_4326 = den_co_geo %>% st_transform(4326)
lookup_date_is_valid_all %>% filter(date_is_valid_all==1)
#Let's use July 4, 2021 and also limit to Denver County
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")
ndvi_den_metro_terr_5_yr$`20210704_NDVI`

#note this is is in 4326, and den_co_might be in something else
ndvi_den_co_20210704 = ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  terra::trim() %>%    #remove NAs
  terra::crop(den_co_4326)  
#This automatically converts to a bbox so it's a rectangle.
#for some reason, this band is not getting the NE area (airport)
#this takes about 30 seconds. probably not worth saving for that amount of time,
#given the complexity of saving raster files.

pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
mv_ndvi_den_co_20210704= ndvi_den_co_20210704 %>% 
  raster::raster() %>% 
  mapview(
    layer.name = "NDVI, pixel",
    col.regions = pal_terrain, 
    at = seq(-0.4, 1, 0.1)
  ) 

mv_ndvi_den_co_20210704
#save this for use in the rmarkdown code:
save(mv_ndvi_den_co_20210704, file = "mv_ndvi_den_co_20210704.RData")
load("den_co_tract_geo.RData") 
mv_den_co_tract_geo = den_co_tract_geo %>% 
  mapview(zcol = "tract_fips", layer.name = "Census Tract") 

mv_den_co_tract_geo@map %>% 
  addFullscreenControl()
mv_ndvi_den_co_20210704 + mv_den_co_tract_geo

### tracts and block groups-----------
#Limit administrative boundaries data to the Denver area only
#This can be done aspatially using the county field :)
#these files excluding water are created here:
# ~/green-space-denver/scripts/2_ndvi_tract_bg_park_den.R
load("den_metro_tract_no_wtr_geo.RData")
names(den_metro_tract_no_wtr_geo)
### Define tracts to remove from northeast based on neighborhood-----------------
#per above, there are a few on this july 4 2021 
#where the data are not complete. create a filter so you
#can remove as needed. If you gather more NDVI data with a wider
#bounding box, this may not be necessary
#ideally, this would be coded more upstream, but we only
#need it for this application, so okay here.
#remove by neighborhood, specifically the following nbhd ids: 23, 28, 45
load("lookup_den_nbhd_tract.RData")
lookup_den_nbhd_tract
class(lookup_den_nbhd_tract$nbhd_id)
lookup_tract_nbhd_northeast_exclude = den_metro_tract_no_wtr_geo %>% 
  left_join(lookup_den_nbhd_tract, by = "tract_fips") %>% 
  mutate(
    nbhd_northeast_exclude = case_when(
      nbhd_id %in% c(23, 28, 45) ~1,
      TRUE ~0
    )
  ) %>% 
  st_set_geometry(NULL) %>% 
  distinct(tract_fips, nbhd_northeast_exclude) %>% 
  as_tibble()
lookup_tract_nbhd_northeast_exclude
#### tracts--------
den_co_tract_no_wtr_filtered_geo = den_metro_tract_no_wtr_geo %>% 
  filter(county_fips == "031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0)

den_co_tract_no_wtr_filtered_geo %>% mapview(zcol = "nbhd_northeast_exclude")
#save this, as it's used below
save(lookup_tract_nbhd_northeast_exclude, file = "lookup_tract_nbhd_northeast_exclude.RData")
lookup_tract_nbhd_northeast_exclude

#make a unioned version of this without the holes and call it
#the study area for use in subsequent scenarios
#install.packages("nngeo") #to remove holes in sf objects
library(nngeo)
den_co_tract_geo %>% mapview()
class(den_co_tract_geo)
sf_use_s2()
study_area = den_co_tract_geo %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>% 
  st_simplify(dTolerance = 10) %>% #simplify every foort
  st_union() %>% 
  st_remove_holes()
study_area %>% mapview()
save(study_area, file = "study_area.RData")
st_crs(study_area)
study_area_2876 = study_area %>% 
  st_transform(2876)
save(study_area_2876, file = "study_area_2876.RData")
study_area_2876 %>% mapview()
#created 

#### block groups-------
load("den_metro_bg_no_wtr_geo.RData")
den_co_bg_no_wtr_filtered_geo = den_metro_bg_no_wtr_geo %>% 
  filter(county_fips == "031") %>% #note this file already has tract id
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>% 
  dplyr::select(-tract_fips)   #remove tract id since I can add it.

save(den_co_bg_no_wtr_filtered_geo, file = "den_co_bg_no_wtr_filtered_geo.RData")
den_co_bg_no_wtr_filtered_geo %>% mapview(zcol = "bg_fips")
#convert to 4326 because the raster is 4326.
#It failed silently when the crs wasn't the same.
den_co_bg_no_wtr_filtered_4326 = den_co_bg_no_wtr_filtered_geo %>% 
  st_transform(4326) %>% 
  #also create a row-number ID since extract automatically creates an id
  mutate(row_id_int=row_number()) #use row_id_int so it's consistent with the intersected pieces further below


lookup_row_id_int = den_co_bg_no_wtr_filtered_4326 %>% 
  distinct(row_id_int, bg_fips) %>% 
  as_tibble()

#a lookup for the no-water block group geometry
lookup_den_co_bg_no_wtr_geo = den_co_bg_no_wtr_filtered_geo  %>% 
  distinct(bg_fips, geometry)
lookup_den_co_bg_no_wtr_geo %>% mapview()
save(lookup_den_co_bg_no_wtr_geo, file = "lookup_den_co_bg_no_wtr_geo.RData")


### Load zoning data, because we will also exclude the airport zone and
# the industrial zone, per meeting with DRR 3/7
# source(here("scripts","0_read_wrangle_denver_land_use.R")) #this takes ~10 s



#I'm first computing NDVI diff for each scenario, and then in a subsequent
#major section, calculating avoidable deaths.
#It's better to isolate the avoidable death calculation as a single calc for bootstrapping
####################################################-
# COMPUTE NDVI-DIFF FOR EACH SCENARIO---------------
####################################################-
#1. Scenario 1: homogenous greening in each census block group---------
## Extract NDVI in those block groups on that day--------
#define native threshold 
ndvi_native_threshold = .5
#update 3/16/22 removing the date from the filename. it gets too long,
#and it's in the columns.
class(ndvi_den_co_20210704)
den_co_bg_no_wtr_filtered_4326 %>% mapview()

### A function for measuring NDVI on each block group or block-group intersection
#first df is the the ndvi raster layer
#second df is the block-group or block-group piece polygon over which NDVI is measured
#Note, it's rare that I use a function with two arguments, but this
#isn't so bad! good job, MDG
extract_wrangle_ndvi_bg_int = function(df1, df2){
  df1 %>% 
    terra::extract(
      #    exact=TRUE, #exact proportion covered. very slow code. don't use.
      weights = TRUE, #approximate proportion of cell covered by polygon
      y = terra::vect(df2)) %>% 
    as_tibble() %>%  #code is slow so break it up here if needed.
    rename(
      ndvi = `20210704_NDVI`,
      row_id_int = ID, #rename to the row id of the intersected block groups
      wt_area = weight #the area weight, as a proportion
    ) %>% 
    mutate(
      ndvi_wt_int = ndvi*wt_area, #for use in the weighted average
      date = lubridate::as_date("20210704")
    )  %>% 
    #link census tract or census-tract piece with the terra::extract id, renamed above
    left_join(df2, by = "row_id_int") %>% # 
    group_by(date, bg_fips) %>% #grouping by date is redundant but it generalizes just in case
    summarise(
      #don't add the suffixes just yet to ease replicability.
      #eventually will add suffixes to specify which NDVI
      sum_of_wts = sum(wt_area, na.rm=TRUE),
      wt_area = mean(wt_area, na.rm=TRUE),#curious so keep track
      ndvi_mean_no_wt = mean(ndvi, na.rm=TRUE), #check to make sure differs from weighted mean
      ndvi_wt_int = sum(ndvi_wt_int, na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      ndvi_mean_wt =ndvi_wt_int/sum_of_wts, #weighted mean
      #this isn't kept in all but can just remove via select
      ndvi_below_native_threshold = case_when( #whether baseline was above native threshold
        ndvi_mean_wt   < ndvi_native_threshold ~1,
        TRUE ~0
      )
    ) %>% 
    left_join(df2, by = "bg_fips") %>% #link in geo and area measurements
    st_as_sf() #make sf object
}

names(den_co_bg_no_wtr_filtered_geo)
names(den_co_bg_no_wtr_filtered_4326)
#Update April 6 2022
#Update April 14 2022. Decision to make long form so my var names are shorter and for easier bootstrapping. 
#It will be aspatial. That's fine. can always filter and then add the geo.
#adding a 30% scenario per interview with parks dept.
den_co_bg_ndvi_geo = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2 = den_co_bg_no_wtr_filtered_4326)
save(den_co_bg_ndvi_geo, file = "den_co_bg_ndvi_geo.RData")


#make a function like for the bg_int scenarios
#bg itself vs bg intersection below
mutate_ndvi_diff_bg_itself = function(df){
  df %>% 
    mutate(
        prop_area_comp = 1-prop_area_tx, #paradoxically, take 1 minus this now
        ndvi_alt_avg = prop_area_tx*ndvi_native_threshold+(1-prop_area_tx)*ndvi_mean_wt,
        ndvi_quo = ndvi_mean_wt, #rename this to _quo to be consistent with other scenarios
        ndvi_diff = ndvi_alt_avg-ndvi_quo
  )
}


## Define scenarios-----------
#100 percent native
den_co_bg_ndvi_alt_100_nogeo = den_co_bg_ndvi_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(
    #I use these vars in subsequent scenarios, so name them the same here, as well
    prop_area_tx = 1, #what proportion of the area (bg, here) is affected?
    scenario = "all-bg",
    scenario_sub = "100-pct" ) 
  

den_co_bg_ndvi_alt_100_nogeo
#30 percent native
den_co_bg_ndvi_alt_30_nogeo = den_co_bg_ndvi_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(
    #I use these vars in subsequent scenarios, so name them the same here, as well
    prop_area_tx = .3, #what proportion of the area (bg, here) is affected?
    scenario = "all-bg",
    scenario_sub = "30-pct" ) 

#20 percent native
den_co_bg_ndvi_alt_20_nogeo = den_co_bg_ndvi_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(
    #I use these vars in subsequent scenarios, so name them the same here, as well
    prop_area_tx = .2, #what proportion of the area (bg, here) is affected?
    scenario = "all-bg",
    scenario_sub = "20-pct" ) 

#combine them and final wrangling
den_co_bg_ndvi_alt_all_nogeo = den_co_bg_ndvi_alt_100_nogeo %>% 
  bind_rows(
    den_co_bg_ndvi_alt_30_nogeo, den_co_bg_ndvi_alt_20_nogeo
  ) %>% 
  mutate_ndvi_diff_bg_itself() %>% 
  #just keep bg_fips (and not tract or county) to avoid join conflicts
  dplyr::select(contains("bg_fips"), contains("scenario"), contains("ndvi"), contains("prop_area")) %>% 
  #4/16/22 add this for consistency here
  #note this is different than prop_area_tx
  #for example, the office-green-infra polygons are all considered "treatment" areas
  #but only 50% might be vegetated,
  #for the bg intervention, we always say 1
  mutate(prop_tx_itself_veg=1) #of the area of the actual treatment, what proportion is treated?


save(den_co_bg_ndvi_alt_all_nogeo, file = "den_co_bg_ndvi_alt_all_nogeo.RData")
den_co_bg_ndvi_alt_all_nogeo
names(den_co_bg_ndvi_alt_all_nogeo)

## intermediate mapviews--------
### visualize ndvi diff for one scenario------------
den_co_bg_ndvi_alt_all_nogeo %>% 
  filter(scenario_sub == "20-pct") %>% 
  left_join(lookup_den_co_bg_no_wtr_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "ndvi_diff",
    zcol = "ndvi_diff")



### Examine weighted NDVI by census block group--------
pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
mv_den_co_bg_ndvi  = den_co_bg_ndvi_geo %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain, 
    at = seq(-0.1, 1, 0.1)
  )
mv_den_co_bg_ndvi + mv_ndvi_den_co_20210704

#Examine block groups above/below native threshold
library(shades)
library(RColorBrewer)

RColorBrewer::brewer.pal(3, "RdYlGn")[c(1,3)]  %>%   swatch()
ndvi_below_thresh_pal = RColorBrewer::brewer.pal(3, "RdYlGn")[c(1,3)]  %>% rev()
ndvi_below_thresh_pal %>% swatch()
mv_bg_below_native_threshold =den_co_bg_ndvi_geo %>% 
  mutate(
  ndvi_below_native_threshold_char = case_when(
    ndvi_below_native_threshold == 1 ~ "Yes",
    ndvi_below_native_threshold == 0 ~ "No"
  )) %>% 
  mapview(
    layer.name = "NDVI below native threhsold",
    zcol = "ndvi_below_native_threshold_char",
    col.regions = ndvi_below_thresh_pal
  )

mv_bg_below_native_threshold

# 2. Scenario 2: waterways--------

## Create various buffers around the waterways from OSM-----
setwd(here("data-processed"))
load("den_co_geo.RData")
load("den_osm_water_poly_both.RData") #created ~scripts/0_load_denver_osm_parks_water.R
#shortening object names but leave for redundancy
den_osm_wtr_poly_both = den_osm_water_poly_both
st_crs(den_osm_water_poly_both)
#restrict to Denver County
den_co_geo_2876 = den_co_geo %>% 
  st_transform(2876) #co for county 
den_co_geo_2876 %>% mapview()

#meta-comment: I need to simplify the name and will lose some description. oh well.
#just remember that this includes both the original polygons and the lines that were
#lines but are now buffered lines, i.e., polygons
table(den_osm_water_poly_both$osm_name_pool_fountain)
table(den_osm_water_poly_both$water_type_pool_fountain)
den_co_osm_wtr = den_osm_wtr_poly_both %>% 
  st_intersection(den_co_geo_2876) %>% 
  #remove very small ponds, as they create unrealistic buffers
  #This varible created in 0_load_denver_osm_parks_water
  filter(area_ft2_lt_1500==0) %>% 
  #also exclude pools and fountains, either in the OSM name or water type
  #e.g., seal fountain and reflecting pool near civic pool will go away
  filter(osm_name_pool_fountain==0) %>% 
  filter(water_type_pool_fountain==0)
  
save(den_co_osm_wtr, file = "den_co_osm_wtr.RData")
den_co_osm_wtr %>% mapview(zcol = "water_type")
names(den_co_osm_wtr)

#make a unioned version of this so you can remove it from the buffers before measuring NDVI
#I do this several times so make a function
st_union_dplyr_way = function(df){
  df %>% 
    mutate(dummy=1) %>%
    group_by(dummy) %>%
    summarise(n=n()) %>%
    ungroup() %>% 
    dplyr::select(geometry)
  }

den_co_osm_wtr_union = den_co_osm_wtr %>% 
  st_union_dplyr_way()
save(den_co_osm_wtr_union, file = "den_co_osm_wtr_union.RData")

names(den_co_osm_wtr_union)

### 500 m buffer to represent residential exposure-------
#4/14/22 Note the creation of the buffers themselves can't be long-form because they each have distinct geometry,
#but I can introduce more long-form steps thereafter.

#First, a simple 500 m buffer around the bodies of water,
#as this will represent the population exposed.
#alternatively, we could create 500 m buffers from the edge of the
#proposed intervention (e.g., 500 m from 200 ft), but that seems
#more confusing to me. it's possible there are people on the perimeter
#of such a buffer where the the greened zone would only represent say 
#1% of their exposed area.
den_co_osm_wtr %>% mapview()
den_co_osm_wtr_res = den_co_osm_wtr %>% #calling it _res instead of _500m
  st_buffer(500*3.28084) #500 meters, but we're in feet
save(den_co_osm_wtr_res, file = "den_co_osm_wtr_res.RData")
den_co_osm_wtr_res %>% mapview()

den_co_osm_wtr_res_union = den_co_osm_wtr_res %>% 
  st_union_dplyr_way() #union this as well so it's one geo

save(den_co_osm_wtr_res_union, 
     file = "den_co_osm_wtr_res_union.RData")
den_co_osm_wtr_res_union %>% mapview()

##Exclude bodies of water from buffer
#this is a little confusing, but we need this both for the buffers
#that we would be greening as well as the full 500 m buffer, as
#we will need to characterize the NDVI of both
#a version excluding the bodies of waters themselves around just
#the zone that we would be greening
st_crs(den_co_osm_wtr_res_union)
st_crs(den_co_osm_wtr_union)
den_co_osm_wtr_res_union_no_wtr = den_co_osm_wtr_res_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_res_union_no_wtr %>% mapview()

### 200 feet (ideal)--------
den_co_osm_wtr_200ft_tx = den_co_osm_wtr %>% 
  st_buffer(200)
save(den_co_osm_wtr_200ft_tx, file = "den_co_osm_wtr_200ft_tx.RData")

den_co_osm_wtr_200ft_tx_union = den_co_osm_wtr_200ft_tx %>% 
  st_union_dplyr_way() #union

den_co_osm_wtr_200ft_union_no_wtr = den_co_osm_wtr_200ft_tx_union %>% 
  st_difference(den_co_osm_wtr_union) #remove water
den_co_osm_wtr_200ft_union_no_wtr %>% mapview()
save(den_co_osm_wtr_200ft_union_no_wtr, file = "den_co_osm_wtr_200ft_union_no_wtr.RData")

#Realization: ultimately we will be calculating a weighted average,
#where part of the exposure area in the 500 m will be greened and part will
#not, so I need to measure the NDVI of the part that would never be greened
#in each scenario, i.e., the complement of the intervention area

#the 500 m buffer with 200 feet taken out of it.
den_co_osm_wtr_200ft_comp = den_co_osm_wtr_res_union %>% 
  st_difference(den_co_osm_wtr_200ft_tx_union)
den_co_osm_wtr_200ft_comp %>% mapview()
save(den_co_osm_wtr_200ft_comp, file = "den_co_osm_wtr_200ft_comp.RData")


### 100 feet (realistic)---------
den_co_osm_wtr_100ft_tx = den_co_osm_wtr %>% 
  st_buffer(100)
save(den_co_osm_wtr_100ft_tx, file = "den_co_osm_wtr_100ft_tx.RData")

den_co_osm_wtr_100ft_tx_union = den_co_osm_wtr_100ft_tx %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_wtr_100ft_union_no_wtr = den_co_osm_wtr_100ft_tx_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_100ft_union_no_wtr %>% mapview()
save(den_co_osm_wtr_100ft_union_no_wtr, file = "den_co_osm_wtr_100ft_union_no_wtr.RData")

#the 500 m buffer with 100 feet taken out of it.
den_co_osm_wtr_100ft_comp = den_co_osm_wtr_res_union %>% 
  st_difference(den_co_osm_wtr_100ft_tx_union)
den_co_osm_wtr_100ft_comp %>% mapview()
save(den_co_osm_wtr_100ft_comp, file = "den_co_osm_wtr_100ft_comp.Rdata")

### 50 feet (very realistic)---------
den_co_osm_wtr_50ft_tx = den_co_osm_wtr %>% 
  st_buffer(50)
save(den_co_osm_wtr_50ft_tx, file = "den_co_osm_wtr_50ft_tx.RData")

den_co_osm_wtr_50ft_tx_union = den_co_osm_wtr_50ft_tx %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_wtr_50ft_union_no_wtr = den_co_osm_wtr_50ft_tx_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_50ft_union_no_wtr %>% mapview()
save(den_co_osm_wtr_50ft_union_no_wtr, file = "den_co_osm_wtr_50ft_union_no_wtr.RData")

#the 500 m buffer with 50 feet taken out of it.
den_co_osm_wtr_50ft_comp = den_co_osm_wtr_res_union %>% 
  st_difference(den_co_osm_wtr_50ft_tx_union)
den_co_osm_wtr_50ft_comp %>% mapview()
save(den_co_osm_wtr_50ft_comp, 
     file = "den_co_osm_wtr_50ft_comp.RData")

## Intersect block groups with buffers-------------
#First, I'm going to intersect the buffers (with holes for water) 
#around the census block groups and then measure the NDVI in each intersecting chunk

#Best to start with the block-group geometry without water
#and intersect it with the full buffer (including the water; it will go away)
#Update 4/3/22 restrict to Denver only (not metro)
load("den_co_bg_no_wtr_filtered_geo.RData")
st_crs(den_co_bg_no_wtr_filtered_geo)

#Intersect block groups with both the complement buffers and the intervention buffers
#what proportion of the block group is covered by the intersection?
#multiply the resulting area covered by the pop_est. density 
#(assume uniform pop_est dens. in block group)
load("den_bg_acs5_wrangle_geo.RData")
st_crs(den_co_osm_wtr_res)
st_crs(den_bg_acs5_wrangle_geo)
names(den_bg_acs5_wrangle_geo)

#I do this for all of them, so make a function, as the code gets to be long
#otherwise
bg_int_wrangle_last_steps = function(df){
  df %>% 
    #update 4/14/22 making these area variables long form (i.e., the same)
    #for every object
    #the area of piece of the block group overlapping the buffer
    mutate(
      area_ft2_bg_int = as.numeric(st_area(geometry)),
      area_mi2_bg_int = area_ft2_bg_int/(5280**2), #miles squared
    #Update 4/3/22 rather than link in the northeast exclusions, I'm
    #beginning with a file that doesn't have it.
    #more straightforward.
    #create a new row ID as you have elsewhere for this sort order.
      row_id_int = row_number()) %>% #make this the same for all of them
    st_transform(4326)  # the NDVI raster file is in 4326
  ##Note I removed the select out of this function, as it can vary by scenario
}

#Note I removed this out of the function 4/3/22
#dplyr::select(bg_fips, row_id_int, starts_with("area"))

#### The 500 m residential buffer-------
#We may not use this but good to have for reference
names(den_co_bg_no_wtr_filtered_geo)
den_co_bg_no_wtr_filtered_geo %>% mapview()# note it doesn't have water
#int_wtr for intersect water
den_bg_int_wtr_res = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_res_union) %>% #use union version
  mutate(   #4/14/22 beginning to make long form
    scenario_sub = "all", #possibly needed for all sub-scenarios
    buff_type = "res" #residential buffer
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

den_bg_int_wtr_res %>% mapview(
  col.regions = rainbow(n_distinct(den_bg_int_wtr_res$bg_fips)),
  zcol = "bg_fips")

#### The complement of the intervention areas within the 500 m buffer-----
#These will be used to calculate the weighted average NDVI where no intervention occurred
den_bg_int_wtr_200ft_comp = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_200ft_comp) %>% #union version
  mutate(
    scenario_sub = "200-ft",
    buff_type = "comp" #for complement, vs tx for intervention
  ) %>% 
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

den_bg_int_wtr_100ft_comp = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_100ft_comp) %>% 
  mutate(
    scenario_sub = "100-ft",
    buff_type = "comp" #for complement, vs tx for intervention
  ) %>% 
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))


den_bg_int_wtr_50ft_comp = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_50ft_comp) %>%  
  mutate(
    scenario_sub = "50-ft",
    buff_type = "comp" #for complement, vs tx for intervention
  ) %>% 
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

#### The intervention areas themselves---------
den_bg_int_wtr_200ft_tx = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_200ft_tx_union) %>% 
  mutate( buff_type = "200-ft") %>%    
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

den_bg_int_wtr_100ft_tx = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_100ft_tx_union) %>% 
  mutate(
    scenario_sub = "100-ft",
    buff_type = "tx" #for complement, vs tx for intervention
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

den_bg_int_wtr_50ft_tx = den_co_bg_no_wtr_filtered_geo %>% 
  st_intersection(den_co_osm_wtr_50ft_tx_union) %>% 
  mutate(
    scenario_sub = "50-ft",
    buff_type = "tx" #for complement, vs tx for intervention
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))


## Measure NDVI on those intersected polygons--------
###  500 m residential buffer----------
#calling it res instead of 500m just in case we change buffer size
#this will be the status quo scenario for the whole area.
st_crs(den_bg_int_wtr_res)
den_bg_int_wtr_res_ndvi = ndvi_den_co_20210704 %>% #renamed object from 500m to res
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_res) %>% #function created above.
  mutate(
    scenario_sub = "all", #possibly needed for all scenarios
    buff_type = "res" #residential buffer
  ) %>%
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), starts_with("ndvi_below"), contains("area")
  )

names(den_bg_int_wtr_res_ndvi)
save(den_bg_int_wtr_res_ndvi, 
     file = "den_bg_int_wtr_res_ndvi.RData")
den_bg_int_wtr_res_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")
den_bg_int_wtr_res_ndvi %>% 
  mapview(zcol = "ndvi_below_native_threshold")


### complement polygons--------
#i.e., the sym_diff between a 500 m buffer and an x-ft buffer

# 200 ft complement 
den_bg_int_wtr_200ft_comp %>% mapview()
den_bg_int_wtr_200ft_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_200ft_comp) %>%  
  mutate(
    scenario_sub = "200-ft",
    buff_type = "comp" #for complement, vs tx for intervention
    ) %>%
  dplyr::select(bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), contains("area"))

den_bg_int_wtr_200ft_comp_ndvi
save(den_bg_int_wtr_200ft_comp_ndvi, 
     file = "den_bg_int_wtr_200ft_comp_ndvi.RData")
den_bg_int_wtr_200ft_comp_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")

# 100 ft complement
den_bg_int_wtr_100ft_comp %>% mapview()
den_bg_int_wtr_100ft_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_100ft_comp) %>% #function created above.
  mutate(
    scenario_sub = "100-ft",
    buff_type = "comp" #for complement, vs tx for intervention
  ) %>%
  dplyr::select(bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), contains("area"))

den_bg_int_wtr_100ft_comp_ndvi
save(den_bg_int_wtr_100ft_comp_ndvi, 
     file = "den_bg_int_wtr_100ft_comp_ndvi.RData")
den_bg_int_wtr_100ft_comp_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")

# 50 ft complement 
den_bg_int_wtr_50ft_comp %>% mapview()
den_bg_int_wtr_50ft_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_50ft_comp) %>% #function created above.
  mutate(
    scenario_sub = "50-ft",
    buff_type = "comp" #for complement, vs tx for intervention
  ) %>%
  dplyr::select(bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), contains("area"))

den_bg_int_wtr_50ft_comp_ndvi
save(den_bg_int_wtr_50ft_comp_ndvi, 
     file = "den_bg_int_wtr_50ft_comp_ndvi.RData")
den_bg_int_wtr_50ft_comp_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")

###  intervention polygons--------

# 200 ft intervention 
den_bg_int_wtr_200ft_tx %>% mapview()
den_bg_int_wtr_200ft_tx_ndvi= ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_200ft_tx) %>% #function created above.
  mutate(
    scenario_sub = "200-ft",
    buff_type = "tx" #for complement, vs tx for intervention
  ) %>%
  dplyr::select(bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), contains("area"))


den_bg_int_wtr_200ft_tx_ndvi
save(den_bg_int_wtr_200ft_tx_ndvi, 
     file = "den_bg_int_wtr_200ft_tx_ndvi.RData")
den_bg_int_wtr_200ft_tx_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")

# 100 ft intervention 
den_bg_int_wtr_100ft_tx %>% mapview()
den_bg_int_wtr_100ft_tx_ndvi= ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_100ft_tx) %>% #function created above.
  mutate(
    scenario_sub = "100-ft",
    buff_type = "tx" #for complement, vs tx for intervention (treatment)
  ) %>%
  dplyr::select(bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), contains("area"))


den_bg_int_wtr_100ft_tx_ndvi
save(den_bg_int_wtr_100ft_tx_ndvi, 
     file = "den_bg_int_wtr_100ft_tx_ndvi.RData")
den_bg_int_wtr_100ft_tx_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")

# 50 ft intervention 
den_bg_int_wtr_50ft_tx %>% mapview()
den_bg_int_wtr_50ft_tx_ndvi= ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_50ft_tx) %>% #function created above.
  mutate(
    scenario_sub = "50-ft",
    buff_type = "tx" #for complement, vs tx for intervention
  ) %>%
  dplyr::select(bg_fips, scenario_sub, buff_type, starts_with("ndvi_mean"), contains("area"))

den_bg_int_wtr_50ft_tx_ndvi
save(den_bg_int_wtr_50ft_tx_ndvi, 
     file = "den_bg_int_wtr_50ft_tx_ndvi.RData")
den_bg_int_wtr_50ft_tx_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")

##  Compute weighted average NDVI under each scenario--------
### Visualize the elements that will be averaged-----
#For example,
pal_terrain_col = rev(terrain.colors(100)) 
names(den_bg_int_wtr_200ft_comp_ndvi)

mv_den_bg_int_wtr_200ft_comp_ndvi= den_bg_int_wtr_200ft_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft - 500 m",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_wtr_200ft_ndvi= den_bg_int_wtr_200ft_tx_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft",
    zcol = "ndvi_mean_wt")

mv_den_co_osm_wtr = den_co_osm_wtr %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_wtr$water_type)),
    zcol = "water_type")

mv_den_bg_int_wtr_200ft_ndvi+
  mv_den_bg_int_wtr_200ft_comp_ndvi +
  mv_den_co_osm_wtr


### Remove geometry from all NDVI datasets for linking-----------

#The full 500 m residential bufer
den_bg_int_wtr_res_ndvi_nogeo = den_bg_int_wtr_res_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

#The areas in the 500 m buffer that are the complement of the intervention areas.
den_bg_int_wtr_200ft_comp_ndvi_nogeo =den_bg_int_wtr_200ft_comp_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_wtr_100ft_comp_ndvi_nogeo =den_bg_int_wtr_100ft_comp_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_wtr_50ft_comp_ndvi_nogeo =den_bg_int_wtr_50ft_comp_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

#The intervention areas
den_bg_int_wtr_200ft_ndvi_nogeo = den_bg_int_wtr_200ft_tx_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_wtr_100ft_ndvi_nogeo = den_bg_int_wtr_100ft_tx_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

den_bg_int_wtr_50ft_ndvi_nogeo = den_bg_int_wtr_50ft_tx_ndvi %>%
  st_set_geometry(NULL) %>% 
  as_tibble()

### pivot_wider ndvi and area within sub-scenario------------------
#Okay, now we actually do have to make wider for each sub-scenario. That's okay.
#Let's practice pivot_wider
names(den_bg_int_wtr_50ft_tx_ndvi)
names(den_bg_int_wtr_50ft_comp_ndvi)
names(den_bg_int_wtr_res_ndvi)
den_bg_int_wtr_res_ndvi_nogeo
#### great example of pivot_wider!------------
#keep it long by sub-scenario but wide by intervention vs complement 
den_bg_int_wtr_200ft_ndvi_wide = den_bg_int_wtr_200ft_ndvi_nogeo %>% 
  bind_rows(
    den_bg_int_wtr_res_ndvi_nogeo, #the residential buffer
    den_bg_int_wtr_200ft_comp_ndvi_nogeo) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
  pivot_wider(
    # note some block groups will be covered by the comp but not by the tx
    names_from = buff_type,
    values_from = c(ndvi_mean_wt, area_mi2_bg_int)
  ) %>% 
  mutate(#add this info now
    scenario = "riparian",
    scenario_sub = "200-ft"
  )

den_bg_int_wtr_200ft_ndvi_wide

den_bg_int_wtr_100ft_ndvi_wide = den_bg_int_wtr_100ft_ndvi_nogeo %>% 
  bind_rows(
    den_bg_int_wtr_res_ndvi_nogeo, #the residential buffer
    den_bg_int_wtr_100ft_comp_ndvi_nogeo) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
  pivot_wider(
    # note some block groups will be covered by the comp but not by the tx
    names_from = buff_type,
    values_from = c(ndvi_mean_wt, area_mi2_bg_int)
  )  %>% 
  mutate(#add this info now
    scenario = "riparian",
    scenario_sub = "100-ft"
  )

den_bg_int_wtr_100ft_ndvi_wide

den_bg_int_wtr_50ft_ndvi_wide = den_bg_int_wtr_50ft_ndvi_nogeo %>% 
  bind_rows(
    den_bg_int_wtr_res_ndvi_nogeo, #the residential buffer
    den_bg_int_wtr_50ft_comp_ndvi_nogeo) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
  pivot_wider(
    # note some block groups will be covered by the comp but not by the tx
    names_from = buff_type,
    values_from = c(ndvi_mean_wt, area_mi2_bg_int)
  )  %>% 
  mutate(#add this info now
    scenario = "riparian",
    scenario_sub = "50-ft"
  )

den_bg_int_wtr_50ft_ndvi_wide
#these two should match
nrow(den_bg_int_wtr_res_ndvi)==nrow(den_bg_int_wtr_res_ndvi_nogeo)

#and the ohter two should have fewer rows
nrow(den_bg_int_wtr_50ft_comp_ndvi_nogeo)#or the same; fine.
nrow(den_bg_int_wtr_50ft_ndvi_nogeo)

### Bind them together and set alternate NDVI values------------

#define a function here, as this framework is used elsewhere - tx, comp, res
#all of the variables that are used to calculate ndvi_diff
mutate_ndvi_diff_bg_int = function(df){
  df %>% 
    #update: take the ndvi_alt_avg definition out of this because it will vary
    #based on the intervention. for example, sometimes we will need to assume that only 50%
    #of the intervened-upon area is affected, so we will multiply the threshold value by .5
    #and take a weighted average with the existing NDVI
    mutate(
      #the areal proportion the intervention area is of the full residential (500 m) buffer
      #note use 1 minus the complement because for some bg-intersections, it will be missing,
      #but the complement will have a value
      prop_area_tx = 1-(area_mi2_bg_int_comp/area_mi2_bg_int_res), 
      prop_area_comp = 1-prop_area_tx, #paradoxically, take 1 minus this now

      #calculate weighted average NDVI of alternative scenario based on area
      #renaming average to emphasize that this is the version that averages the comp area as well
      ndvi_alt_avg = 
        case_when(
          ndvi_alt_tx_only>0 ~prop_area_comp*ndvi_mean_wt_comp+prop_area_tx*ndvi_alt_tx_only, 
          TRUE ~ndvi_mean_wt_res #otherwise, it's just the full res buffer
        ),

      #3/22: note! because of the imperfect resolution, the weighted average NDVI
      #over the whole area (ndvi_mean_wt_res) does not necessarily 
      #equal the weighted average in terms of the areal proportion.
      #conceptually, they should equal one another, but they might not.
      #that suggests we need to re-calculate
      #the weighted average at baseline and instead of using ndvi_mean_wt_res for the status quo
      #calculate a new status-quo weighted average using the areal proportions
      #use quo for status quo; avoid sq because it could be squared
      
      #update 4/15/22
      #this has to be conditional based on whether treatment-area NDVI is non-missing
      ndvi_quo = case_when(
        ndvi_mean_wt_tx>0 ~prop_area_comp*ndvi_mean_wt_comp+prop_area_tx*ndvi_mean_wt_tx, #if not missing
        TRUE ~ndvi_mean_wt_res #otherwise, it's just the full res buffer
      ),
      
      #now we can calculate the linear exposure difference between those values
      #and the baseline mean NDVI in the 500 m buffer. use ndvi_diff as done above
      ##alternative minus baseline
      ndvi_diff = ndvi_alt_avg - ndvi_quo
    )
}

names(den_bg_int_wtr_50ft_ndvi_wide)
den_bg_int_wtr_ndvi_all_nogeo =den_bg_int_wtr_50ft_ndvi_wide %>% 
  bind_rows(den_bg_int_wtr_100ft_ndvi_wide, den_bg_int_wtr_200ft_ndvi_wide ) %>% 
  mutate(
    prop_tx_itself_veg=1, #of the area of the actual treatment, what proportion is treated?
                          #note this is different than prop_area_tx
                          #for example, the office-green-infra polygons are all considered "treatment" areas
                          #but only 50% might be vegetated
    #set what the ndvi_alt_tx_only should be
    #in this case, it's just 0.5, but write as a weighted average to more easily generalize
    #note this will be missing wherever treatment ndvi is missing within a block group; that's okay.
    ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + (1-prop_tx_itself_veg)*ndvi_mean_wt_tx 
  ) %>% 
  mutate_ndvi_diff_bg_int()

save(den_bg_int_wtr_ndvi_all_nogeo, file = "den_bg_int_wtr_ndvi_all_nogeo.RData")
den_bg_int_wtr_ndvi_all_nogeo





# 3. Scenario 3. Office of Green Infrastructure initiatives---------------

## Per conversations with CB @ OGI, we consider 3 sub-scenarios.
#1. Regional projects
#2. OGI Green streets
#3. Stormwater regulations on new developments or re-developments.
#See this script (0_read_office_green_inf_data.R) for additional detail.

setwd(here("data-processed"))
load("ogi_proj.RData") 

## Prep buffers around regional OGI projects--------
# Data managed here: ~0_read_office_green_inf_data.R

### 500 m buffer around all projects 
names(ogi_proj)
ogi_proj_tx_marg  = ogi_proj %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>%
  summarise( #4/15/22 I had area measurements here before, but I don't need them. and it gets messy.
    n=n()
  ) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  dplyr::select(-dummy, -n)

ogi_proj_tx_marg %>% mapview()
st_crs(ogi_proj_tx_marg)
dist_500_m = 500*3.28084
#omit the _marg_ for the buffers. it gets to be too much nomenclature.
ogi_proj_res = ogi_proj_tx_marg %>% 
  st_union() %>% 
  st_buffer(dist_500_m) %>%  #500 meters, but we're in feet
  st_as_sf()

setwd(here("data-processed"))
save(ogi_proj_res, file = "ogi_proj_res.RData")
ogi_proj_res %>% mapview()

### 500 m buffer by priority (short-term vs later); see docs 
#updated with all 5 short-term priority projects
table(ogi_proj$short_term_proj)
ogi_proj %>% mapview(zcol = "short_term_proj")
ogi_proj_by_term  = ogi_proj %>% 
  group_by(short_term_proj) %>%
  summarise( #4/15/22 I had area measurements here before, but I don't need them. and it gets messy.
    n=n()
  ) %>% 
  ungroup() %>% 
  st_simplify() %>% 
  st_as_sf() %>% 
  dplyr::select(-n)

ogi_proj_by_term_res = ogi_proj_by_term %>% 
  st_buffer(dist_500_m) %>%  #500 meters, but we're in feet
  st_as_sf() 

ogi_proj_by_term_res %>% mapview(zcol = "short_term_proj")

#Remove water from those buffers
setwd(here("data-processed"))
load("den_co_osm_water_union.RData")
ogi_proj_res_no_wtr = ogi_proj_res %>%  
  st_difference(den_co_osm_water_union)
ogi_proj_res_no_wtr %>% mapview()

#symmetric difference between project buffers and the projects themselves
#remove ogi projects from the ogi project buffer (i.e., the complement (comp))
ogi_proj_comp = ogi_proj_res  %>% 
  st_difference(ogi_proj_tx_marg) %>% 
  st_as_sf()
ogi_proj_comp %>% mapview(layer.name = "comp")

#remove water from the ogi-proj buffer
ogi_proj_comp_no_wtr= ogi_proj_res_no_wtr %>% 
  st_difference(ogi_proj_tx_marg)

ogi_proj_comp_no_wtr%>% 
  mapview(layer.name = "comp, no water")


## Intersect these polygons with tracts--------
### With the full 500 m buffer--------
#These polygons will serve as the baseline measurement.
load("den_co_bg_no_wtr_filtered_geo.RData")
#ogi_proj-pct-75
den_bg_int_ogi_proj_res = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(ogi_proj_res) %>% 
  mutate(   #4/14/22 beginning to make long form
    scenario_sub = "ogi_proj", 
    buff_type = "res" #residential buffer
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

names(den_bg_int_ogi_proj_res)
den_bg_int_ogi_proj_res %>% mapview(zcol = "area_mi2_bg_int")

### Complement within the buffer---------
#This polygon will be used to create a weighted average of the alternative scenario
st_crs(den_co_bg_no_wtr_filtered_geo)
den_co_bg_no_wtr_filtered_geo %>% mapview()
ogi_proj_comp %>% mapview()
den_co_bg_no_wtr_filtered_geo %>% mapview()
den_bg_int_ogi_proj_comp = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(ogi_proj_comp) %>% #use union version
  mutate(
    scenario_sub = "ogi_proj",  
    buff_type = "comp" #complement with the residential buffer
  ) %>% 
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

save(den_bg_int_ogi_proj_comp, file = "den_bg_int_ogi_proj_comp.RData")
den_bg_int_ogi_proj_comp %>% mapview()

### Only the projects themselves----------
#Again, for the weighted average
ogi_proj_tx_marg %>% mapview()
den_co_bg_no_wtr_filtered_geo %>% mapview()
den_bg_int_ogi_proj_tx = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(ogi_proj_tx_marg) %>% #use union version
  mutate(
    scenario_sub = "ogi_proj",  
    buff_type = "tx"  
  ) %>% 
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

save(den_bg_int_ogi_proj_tx, file = "den_bg_int_ogi_proj_tx.RData")
names(den_bg_int_ogi_proj_tx)
den_bg_int_ogi_proj_tx %>% mapview(
  zcol = "bg_fips",
  col.regions = rainbow(n=n_distinct(den_bg_int_ogi_proj_tx$bg_fips)))

## Measure NDVI on those intersected polygons--------
###  500 m residential buffer 
#including the projects, but without water.
#use function again
names(den_bg_int_ogi_proj_res)
den_bg_int_ogi_proj_res_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_ogi_proj_res) %>% #function created above.
  mutate(
    scenario_sub = "ogi_proj", #possibly needed for all scenarios
    buff_type = "res" #residential buffer
  ) %>%
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area") #keep ndvi_below here
  )

names(den_bg_int_ogi_proj_res_ndvi)
save(den_bg_int_ogi_proj_res_ndvi, 
     file = "den_bg_int_ogi_proj_res_ndvi.RData")
den_bg_int_ogi_proj_res_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)
den_bg_int_ogi_proj_res_ndvi %>% 
  mapview(
    zcol = "ndvi_below_native_threshold")


### complement polygons 
#and excluding water.
names(den_bg_int_ogi_proj_comp)
den_bg_int_ogi_proj_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_ogi_proj_comp) %>% #function created above.
  mutate(
    scenario_sub = "ogi_proj", #possibly needed for all scenarios
    buff_type = "comp" #residential buffer
  ) %>%
  dplyr::select(bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"),  contains("area")  )


save(den_bg_int_ogi_proj_comp_ndvi, 
     file = "den_bg_int_ogi_proj_comp_ndvi.RData")
den_bg_int_ogi_proj_comp_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)

### intervention polygons
den_bg_int_ogi_proj_tx %>% mapview()
den_bg_int_ogi_proj_tx_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_ogi_proj_tx) %>% #function created above.
  mutate(
    scenario_sub = "ogi_proj", #possibly needed for all scenarios
    buff_type = "tx" #residential buffer
  ) %>%
  dplyr::select(bg_fips, buff_type,scenario_sub, starts_with("ndvi_mean"),  contains("area")  )

save(den_bg_int_ogi_proj_tx_ndvi, 
     file = "den_bg_int_ogi_proj_tx_ndvi.RData")
den_bg_int_ogi_proj_tx_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)

##  Compute weighted average NDVI under each scenario--------
### Visualize the elements that will be averaged-----
# Buffer excluding the project
names(den_bg_int_ogi_proj_comp_ndvi)
mv_den_bg_int_ogi_proj_comp_ndvi= den_bg_int_ogi_proj_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "ndvi, comp",
    zcol = "ndvi_mean_wt")

# The project only
names(den_bg_int_ogi_proj_tx_ndvi)
mv_den_bg_int_ogi_proj_tx_ndvi= den_bg_int_ogi_proj_tx_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "ndvi, tx",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_ogi_proj_tx_ndvi
# Bodies of water 
load("den_co_osm_water_union.RData")
load("den_co_osm_water.RData")
mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")

# Visualize all at once 
mv_den_bg_int_ogi_proj_comp_ndvi+
  mv_den_bg_int_ogi_proj_tx_ndvi +
  mv_den_co_osm_water

### Remove geometry from NDVI datasets-----------
den_bg_int_ogi_proj_res_ndvi_nogeo = den_bg_int_ogi_proj_res_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()
den_bg_int_ogi_proj_comp_ndvi_nogeo = den_bg_int_ogi_proj_comp_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

den_bg_int_ogi_proj_tx_ndvi_nogeo = den_bg_int_ogi_proj_tx_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

### pivot_wider ndvi and area within sub-scenario------------------
#keep it long by sub-scenario (just one, here) but wide by intervention vs complement 
den_bg_int_ogi_proj_ndvi_wide = den_bg_int_ogi_proj_res_ndvi_nogeo %>% 
  bind_rows(
    den_bg_int_ogi_proj_comp_ndvi_nogeo, #the residential buffer
    den_bg_int_ogi_proj_tx_ndvi_nogeo) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
  pivot_wider(
    # note some block groups will be covered by the comp but not by the tx
    names_from = buff_type,
    values_from = c(ndvi_mean_wt, area_mi2_bg_int)
  ) %>% 
  mutate(#add this info now
    scenario = "ogi",
    scenario_sub = "ogi_proj"
  ) %>% 
  #all be one step here
  mutate(#expect about 75% to be landscaped w native
    prop_tx_itself_veg=.75,
    ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
      (1-prop_tx_itself_veg)*ndvi_mean_wt_tx ) %>% 
  mutate_ndvi_diff_bg_int()

#save for use in bootstrap code
save(den_bg_int_ogi_proj_ndvi_wide, file = "den_bg_int_ogi_proj_ndvi_wide.RData")
den_bg_int_ogi_proj_ndvi_wide
names(den_bg_int_ogi_proj_ndvi_wide)

## OGI: Green streets---------------
#Overarching question here shoudl be: what overall timeframe? 5 yr?
#Status quo:
# 2.7 miles per year, and each mile of street equates to 0.15 acres
#1 acre equals 43560 square feet
.15*43560
#assume a rectangle, so l*w=a; we know l; solve for w; w=a/l
(.15*43560)/5280
#so currently about 1.2 feet per foot of street....

#goal is 0.75 acres per green mile
(.75*43560)/5280 #so 6.1875 feet

#Assume green streets can occur on all road types except motorway and trunk
#randomly sample a cumulative sum of just below one mile
load("den_osm_roads.RData") #see for definitions
#~/0_load_denver_osm_roads.R
den_osm_roads_shuffle = den_osm_roads %>% 
  filter(highway_primary_or_lower==1) %>% 
  slice_sample(prop=1) %>% #shuffle the dataset by row so the cumulative sum is random
  mutate(
    length_mi_cumsum=cumsum(length_mi),
    #status quo is 2.7 miles
    #goal to increase to 5.
    length_mi_cumsum_cat = case_when(
      length_mi_cumsum < 2.7 ~ "<2.7",
      length_mi_cumsum >=2.7 | length_mi_cumsum < 5 ~ "2.7-5.0",
      length_mi_cumsum >5 ~ "5.0+"
    ) 
  )

den_osm_roads_shuffle %>% 
  filter(length_mi_cumsum_cat== "<2.7") %>% 
  mapview(zcol = "length_mi_cumsum")
den_osm_roads_shuffle %>% 
  filter(length_mi_cumsum < 5) %>% 
  mapview(zcol = "length_mi_cumsum")

## OGI: stormwater regulations--------------
###  Sample stormwater reg locations---------
#How to operationalize? Randomly sample parcels based on size class
load("den_landuse_2018.RData")
names(den_landuse_2018)
#As suggested by CB, we could expect the following new or redevelopments per year:
# •	100 sites / year > 1.0 ac
# •	25 sites / year 0.5 to 1.0 ac
# •	400 sites / year < 0.5 ac and adding 3000 SF of impervious cover
#call these large, medium, small
table(den_landuse_2018$parcel_size_cat)
table(den_landuse_2018$n_redevelop_per_y)
st_crs(den_landuse_2018)
st_crs(study_area_2876)
names(den_landuse_2018)
table(den_landuse_2018$parcel_size_cat)
den_landuse_filtered = den_landuse_2018 %>% 
  st_intersection(study_area_2876) #remove northeast tracts as we've done elsewhere

den_landuse_sample_large = den_landuse_filtered %>% 
  filter(parcel_size_cat == ">1.0 acre") %>% 
  slice_sample(n=100, replace=FALSE)
den_landuse_sample_large %>% mapview()

den_landuse_sample_medium = den_landuse_filtered %>% 
  filter(parcel_size_cat == "0.5-1.0 acre") %>% 
  slice_sample(n=25, replace=FALSE)
den_landuse_sample_medium %>% mapview()

den_landuse_sample_small = den_landuse_filtered %>% 
  filter(parcel_size_cat == "<0.5 acre") %>% 
  slice_sample(n=400, replace=FALSE)

#combine them again
den_landuse_sample = den_landuse_sample_large %>% 
  bind_rows(
    den_landuse_sample_medium,
    den_landuse_sample_small
  )

den_landuse_sample %>% mapview(zcol = "parcel_size_cat")
#even though this is a sample and will be re-saved constantly, save it so I can use it in the
#rmarkdown doc
save(den_landuse_sample, file = "den_landuse_sample.RData")

names(den_landuse_sample)
### Prep buffers around these places----------
parcel_by_size  = den_landuse_sample %>% 
  group_by(parcel_size_cat) %>%
  summarise(
    area_ft2_parcel = sum(area_ft2_parcel),
    area_ac_parcel  = sum(area_ac_parcel),
    area_mi2_parcel = sum(area_mi2_parcel)  ,
  ) %>% 
  ungroup() %>% 
  st_as_sf() 

class(parcel_by_size$geometry)
parcel_by_size %>% mapview(zcol = "parcel_size_cat")

#and a version that's ungrouped; make naming the same as elsewhere
den_parcel_tx_marg = parcel_by_size %>%
  st_union_dplyr_way() %>% #use this to see if it fixes mapview's mixed-type issue
  st_as_sf() 

den_parcel_tx_marg %>% mapview()
dist_500_m = 500*3.28084

# 500 m buffer around all parcel, ungrouped 
den_parcel_res = parcel_by_size %>%
  st_union_dplyr_way() %>% 
  st_buffer(dist_500_m) %>%  #500 meters, but we're in feet
  st_as_sf() 

den_parcel_res   %>% mapview()
### remove parcel from the parcel buffer
#remove parking from the parking buffer (i.e., the complement (comp))
den_parcel_comp= den_parcel_res  %>% 
  st_difference(den_parcel_tx_marg) %>% 
  st_as_sf()%>% 
  st_make_valid() 

den_parcel_comp %>% mapview(layer.name = "comp")

## Intersect these polygons with tracts--------
### With the full 500 m buffer
#These polygons will serve as the baseline measurement.
load("den_co_bg_no_wtr_filtered_geo.RData")
den_bg_int_parcel_res = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(den_parcel_res) %>% #grouped is fine here, not using near vs short term yet
  mutate(   #4/14/22 beginning to make long form
    scenario_sub = "parcel", 
    buff_type = "res" #residential buffer
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

den_bg_int_parcel_res %>% 
  mapview(zcol = "area_mi2_bg_int")

### Complement within the buffer
#This polygon will be used to create a weighted average of the alternative scenario
den_parcel_comp %>% mapview()
bg_int_wrangle_last_steps
den_bg_int_parcel_comp = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(den_parcel_comp) %>% #grouped is fine here
  #it seems as though some of these might be points. I'm going to try adding a small buffer. worked.
  st_as_sf() %>% 
  st_buffer(0) %>% 
  mutate(   
    scenario_sub = "parcel",  
    buff_type = "comp"  
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  filter(area_mi2_bg_int >0) %>%   #filter to area above 0
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area")) 

save(den_bg_int_parcel_comp, file = "den_bg_int_parcel_comp.RData")
nrow(den_bg_int_parcel_comp)
den_bg_int_parcel_comp %>% 
  mapview(
    layer.name = "comp",
    zcol = "area_mi2_bg_int")

### Only the projects themselves 
#Again, for the weighted average
parcel_by_size %>% mapview(zcol = "parcel_size_cat")
class(parcel_by_size$geometry)
den_landuse_sample
den_landuse_sample_geo_only = den_landuse_sample %>% 
  dplyr::select(starts_with("parcel"), geometry)
#renaming this by_feat for by feature to contrast with marginal; do same thing for parking
den_bg_int_parcel_tx_by_feat = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  #4/3 update: as elsewhere, use the full ungrouped dataset here for a more accurate
  #measurement of NDVI on each individual piece
  st_intersection(den_landuse_sample_geo_only) %>% #use grouped version with categories
  dplyr::select(-starts_with("area")) %>% #remove these as we will re-measure
  st_as_sf() %>% 
  st_buffer(0) %>% #adding buffer to make sure they're not points
  mutate(   
    scenario_sub = "parcel",  
    buff_type = "tx"  ,
    unioned = "no"
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  filter(area_mi2_bg_int >0) %>%   #filter to area above 0
  dplyr::select(bg_fips, buff_type, starts_with("union"), row_id_int, starts_with("area"), starts_with("parcel")) 

save(den_bg_int_parcel_tx_by_feat, file = "den_bg_int_parcel_tx_by_feat.RData")
names(den_bg_int_parcel_tx_by_feat)
den_bg_int_parcel_tx_by_feat %>% mapview(zcol = "area_mi2_bg_int")

#as with parking below, because I'm using the non-unioned version to measure NDVI,
#I also need the unioned version for the weighted-average (to block group) calculation
#I also need one that's not by row, i.e, the union that we already had.
den_parcel_tx_marg %>% mapview()
den_bg_int_parcel_tx_marg = den_co_bg_no_wtr_filtered_geo %>% #
  st_intersection(den_parcel_tx_marg) %>% 
  dplyr::select(-contains("area")) %>%
  mutate(   
    scenario_sub = "parcel",  
    buff_type = "tx"  ,
    unioned = "yes" #this is the unioned geometry 
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  rename(   #for this one, we have to rename the area to specify that it's from the full unioned (marg) area
    area_ft2_bg_int_marg = area_ft2_bg_int,
    area_mi2_bg_int_marg = area_mi2_bg_int
  ) %>% 
  dplyr::select(bg_fips, buff_type, starts_with("union"), starts_with("area")) #not measuring NDVI on this.

save(den_bg_int_parcel_tx_marg, file = "den_bg_int_parcel_tx_marg.RData")
den_bg_int_parcel_tx_marg %>% 
  mapview(
    zcol = "bg_fips",
    col.regions = rainbow(n=n_distinct(den_bg_int_parcel_tx_marg$bg_fips))
  )
nrow(den_bg_int_parcel_tx_marg)
lookup_den_bg_int_parcel_tx_marg_area = den_bg_int_parcel_tx_marg %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(bg_fips, contains("area"))
lookup_den_bg_int_parcel_tx_marg_area

## Measure NDVI on those intersected polygons--------
####  residential buffer
names(den_bg_int_parcel_res)
den_bg_int_parcel_res_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_parcel_res) %>%  
  mutate(
    scenario_sub = "parcel",  
    buff_type = "res"  
  ) %>%
  dplyr::select(  
    bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area") #keep ndvi_below here
  )

names(den_bg_int_parcel_res_ndvi)
save(den_bg_int_parcel_res_ndvi, 
     file = "den_bg_int_parcel_res_ndvi.RData")
den_bg_int_parcel_res_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)


#### Complement
names(den_bg_int_parcel_comp)
den_bg_int_parcel_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_parcel_comp) %>%  
  mutate(
    scenario_sub = "parcel",  
    buff_type = "comp"  
  ) %>%
  dplyr::select( bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("area"))

save(den_bg_int_parcel_comp_ndvi, 
     file = "den_bg_int_parcel_comp_ndvi.RData")
den_bg_int_parcel_comp_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)

#### treatment area only
den_bg_int_parcel_tx_by_feat %>% mapview()
class(den_bg_int_parcel_tx_by_feat)
names(den_bg_int_parcel_tx_by_feat)
den_bg_int_parcel_tx_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_parcel_tx_by_feat) %>% #distinct for each project within bg
  #for the measurements of the intervention area itself, we need
  #to take another weighted average, summing over parcel to the bg level
  #as is, the weighted average is summed over parcel-bg chunk from pixels, 
  #not all the way to the block group,
#  which may lose some precision
  left_join(lookup_den_bg_int_parcel_tx_marg_area, by = "bg_fips") %>% 
  mutate(
    prop_area_bg_int_marg = area_ft2_bg_int/area_ft2_bg_int_marg, #int for block group intersection
    ndvi_mean_int = prop_area_bg_int_marg*ndvi_mean_wt #int for intermediate
  ) %>% 
  group_by(bg_fips) %>% #keep buff_type in 
  summarise(
    #sum these intermediates to get a new weighted average
    ndvi_mean_wt = sum(ndvi_mean_int, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  bg_int_wrangle_last_steps() %>% #re-calculate area
  mutate(   
    scenario_sub = "parcel",  
    buff_type = "tx" ) %>%
  dplyr::select( bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("area"))

den_bg_int_parcel_tx_ndvi
save(den_bg_int_parcel_tx_ndvi, 
     file = "den_bg_int_parcel_tx_ndvi.RData")
den_bg_int_parcel_tx_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)
names(den_bg_int_parcel_tx_ndvi)

##  Compute weighted average NDVI --------
### Visualize the elements that will be averaged-----
#### Buffer excluding the project 
names(den_bg_int_parcel_comp_ndvi)
mv_den_bg_int_parcel_comp_ndvi= den_bg_int_parcel_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 500 m buffer around parcels",
    zcol = "ndvi_mean_wt")

#### The project only 
names(den_bg_int_parcel_tx_ndvi)
mv_den_bg_int_parcel_tx_ndvi= den_bg_int_parcel_tx_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, parcels only",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_parcel_tx_ndvi
#### Bodies of water 
load("den_co_osm_water_union.RData")
load("den_co_osm_water.RData")
mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")

#### Visualize all at once 
mv_den_bg_int_parcel_comp_ndvi+
  mv_den_bg_int_parcel_tx_ndvi +
  mv_den_co_osm_water

### Remove geometry from NDVI datasets-----------
den_bg_int_parcel_res_ndvi_nogeo = den_bg_int_parcel_res_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()
den_bg_int_parcel_comp_ndvi_nogeo = den_bg_int_parcel_comp_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

den_bg_int_parcel_tx_ndvi_nogeo = den_bg_int_parcel_tx_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

### pivot_wider ndvi and area within sub-scenario------------------
#keep it long by sub-scenario (just one, here) but wide by intervention vs complement 
den_bg_int_parcel_ndvi_wide = den_bg_int_parcel_res_ndvi_nogeo %>% 
  bind_rows(
    den_bg_int_parcel_comp_ndvi_nogeo, #the residential buffer
    den_bg_int_parcel_tx_ndvi_nogeo) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
  pivot_wider(
    names_from = buff_type,
    values_from = c(ndvi_mean_wt, area_mi2_bg_int)
  ) %>% 
  mutate(#add this info now
    scenario = "ogi",
    scenario_sub = "parcel"
  ) %>% 
  mutate(  #say about 50% of the new dev't is landscaped; may not be that high.
    prop_tx_itself_veg=.5,
    ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
      (1-prop_tx_itself_veg)*ndvi_mean_wt_tx ) %>% 
  mutate_ndvi_diff_bg_int() #all one step in contrast with riparian areas

save(den_bg_int_parcel_ndvi_wide, file = "den_bg_int_parcel_ndvi_wide.RData")


# 4. Scenario 4: Parking -----------
## Prep parking buffers--------
#Parking data managed here: 0_read_denver_parking.R
setwd(here("data-processed"))
load("den_prkng_500m.RData") #Note rename from parking to prkng
den_prkng_res = den_prkng_500m #takes forever to re-create, so just load and rename
names(den_prkng_res)
load("den_prkng_marg.RData") #I used to call this sum_union; marg for marginal is more clear to me
#actually one more difference; above, we use this syntax: den_prkng_tx_marg
den_prkng_tx_marg = den_prkng_marg #focuses on the fact that it's not the complement or the res
den_prkng_res = den_prkng_res %>% #for redundancy to be sure find + replace works.
  st_as_sf()
names(den_prkng_tx_marg)
load("den_co_osm_wtr_union.RData")
load("den_co_bg_no_wtr_filtered_geo.RData")
den_prkng_res%>% mapview()
#Remove water from that buffer
den_prkng_res_no_wtr = den_prkng_res %>%  #prkng=parking; trying to reduce chars in object names
  st_difference(den_co_osm_water_union)
den_prkng_res_no_wtr %>% mapview()

#remove parking from the parking buffer (i.e., the complement (comp))
den_prkng_comp = den_prkng_res  %>% 
  st_difference(den_prkng_tx_marg) %>% 
  st_as_sf()
den_prkng_comp %>% mapview(layer.name = "comp")

#remove water and parking from the parking buffer
den_prkng_res_no_wtr_prkng_comp = den_prkng_res_no_wtr %>% 
  st_difference(den_prkng_tx_marg)

den_prkng_res_no_wtr_prkng_comp %>% mapview(layer.name = "comp, no water")

## Intersect these polygons with tracts--------
### With the full 500 m buffer
#These polygons will serve as the baseline measurement.
load("den_co_bg_no_wtr_filtered_geo.RData")
den_bg_int_prkng_res = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(den_prkng_res) %>% #grouped is fine here, not using near vs short term yet
  mutate(   #4/14/22 beginning to make long form
    scenario_sub = "prkng", 
    buff_type = "res" #residential buffer
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area"))

den_bg_int_prkng_res %>% 
  mapview(zcol = "area_mi2_bg_int")

### Complement within the buffer
#This polygon will be used to create a weighted average of the alternative scenario
den_prkng_comp %>% mapview()
bg_int_wrangle_last_steps
den_bg_int_prkng_comp = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  st_intersection(den_prkng_comp) %>% #grouped is fine here
  mutate(   
    scenario_sub = "prkng",  
    buff_type = "comp"  
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  dplyr::select(bg_fips, buff_type, row_id_int, starts_with("area")) 

save(den_bg_int_prkng_comp, file = "den_bg_int_prkng_comp.RData")
nrow(den_bg_int_prkng_comp)
den_bg_int_prkng_comp %>% 
  mapview(
    layer.name = "comp",
    zcol = "area_mi2_bg_int")

### Only the projects themselves 
#Again, for the weighted average
den_prkng_tx_marg %>% mapview()
den_co_bg_no_wtr_filtered_geo %>% mapview()
load("den_prkng.RData") #the original one that's not unioned
names(den_prkng)
den_prkng_geo_only = den_prkng %>% 
  dplyr::select(prkng_id, geometry)

den_landuse_sample
den_landuse_sample_geo_only = den_landuse_sample %>% 
  dplyr::select(starts_with("prkng"), geometry)
#renaming this by_feat for by feature to contrast with marginal (unioned)
den_bg_int_prkng_tx_by_feat = den_co_bg_no_wtr_filtered_geo %>% #we already dropped water
  #4/3 update: as elsewhere, use the full ungrouped dataset here for a more accurate
  #measurement of NDVI on each individual piece
  st_intersection(den_prkng_geo_only) %>% #the ungrouped version
  st_as_sf() %>% 
  st_buffer(0) %>% #adding buffer to make sure they're not points
  st_make_valid() %>% 
  mutate(   
    scenario_sub = "prkng",  
    buff_type = "tx"  ,
    unioned = "no"
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  filter(area_mi2_bg_int >0) %>%   #filter to area above 0
  dplyr::select(bg_fips, buff_type, starts_with("union"), row_id_int, starts_with("area"), starts_with("prkng")) 

save(den_bg_int_prkng_tx_by_feat, file = "den_bg_int_prkng_tx_by_feat.RData")
names(den_bg_int_prkng_tx_by_feat)
den_bg_int_prkng_tx_by_feat %>% 
  mapview(
    #note that these are all distinct features within block group
    zcol = "bg_fips",
    col.regions = rainbow(n_distinct(den_bg_int_prkng_tx_by_feat$bg_fips)))

#I also need the unioned version for the weighted-average (to block group) calculation
#I also need one that's not by row, i.e, the union that we already had.
den_prkng_tx_marg %>% mapview()
den_bg_int_prkng_tx_marg = den_co_bg_no_wtr_filtered_geo %>% #
  st_intersection(den_prkng_tx_marg) %>% 
  dplyr::select(-contains("area")) %>%
  mutate(   
    scenario_sub = "prkng",  
    buff_type = "tx"  ,
    unioned = "yes" #this is the unioned geometry 
  ) %>%
  bg_int_wrangle_last_steps() %>% 
  rename(   #for this one, we have to rename the area to specify that it's from the full unioned (marg) area
    area_ft2_bg_int_marg = area_ft2_bg_int,
    area_mi2_bg_int_marg = area_mi2_bg_int
  ) %>% 
  dplyr::select(bg_fips, buff_type, starts_with("union"), starts_with("area")) #not measuring NDVI on this.

save(den_bg_int_prkng_tx_marg, file = "den_bg_int_prkng_tx_marg.RData")
den_bg_int_prkng_tx_marg %>% 
  mapview(
    zcol = "bg_fips",
    #note that these are the same feature within block group
    col.regions = rainbow(n=n_distinct(den_bg_int_prkng_tx_marg$bg_fips))
  )
nrow(den_bg_int_prkng_tx_marg)
lookup_den_bg_int_prkng_tx_marg_area = den_bg_int_prkng_tx_marg %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  dplyr::select(bg_fips, contains("area"))
lookup_den_bg_int_prkng_tx_marg_area

## Measure NDVI on those intersected polygons--------
####  residential buffer
names(den_bg_int_prkng_res)
den_bg_int_prkng_res_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_prkng_res) %>%  
  mutate(
    scenario_sub = "prkng",  
    buff_type = "res"  
  ) %>%
  dplyr::select(  
    bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area") #keep ndvi_below here
  )

names(den_bg_int_prkng_res_ndvi)
save(den_bg_int_prkng_res_ndvi, 
     file = "den_bg_int_prkng_res_ndvi.RData")
den_bg_int_prkng_res_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)


#### Complement
names(den_bg_int_prkng_comp)
den_bg_int_prkng_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_prkng_comp) %>%  
  mutate(
    scenario_sub = "prkng",  
    buff_type = "comp"  
  ) %>%
  dplyr::select( bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("area"))

save(den_bg_int_prkng_comp_ndvi, 
     file = "den_bg_int_prkng_comp_ndvi.RData")
den_bg_int_prkng_comp_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)

#### treatment area only
den_bg_int_prkng_tx_by_feat %>% mapview()
class(den_bg_int_prkng_tx_by_feat)
names(den_bg_int_prkng_tx_by_feat)
sf::sf_use_s2(FALSE)
den_bg_int_prkng_tx_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_prkng_tx_by_feat) %>% #distinct for each project within bg
  #for the measurements of the intervention area itself, we need
  #to take another weighted average, summing over prkng to the bg level
  #as is, the weighted average is summed over prkng-bg chunk from pixels, 
  #not all the way to the block group,
  #  which may lose some precision
  left_join(lookup_den_bg_int_prkng_tx_marg_area, by = "bg_fips") %>% 
  mutate(
    prop_area_bg_int_marg = area_ft2_bg_int/area_ft2_bg_int_marg, #int for block group intersection
    ndvi_mean_int = prop_area_bg_int_marg*ndvi_mean_wt #int for intermediate
  ) %>% 
  group_by(bg_fips) %>% #keep buff_type in 
  summarise(
    #sum these intermediates to get a new weighted average
    ndvi_mean_wt = sum(ndvi_mean_int, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  bg_int_wrangle_last_steps() %>% #re-calculate area
  mutate(   
    scenario_sub = "prkng",  
    buff_type = "tx" ) %>%
  dplyr::select( bg_fips, buff_type, scenario_sub, starts_with("ndvi_mean"), contains("area"))

save(den_bg_int_prkng_tx_ndvi, 
     file = "den_bg_int_prkng_tx_ndvi.RData")
den_bg_int_prkng_tx_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)
names(den_bg_int_prkng_tx_ndvi)

##  Compute weighted average NDVI -------
### Visualize the elements that will be averaged-----
#### Buffer excluding the project 
names(den_bg_int_prkng_comp_ndvi)
mv_den_bg_int_prkng_comp_ndvi= den_bg_int_prkng_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 500 m buffer around prkngs",
    zcol = "ndvi_mean_wt")

#### The project only 
names(den_bg_int_prkng_tx_ndvi)
mv_den_bg_int_prkng_tx_ndvi= den_bg_int_prkng_tx_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, prkngs only",
    zcol = "ndvi_mean_wt")

mv_den_bg_int_prkng_tx_ndvi
#### Bodies of water 
load("den_co_osm_water_union.RData")
load("den_co_osm_water.RData")
mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")

#### Visualize all at once 
mv_den_bg_int_prkng_comp_ndvi+
  mv_den_bg_int_prkng_tx_ndvi +
  mv_den_co_osm_water

### Remove geometry from NDVI datasets-----------
den_bg_int_prkng_res_ndvi_nogeo = den_bg_int_prkng_res_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()
den_bg_int_prkng_comp_ndvi_nogeo = den_bg_int_prkng_comp_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

den_bg_int_prkng_tx_ndvi_nogeo = den_bg_int_prkng_tx_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

### pivot_wider ndvi and area ------------------
#keep it long by sub-scenario (just one, here) but wide by intervention vs complement 
den_bg_int_prkng_ndvi_wide = den_bg_int_prkng_res_ndvi_nogeo %>% 
  bind_rows(
    den_bg_int_prkng_comp_ndvi_nogeo, #the residential buffer
    den_bg_int_prkng_tx_ndvi_nogeo) %>% 
  dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
  pivot_wider(
    names_from = buff_type,
    values_from = c(ndvi_mean_wt, area_mi2_bg_int)
  ) %>% 
  mutate(#add this info now
    scenario = "ogi",
    scenario_sub = "prkng"
  ) 

#Now consider a few different scenarios for the proportion of the parking lots that we would change
#100%; 50%; 20%
names(den_bg_int_prkng_ndvi_wide)
den_bg_int_prkng_alt_100 = den_bg_int_prkng_ndvi_wide %>% 
  mutate(
    scenario = "prkng",
    scenario_sub = "100-pct",
    prop_tx_itself_veg=1,
    ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
      (1-prop_tx_itself_veg)*ndvi_mean_wt_tx 
    ) 
den_bg_int_prkng_alt_50 = den_bg_int_prkng_ndvi_wide %>% 
  mutate(
    scenario = "prkng",
    scenario_sub = "50-pct",
    prop_tx_itself_veg=.5,
    ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
      (1-prop_tx_itself_veg)*ndvi_mean_wt_tx 
  ) 
den_bg_int_prkng_alt_20 = den_bg_int_prkng_ndvi_wide %>% 
  mutate(
    scenario = "prkng",
    scenario_sub = "20-pct",
    prop_tx_itself_veg=.2,
    ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
      (1-prop_tx_itself_veg)*ndvi_mean_wt_tx 
  ) 

den_bg_int_prkng_alt_all = den_bg_int_prkng_alt_100 %>% 
  bind_rows(
    den_bg_int_prkng_alt_50,
    den_bg_int_prkng_alt_20
  ) %>% 
  mutate_ndvi_diff_bg_int() 

#save for use in bootstrap code
save(den_bg_int_prkng_alt_all, file = "den_bg_int_prkng_alt_all.RData")

#confirm ndvi diff differs by sub-scenario
den_bg_int_prkng_alt_all %>% 
  group_by(scenario_sub) %>% 
  summarise(ndvi_diff_mean = mean(ndvi_diff, na.rm=TRUE))
            


####################################################-
# COMBINE SCENARIOS AND COMPUTE ATTRIB DEATHS---------------
####################################################-


## Prep sex-by-age pop_est data & link with GBD rate data--------------
# 4/16/22 I originally had this above, but I like it better here,
#as here is where we do the HIA in the code

#Note these are long-form
### Add dose-response function to GBD data----------
#source this script:
source( here("scripts","0_read_gbd_colorado.R"))  
names(ihme_co)
View(ihme_co)
log(.97)
log(.96)
log(.94)
exp(log(.97))

### create a dataset with the dose-response function-------- 
#values from From Rojas 2019 Green space-mortality meta-analysis
# dose-response function (drf)

drf_deaths =  0.96 %>% 
  as_tibble() %>% 
  rename(drf_est=value) %>% 
  mutate(
    drf_ll = 0.94,
    drf_ul = 0.97,
    #the amount of change in NDVI corresponding to the change in risk
    #measured by the risk ratio from the meta-analysis
    drf_increment = 0.1,
    
    drf_est_log = log(drf_est),
    drf_ll_log = log(drf_ll),
    drf_ul_log = log(drf_ul),
    measure = "deaths", #to link with the IHME data
    cause_short = "all" ,#to link with the IHME data
    
    #assume symmetrical around lower bound on the log scale.
    #assume lower bound is estimate - 1.96*SD
    drf_sd_log_scale = abs(drf_est_log-drf_ll_log)/1.96
  )

summary(drf_deaths$drf_sd_log_scale)
summary(drf_deaths$drf_est_log)
save(drf_deaths, file = "drf_deaths.RData") #to quickly load elsewhere
ihme_co_w_drf = ihme_co %>% 
  left_join(drf_deaths, by = c("measure", "cause_short"))



#We created a spreadsheet to link the age groups here:
setwd(here("data-processed"))
lookup_acs_gbd_age = readxl::read_excel("lookup_acs_gbd_age.xlsx")
load("den_metro_bg_sex_age_wrangle.RData") #long form

### Link GBD to bg data & restrict to certain age groups----------
#begin with the long-form metro data, restrict to denver only, link with GBD,
#### Define lower bound for age-----------
age_lowest_to_include=30
den_co_bg_sex_age_gbd_wrangle = den_metro_bg_sex_age_wrangle %>% 
  filter(county_fips == "031" ) %>% 
  left_join(lookup_acs_gbd_age, by = "age_group_acs") %>%  #link age groups for linking
  left_join(ihme_co_w_drf, #link to GBD data
            by = c("age_group_gbd", "sex")) %>% 
  filter(age_group_acs != "all") %>% #exclude "all" from ages
  filter(sex != "all") %>%  #exclude "all" from sex
  #limit to adults 30 and older, as most studies from meta-analysis
  #can change if needed
  filter(age_group_acs_lb>=age_lowest_to_include) %>% 
  #also apply these filters. remove as we determine additional
  #dose-response functions 
  #(e.g., the Paul 2020; Urban green space and the risks of dementia and stroke)
  filter(measure == "deaths" & cause_short == "all") %>% 
  dplyr::select(-var_label, -var_name) #drop these

#save for use in bootstrap
save(den_co_bg_sex_age_gbd_wrangle, file = "den_co_bg_sex_age_gbd_wrangle.RData") 
names(den_co_bg_sex_age_gbd_wrangle)
summary(den_co_bg_sex_age_gbd_wrangle$pop_moe)
summary(den_co_bg_sex_age_gbd_wrangle$pop_est)
# den_co_bg_sex_age_gbd_wrangle %>% 
#   dplyr::select(starts_with("pop")) %>% 
#   View()
## Bind each scenario, link with pop, and conduct HIA---------
#the block-group based scenario is slightly different. the others can be combined for shorter code.
#We can write this more concisely.
#Begin with the unique list of block groups; actually no, because we would have to left join
#with the unique list anyway, which is more steps. leave as is.
den_co_bg_unique_list = den_metro_bg_sex_age_wrangle %>% 
  filter(county_fips == "031" ) %>% 
  dplyr::select(bg_fips) %>% 
  distinct()
den_co_bg_unique_list

n_distinct(den_bg_int_ogi_proj_ndvi_wide$bg_fips) #193 bgs affected
n_distinct(den_bg_int_wtr_ndvi_all_nogeo$bg_fips)#405 bgs affected
#actually, just stack them all, link den_co_bg, and use case_when to define vars
names(den_bg_int_ogi_proj_ndvi_wide)
table( den_co_bg_ndvi_alt_all_nogeo$scenario)
hia_all  = den_co_bg_ndvi_alt_all_nogeo %>% #scenario 1 - all bg
  bind_rows(
    den_bg_int_wtr_ndvi_all_nogeo, #scenario 2 - riparian
    den_bg_int_ogi_proj_ndvi_wide, #scenario 3 - ogi proj
    den_bg_int_parcel_ndvi_wide, #scenario 3 - ogi parcel
    den_bg_int_prkng_alt_all #scenario 4 - parking
  ) %>% #link pop data here
  left_join(den_co_bg_sex_age_gbd_wrangle, by = "bg_fips") %>% 
  mutate(
    pop_affected = case_when(
      scenario == "all-bg" ~ pop_est, #for scenario 1, it's just the pop of the bg
      TRUE ~ area_mi2_bg_int_res*pop_dens_mi2 #for other scenarios, multiply area by pop dens
          ),
    pop_affected_type = case_when(
      scenario == "all-bg" ~ "bg", #to keep track of how calculated
      TRUE ~   "res-buffer" #residential buffer otherwise
    ),
    #now conduct HIA
    rr_alt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
    paf =(rr_alt -1)/rr_alt , #pop_est attrib fraction
    #attributable deaths
    #rate is per 100,000 so first divide by 100,000 before multiplying by the pop_est. in age group
    #marginal totals were filtered out above, so it's just the joint sex-by-age categories
    #could generalize from d to o for outcome if we include other outcomes rather than just death
    attrib_d = paf*(rate_per_100k_est/100000)*pop_affected, #attrib deaths. note divide by 100000
    
    #one thing to check is whether we've limited to ndvi_below_threshold and how
    #does it make sense to simply not intervene upon any block group that has a ndvi above threshold?
    #or, so we don't lose so much, for the non-bg interventions, we could only not intervene
    #if the block-group piece is below ndvi
    #probably simplest to re-calculate this. I think this will suffice.
    ndvi_below_native_threshold = case_when( 
      ndvi_quo   < ndvi_native_threshold ~1,
      TRUE ~0
    )
  ) %>% 
  dplyr::select(
    contains("fips"), contains("scenario"), 
    starts_with("pop"), starts_with("area"), contains("area"),
    contains("ndvi"), contains("drf"),
    everything())

save(hia_all, file = "hia_all.RData")
#checks
table(hia_all$scenario)
table(hia_all$scenario_sub)
names(hia_all)
table(hia_all$ndvi_below_native_threshold)
nrow(hia_all)

## Summarize HIA------------
### Summarize overall-----------
hia_all_overall = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(scenario, scenario_sub) %>% 
  summarise(
    pop_affected = sum(pop_affected, na.rm=TRUE),
    attrib_d = sum(attrib_d, na.rm=TRUE)
  )

hia_all_overall

### Summarize by block group----------
load("lookup_den_metro_bg_geo.RData") #created 0_import_manage_denver_acs.R
lookup_den_metro_bg_geo %>% mapview()
hia_all_bg = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(bg_fips, scenario, scenario_sub) %>% 
  summarise(
    pop_affected = sum(pop_affected, na.rm=TRUE),
    attrib_d = sum(attrib_d, na.rm=TRUE)
  )
hia_all_bg

hia_all_bg %>% 
  filter(scenario == "all-bg") %>% 
  filter(scenario_sub == "100-pct") %>% 
  left_join(lookup_den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(zcol = "attrib_d")
  
### Summarize by age group
#todo


#final note: nice work, mike. this is much more streamlined and ready for
#bootstrapping
