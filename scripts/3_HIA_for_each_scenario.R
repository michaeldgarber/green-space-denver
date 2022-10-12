#filename: 3_HIA_for_each_scenario
library(tidyverse)
library(sf)
library(mapview)
library(here)
library(remotes)
library(terra) #had to install from "binary" rather than "source"
library(raster)#had to install from "binary" rather than "source"
library(leaflet)
library(leaflet.extras)
library(rgdal) #note it will be retired soon
setwd(here("data-processed"))



#Major revision april 14 2022 to make scenario datasets long form
# code is less repetitive and will be easier to bootstrap
# Update May 20 2022 to remove the equity-focused scenario and instead
# will plan to stratify all scenarios by equity

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
lookup_date_is_valid_all %>% 
  filter(date_is_valid_all==1)
#Let's use July 4, 2021 and also limit to Denver County
ndvi_den_metro_terr_5_yr = terra::rast("ndvi_den_metro_terr_5_yr.tif")
ndvi_den_metro_terr_5_yr$`20210704_NDVI`

#note this is is in 4326, and den_co_might be in something else
ndvi_den_co_20210704 = ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  terra::trim() %>%    #remove NAs
  terra::crop(den_co_4326)  

plot(ndvi_den_co_20210704)
#This automatically converts to a bbox so it's a rectangle.
#for some reason, this band is not getting the NE area (airport)
#this takes about 30 seconds. probably not worth saving for that amount of time,
#given the complexity of saving raster files.

#mapviews for rasters aren't working anymore...

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
save(lookup_tract_nbhd_northeast_exclude,
     file = "lookup_tract_nbhd_northeast_exclude.RData")

# make a geometry version of this if you want to exclude it spatially using st_difference
load("den_metro_tract_geo.RData")
nbhd_northeast_exclude_geo = den_metro_tract_geo %>% 
  left_join(lookup_den_nbhd_tract, by = "tract_fips") %>% 
  mutate(
    nbhd_northeast_exclude = case_when(
      nbhd_id %in% c(23, 28, 45) ~1,
      TRUE ~0
    )
  ) %>% 
  filter(nbhd_northeast_exclude==1) %>% 
  st_union() %>% 
  st_sf()

#add a 100-foot buffer around it to make sure there aren't weird border issues
nbhd_northeast_exclude_geo %>% mapview()
nbhd_northeast_exclude_100_ft_geo =nbhd_northeast_exclude_geo %>% 
  st_buffer(100)%>% 
  st_sf()
  
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
load("den_co_tract_geo.RData")
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

nrow(lookup_row_id_int)
n_distinct(lookup_row_id_int$bg_fips)
#a lookup for the no-water block group geometry
lookup_den_co_bg_no_wtr_geo = den_co_bg_no_wtr_filtered_geo  %>% 
  distinct(bg_fips, geometry)
lookup_den_co_bg_no_wtr_geo %>% mapview()
save(lookup_den_co_bg_no_wtr_geo, file = "lookup_den_co_bg_no_wtr_geo.RData")


#also make a 4326 version that doesn't exclude thoes northeast tracts
#make a 4326 version of this, as it's needed sometimes
den_metro_bg_no_wtr_4326_geo = den_metro_bg_no_wtr_geo %>% 
  st_transform(4326) %>% 
  mutate(row_id_int=row_number())
save(den_metro_bg_no_wtr_4326_geo, file = "den_metro_bg_no_wtr_4326_geo.RData")

### Load zoning data, because we will also exclude the airport zone and
# the industrial zone, per meeting with DRR 3/7
# source(here("scripts","0_read_wrangle_denver_land_use.R")) #this takes ~10 s
#update: actually don't takes too long

# Miscellaneous calculations in preparation for appendix tables------


#I'm first computing NDVI diff for each scenario, and then in a subsequent
#major section, calculating avoidable deaths.
#It's better to isolate the avoidable death calculation as a single calc for bootstrapping
####################################################-
# COMPUTE NDVI-DIFF FOR EACH SCENARIO---------------
####################################################-
#1. Scenario 1: homogenous greening in each census block group---------
## Extract NDVI in those block groups on that day--------
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
    #6/6/22 reminder. these all summarize by bg_fips so there should be no repeating within bg_fips
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
      ndvi_mean_wt =ndvi_wt_int/sum_of_wts #weighted mean
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


#Because I need it elsewhere, also create one that doesn't exclude northeast tracts
load("den_metro_bg_no_wtr_4326_geo.RData") #created ~scripts/1_remove_water_tract_bg_park.R
den_co_bg_ndvi_no_exclusions_geo = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2 = den_metro_bg_no_wtr_4326_geo)


#look up the main NDVI (weighted mean) for use in another script
lookup_den_co_bg_ndvi_mean_wt = den_co_bg_ndvi_no_exclusions_geo %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, ndvi_mean_wt)
save(lookup_den_co_bg_ndvi_mean_wt, file = "lookup_den_co_bg_ndvi_mean_wt.RData")

#make a function like for the bg_int scenarios
#bg itself vs bg intersection below
#define native threshold 
mutate_ndvi_diff_bg_itself = function(df){
  df %>% 
    mutate(
      #I need a character version of this, too, for plotting
      ndvi_below_native_threshold_char = case_when(
        ndvi_below_native_threshold == 1 ~ "Yes",
        ndvi_below_native_threshold == 0 ~ "No"
      ),
      prop_area_comp = 1-prop_area_tx, #paradoxically, take 1 minus this now
      #renamed may 20 2022 from ndvi_alt_avg. i use ndvi_mean elsewhere, so this is more consistent.
      ndvi_mean_alt = prop_area_tx*ndvi_native_threshold+(1-prop_area_tx)*ndvi_mean_wt,
      ndvi_quo = ndvi_mean_wt, #rename this to _quo to be consistent with other scenarios
      ndvi_diff = ndvi_mean_alt-ndvi_quo,
      
      #for consistency with the below
      #6/6/22. because I may have block groups IDs that repeat here, I need a unique identifier.
      #how about this, just to be sure we can keep track of the area measurements.
      row_id_int = row_number()
  )
}

## Define scenarios-----------
#Update June 21, 2022, creating another variable called scenario label for the table
#100 percent native
den_co_bg_ndvi_alt_100_nogeo = den_co_bg_ndvi_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(
    #I use these vars in subsequent scenarios, so name them the same here, as well
    prop_area_tx = 1, #what proportion of the area (bg, here) is affected?
    #4/16/22 add this for consistency here
    #note this is different than prop_area_tx
    #for example, the office-green-infra polygons are all considered "treatment" areas
    #but only 50% might be vegetated,
    #for the bg intervention, we always say 1
    prop_tx_itself_veg=1,#of the area of the actual treatment, what proportion is treated?
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
    prop_tx_itself_veg=1,#of the area of the actual treatment, what proportion is treated?
    scenario = "all-bg",
    scenario_sub = "30-pct" ) 

#20 percent native
den_co_bg_ndvi_alt_20_nogeo = den_co_bg_ndvi_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(
    #I use these vars in subsequent scenarios, so name them the same here, as well
    prop_area_tx = .2, #what proportion of the area (bg, here) is affected?
    prop_tx_itself_veg=1,#of the area of the actual treatment, what proportion is treated?
    scenario = "all-bg",
    scenario_sub = "20-pct" ) 

## combine them and final wrangling--------
#load the lookups between the 2020 vs 2019 definitions of tracts and block groups
#so you can link in the corresponding definitions of equity
load("lookup_colorado_tract_2020_vs_2019_nogeo.RData")
load("lookup_colorado_bg_2020_vs_2019_nogeo.RData")
#to link 2020 block groups with 2020 census tracts
load("lookup_colorado_bg_tract.RData")
load("lookup_den_nbhd_tract.RData") #neighborhood ids in denver linked to tracts
source(here("scripts", "0_read_equity_indices.R")) #source this to load most recent equity files
#I use this twice, so make a function
link_equity_indices= function(df){
  df %>% 
    left_join(lookup_colorado_bg_tract, by = "bg_fips") %>% #2020 ok
    left_join(lookup_den_nbhd_tract, by = "tract_fips") %>%  #link neighbs so can link den equity neighb data; 2020 OK
    left_join(lookup_equity_nbhd_denver, by = "nbhd_id") %>% #denver neighborhood equity score
    rename(bg_fips_2020 = bg_fips) %>%    #link the 2019 block-group definition to link the CDPHE equity score
    left_join(lookup_colorado_bg_2020_vs_2019_nogeo, by = "bg_fips_2020") %>% 
    left_join(lookup_equity_bg_cdphe_2019, by = "bg_fips_2019") %>% #link block-group level equity; note 2019 bg 
    rename(bg_fips = bg_fips_2020)      #change name back
}

map_over_native_ndvi_all_bg = function(ndvi_native_threshold_val){
  df = den_co_bg_ndvi_alt_100_nogeo %>% 
    bind_rows(
      den_co_bg_ndvi_alt_30_nogeo, 
      den_co_bg_ndvi_alt_20_nogeo
    ) %>% 
    mutate(
      #add the native-plants NDVI value here instead of above so I can loop it through
      #various possible values 
      ndvi_native_threshold = ndvi_native_threshold_val, #define it here as a variable
      ndvi_below_native_threshold = case_when( #whether baseline was above native threshold
        ndvi_mean_wt   < ndvi_native_threshold ~1,
        TRUE ~0)
    ) %>% 
    mutate_ndvi_diff_bg_itself() %>% 
    link_equity_indices() %>% #see above
    mutate(
      scenario_label = "Block-group level", #added June 21, 2022
      #re-order scenario_sub to be a factor; this doesn't really work.
      scenario_sub = factor(
        scenario_sub, 
        levels = c("100-pct", "30-pct", "20-pct"))
    ) %>% 
      dplyr::select(
        contains("bg_fips"), 
        starts_with("row_id_int"),
        starts_with("tract_fips"),
        starts_with("equity"),
        starts_with("nbhd"),
        contains("scenario"), contains("ndvi"), contains("prop_")
      )
}

# define NDVI of native plants-----------
# note in meeting on May 2, 2022 we decided we would use the lower value
#corresponding to the NDVI of the denver botanic gardens green roof
#and the upper value corresponding to the lager native plot in denver botanic gardens
#visualized here:
#https://michaeldgarber.github.io/green-space-denver/ndvi-of-places-tracts.html
#pull out the actual values here

load("native_places_ndvi_day_nogeo.RData")
ndvi_native_places_summary = native_places_ndvi_day_nogeo %>% 
  filter(date_is_valid_all==1) %>% 
  group_by(place_type, place_name_fac) %>% 
  summarise(
    ndvi_mean = mean(ndvi_mean_wt, na.rm=TRUE), # mean of means
    ndvi_25th = quantile(ndvi_med, probs =c(0.25), na.rm=TRUE), #percentile of medians
    ndvi_med = median(ndvi_med, na.rm=TRUE), #percentile of medians
    ndvi_75th = quantile(ndvi_med, probs =c(0.75), na.rm=TRUE) ) %>% #percentile of medians
  ungroup() 
# 0.3228 for a lower value
# 0.4909 for denver botanic gardens

#iterate over three values of ndvi for native plants
#May 25, 2022: I want to add green mountain park and then
ndvi_native_threshold_values = ndvi_native_places_summary %>% 
  filter( 
    place_name_fac == "Denver Botanic Gardens Green Roof" |
      place_name_fac == "Denver Botanic Gardens, 100% Native" |
      place_name_fac == "Green Mountain Park, 85% Native") %>% 
  dplyr::select(ndvi_mean) %>% 
  pull()

ndvi_native_threshold_values
 
den_co_bg_ndvi_alt_all_nogeo = ndvi_native_threshold_values %>% 
  map_dfr(map_over_native_ndvi_all_bg)


table(den_co_bg_ndvi_alt_all_nogeo$ndvi_native_threshold)
class(den_co_bg_ndvi_alt_all_nogeo$ndvi_below_native_threshold)
table(den_co_bg_ndvi_alt_all_nogeo$scenario_sub)
save(den_co_bg_ndvi_alt_all_nogeo, file = "den_co_bg_ndvi_alt_all_nogeo.RData")
den_co_bg_ndvi_alt_all_nogeo
names(den_co_bg_ndvi_alt_all_nogeo)
nrow(den_co_bg_ndvi_alt_all_nogeo)
n_distinct(den_co_bg_ndvi_alt_all_nogeo$bg_int_row_id)
## intermediate mapviews--------
### visualize ndvi diff for one scenario------------
den_co_bg_ndvi_alt_all_nogeo %>% 
  filter(ndvi_native_threshold<0.41) %>% 
  filter(scenario_sub == "20-pct") %>% 
  left_join(lookup_den_co_bg_no_wtr_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "ndvi_diff",
    zcol = "ndvi_diff")

names(den_co_bg_ndvi_alt_all_nogeo)
den_co_bg_ndvi_alt_all_nogeo %>% 
  filter(ndvi_native_threshold<0.41) %>% 
  filter(scenario_sub == "20-pct") %>% 
  left_join(lookup_den_co_bg_no_wtr_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "equity_bg_cdphe",
    zcol = "equity_bg_cdphe")

den_co_bg_ndvi_alt_all_nogeo %>% 
  filter(ndvi_native_threshold<0.41) %>% 
  filter(scenario_sub == "20-pct") %>% 
  left_join(lookup_den_co_bg_no_wtr_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "equity_nbhd_denver",
    zcol = "equity_nbhd_denver")


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
mv_den_co_bg_ndvi

#Examine block groups above/below native threshold
library(shades)
library(RColorBrewer)

RColorBrewer::brewer.pal(3, "RdYlGn")[c(1,3)]  %>%   swatch()
ndvi_below_thresh_pal = RColorBrewer::brewer.pal(3, "RdYlGn")[c(1,3)]  %>% rev()
ndvi_below_thresh_pal %>% swatch()
mv_bg_below_native_threshold =den_co_bg_ndvi_alt_all_nogeo %>% 
  filter(ndvi_native_threshold<0.41) %>% #can pick either side of this.
  filter(scenario_sub == "20-pct") %>% 
  left_join(lookup_den_co_bg_no_wtr_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
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

    st_transform(2876) %>% #just to be sure before we take the area measurements
          #6/6/22 this fixed a major error.
    #update 4/14/22 making these area variables long form (i.e., the same)
    #for every object
    #the area of piece of the block group overlapping the buffer OR TREATMENT
    mutate(
      area_ft2_bg_int = as.numeric(st_area(geometry)),
      area_mi2_bg_int = area_ft2_bg_int/(5280**2), #miles squared
    #Update 4/3/22 rather than link in the northeast exclusions, I'm
    #beginning with a file that doesn't have it.
    #more straightforward.
    
    row_id_int = row_number()) %>%  # to keep track
      st_transform(4326)   # the NDVI raster file is in 4326
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

#note this is where area_mi2_bg_int_tx is created; via pivot_wider, not directly
#named in the script
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
    #update: take the ndvi_mean_alt definition out of this because it will vary
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
      ndvi_mean_alt = 
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
      #use quo for status quo; avoid using sq for nomenclature because it could mean squared
      
      #update 4/15/22
      #this has to be conditional based on whether treatment-area NDVI is non-missing
      ndvi_quo = case_when(
        ndvi_mean_wt_tx>0 ~prop_area_comp*ndvi_mean_wt_comp+prop_area_tx*ndvi_mean_wt_tx, #if not missing
        TRUE ~ndvi_mean_wt_res #otherwise, it's just the full res buffer
      ),
      
      #now we can calculate the linear exposure difference between those values
      #and the baseline mean NDVI in the 500 m buffer. use ndvi_diff as done above
      ##alternative minus baseline
      ndvi_diff = ndvi_mean_alt - ndvi_quo
    )
}

names(den_bg_int_wtr_50ft_ndvi_wide)
#rip for riparian
map_over_native_ndvi_rip = function(ndvi_native_threshold_val){
  df =den_bg_int_wtr_50ft_ndvi_wide %>% 
    bind_rows(
      den_bg_int_wtr_100ft_ndvi_wide, 
      den_bg_int_wtr_200ft_ndvi_wide ) %>% 
    mutate(
      #add the native-plants NDVI value here instead of above so I can loop it through
      #various possible values 
      ndvi_native_threshold = ndvi_native_threshold_val, #define it here as a variable
      #whether treatment area was above native threshold. note this is different than the scenarios
      #at the block-group level
      ndvi_below_native_threshold = case_when( 
        ndvi_mean_wt_tx   < ndvi_native_threshold ~1,
        TRUE ~0),
      prop_tx_itself_veg=1, #of the area of the actual treatment, what proportion is treated?
                            #note this is different than prop_area_tx
                            #for example, the office-green-infra polygons are all considered "treatment" areas
                            #but only 50% might be vegetated
      #set what the ndvi_alt_tx_only should be
      #in this case, it's just 0.5, but write as a weighted average to more easily generalize
      #note this will be missing wherever treatment ndvi is missing within a block group; that's okay.
      ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + (1-prop_tx_itself_veg)*ndvi_mean_wt_tx 
    ) %>% 
    mutate_ndvi_diff_bg_int() %>% 
    mutate(  
      scenario_label = "Riparian areas", #added June 21, 2022
      #re-order scenario_sub to be a factor
      scenario_sub = factor(
        scenario_sub, 
        levels = c("200-ft", "100-ft", "50-ft"))
    )
}

den_bg_int_wtr_ndvi_all_nogeo = ndvi_native_threshold_values %>% 
  map_dfr(map_over_native_ndvi_rip)
table(den_bg_int_wtr_ndvi_all_nogeo$ndvi_native_threshold)
table(den_bg_int_wtr_ndvi_all_nogeo$scenario_sub)
save(den_bg_int_wtr_ndvi_all_nogeo, file = "den_bg_int_wtr_ndvi_all_nogeo.RData")
den_bg_int_wtr_ndvi_all_nogeo
names(den_bg_int_wtr_ndvi_all_nogeo)

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
  #4/15/22 I had area measurements here before, but I don't need them. and it gets messy.
  summarise( 
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
map_over_native_ndvi_all_ogi_proj = function(ndvi_native_threshold_val){
  df = den_bg_int_ogi_proj_res_ndvi_nogeo %>% 
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
      scenario_sub = "ogi_proj",
      scenario_label = "Retention basins" #added June 21, 2022
    ) %>% 
    mutate(
      #add the native-plants NDVI value here instead of above so I can loop it through
      #various possible values 
      ndvi_native_threshold = ndvi_native_threshold_val, #define it here as a variable
      #whether treatment area was above native threshold. note this is different than the scenarios
      #at the block-group level
      #note! ndvi_mean_wt_tx is created in the pivot_wider calculation above
      ndvi_below_native_threshold = case_when( 
        ndvi_mean_wt_tx   < ndvi_native_threshold ~1,
        TRUE ~0),
      #expect about 75% to be landscaped w native
      prop_tx_itself_veg=.75,
      ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
        (1-prop_tx_itself_veg)*ndvi_mean_wt_tx ) %>% 
    mutate_ndvi_diff_bg_int()  
}

#save for use in bootstrap code
den_bg_int_ogi_proj_ndvi_wide = ndvi_native_threshold_values %>% 
  map_dfr(map_over_native_ndvi_all_ogi_proj)
table(den_bg_int_ogi_proj_ndvi_wide$ndvi_native_threshold)
save(den_bg_int_ogi_proj_ndvi_wide, file = "den_bg_int_ogi_proj_ndvi_wide.RData")
den_bg_int_ogi_proj_ndvi_wide
names(den_bg_int_ogi_proj_ndvi_wide)

## OGI: Green streets---------------
#Update May 23 2022 I decided to completely scratch the green-streets work.
#Comment September 8, 2022: there are some green streets in the ogi_project
#shapefile above. They should probably be deleted so that these projects
#are separated.
#Overarching question here should be:
#Status quo:
# 2.7 miles per year, and each mile of street equates to 0.15 acres
#1 acre equals 43560 square feet
.15*43560 #per mile
#assume a rectangle, so l*w=a; we know l; solve for w; w=a/l
(.15*43560)/5280
#so currently about 1.2 feet per foot of street....
#call that the radius of the buffer so a buffer for the status quo can be
((.15*43560)/5280)/2 #.6 foot buffer

#goal is 0.75 acres per green mile
(.75*43560)/5280 #so 6.1875 feet

#and a buffer for the more ambitious vegetation can be
((.75*43560)/5280)/2 #3 foot buffer
#Assume green streets can occur on all road types except motorway and trunk
#randomly sample a cumulative sum of just below one mile
setwd(here("data-processed"))
load("den_osm_roads.RData") #see for definitions
#~scripts/0_load_denver_osm_roads.R
sf::sf_use_s2(FALSE)
st_crs(den_osm_roads)
st_crs(study_area_2876)
lookup_tract_nbhd_northeast_exclude
den_osm_roads_shuffle = den_osm_roads %>% 
  filter(highway_primary_or_lower==1) %>% 
  st_buffer(1) %>% #1 foot buffer to make sure it works..
  #remove northeast tracts as we've done elsewhere to restrict sampling frame
  #note st_intersection using study_area didn't work, so I'm trying st_difference instead
  #  st_intersection(study_area_2876) %>%  
  st_difference(nbhd_northeast_exclude_100_ft_geo) %>% 
  slice_sample(prop=1) %>% #shuffle the dataset by row so the cumulative sum is random
  mutate(
    length_mi_cumsum=cumsum(length_mi),
    #status quo is 2.7 miles
    #goal to increase to 5 miles
    length_mi_cumsum_cat = case_when(
      length_mi_cumsum < 2.7 ~ "<2.7",
      length_mi_cumsum >=2.7 | length_mi_cumsum < 5 ~ "2.7-5.0",
      length_mi_cumsum >5 ~ "5.0+"
    )
  ) 
den_osm_roads_shuffle %>% mapview()

# a version limited to cum sum of 5 mi or less
den_osm_roads_cumsum_5mi = den_osm_roads_shuffle %>% 
  filter(length_mi_cumsum < 5)


## Prep buffers for green streets-------------
#adopt same strategy as with the riparian areas.
#Create a 500 m residential buffer around each mileage of streets (2.7 miles, 5 miles)
#You will then define the treatment area for each mileage of streets as 
#1) .15 acres per mile - about a .6 foot radial buffer, specifically
gs_veg_radius_small =((.15*43560)/5280)/2 #gs for green streets
#2) 0.75 acres per mile - about 3 foot radial buffer, specifically
gs_veg_radius_large = ((.75*43560)/5280)/2

#the treatment areas. can filter to the shorter category if you want.
#small corresponds to smaller buffer area.
#marg just for the street itself
den_osm_roads_tx_marg = den_osm_roads_cumsum_5mi %>%
  group_by(length_mi_cumsum_cat) %>%  #rather than a full union, group by length category. simpler.
  summarise(n=n()) %>% 
  ungroup() %>% 
  dplyr::select(-n) %>% 
  st_as_sf() %>% 
  st_buffer(50)      #suppose the width of the street itself is 50 ft

den_osm_roads_tx_marg %>% mapview(zcol = "length_mi_cumsum_cat")
  
den_osm_roads_tx_marg_union = den_osm_roads_tx_marg %>% 
  st_union()

#this is silly but it does correspond to what we were told.
den_osm_roads_tx_marg_small = den_osm_roads_tx_marg %>%
  st_buffer(gs_veg_radius_small) %>% 
  st_as_sf() %>% 
  st_difference(den_osm_roads_tx_marg_union) #and now get the rid of the street itself to measure the vegetated part.


den_osm_roads_tx_marg_small %>% mapview(zcol = "length_mi_cumsum_cat")

den_osm_roads_tx_marg_large = den_osm_roads_tx_marg %>%
  st_buffer(gs_veg_radius_large) %>% 
  st_as_sf() %>% 
  st_difference(den_osm_roads_tx_marg_union) #and now get the rid of the street itself to measure the vegetated part.

den_osm_roads_tx_marg_large %>% mapview(zcol = "length_mi_cumsum_cat")


#I need a union version for res buffer
den_osm_roads_tx_marg_small_union = den_osm_roads_tx_marg_small %>% 
  st_union()
den_osm_roads_tx_marg_large_union = den_osm_roads_tx_marg_large %>% 
  st_union()

#residential buffer
den_osm_roads_res = den_osm_roads_cumsum_5mi %>% #calling it _res instead of _500m
  group_by(length_mi_cumsum_cat) %>%  #rather than a full union, group by length category. simpler.
  summarise(n=n()) %>% 
  ungroup() %>% 
  dplyr::select(-n) %>% 
  st_as_sf() %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet

den_osm_roads_res %>% mapview(zcol = "length_mi_cumsum_cat")
#complement buffer for both small and large green-streets scenario
den_osm_roads_comp_small = den_osm_roads_res %>%
  st_difference(den_osm_roads_tx_marg_small_union)

den_osm_roads_comp_large = den_osm_roads_res %>%
  st_difference(den_osm_roads_tx_marg_small_union)

den_osm_roads_comp_large %>% mapview()

#Conclusion: don't do an HIA for the green-streets scenarios. Our area-based methods are not up to task
#for something that would have a very minimal area-based impact on land use...
#we're talking an extra foot to 6 feet of greening, and our methods don't
#have enough precision to meaningfully make sense of that.

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
  st_intersection(study_area_2876) #remove northeast tracts as we've done elsewhere to
                                    #restrict sampling frame

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
    area_mi2_parcel = sum(area_mi2_parcel) 
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
map_over_native_ndvi_all_ogi_parcel = function(ndvi_native_threshold_val){
  df = den_bg_int_parcel_res_ndvi_nogeo %>% 
    bind_rows(
      den_bg_int_parcel_comp_ndvi_nogeo, #the residential buffer
      den_bg_int_parcel_tx_ndvi_nogeo) %>% 
    dplyr::select(bg_fips, ndvi_mean_wt, area_mi2_bg_int, buff_type) %>% 
    pivot_wider(
      names_from = buff_type,
      values_from = c(ndvi_mean_wt, area_mi2_bg_int)
    ) %>% 
    mutate(#add this info here
      scenario = "ogi",
      scenario_label = "Parcel regulations", #added June 21, 2022
      scenario_sub = "parcel"
    ) %>% 
    mutate(  
      #add the native-plants NDVI value here instead of above so I can loop it through
      #various possible values 
      ndvi_native_threshold = ndvi_native_threshold_val, #define it here as a variable
      #whether treatment area was above native threshold. note this is different than the scenarios
      #at the block-group level
      ndvi_below_native_threshold = case_when( 
        ndvi_mean_wt_tx   < ndvi_native_threshold ~1,
        TRUE ~0),
      #say about 50% of the new dev't is landscaped; may not be that high.
      prop_tx_itself_veg=.5,
      ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
        (1-prop_tx_itself_veg)*ndvi_mean_wt_tx ) %>% 
    mutate_ndvi_diff_bg_int()    #all one step in contrast with riparian areas
}


den_bg_int_parcel_ndvi_wide = ndvi_native_threshold_values %>% 
  map_dfr(map_over_native_ndvi_all_ogi_parcel)
table(den_bg_int_parcel_ndvi_wide$scenario)
table(den_bg_int_ogi_proj_ndvi_wide$ndvi_native_threshold)
save(den_bg_int_parcel_ndvi_wide, file = "den_bg_int_parcel_ndvi_wide.RData")


# 4. Scenario 4: Parking -----------
## Prep parking buffers--------
#Parking data managed here: 0_read_denver_parking.R
setwd(here("data-processed"))
load("den_prkng_res.RData") #Note rename from _500m
names(den_prkng_res)
den_prkng_res = den_prkng_res %>% #for redundancy to be sure find + replace works.
  st_as_sf()
load("den_prkng_tx_marg.RData") #I used to call this sum_union; marg for marginal is more clear to me
#actually one more difference; above, we use this syntax: den_prkng_tx_marg
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
    buff_type = "res"  
  ) %>%
  dplyr::select(  
    bg_fips, buff_type,  starts_with("ndvi_mean"), 
    contains("ndvi_below"), contains("area") #keep ndvi_below here
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
    buff_type = "comp"  
  ) %>%
  dplyr::select( bg_fips, buff_type,  starts_with("ndvi_mean"), contains("area"))

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
  bg_int_wrangle_last_steps() %>% #re-calculate area at level of bg_fips
  mutate(   
    buff_type = "tx" ) %>%
  dplyr::select( bg_fips, buff_type,  starts_with("ndvi_mean"), contains("area"))

save(den_bg_int_prkng_tx_ndvi, 
     file = "den_bg_int_prkng_tx_ndvi.RData")
den_bg_int_prkng_tx_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain)
names(den_bg_int_prkng_tx_ndvi)

#check. does this add up to the total parking area?
den_bg_int_prkng_tx_ndvi %>% 
  st_set_geometry(NULL) %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(area_mi2_bg_int = sum(area_mi2_bg_int, na.rm=TRUE))
  

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

#test another visual. see how these vary by block group. the area measurement isn't
#working as expected
mv_den_bg_int_prkng_tx_ndvi

mv_den_bg_int_prkng_tx_bg= den_bg_int_prkng_tx_ndvi %>% 
  mapview(
    col.regions = rainbow(n=n_distinct(den_bg_int_prkng_tx_ndvi$bg_fips)),
    layer.name = "Block group FIPS",
    zcol = "bg_fips")

mv_den_bg_int_prkng_tx_bg


### Union the parking lots and their corresponding residential buffer for an alternate visual------
names(den_bg_int_prkng_tx_ndvi)
den_bg_int_prkng_tx_union = den_bg_int_prkng_tx_ndvi %>% 
  mutate(dummy=1) %>% 
  group_by(dummy) %>% 
  summarise(area_mi2_bg_int = sum(area_mi2_bg_int, na.rm=TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(-dummy)

save(den_bg_int_prkng_tx_union, file = "den_bg_int_prkng_tx_union.RData")
den_bg_int_prkng_comp_union = den_bg_int_prkng_comp_ndvi %>% 
  mutate(dummy=1) %>% 
  st_make_valid() %>% 
  group_by(dummy) %>% 
  summarise(area_mi2_bg_int = sum(area_mi2_bg_int, na.rm=TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(-dummy)
save(den_bg_int_prkng_comp_union, file = "den_bg_int_prkng_comp_union.RData")


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
  ) 

n_distinct(den_bg_int_ogi_proj_ndvi_wide$bg_fips)
nrow(den_bg_int_prkng_ndvi_wide)
#Now consider a few different scenarios for the proportion of the parking lots that we would change
#100%; 50%; 20%
names(den_bg_int_prkng_ndvi_wide)
den_bg_int_prkng_alt_100 = den_bg_int_prkng_ndvi_wide %>% 
  mutate(
    scenario = "prkng",
    scenario_sub = "100-pct",
    prop_tx_itself_veg=1
    ) 
den_bg_int_prkng_alt_50 = den_bg_int_prkng_ndvi_wide %>% 
  mutate(
    scenario = "prkng",
    scenario_sub = "50-pct",
    prop_tx_itself_veg=.5
  ) 
#update may 23 2022 change this from 20 to 30 to be consistent with 30% by 2030 above
#easier to talk about
den_bg_int_prkng_alt_30 = den_bg_int_prkng_ndvi_wide %>% 
  mutate(
    scenario = "prkng",
    scenario_sub = "30-pct",
    prop_tx_itself_veg=.3
  ) 

den_bg_int_prkng_alt_100
### Combine them all here----------
map_over_native_ndvi_all_ogi_prkng = function(ndvi_native_threshold_val){
  df = den_bg_int_prkng_alt_100 %>% 
    bind_rows(
      den_bg_int_prkng_alt_50,
      den_bg_int_prkng_alt_30
    ) %>% 
    mutate(
      scenario_label = "Parking", #added June 21, 2022
      #re-order scenario_sub to be a factor
      scenario_sub = factor(
        scenario_sub, 
        levels = c("100-pct", "50-pct", "30-pct")),
      
      #add the native-plants NDVI value here instead of above so I can loop it through
      #various possible values 
      ndvi_native_threshold = ndvi_native_threshold_val, #define it here as a variable
      #whether treatment area was above native threshold. note this is different than the scenarios
      #at the block-group level
      ndvi_below_native_threshold = case_when( 
        ndvi_mean_wt_tx   < ndvi_native_threshold ~1,
        TRUE ~0),
    
      #this works here rather than in each constituent scenario's data frame.
      ndvi_alt_tx_only = prop_tx_itself_veg*ndvi_native_threshold + 
        (1-prop_tx_itself_veg)*ndvi_mean_wt_tx
      )%>% 
      mutate_ndvi_diff_bg_int() 
}

den_bg_int_prkng_alt_all = ndvi_native_threshold_values %>% 
  map_dfr(map_over_native_ndvi_all_ogi_prkng)
names(den_bg_int_prkng_alt_all)
summary(den_bg_int_prkng_alt_all$area_mi2_bg_int_tx)
table(den_bg_int_prkng_alt_all$ndvi_native_threshold)
#save for use in bootstrap code
save(den_bg_int_prkng_alt_all, file = "den_bg_int_prkng_alt_all.RData")
table(den_bg_int_prkng_alt_all$scenario_sub)
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
  #Update: we decided not to use the other dose-response functions
  #(e.g., the Paul 2020; Urban green space and the risks of dementia and stroke)
  filter(measure == "deaths" & cause_short == "all") %>% 
  mutate(
    #calculate the standard deviation so that we can re-sample the uncertainty.
    #note this makes a normality assumption
    rate_per_100k_sd = abs(rate_per_100k_est-rate_per_100k_ll)/1.96,
    #calculate total deaths for easier collapsed rate calc. below
    deaths_annual_est = pop_est*(rate_per_100k_est/100000),
    deaths_annual_ll = pop_est*(rate_per_100k_ll/100000),
    deaths_annual_ul = pop_est*(rate_per_100k_ul/100000)
  ) %>% 
  dplyr::select(-var_label, -var_name) #drop these


#Note I had some calculations here that I moved to
# scripts/4_mortality_area_study_area.R
#because they were superfluous to the calculation of the HIA results
den_co_bg_sex_age_gbd_wrangle
table(den_co_bg_sex_age_gbd_wrangle$age_group_acs_30_plus)

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
table(den_co_bg_ndvi_alt_all_nogeo$scenario)
names(den_bg_int_wtr_ndvi_all_nogeo)
names(den_bg_int_ogi_proj_ndvi_wide)
names(den_co_bg_ndvi_alt_all_nogeo)
names(den_co_bg_sex_age_gbd_wrangle)
setwd(here("data-processed"))
load("lookup_bg_no_wtr_area.RData")
lookup_bg_no_wtr_area #link in area to define the treatment area for
#whole-block-group scenarios
mutate_part_of_hia = function(df){ #make a function out of it since 
  df %>%    #I use it again in the bootstrap
    mutate(
      rr_alt = drf_est**(ndvi_diff/drf_increment), #calc. risk ratios per dose-response funct
      paf =(rr_alt -1)/rr_alt , #pop_est attrib fraction
      #attributable deaths
      #rate is per 100,000 so first divide by 100,000 before multiplying by the pop_est. in age group
      #marginal totals were filtered out above, so it's just the joint sex-by-age categories
      #could generalize from d to o for outcome if we include other outcomes rather than just death
      attrib_d = paf*(rate_per_100k_est/100000)*pop_affected, #attrib deaths. note divide by 100000
      deaths_prevented = attrib_d*-1,
      deaths_prevented_per_pop = deaths_prevented/pop_affected #pp is per person
    )
}
names(den_co_bg_ndvi_alt_all_nogeo)
names(den_bg_int_wtr_ndvi_all_nogeo)
hia_all  = den_co_bg_ndvi_alt_all_nogeo %>% #scenario all bg
  #to make sure that ndvi_mean_wt_tx works throughout, add it here for the block-group-level scenario
  #as simply equal to ndvi_mean_wt
  mutate(ndvi_mean_wt_tx = ndvi_mean_wt) %>% 
  bind_rows(
    den_bg_int_wtr_ndvi_all_nogeo, #  riparian
    den_bg_int_ogi_proj_ndvi_wide, # ogi proj
    den_bg_int_parcel_ndvi_wide, #  ogi parcel
    den_bg_int_prkng_alt_all # parking
  ) %>% #link pop data here
  #make sure tract_fips and county_fips aren't here
  dplyr::select(-contains("tract_fips"), -contains("county_fips")) %>% 
  #add equity indices to all scenarios but before I do that,
  #remove variables that may duplicate and get in the way
  dplyr::select(-starts_with("equity"), -starts_with("nbhd"), -starts_with("bg_fips_2019")) %>% 
  link_equity_indices() %>% #defined above
  dplyr::select(-starts_with("tract_fips")) %>% #again, tract_fips gets duplicated so remove 
  left_join(den_co_bg_sex_age_gbd_wrangle, by = "bg_fips") %>% 
  mutate(
    pop_affected = case_when(
      scenario == "all-bg" ~ pop_est, #for the complete bg scenarios, it's just pop of the bg
      TRUE ~ area_mi2_bg_int_res*pop_dens_mi2 #for other scenarios, multiply area by pop dens
          ),
    pop_affected_type = case_when(
      scenario == "all-bg" ~ "bg", #to keep track of how calculated
      TRUE ~   "res-buffer" #residential buffer otherwise
    )) %>% 
  mutate_part_of_hia() %>% 
  left_join(lookup_bg_no_wtr_area, by = "bg_fips") %>% #link area for first 2 scenarios
  mutate(
    #for easier summary, rename the area variable for the block-group-level scenarios
    #to be the same as that of those scenarios that intersect the block groups.
    #the treated area and the residential area will be the same.
    area_mi2_bg_int_tx = case_when(
      scenario == "all-bg" ~ area_mi2_no_wtr, 
      TRUE ~ area_mi2_bg_int_tx 
    ),
    area_mi2_bg_int_res = case_when(
      scenario == "all-bg" ~ area_mi2_no_wtr, 
      TRUE ~ area_mi2_bg_int_res 
    ),
    
    #one thing to check is whether we've limited to ndvi_below_threshold and how
    #does it make sense to simply not intervene upon any block group that has a ndvi above threshold?
    #or, so we don't lose so much, for the non-bg interventions, we could only not intervene
    #if the block-group piece is below ndvi
    #probably simplest to re-calculate this. I think this will suffice.
    ndvi_below_native_threshold = case_when( 
      ndvi_quo   < ndvi_native_threshold ~1,
      TRUE ~0
    ),
    scenario_main_text = case_when(
      scenario == "all-bg" & scenario_sub == "30-pct" ~1,
      scenario == "riparian" & scenario_sub == "200-ft" ~1,
      scenario == "ogi" & scenario_sub == "ogi_proj" ~1,
      scenario == "prkng" & scenario_sub == "30-pct" ~1,
      TRUE ~0
    ),
    
    #to re-calculate the weighted average for later summaries, I need these intermediate values
    #(products of the value times its weight, and then will divide by weights later).
    #easiest to put them here:
    
    #re-calculate weighted average ndvi - baseline and alternate
    ndvi_mean_alt_int = ndvi_mean_alt*area_mi2_bg_int_res,
    ndvi_quo_int = ndvi_quo*area_mi2_bg_int_res,
    ndvi_mean_wt_tx_int = ndvi_mean_wt_tx*area_mi2_bg_int_tx,
      
    #I thought I could get these to sort based on factor order, but nope, sort this way
    scenario_sort_order = case_when(
      scenario == "all-bg" & scenario_sub == "100-pct" ~1,
      scenario == "all-bg" & scenario_sub == "30-pct" ~2,
      scenario == "all-bg" & scenario_sub == "20-pct" ~3,
      scenario == "riparian" & scenario_sub == "200-ft" ~4,
      scenario == "riparian" & scenario_sub == "100-ft" ~5,
      scenario == "riparian" & scenario_sub == "50-ft" ~6,
      scenario == "ogi" & scenario_sub == "ogi_proj" ~7,
      scenario == "ogi" & scenario_sub == "parcel" ~8,
      scenario == "prkng" & scenario_sub == "100-pct" ~9,
      scenario == "prkng" & scenario_sub == "50-pct" ~10,
      scenario == "prkng" & scenario_sub == "30-pct" ~11
  ),
  scenario_num = case_when(
    scenario == "all-bg" & scenario_sub == "100-pct" ~1,
    scenario == "all-bg" & scenario_sub == "30-pct" ~1,
    scenario == "all-bg" & scenario_sub == "20-pct" ~1,
    scenario == "riparian" & scenario_sub == "200-ft" ~2,
    scenario == "riparian" & scenario_sub == "100-ft" ~2,
    scenario == "riparian" & scenario_sub == "50-ft" ~2,
    scenario == "ogi" & scenario_sub == "ogi_proj" ~3,
    scenario == "ogi" & scenario_sub == "parcel" ~3,
    scenario == "prkng" & scenario_sub == "100-pct" ~4,
    scenario == "prkng" & scenario_sub == "50-pct" ~4,
    scenario == "prkng" & scenario_sub == "30-pct" ~4
  )
  ) %>% 
  dplyr::select(
    contains("fips"), starts_with("scenario") ,
    starts_with("ndvi_native_threshold"),
    starts_with("pop"), starts_with("area"), contains("area"),
    contains("ndvi"), contains("drf"),
    contains("equity"),
    everything())

save(hia_all, file = "hia_all.RData")
#create a lookup for scenario_main_text
lookup_scenario_main_text = hia_all %>% 
  distinct(scenario, scenario_sub, scenario_main_text)
save(lookup_scenario_main_text, file = "lookup_scenario_main_text.RData")

lookup_scenario_num = hia_all %>% 
  distinct(scenario, scenario_sub, scenario_num)
save(lookup_scenario_num, file = "lookup_scenario_num.RData")
lookup_scenario_num
#checks
table(hia_all$scenario)
table(hia_all$scenario_sub)
table(hia_all$scenario_main_text)
table(hia_all$scenario_sort_order)
names(hia_all)
table(hia_all$ndvi_below_native_threshold)
summary(hia_all$ndvi_mean_wt_tx)
summary(hia_all$ndvi_quo)
nrow(hia_all)
hia_all
#look up scenario sort order
lookup_scenario_sort_order = hia_all %>% 
  distinct(scenario, scenario_sub, scenario_sort_order)
lookup_scenario_sort_order
save(lookup_scenario_sort_order, file = "lookup_scenario_sort_order.RData")

#look up scenario label
lookup_scenario_label = hia_all %>% 
  distinct(scenario, scenario_sub, scenario_label)
save(lookup_scenario_label, file = "lookup_scenario_label.RData")
lookup_scenario_label

#first extract area values at the level of the bg or bg_int to avoid the error
#of summarizing over all the age-groups. do this before the below function.
#I actually think we should do this as a look up rather than as a function. Simpler.
lookup_bg_fips_scenario_area = hia_all %>% 
  distinct(
    bg_fips,
    scenario, scenario_sub, 
    # ndvi_native_threshold, 
    ##not necessary for lookup. doesn't change the size of each bg. just affects what bgs are excluded/included
    area_mi2_bg_int_tx, area_mi2_bg_int_res ) %>% 
  arrange(bg_fips)
# 
# lookup_bg_fips_scenario_area %>% 
#   group_by(bg_fips,
#            scenario, scenario_sub, ndvi_native_threshold, area_mi2_bg_int_tx, area_mi2_bg_int_res) %>% 
#   summarise(n=n()) %>% 
#   View()
lookup_bg_fips_scenario_area
n_distinct(lookup_bg_fips_scenario_area$bg_fips)
nrow(lookup_bg_fips_scenario_area)
n_distinct(lookup_bg_fips_scenario_area$scenario)
n_distinct(lookup_bg_fips_scenario_area$scenario_sub)

11*3*531
# Summarize HIA------------


#Separate it in two. One for pop-related and one for area-related.
#I was having issues correctly calculating area in the same group-by-summarise call

#write a function for the summarise since it's used so often
summarise_ungroup_hia_pop_stuff_by_ndvi = function(df){
  df %>% 
    summarise(
      pop_affected = sum(pop_affected, na.rm=TRUE),
      attrib_d = sum(attrib_d, na.rm=TRUE),
      deaths_prevented = sum(deaths_prevented, na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      #this has to be in its own separate mutate by order of operations
      deaths_prevented_per_pop = deaths_prevented/pop_affected,
      deaths_prevented_per_pop_100k = deaths_prevented_per_pop*100000
    ) 
}

summarise_ungroup_hia_area_stuff_by_ndvi = function(df){
  df %>% 
    summarise(
      #recall, area_mi2_bg_int_res also works for the block-group-level ones
      area_mi2_bg_int_res = sum(area_mi2_bg_int_res, na.rm=TRUE), #residential buffer area
      area_mi2_bg_int_tx = sum(area_mi2_bg_int_tx, na.rm=TRUE), #treatment area; same for bg-level ones
      
      #sum these products before computing the weighted average below.
      #note these _int values are products of the block-group-level value and the corresponding area (weight)
      ndvi_mean_alt_int = sum(ndvi_mean_alt_int, na.rm=TRUE),
      ndvi_quo_int = sum(ndvi_quo_int, na.rm=TRUE),
      ndvi_mean_wt_tx_int = sum(ndvi_mean_wt_tx_int, na.rm=TRUE) #keep track of this for summary
    ) %>% 
    ungroup() %>% 
    mutate(
      #adding this 6/24/22 - what proportion of the residential area is
      #covered by the treatment area? I think this is useful to show how how dependent results are
      #on use of 500 m buffer
      area_prop_tx_res = area_mi2_bg_int_tx/area_mi2_bg_int_res,
      
      ndvi_mean_alt =ndvi_mean_alt_int/area_mi2_bg_int_res, #weighted mean
      ndvi_quo =ndvi_quo_int/area_mi2_bg_int_res, #weighted mean
      ndvi_diff = ndvi_mean_alt-ndvi_quo, #useful to keep
      ndvi_mean_wt_tx = ndvi_mean_wt_tx_int/area_mi2_bg_int_tx  #weighted average of NDVI over treatment area only. note diff denom
    ) 
}
  


## Stratify by NDVI definition--------
### by NDVI, over equity ------------
#Update 6/6/22 I have to separate this in two to fix the area calculations
table(hia_all$scenario)
table(hia_all$scenario_sub)
table(hia_all$ndvi_native_threshold)
hia_all_pop_stuff_by_ndvi_over_equity = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>%  
  group_by(scenario, scenario_sub, ndvi_native_threshold ) %>% 
  summarise_ungroup_hia_pop_stuff_by_ndvi()


hia_all_area_stuff_by_ndvi_over_equity = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>%  
  group_by(bg_fips, scenario, scenario_sub, ndvi_native_threshold) %>% 
  distinct(
    area_mi2_bg_int_res, area_mi2_bg_int_tx,
    ndvi_mean_alt_int, ndvi_quo_int, ndvi_mean_wt_tx_int ) %>% 
  group_by(scenario, scenario_sub, ndvi_native_threshold) %>% #now lose block group
  summarise_ungroup_hia_area_stuff_by_ndvi()#see definition above.


#check the parking area measurement is right.
hia_all_area_stuff_by_ndvi_over_equity %>% 
  filter(scenario == "prkng") %>% 
  filter(ndvi_native_threshold >.5) %>% 
  group_by(scenario_sub) %>% 
  summarise(area_mi2_bg_int_tx = sum(area_mi2_bg_int_tx)) #yes, it is good now.

#and the riparian areas?
hia_all_area_stuff_by_ndvi_over_equity %>% 
  filter(scenario == "riparian") %>% 
  filter(ndvi_native_threshold >.5) %>% 
  group_by(scenario_sub) %>% 
  summarise(area_mi2_bg_int_tx = sum(area_mi2_bg_int_tx)) #yes, it is good now.

hia_all_area_stuff_by_ndvi_over_equity
  
hia_all_by_ndvi_over_equity=hia_all_pop_stuff_by_ndvi_over_equity %>% 
  left_join(hia_all_area_stuff_by_ndvi_over_equity, by = c("scenario", "scenario_sub", "ndvi_native_threshold")) %>% 
  left_join(lookup_scenario_sort_order, by = c("scenario", "scenario_sub")) %>% 
  arrange(scenario_sort_order) %>% 
  dplyr::select(
    contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("equity"),
    starts_with("pop"), 
    starts_with("area"),
    starts_with("ndvi"), 
    starts_with("death"),
    everything())

hia_all_by_ndvi_over_equity
setwd(here("data-processed"))
save(hia_all_by_ndvi_over_equity, file = "hia_all_by_ndvi_over_equity.RData")


### by NDVI by equity-----------
table(hia_all$equity_nbhd_denver_tertile)
hia_all_pop_stuff_by_ndvi_by_equity = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>%  
  group_by(scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile ) %>% 
  summarise_ungroup_hia_pop_stuff_by_ndvi()


hia_all_area_stuff_by_ndvi_by_equity = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>%  
  group_by(bg_fips, scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile) %>% 
  distinct(
    area_mi2_bg_int_res, area_mi2_bg_int_tx,
    ndvi_mean_alt_int, ndvi_quo_int, ndvi_mean_wt_tx_int, equity_nbhd_denver_tertile) %>% 
  group_by(scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile) %>% #now lose block group
  summarise_ungroup_hia_area_stuff_by_ndvi()#see definition above.

#check the parking area measurement is right.
hia_all_area_stuff_by_ndvi_by_equity %>% 
  filter(scenario == "prkng") %>% 
  filter(ndvi_native_threshold >.5) %>% 
  group_by(scenario_sub,  equity_nbhd_denver_tertile) %>% 
  summarise(area_mi2_bg_int_tx = sum(area_mi2_bg_int_tx)) 

#and the riparian areas?
hia_all_area_stuff_by_ndvi_by_equity %>% 
  filter(scenario == "riparian") %>% 
  filter(ndvi_native_threshold >.5) %>% 
  group_by(scenario_sub, equity_nbhd_denver_tertile) %>% 
  summarise(area_mi2_bg_int_tx = sum(area_mi2_bg_int_tx))  


hia_all_by_ndvi_by_equity=hia_all_pop_stuff_by_ndvi_by_equity %>% 
  left_join(
    hia_all_area_stuff_by_ndvi_by_equity, 
    by = c("scenario", "scenario_sub", "ndvi_native_threshold", "equity_nbhd_denver_tertile")) %>% 
  left_join(lookup_scenario_sort_order, by = c("scenario", "scenario_sub")) %>% 
  arrange(scenario_sort_order) %>% 
  dplyr::select(
    contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("equity"),
    starts_with("pop"), 
    starts_with("area"),
    starts_with("ndvi"), 
    starts_with("death"),
    everything())

hia_all_by_ndvi_by_equity
setwd(here("data-processed"))
save(hia_all_by_ndvi_by_equity, file = "hia_all_by_ndvi_by_equity.RData")

### by block group by native def----------

load("lookup_den_metro_bg_geo.RData") #created 0_import_manage_denver_acs.R
hia_all_pop_stuff_by_ndvi_bg = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(bg_fips, scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile ) %>% 
  summarise_ungroup_hia_pop_stuff_by_ndvi()

hia_all_area_stuff_by_ndvi_bg = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>%  
  group_by(bg_fips, scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile) %>% 
  distinct(
    area_mi2_bg_int_res, area_mi2_bg_int_tx,
    ndvi_mean_alt_int, ndvi_quo_int, ndvi_mean_wt_tx_int, equity_nbhd_denver_tertile) %>% 
  group_by(bg_fips, scenario, scenario_sub, ndvi_native_threshold, equity_nbhd_denver_tertile) %>% #now lose block group
  summarise_ungroup_hia_area_stuff_by_ndvi()#see definition above.


hia_all_by_ndvi_bg = hia_all_pop_stuff_by_ndvi_bg %>% 
  left_join(
    hia_all_area_stuff_by_ndvi_bg, 
    by = c("bg_fips", "scenario", "scenario_sub", "ndvi_native_threshold")) %>% 
  left_join(lookup_scenario_sort_order, by = c("scenario", "scenario_sub")) %>% 
  arrange(scenario_sort_order) %>% 
  dplyr::select(
    contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("equity"),
    starts_with("pop"), 
    starts_with("area"),
    starts_with("ndvi"), 
    starts_with("death"),
    everything())
names(hia_all_by_ndvi_bg)

hia_all_by_ndvi_bg %>% 
  filter(scenario == "all-bg") %>% 
  filter(scenario_sub == "100-pct") %>% 
  filter(ndvi_native_threshold>.5) %>% 
  left_join(lookup_den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(zcol = "deaths_prevented_per_pop_100k")
  
### by NDVI by age group-----------
table(hia_all$age_group_acs)
hia_all_pop_stuff_by_ndvi_age = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>% 
  group_by( scenario, scenario_sub, ndvi_native_threshold, age_group_acs ) %>% 
  summarise_ungroup_hia_pop_stuff_by_ndvi()


hia_all_area_stuff_by_ndvi_age = hia_all %>% 
  filter(ndvi_below_native_threshold==1) %>%  
  group_by(bg_fips, scenario, scenario_sub, ndvi_native_threshold, age_group_acs) %>% 
  distinct(bg_fips,
    area_mi2_bg_int_res, area_mi2_bg_int_tx,
    ndvi_mean_alt_int, ndvi_quo_int, ndvi_mean_wt_tx_int, age_group_acs) %>% 
  group_by(age_group_acs, scenario, scenario_sub, ndvi_native_threshold ) %>% #now lose block group
  summarise_ungroup_hia_area_stuff_by_ndvi()#see definition above.


hia_all_by_ndvi_age = hia_all_pop_stuff_by_ndvi_age %>% 
  left_join(
    hia_all_area_stuff_by_ndvi_age, 
    by = c("age_group_acs", "scenario", "scenario_sub", "ndvi_native_threshold")) %>% 
  left_join(lookup_scenario_sort_order, by = c("scenario", "scenario_sub")) %>% 
  arrange(scenario_sort_order) %>% 
  dplyr::select(
    contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("equity"),
    starts_with("pop"), 
    starts_with("area"),
    starts_with("ndvi"), 
    starts_with("death"),
    everything())

hia_all_by_ndvi_age

## Summarize over NDVI definition---------
#What varies between NDVI definitions but not between subsequent bootstrap replications,
#which vary the risk ratio and the population?
#treatment area, residential buffer area, baseline NDVI, alternative NDVI
#what varies over subsequent bootstrap reps?
#get a median for all of them and 95% 100%
#we have to group by ndvi group first and then sum over it in the next step

names(hia_all_by_ndvi_over_equity)
#get point estimates. mean or median? probably median, I guess?
table(hia_all_by_ndvi_over_equity$ndvi_native_threshold)

skimr::skim(hia_all_by_ndvi_over_equity)
summarise_ungroup_hia_over_ndvi = function(df){
  df %>% 
    dplyr::summarise(
      pop_affected_med = median(pop_affected, na.rm=TRUE), #just median for pop
      attrib_d_med = median(attrib_d, na.rm=TRUE), #just median for attrib deaths
      #call this _count so it can be dynamically selected later.
      deaths_prevented_count_med = median(deaths_prevented, na.rm=TRUE), #same
      deaths_prevented_per_pop_med  = median(deaths_prevented_per_pop, na.rm=TRUE),#Same
      deaths_prevented_per_pop_100k_med = median(deaths_prevented_per_pop_100k, na.rm=TRUE),#Same
      #the rest vary over NDVI definition but will not vary over the other replications, 
      #so report the interval at this stage.
      area_mi2_bg_int_tx_med = median(area_mi2_bg_int_tx, na.rm=TRUE),
      area_mi2_bg_int_tx_min = min(area_mi2_bg_int_tx, na.rm=TRUE),
      area_mi2_bg_int_tx_max = max(area_mi2_bg_int_tx,  na.rm=TRUE),
      area_mi2_bg_int_res_med = median(area_mi2_bg_int_res, na.rm=TRUE),
      area_mi2_bg_int_res_min = min(area_mi2_bg_int_res, na.rm=TRUE),
      area_mi2_bg_int_res_max = max(area_mi2_bg_int_res, na.rm=TRUE),
      area_prop_tx_res_med = median(area_prop_tx_res, na.rm=TRUE), #adding 6/24/22
      area_prop_tx_res_min = min(area_prop_tx_res, na.rm=TRUE), #adding 6/24/22
      area_prop_tx_res_max = max(area_prop_tx_res, na.rm=TRUE), #adding 6/24/22
      ndvi_quo_med = median(ndvi_quo, na.rm=TRUE),
      ndvi_quo_min = min(ndvi_quo, na.rm=TRUE),
      ndvi_quo_max = max(ndvi_quo, na.rm=TRUE),
      ndvi_mean_wt_tx_med = median(ndvi_mean_wt_tx, na.rm=TRUE), #adding these to tables 6/6/22
      ndvi_mean_wt_tx_min = min(ndvi_mean_wt_tx, na.rm=TRUE), #adding these to tables 6/6/22
      ndvi_mean_wt_tx_max = max(ndvi_mean_wt_tx, na.rm=TRUE), #adding these to tables 6/6/22
      ndvi_mean_alt_med = median(ndvi_mean_alt, na.rm=TRUE),
      ndvi_mean_alt_min = min(ndvi_mean_alt, na.rm=TRUE),
      ndvi_mean_alt_max = max(ndvi_mean_alt, na.rm=TRUE),
      ndvi_diff_med = median(ndvi_diff, na.rm=TRUE),
      ndvi_diff_min = min(ndvi_diff, na.rm=TRUE),
      ndvi_diff_max = max(ndvi_diff, na.rm=TRUE)
    ) %>% 
    ungroup()
}

### over NDVI and over equity--------
hia_all_over_ndvi_over_equity = hia_all_by_ndvi_over_equity %>% #begin with this one, created just above.
  group_by(scenario, scenario_sub) %>% #collapse over ndvi category here
  summarise_ungroup_hia_over_ndvi() %>% 
  left_join(lookup_scenario_sort_order, by = c("scenario", "scenario_sub")) %>% 
  arrange(scenario_sort_order) %>% 
  dplyr::select(
    contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("equity"),
    starts_with("pop"), 
    starts_with("area"),
    starts_with("ndvi"), 
    starts_with("death"),
    everything())

hia_all_over_ndvi_over_equity
View(hia_all_over_ndvi_over_equity)
save(hia_all_over_ndvi_over_equity, file = "hia_all_over_ndvi_over_equity.RData")
### over NDVI by equity--------
hia_all_over_ndvi_by_equity = hia_all_by_ndvi_by_equity %>% 
  group_by(scenario, scenario_sub, equity_nbhd_denver_tertile) %>%   #collapse over ndvi category here
  summarise_ungroup_hia_over_ndvi() %>%   
  left_join(lookup_scenario_sort_order, by = c("scenario", "scenario_sub")) %>% 
  arrange(scenario_sort_order) %>% 
  dplyr::select(
    contains("scenario"), 
    starts_with("ndvi_native_threshold"),
    starts_with("equity"),
    starts_with("pop"), 
    starts_with("area"),
    starts_with("ndvi"), 
    starts_with("death"),
    everything())

hia_all_over_ndvi_by_equity
save(hia_all_over_ndvi_by_equity, file = "hia_all_over_ndvi_by_equity.RData")

