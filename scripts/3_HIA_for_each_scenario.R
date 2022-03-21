#filename: 3_HIA_for_each_scenario
library(tidyverse)
library(sf)
library(mapview)
library(here)
library(terra)
library(raster)
setwd(here("data-processed"))
#Revised 3/16/22

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
    layer.name = "NDVI",
    col.regions = pal_terrain, 
    at = seq(-0.4, 1, 0.1)
  )

load("den_co_tract_geo.RData") 
mv_den_co_tract_geo = den_co_tract_geo %>% 
  mapview(zcol = "tract_fips")

mv_ndvi_den_co_20210704 + mv_den_co_tract_geo

### tracts and block groups-----------
#Limit administrative boundaries data to the Denver area only
#This can be done aspatially using the county field :)
#these files excluding water are created here:
# ~/green-space-denver/scripts/2_ndvi_tract_bg_park_den.R
load("den_metro_tract_no_water_geo.RData")
names(den_metro_tract_no_water_geo)
den_co_tract_no_water_geo = den_metro_tract_no_water_geo %>% 
  filter(county_fips == "031") %>% 
  #per above, there are a few on this july 4 2021 
  #where the data are not complete. create a filter so you
  #can remove as needed. If you gather more NDVI data with a wider
  #bounding box, this may not be necessary
  #ideally, this would be coded more upstream, but we only
  #need it for this application, so okay here.
  mutate(
    tracts_north_east_no_ls8 = case_when( #no landsat-8 data
      tract_fips == "08031980000" ~ 1,
      tract_fips == "08031008388" ~1,
      tract_fips == "08031008389" ~1,
      tract_fips == "08031008390" ~1,
      tract_fips == "08031008391" ~1,
      TRUE ~0
    )
  )  %>% 
  filter(tracts_north_east_no_ls8==0)

den_co_tract_no_water_geo %>% mapview(zcol = "tracts_north_east_no_ls8")

den_co_tract_no_water_geo %>% mapview(zcol = "county_fips")

lookup_tracts_to_exclude = den_co_tract_geo %>% 
  mutate(
    tracts_north_east_no_ls8 = case_when( #no landsat-8 data
      tract_fips == "08031980000" ~ 1,
      tract_fips == "08031008388" ~1,
      tract_fips == "08031008389" ~1,
      tract_fips == "08031008390" ~1,
      tract_fips == "08031008391" ~1,
      TRUE ~0
    )
  )  %>% 
  distinct(tract_fips, tracts_north_east_no_ls8)

#make a unioned version of this without the holes and call it
#the study area for use in subsequent scenarios
#install.packages("nngeo") #to remove holes in sf objects
library(nngeo)
den_co_tract_geo %>% mapview()
study_area = den_co_tract_geo %>% 
  mutate(
    tracts_north_east_no_ls8 = case_when( #no landsat-8 data
      tract_fips == "08031980000" ~ 1,
      tract_fips == "08031008388" ~1,
      tract_fips == "08031008389" ~1,
      tract_fips == "08031008390" ~1,
      tract_fips == "08031008391" ~1,
      TRUE ~0
    )
  )  %>% 
  filter(tracts_north_east_no_ls8==0) %>% 
  st_simplify() %>% 
  st_union() %>% 
  st_remove_holes()
study_area %>% mapview()
save(study_area, file = "study_area.RData")
st_crs(study_area)
study_area_2876 = study_area %>% 
  st_transform(2876)
#created 
load("den_metro_bg_no_water_geo.RData") 
#note this excludes northeast tracts.
den_co_bg_no_water_geo = den_metro_bg_no_water_geo %>% 
  filter(county_fips == "031") %>% 
  #this already has tract-id
  mutate(
    tracts_north_east_no_ls8 = case_when(
      tract_fips == "08031980000" ~ 1,
      tract_fips == "08031008388" ~1,
      tract_fips == "08031008389" ~1,
      tract_fips == "08031008390" ~1,
      tract_fips == "08031008391" ~1,
      TRUE ~0
    )
  ) %>% 
  #remove tract here since I add it down there
  dplyr::select(-tract_fips) %>% 
  filter(tracts_north_east_no_ls8==0)
save(den_co_bg_no_water_geo, file = "den_co_bg_no_water_geo.RData")
den_co_bg_no_water_geo %>% mapview(zcol = "tracts_north_east_no_ls8")
#convert to 4326 because the raster is 4326.
#It failed silently when the crs wasn't the same.
den_co_bg_no_water_4326 = den_co_bg_no_water_geo %>% 
  st_transform(4326) %>% 
  #also create a row-number ID since extract automatically creates
  #an ID based on the row-number
  mutate(bg_id_row_number=row_number())

lookup_bg_id_row_number = den_co_bg_no_water_4326 %>% 
  distinct(bg_id_row_number, bg_fips)

### Load zoning data, because we will also exclude the airport zone and
# the industrial zone, per meeting with DRR 3/7
# source(here("scripts","0_read_wrangle_denver_land_use.R")) #this takes ~10 s


## Prep sex-by-age pop data & link with GBD rate data--------------
#Do here since it's used for all scenarios
#Note these are long-form
### Add dose-response function to GBD data----------
#source this script:
source( here("scripts","0_read_gbd_colorado.R"))  
names(ihme_co)
ihme_co_w_drf = ihme_co %>% 
  #add dose-response function here conditional on outcome.
  mutate(
    #From Rojas 2019
    #    Green spaces and mortality: a systematic review and 
    #meta-analysis of cohort studies
    #dose-response function (drf)
    drf_est = case_when(
      measure == "deaths" & cause_short == "all" ~ 0.96,
      TRUE ~ NA_real_),
    drf_ll = case_when(
      measure == "deaths" & cause_short == "all" ~ 0.94,
      TRUE ~ NA_real_),
    drf_ul = case_when(
      measure == "deaths" & cause_short == "all" ~ 0.97,
      TRUE ~ NA_real_),
    
    drf_measure = case_when(
      measure == "deaths" & cause_short == "all" ~  "risk-ratio",
      TRUE ~ NA_character_),
    #the amount of change in NDVI corresponding to the change in risk
    #measured by the risk ratio from the meta-analysis
    drf_increment = case_when( 
      measure == "deaths" & cause_short == "all" ~  .1,
      TRUE ~ NA_real_)
  )


#We created a spreadsheet to link the age groups here:
setwd(here("data-processed"))
lookup_acs_gbd_age = readxl::read_excel("lookup_acs_gbd_age.xlsx")
load("den_metro_bg_s_by_a_long_wrangle.RData") #s_by_a denotes sex by age

### Link GBD to bg data & restrict to certain age groups----------
#begin with the long-form metro data, restrict to denver only, link with GBD,
age_lowest_to_include=30
den_co_bg_s_by_a_gbd_long_wrangle = den_metro_bg_s_by_a_long_wrangle %>% 
  filter(county_fips == "031" ) %>% 
  left_join(lookup_acs_gbd_age, by = "age_group_acs") %>%  #link age groups for linking
  left_join(ihme_co_w_drf, #link to GBD data
            by = c("age_group_gbd", "sex")) %>% 
  #exclude the "all" ages and sexes. that is, filter them out
  filter(age_group_acs != "all") %>% 
  filter(sex != "all") %>% 
  #limit to adults 30 and older, as most studies from meta-analysis
  #can change if needed
  filter(age_group_acs_lb>=age_lowest_to_include) %>% 
  #also apply these filters. remove as we determine additional
  #dose-response functions 
  #(e.g., the Paul 2020; Urban green space and the risks of dementia and stroke)
  filter(measure == "deaths" & cause_short == "all") %>% 
  dplyr::select(-var_label) #drop this var
  

#1. Scenario 1: homogenous greening in each census block group---------
load("lookup_den_metro_bg_tract.RData")
## Extract NDVI in those block groups on that day--------
#define native threshold 
ndvi_native_threshold = .5
#update 3/16/22 removing the date from the filename. it gets too long,
#and it's in the columns.
den_co_bg_ndvi = ndvi_den_co_20210704 %>% 
  #see documentation. we need to take a weighted average based on area
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    #    exact=TRUE, #exact proportion covered. very slow code. don't use.
    y = terra::vect(den_co_bg_no_water_4326)) %>% 
  as_tibble() %>% 
  rename(
    ndvi = `20210704_NDVI`,
    bg_id_row_number = ID,
    wt_area = weight #the area weight, as a proportion
  ) %>% 
  mutate(
    ndvi_wt_int = ndvi*wt_area, #for use in the weighted average
    date = lubridate::as_date("20210704")
  ) %>% 
  left_join(den_co_bg_no_water_4326, by = "bg_id_row_number") %>% 
  group_by(date, bg_fips) %>% #date is redundant but it generalizes
  summarise(
    sum_of_wts = sum(wt_area, na.rm=TRUE),
    wt_area_mean = mean(wt_area, na.rm=TRUE),#curious
    ndvi_mean_no_wt = mean(ndvi, na.rm=TRUE), #as a check to make sure different.
    ndvi_wt_int = sum(ndvi_wt_int, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    ndvi_mean_wt =ndvi_wt_int /sum_of_wts, #weighted mean
    ndvi_below_native_threshold = case_when(
      ndvi_mean_wt < ndvi_native_threshold ~1,
      TRUE ~0
    ),
    #exposure difference: how much would NDVI increase if it went up to 5?
      #if the whole place went up that much?
      ndvi_alt_100 = ndvi_native_threshold ,#100 denoting 100%
      #if only 20% of the area went up that much?
      ndvi_alt_20 = ndvi_native_threshold*.2+.8*ndvi_mean_wt,
      ndvi_diff_100 = ndvi_alt_100-ndvi_mean_wt, #alternative minus baseline
      ndvi_diff_20 = ndvi_alt_20-ndvi_mean_wt
    ) %>%  
  left_join(den_co_bg_no_water_geo, by = "bg_fips") %>% 
  left_join(lookup_den_metro_bg_tract, by = "bg_fips") %>% #link tract ID
  st_as_sf() %>% 
  dplyr::select(contains("fips"), contains("ndvi"), everything())
  #above I call the alternate NDVI _alt, so use that syntax here as well


names(den_co_bg_ndvi)
den_co_bg_ndvi

## Visualize NDVI by census block group--------
pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
# Examine weighted NDVI by census block group
mv_den_co_bg_ndvi  = den_co_bg_ndvi %>% 
  mapview(
    layer.name = "ndvi (weighted)",
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain, 
    at = seq(-0.1, 1, 0.1)
  )
mv_den_co_bg_ndvi + mv_ndvi_den_co_20210704

n_distinct(den_co_bg_ndvi$bg_id_row_number)
nrow(den_co_bg_no_water_geo) #very good. so it's not dividing up the bgs further.

#Examine block groups above/below native threshold
den_co_bg_ndvi %>% 
  mapview(
    layer.name = "Below native threhsold",
    zcol = "ndvi_below_native_threshold",
    col.regions = rainbow(n=2)
  )

#where would gain the most, under the 20% scenario?
den_co_bg_ndvi %>% 
  mapview(
    layer.name = "ndvi_diff_20",
    zcol = "ndvi_diff_20"  )

summary(den_co_bg_ndvi$ndvi_mean_wt)
summary(den_co_bg_ndvi$ndvi_diff_20)
summary(den_co_bg_ndvi$ndvi_diff_100)

## Link NDVI data with population data & incidence data (from GBD)------

#look up the mean weighted ndvi by block group and the other
#ndvi-derived values, including the diffs
lookup_bg_ndvi = den_co_bg_ndvi %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(bg_fips, contains("ndvi")) %>% 
  distinct()
nrow(lookup_bg_ndvi)
nrow(den_co_bg_ndvi)
names(lookup_bg_ndvi)

## Link NDVI and estimate number of deaths prevented---------
table(ihme_co_w_drf$drf_increment)

den_co_bg_s_by_a_gbd_ndvi_long_wrangle = den_co_bg_s_by_a_gbd_long_wrangle %>% 
  left_join(lookup_bg_ndvi, by = "bg_fips") %>%  #link baseline ndvis
  mutate(
    #calculate the risk ratios based on the dose-response
    #function
    rr_alt_20 = drf_est**(ndvi_diff_20/drf_increment),
    rr_alt_100 = drf_est**(ndvi_diff_100/drf_increment),
    
    #population-attributable fraction
    paf_alt_20 =(rr_alt_20 -1)/rr_alt_20  ,
    paf_alt_100 =(rr_alt_100 -1)/rr_alt_100  ,
    
    #attributable deaths
    #note the rate is per 100,000 so first divide it by
    #100,000 before multiplying by the pop. in that age group.
    #we've already filtered out the marginal totals above, so this is
    #just the joint sex-by-age categories
    #ao = attributable outcomes, as this is a long-form dataset#
    #and presumably we will include some incidence data as well, s
    attrib_o_alt_20 = paf_alt_20*(rate_per_100k_est/100000)*pop,
    attrib_o_alt_100 = paf_alt_100*(rate_per_100k_est/100000)*pop
  )


## summarize long-form estimates----------
### deaths prevented by age----------
sc_1_deaths_prev_by_age_group = den_co_bg_long_w_ndvi_gbd_rates %>% 
  #limit to areas with baseline NDVI below the native threshold.
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(age_group_acs) %>% 
  summarise(
    attrib_o_alt_20 = sum(attrib_o_alt_20, na.rm=TRUE),
    attrib_o_alt_100 = sum(attrib_o_alt_100, na.rm=TRUE))
  
sc_1_deaths_prev_by_age_group
#save this to Excel for easier copy/paste
setwd(here("results"))
writexl::write_xlsx(
  sc_1_deaths_prev_by_age_group,
  "sc_1_deaths_prev_by_age_group.xlsx"
)


### deaths prevented by block group----------
sc_1_deaths_prev_by_BG = den_co_bg_long_w_ndvi_gbd_rates %>% 
  #limit to areas with baseline NDVI below the native threshold.
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(bg_fips) %>% 
  summarise(
    attrib_o_alt_20 = sum(attrib_o_alt_20, na.rm=TRUE),
    attrib_o_alt_100 = sum(attrib_o_alt_100, na.rm=TRUE))

sc_1_deaths_prev_by_BG
#visualize this
library(viridis)
sc_1_deaths_prev_by_BG %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_o_alt_20",
    col.regions = viridis_pal(direction=-1)
  )

### deaths prevented, overall----------
sc_1_deaths_prev_overall = den_co_bg_long_w_ndvi_gbd_rates %>% 
  #limit to areas with baseline NDVI below the native threshold.
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(ndvi_below_native_threshold) %>% 
  summarise(
    attrib_o_alt_20 = sum(attrib_o_alt_20, na.rm=TRUE),
    attrib_o_alt_100 = sum(attrib_o_alt_100, na.rm=TRUE))

sc_1_deaths_prev_overall 

# 2. Scenaro 2: waterways--------

## Create various buffers around the waterways from OSM-----
setwd(here("data-processed"))
load("den_co_geo.RData")
load("den_osm_water_poly_both.RData") #created ~scripts/0_load_denver_osm_parks_water.R
st_crs(den_osm_water_poly_both)
#restrict to Denver County
den_co_geo_2876 = den_co_geo %>% 
  st_transform(2876) #co for county 
den_co_geo_2876 %>% mapview()

names(den_osm_water_poly_both)
#meta-comment: I need to simplify the name and will lose some description. oh well.
#just remember that this includes both the original polygons and the lines that were
#lines but are now buffered lines, i.e., polygons
table(den_osm_water_poly_both$osm_name_pool_fountain)
table(den_osm_water_poly_both$water_type_pool_fountain)
den_co_osm_water = den_osm_water_poly_both %>% 
  st_intersection(den_co_geo_2876) %>% 
  #remove very small ponds, as they create unrealistic buffers
  #This varible created in 0_load_denver_osm_parks_water
  filter(area_ft2_lt_1500==0) %>% 
  #also exclude pools and fountains, either in the OSM name or water type
  #e.g., seal fountain and reflecting pool near civic pool will go away
  filter(osm_name_pool_fountain==0) %>% 
  filter(water_type_pool_fountain==0)
  
save(den_co_osm_water, file = "den_co_osm_water.RData")
den_co_osm_water %>% mapview(zcol = "water_type")
names(den_co_osm_water)

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

den_co_osm_water_union = den_co_osm_water %>% 
  st_union_dplyr_way()
save(den_co_osm_water_union, file = "den_co_osm_water_union.RData")

names(den_co_osm_water_union)

### 500 m buffer to represent residential exposure-------
#First, a simple 500 m buffer around the bodies of water,
#as this will represent the population exposed.
#alternatively, we could create 500 m buffers from the edge of the
#proposed intervention (e.g., 500 m from 200 ft), but that seems
#more confusing to me. it's possible there are people on the perimeter
#of such a buffer where the the greened zone would only represent say 
#1% of their exposed area.
den_co_osm_water %>% mapview()
den_co_osm_water_500m = den_co_osm_water %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
save(den_co_osm_water_500m, file = "den_co_osm_water_500m.RData")
den_co_osm_water_500m %>% mapview()
#union this as well so it's one geo
den_co_osm_water_500m_union = den_co_osm_water_500m %>% 
  st_union_dplyr_way()

den_co_osm_water_500m_union %>% mapview()
save(den_co_osm_water_500m_union, 
     file = "den_co_osm_water_500m_union.RData")

##Exclude bodies of water from buffer
#this is a little confusing, but we need this both for the buffers
#that we would be greening as well as the full 500 m buffer, as
#we will need to characterize the NDVI of both
#a version excluding the bodies of waters themselves around just
#the zone that we would be greening
st_crs(den_co_osm_water_500m_union)
st_crs(den_co_osm_water_union)
den_co_osm_water_500m_union_hole = den_co_osm_water_500m_union %>% 
  st_difference(den_co_osm_water_union)
den_co_osm_water_500m_union_hole %>% mapview()

### 200 feet (ideal)--------
den_co_osm_water_200ft = den_co_osm_water %>% 
  st_buffer(200)
save(den_co_osm_water_200ft, file = "den_co_osm_water_200ft.RData")

den_co_osm_water_200ft_union = den_co_osm_water_200ft %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_water_200ft_union_hole = den_co_osm_water_200ft_union %>% 
  st_difference(den_co_osm_water_union)
den_co_osm_water_200ft_union_hole %>% mapview()
save(den_co_osm_water_200ft_union_hole, file = "den_co_osm_water_200ft_union_hole.RData")


#Realization: ultimately we will be calculating a weighted average,
#where part of the exposure area in the 500 m will be greened and part wil
#not, so I need to measure the NDVI of the part that would never be greened
#in each scenario, i.e., the complement of the intervention area

#the 500 m buffer with 200 feet taken out of it.
den_co_osm_water_500m_diff_200ft = den_co_osm_water_500m_union %>% 
  st_difference(den_co_osm_water_200ft_union)
den_co_osm_water_500m_diff_200ft %>% mapview()

#500 m from the perimeter of the 200 foot buffer
#not currently using, but we may
den_co_osm_water_200ft_500m = den_co_osm_water_200ft %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
den_co_osm_water_200ft_500m %>% mapview()
save(den_co_osm_water_200ft_500m, file = "den_co_osm_water_200ft_500m.RData")

#union it for simpler spatial intersection
den_co_osm_water_200ft_500m_union = den_co_osm_water_200ft_500m %>% 
  st_union_dplyr_way()

den_co_osm_water_200ft_500m_union %>% mapview()
save(den_co_osm_water_200ft_500m_union, 
     file = "den_co_osm_water_200ft_500m_union.RData")


### 100 feet (realistic)---------
den_co_osm_water_100ft = den_co_osm_water %>% 
  st_buffer(100)
save(den_co_osm_water_100ft, file = "den_co_osm_water_100ft.RData")

den_co_osm_water_100ft_union = den_co_osm_water_100ft %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_water_100ft_union_hole = den_co_osm_water_100ft_union %>% 
  st_difference(den_co_osm_water_union)
den_co_osm_water_100ft_union_hole %>% mapview()
save(den_co_osm_water_100ft_union_hole, file = "den_co_osm_water_100ft_union_hole.RData")

#the 500 m buffer with 100 feet taken out of it.
den_co_osm_water_500m_diff_100ft = den_co_osm_water_500m_union %>% 
  st_difference(den_co_osm_water_100ft_union)
den_co_osm_water_500m_diff_100ft %>% mapview()

#500 m from the perimeter of the 100 foot buffer
#not currently using, but we may
den_co_osm_water_100ft_500m = den_co_osm_water_100ft %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
den_co_osm_water_100ft_500m %>% mapview()
save(den_co_osm_water_100ft_500m, file = "den_co_osm_water_100ft_500m.RData")

#union it for simpler spatial intersection
den_co_osm_water_100ft_500m_union = den_co_osm_water_100ft_500m %>% 
  st_union_dplyr_way()

den_co_osm_water_100ft_500m_union %>% mapview()
save(den_co_osm_water_100ft_500m_union, 
     file = "den_co_osm_water_100ft_500m_union.RData")

### 50 feet (very realistic)---------
den_co_osm_water_50ft = den_co_osm_water %>% 
  st_buffer(50)
save(den_co_osm_water_50ft, file = "den_co_osm_water_50ft.RData")

den_co_osm_water_50ft_union = den_co_osm_water_50ft %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_water_50ft_union_hole = den_co_osm_water_50ft_union %>% 
  st_difference(den_co_osm_water_union)
den_co_osm_water_50ft_union_hole %>% mapview()
save(den_co_osm_water_50ft_union_hole, file = "den_co_osm_water_50ft_union_hole.RData")

#the 500 m buffer with 50 feet taken out of it.
den_co_osm_water_500m_diff_50ft = den_co_osm_water_500m_union %>% 
  st_difference(den_co_osm_water_50ft_union)
den_co_osm_water_500m_diff_50ft %>% mapview()

#500 m from the perimeter of the 100 foot buffer
#not currently using, but we may
den_co_osm_water_50ft_500m = den_co_osm_water_50ft %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
den_co_osm_water_50ft_500m %>% mapview()
save(den_co_osm_water_50ft_500m, file = "den_co_osm_water_50ft_500m.RData")

#union it for simpler spatial intersection
den_co_osm_water_50ft_500m_union = den_co_osm_water_50ft_500m %>% 
  st_union_dplyr_way()

den_co_osm_water_50ft_500m_union %>% mapview()
save(den_co_osm_water_50ft_500m_union, 
     file = "den_co_osm_water_50ft_500m_union.RData")

## Measure NDVI of buffer summarized intersecting census block groups ------
#First, I'm going to intersect the buffers (with holes for water) 
#around the census block groups and then measure the NDVI in each intersecting chunk

#Best to start with the block-group geometry without water
#and intersect it with the full buffer (including the water; it will go away)
load("den_metro_bg_geo.RData") #from 0_import_manage_denver_acs.R
load("den_metro_bg_no_water_geo.RData")
st_crs(den_metro_bg_geo)

### Intersect block groups with buffers-------------
#Intersect block groups with both the complement buffers and the intervention buffers
#what proportion of the block group is covered by the intersection?
#multiply the resulting area covered by the pop. density 
#(assume uniform pop dens. in block group)
st_crs(den_co_osm_water_500m)
st_crs(den_bg_acs5_2019_wrangle_geo)
names(den_bg_acs5_2019_wrangle_geo)


#I do this for all of them, so make a function, as the code gets to be long
#otherwise
bg_int_wrangle_last_steps = function(df){
  df %>% 
    #exclude the tracts near airport that don't have NDVI data.
    #if we re-run the NDVI data, this part may not be necessary,
    #or we might choose to use a different exclusion
    left_join(lookup_tracts_to_exclude, by = "tract_fips") %>% 
    filter(tracts_north_east_no_ls8==0) %>% 
    #create a new row ID as you have elsewhere for this sort order.
    mutate(row_id_int = row_number()) %>% #make this the same for all of them
    st_transform(4326) %>%  # the NDVI raster file is in 4326
    dplyr::select(bg_fips, row_id_int, starts_with("area"))
}
#### The 500 m residential buffer-------
#We may not use this but good to have for reference

den_bg_int_water_500m = den_metro_bg_no_water_geo %>% 
  st_intersection(den_co_osm_water_500m_union) %>% #use union version
  mutate(
    #the piece of the block group overlapping the buffer
    area_ft2_500m = as.numeric(st_area(geometry)),
    area_mi2_500m = area_ft2_500m/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_water_500m %>% mapview(
  col.regions = rainbow(n_distinct(den_bg_int_water_500m$bg_fips)),
  zcol = "bg_fips")


#### Then the complement of the intervention areas within the 500 m buffer-----
#These will be used to calculate the weighted average NDVI where no intervention occurred
den_bg_int_water_500m_diff_200ft = den_metro_bg_no_water_geo %>% 
  st_intersection(den_co_osm_water_500m_diff_200ft) %>% 
  mutate(
    area_ft2_500m_diff_200ft = as.numeric(st_area(geometry)),
    area_mi2_500m_diff_200ft = area_ft2_500m_diff_200ft/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps() 

den_bg_int_water_500m_diff_100ft = den_metro_bg_no_water_geo %>% 
  st_intersection(den_co_osm_water_500m_diff_100ft) %>% #unioned version
  mutate(
    area_ft2_500m_diff_100ft = as.numeric(st_area(geometry)),
    area_mi2_500m_diff_100ft = area_ft2_500m_diff_100ft/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_water_500m_diff_50ft = den_metro_bg_no_water_geo %>% 
  st_intersection(den_co_osm_water_500m_diff_50ft) %>% #unioned version
  mutate(
    area_ft2_500m_diff_50ft = as.numeric(st_area(geometry)),
    area_mi2_500m_diff_50ft = area_ft2_500m_diff_50ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()

#### Then the intervention areas themselves---------
den_bg_int_water_200ft = den_metro_bg_no_water_geo %>% 
  st_intersection(den_co_osm_water_200ft_union) %>% #unioned version
  mutate(
    area_ft2_200ft = as.numeric(st_area(geometry)),
    area_mi2_200ft = area_ft2_200ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_water_100ft = den_metro_bg_no_water_geo %>% 
  #use the unary unioned version so you don't create overlapping pieces
  st_intersection(den_co_osm_water_100ft_union) %>% 
  mutate(
    area_ft2_100ft = as.numeric(st_area(geometry)),
    area_mi2_100ft = area_ft2_100ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_water_50ft = den_metro_bg_no_water_geo %>% 
  st_intersection(den_co_osm_water_50ft_union) %>% #unioned version
  mutate(
    area_ft2_50ft = as.numeric(st_area(geometry)),
    area_mi2_50ft = area_ft2_50ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()


## Measure NDVI on those intersected polygons--------
#consider making a function out of it since it will be very similar
#not putting the date in the filename. it's in the columns.

#Define some functions to reduce code
wrangle_ndvi_bg_1 = function(df){
  df %>% 
    rename(
      ndvi = `20210704_NDVI`,
      row_id_int = ID, #rename to the row id of the intersected block groups
      wt_area = weight #the area weight, as a proportion
    ) %>% 
    mutate(
      ndvi_wt_int = ndvi*wt_area, #for use in the weighted average
      date = lubridate::as_date("20210704")
    )  
}

wrangle_ndvi_bg_2 = function(df){
  df %>% 
  group_by(date, bg_fips) %>% #date is redundant but it generalizes
    summarise(
      #don't add the suffixes just yet to ease replicability.
      #eventually will add suffixes to specify which NDVI
      sum_of_wts = sum(wt_area, na.rm=TRUE),
      wt_area = mean(wt_area, na.rm=TRUE),#curious so keep track
      ndvi_mean_no_wt = mean(ndvi, na.rm=TRUE), #check to make sure differs from weigthed mean
      ndvi_wt_int = sum(ndvi_wt_int, na.rm=TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
      ndvi_mean_wt =ndvi_wt_int/sum_of_wts #weighted mean
    ) 
}
###  Measure NDVI on the 500 m residential buffer----------
#this will be the status quo scenario for the whole area.
st_crs(den_bg_int_water_500m)
den_bg_int_water_500m_ndvi = ndvi_den_co_20210704 %>% 
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    y = terra::vect(den_bg_int_water_500m)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_500m, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_500m, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m = wt_area
  ) %>% 
  mutate(
    #create an indicator for whether it's baseline was above the threshold
    ndvi_below_native_threshold = case_when(
      ndvi_mean_wt_500m  < ndvi_native_threshold ~1,
      TRUE ~0)
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area")
  )

den_bg_int_water_500m_ndvi
save(den_bg_int_water_500m_ndvi, 
     file = "den_bg_int_water_500m_ndvi.RData")
den_bg_int_water_500m_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m")
den_bg_int_water_500m_ndvi %>% 
  mapview(zcol = "ndvi_below_native_threshold")


### Measure NDVI on the complement polygons--------
#i.e., the sym_diff between a 500 m buffer and an x-ft buffer
#### 200 ft complement------
den_bg_int_water_500m_diff_200ft_ndvi = ndvi_den_co_20210704 %>% 
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    y = terra::vect(den_bg_int_water_500m_diff_200ft)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_500m_diff_200ft, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_500m_diff_200ft, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_diff_200ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_diff_200ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_diff_200ft = wt_area
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_water_500m_diff_200ft_ndvi
save(den_bg_int_water_500m_diff_200ft_ndvi, 
     file = "den_bg_int_water_500m_diff_200ft_ndvi.RData")
den_bg_int_water_500m_diff_200ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m_diff_200ft")

#### 100 ft complement------
den_bg_int_water_500m_diff_100ft_ndvi = ndvi_den_co_20210704 %>% 
  #see documentation. take a weighted average based on area
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    #    exact=TRUE, #exact proportion covered. very slow code. don't use.
    y = terra::vect(den_bg_int_water_500m_diff_100ft)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.  
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_500m_diff_100ft, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_500m_diff_100ft, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_diff_100ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_diff_100ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_diff_100ft = wt_area
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_water_500m_diff_100ft_ndvi
save(den_bg_int_water_500m_diff_100ft_ndvi, 
     file = "den_bg_int_water_500m_diff_100ft_ndvi.RData")
den_bg_int_water_500m_diff_100ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m_diff_100ft")

#### 50 ft complement------
den_bg_int_water_500m_diff_50ft_ndvi = ndvi_den_co_20210704 %>% 
  #see documentation. take a weighted average based on area
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    #    exact=TRUE, #exact proportion covered. very slow code. don't use.
    y = terra::vect(den_bg_int_water_500m_diff_50ft)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.  
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_500m_diff_50ft, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_500m_diff_50ft, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_diff_50ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_diff_50ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_diff_50ft = wt_area
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_water_500m_diff_50ft_ndvi
save(den_bg_int_water_500m_diff_50ft_ndvi, 
     file = "den_bg_int_water_500m_diff_50ft_ndvi.RData")
den_bg_int_water_500m_diff_50ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m_diff_50ft")

### Measure NDVI on the intervention polygons--------

#### 200 ft intervention------
den_bg_int_water_200ft_ndvi= ndvi_den_co_20210704 %>% 
  #see documentation. take a weighted average based on area
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    #    exact=TRUE, #exact proportion covered. very slow code. don't use.
    y = terra::vect(den_bg_int_water_200ft)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_200ft, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_200ft, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_200ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_200ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_200ft = wt_area
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_water_200ft_ndvi
save(den_bg_int_water_200ft_ndvi, 
     file = "den_bg_int_water_200ft_ndvi.RData")
den_bg_int_water_200ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_200ft")

#### 100 ft intervention------
den_bg_int_water_100ft_ndvi= ndvi_den_co_20210704 %>% 
  #see documentation. take a weighted average based on area
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    #    exact=TRUE, #exact proportion covered. very slow code. don't use.
    y = terra::vect(den_bg_int_water_100ft)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_100ft, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_100ft, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_100ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_100ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_100ft = wt_area
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_water_100ft_ndvi
save(den_bg_int_water_100ft_ndvi, 
     file = "den_bg_int_water_100ft_ndvi.RData")
den_bg_int_water_100ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_100ft")


#### 50 ft intervention------
den_bg_int_water_50ft_ndvi= ndvi_den_co_20210704 %>% 
  terra::extract( #weighted average based on area
    weights = TRUE, #approximate proportion of cell covered by polygon
    y = terra::vect(den_bg_int_water_50ft)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_50ft, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_50ft, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_50ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_50ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_50ft = wt_area
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_water_50ft_ndvi
save(den_bg_int_water_50ft_ndvi, 
     file = "den_bg_int_water_50ft_ndvi.RData")
den_bg_int_water_50ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_50ft")

##  Compute weighted average NDVI under each scenario--------
### Visualize the elements that will be averaged-----
#For example,
pal_terrain_col = rev(terrain.colors(100)) 
names(den_bg_int_water_500m_diff_200ft_ndvi)
mv_den_bg_int_water_500m_diff_200ft_ndvi= den_bg_int_water_500m_diff_200ft_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft - 500 m",
    zcol = "ndvi_mean_wt_500m_diff_200ft")

mv_den_bg_int_water_200ft_ndvi= den_bg_int_water_200ft_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft",
    zcol = "ndvi_mean_wt_200ft")

mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")
mv_den_bg_int_water_200ft_ndvi+
  mv_den_bg_int_water_500m_diff_200ft_ndvi +
  mv_den_co_osm_water


#Summarizing, we have the following objects which can be linked together
#Note: we need no-geo versions of all of them so they can be linked together.

#The full 500 m residential bufer
den_bg_int_water_500m_ndvi_nogeo = den_bg_int_water_500m_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

#The areas in the 500 m buffer that are the complement of the intervention areas.
den_bg_int_water_500m_diff_200ft_ndvi_nogeo =den_bg_int_water_500m_diff_200ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_water_500m_diff_100ft_ndvi_nogeo =den_bg_int_water_500m_diff_100ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_water_500m_diff_50ft_ndvi_nogeo =den_bg_int_water_500m_diff_50ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()


#The intervention areas
den_bg_int_water_200ft_ndvi_nogeo = den_bg_int_water_200ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_water_100ft_ndvi_nogeo = den_bg_int_water_100ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

den_bg_int_water_50ft_ndvi_nogeo = den_bg_int_water_50ft_ndvi %>%
  st_set_geometry(NULL) %>% 
  as_tibble()

### Link them together and set alternate NDVI values------------
#Begin with this, which is created above. Recall it has those NE restrictions.
load("lookup_bg_no_water_area.RData") #created 2_ndvi_tract_bg_park_den.R
den_co_bg_no_water_ndvi_geo = den_co_bg_no_water_geo %>% 
  left_join(lookup_bg_no_water_area, by = "bg_fips") %>% #link no-water area
  left_join(den_bg_int_water_500m_ndvi_nogeo, by = "bg_fips") %>% 
  #the complement buffer areas
  left_join(den_bg_int_water_500m_diff_200ft_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_water_500m_diff_100ft_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_water_500m_diff_50ft_ndvi_nogeo, by = "bg_fips") %>% 
  #the intervention areas
  left_join(den_bg_int_water_200ft_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_water_100ft_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_water_50ft_ndvi_nogeo, by = "bg_fips") %>% 
  #drop the wt_area_ variables, as it will get confusing. they are the average
  dplyr::select(-contains("wt_area_")) %>% 
  mutate(
    #value of the area-based weights when the NDVI was calculated in each area.
    #see above for definition
    #the areal proportion of the full 500 m buffer
    prop_area_200ft = area_mi2_200ft/area_mi2_500m,
    prop_area_100ft = area_mi2_100ft/area_mi2_500m,
    prop_area_50ft = area_mi2_50ft/area_mi2_500m,
    
    #the proportion of the complement. just do 1 minus.
    prop_area_comp_200ft=1-prop_area_200ft,
    prop_area_comp_100ft=1-prop_area_100ft,
    prop_area_comp_50ft=1-prop_area_50ft,
  ) %>% 
  #also note that we will be using the NDVI values weighted to account for the proportion
  #the pixel overlapped the area, so omit the unweighted means:
  dplyr::select(-contains("no_wt")) %>% 
  #intermediates
  #sort them so easier to see in mapview
  dplyr::select(contains("fips"), contains("area"), contains("ndvi"), everything()) %>% 
  ### Compute alternative NDVI under each scenario--------
  #what proportion of the 500 m buffer is in the riparian area buffers? we need
  #this to calculate a weighted average.
  mutate(
    #set the NDVI to the native value. note because we are now aspatial with respect
    #to the specific buffers, we can just use one alternate NDVI value
    ndvi_value_alt = ndvi_native_threshold, #currently set at 0.5
    #and then calculate a weighted average using this value,
    #the sym_diff NDVI values, and the areal proportions
    #above
    #this naming convention is...here's the weighted average NDVI for the whole
    #500 m area under the alternate
    #scenario where we green the 200 ft buffer 
    ndvi_mean_alt_500m_200ft = ndvi_mean_wt_500m_diff_200ft*prop_area_comp_200ft+
      prop_area_200ft*ndvi_value_alt,
    ndvi_mean_alt_500m_100ft = ndvi_mean_wt_500m_diff_100ft*prop_area_comp_100ft+
      prop_area_100ft*ndvi_value_alt,
    ndvi_mean_alt_500m_50ft = ndvi_mean_wt_500m_diff_50ft*prop_area_comp_50ft+
      prop_area_50ft*ndvi_value_alt,
    
    #now we can calculate the linear exposure difference between those values
    #and the baseline mean NDVI in the 500 m buffer. use ndvi_diff as done above
    ##alternative minus baseline
    ndvi_diff_200ft= ndvi_mean_alt_500m_200ft-ndvi_mean_wt_500m,
    ndvi_diff_100ft= ndvi_mean_alt_500m_100ft-ndvi_mean_wt_500m,
    ndvi_diff_50ft= ndvi_mean_alt_500m_50ft-ndvi_mean_wt_500m
  )

save(den_co_bg_no_water_ndvi_geo, file = "den_co_bg_no_water_ndvi_geo.RData")
names(den_co_bg_no_water_ndvi_geo)
den_co_bg_no_water_ndvi_geo %>% mapview(zcol = "ndvi_diff_200ft")
summary(den_co_bg_no_water_ndvi_geo$ndvi_diff_200ft)
summary(den_co_bg_no_water_ndvi_geo$ndvi_diff_100ft)
summary(den_co_bg_no_water_ndvi_geo$ndvi_diff_50ft)

#a look up for just the NDVI vars
lookup_bg_ndvi_riparian_buffs = den_co_bg_no_water_ndvi_geo %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(bg_fips, contains("ndvi")) %>% 
  distinct() %>% 
  as_tibble()

lookup_bg_ndvi_riparian_buffs

### Link with population data and estimate AF (sex by age)----------
#calculation population in each age group in each small area
#using the population density values in each age group and the
#area of each chunk

#look up just the area of the 500 m area from above
names(den_bg_int_water_500m)
lookup_bg_500m_area = den_bg_int_water_500m %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_500m) %>% 
  as_tibble()
  
lookup_bg_500m_area
#Begin with this, which we created above
names(den_co_bg_s_by_a_gbd_long_wrangle)
names(lookup_bg_ndvi_riparian_buffs)
#rip for riparian. this is the final long-form dataset for scenario 2.
den_co_bg_long_rip = den_co_bg_s_by_a_gbd_long_wrangle %>% 
  left_join(lookup_bg_500m_area, by = "bg_fips") %>% #link area of 500 m buffer
  left_join(lookup_bg_ndvi_riparian_buffs, by = "bg_fips") %>% #link NDVI data
  #because this is long form a simple multiplication will do to estimate
  #pop in each of these pieces in each age-sex group. recall, these areas
  #are estimated without the bodies of water there.
  mutate(
    pop_500m = area_mi2_500m*pop_dens_mi2,
    #calculate the risk ratios based on the dose-response
    #function
    rr_alt_200ft = drf_est**(ndvi_diff_200ft/drf_increment),
    rr_alt_100ft = drf_est**(ndvi_diff_100ft/drf_increment),
    rr_alt_50ft = drf_est**(ndvi_diff_50ft/drf_increment),

    #population-attributable fraction
    paf_alt_200ft =(rr_alt_200ft  -1)/rr_alt_200ft  ,
    paf_alt_100ft =(rr_alt_100ft -1)/rr_alt_100ft  ,
    paf_alt_50ft =(rr_alt_50ft -1)/rr_alt_50ft  ,
    
    #attributable deaths
    attrib_o_200ft = paf_alt_200ft*(rate_per_100k_est/100000)*pop_500m,
    attrib_o_100ft = paf_alt_100ft*(rate_per_100k_est/100000)*pop_500m,
    attrib_o_50ft = paf_alt_50ft*(rate_per_100k_est/100000)*pop_500m
  )


save(den_co_bg_long_rip, file = "den_co_bg_long_rip.RData")

## summarize long-form estimates----------
names(den_co_bg_long_rip)
### deaths prevented by age----------
sc_2_deaths_prev_by_age_group = den_co_bg_long_rip %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(age_group_acs) %>% 
  summarise(
    attrib_o_200ft = sum(attrib_o_200ft, na.rm=TRUE),
    attrib_o_100ft = sum(attrib_o_100ft, na.rm=TRUE),
    attrib_o_50ft = sum(attrib_o_50ft, na.rm=TRUE)
    ) %>% 
  ungroup()

sc_2_deaths_prev_by_age_group
#save this to Excel for easier copy/paste
setwd(here("results"))
writexl::write_xlsx(
  sc_1_deaths_prev_by_age_group,
  "sc_1_deaths_prev_by_age_group.xlsx"
)


### deaths prevented by block group----------
sc_2_deaths_prev_by_BG = den_co_bg_long_rip %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(bg_fips) %>% 
  summarise(
    attrib_o_200ft = sum(attrib_o_200ft, na.rm=TRUE),
    attrib_o_100ft = sum(attrib_o_100ft, na.rm=TRUE),
    attrib_o_50ft = sum(attrib_o_50ft, na.rm=TRUE)
  ) %>% 
  ungroup()

sc_2_deaths_prev_by_BG
#visualize this
library(viridis)
sc_2_deaths_prev_by_BG %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_o_200ft",
    col.regions = viridis_pal(direction=-1)
  )

### deaths prevented, overall----------
sc_2_deaths_prev_overall = den_co_bg_long_rip %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(ndvi_below_native_threshold) %>% 
  summarise(
    attrib_o_200ft = sum(attrib_o_200ft, na.rm=TRUE),
    attrib_o_100ft = sum(attrib_o_100ft, na.rm=TRUE),
    attrib_o_50ft = sum(attrib_o_50ft, na.rm=TRUE)
  ) %>% 
  ungroup()


sc_2_deaths_prev_overall

# Stormwater scenario--------

# Scenario 4: Parking -----------
## Prep parking buffers--------
#Parking data managd here: 0_read_denver_parking.R
setwd(here("data-processed"))
load("den_parking_500m.RData")
load("den_co_osm_water_union.RData")
den_parking_500m %>% mapview()
#Remove water from that buffer
den_parking_500m_no_water = den_parking_500m %>% 
  st_difference(den_co_osm_water_union)
den_parking_500m_no_water %>% mapview()

## Symmetric difference between parking lots and 500 m buffer
#Remove parking lots themselves from the buffer to compute the
#weighted average as we did for scenario 2
load("den_parking_sum_overall.RData")
#remove parking from the parking buffer
den_parking_500m_no_parking = den_parking_500m  %>% 
  st_difference(den_parking_sum_overall)


#remove water and parking from the parking buffer
den_parking_500m_no_water_no_parking = den_parking_500m_no_water %>% 
  st_difference(den_parking_sum_overall)

den_parking_500m_no_water_no_parking %>% mapview()

## Intersect these parking polygons with tracts--------
### With the full 500 m buffer, including the parking lots--------
den_bg_int_parking_500m = den_metro_bg_no_water_geo %>% #we already dropped water
  st_intersection(den_parking_500m) %>% 
  mutate(
    area_ft2_full_buff = as.numeric(st_area(geometry)),
    area_mi2_full_buff = area_ft2_full_buff /(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_parking_500m %>% mapview()

### Complement within the parking buffer---------
load("den_metro_bg_no_water_geo.RData")
den_metro_bg_no_water_geo

den_bg_int_parking_500m_no_parking = den_metro_bg_no_water_geo %>% #we already dropped water
  st_intersection(den_parking_500m_no_parking) %>% #use union version
  mutate(
    #the piece of the block group overlapping the buffer
    area_ft2_no_park = as.numeric(st_area(geometry)),
    area_mi2_no_park = area_ft2_no_park/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

save(den_bg_int_parking_500m_no_parking, file = "den_bg_int_parking_500m_no_parking.RData")
den_bg_int_parking_500m_no_parking %>% mapview()

### With just the parking lots themselves----------
den_bg_int_parking_only = den_metro_bg_no_water_geo %>% #we already dropped water
  st_intersection(den_parking_sum_overall) %>% #use union version
  mutate(
    area_ft2_park_only = as.numeric(st_area(geometry)),
    area_mi2_park_only = area_ft2_park_only/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()
save(den_bg_int_parking_only, file = "den_bg_int_parking_only.RData")
den_bg_int_parking_only %>% mapview()

## Measure NDVI on those intersected polygons--------
###  Measure NDVI on the full 500 m buffer around the parking lots,
#including the parking lots, but without water.

den_bg_int_parking_500m_ndvi = ndvi_den_co_20210704 %>% 
  terra::extract(
    weights = TRUE, #approximate proportion of cell covered by polygon
    y = terra::vect(den_bg_int_water_500m)) %>% 
  as_tibble() %>%  #code is slow so break it up here if needed.
  wrangle_ndvi_bg_1() %>% 
  left_join(den_bg_int_water_500m, by = "row_id_int") %>% #link once
  wrangle_ndvi_bg_2() %>% 
  left_join(den_bg_int_water_500m, by = "bg_fips") %>% #link in geo and area
  st_as_sf() %>% 
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m = wt_area
  ) %>% 
  mutate(
    #create an indicator for whether it's baseline was above the threshold
    ndvi_below_native_threshold = case_when(
      ndvi_mean_wt_500m  < ndvi_native_threshold ~1,
      TRUE ~0)
  ) %>% 
  #select only what is unique about this dataset. everything else can be linked.
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area")
  )


  