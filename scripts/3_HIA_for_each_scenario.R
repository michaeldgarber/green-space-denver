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
den_co_tract_no_wtr_geo = den_metro_tract_no_wtr_geo %>% 
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

den_co_tract_no_wtr_geo %>% mapview(zcol = "tracts_north_east_no_ls8")

den_co_tract_no_wtr_geo %>% mapview(zcol = "county_fips")

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

#save this, as it's used below
save(lookup_tracts_to_exclude, file = "lookup_tracts_to_exclude.RData")


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
save(study_area_2876, file = "study_area_2876.RData")
study_area_2876 %>% mapview()
#created 
load("den_metro_bg_no_wtr_geo.RData") 
#note this excludes northeast tracts.
den_co_bg_no_wtr_geo = den_metro_bg_no_wtr_geo %>% 
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
save(den_co_bg_no_wtr_geo, file = "den_co_bg_no_wtr_geo.RData")
den_co_bg_no_wtr_geo %>% mapview(zcol = "tracts_north_east_no_ls8")
#convert to 4326 because the raster is 4326.
#It failed silently when the crs wasn't the same.
den_co_bg_no_wtr_4326 = den_co_bg_no_wtr_geo %>% 
  st_transform(4326) %>% 
  #also create a row-number ID since extract automatically creates an id
  mutate(row_id_int=row_number()) #use row_id_int so it's consistent with the intersected pieces further below

lookup_row_id_int = den_co_bg_no_wtr_4326 %>% 
  distinct(row_id_int, bg_fips)

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
#### Define lower bound for age-----------
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
class(ndvi_den_co_20210704)
den_co_bg_no_wtr_4326 %>% mapview()

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

names(den_co_bg_no_wtr_geo)
names(den_co_bg_no_wtr_4326)
den_co_bg_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2 = den_co_bg_no_wtr_4326) %>% 
  mutate(
    #here, unlike further below, we can just continue the wrangling all in one step.
    #Below, it's broken up.
    #exposure difference: how much would NDVI increase if it went up to 5?
      #if the whole place went up that much?
      ndvi_alt_100 = ndvi_native_threshold ,#100 denoting 100%
      #if only 20% of the area went up that much?
      ndvi_alt_20 = .2*ndvi_native_threshold+.8*ndvi_mean_wt,
      ndvi_diff_100 = ndvi_alt_100-ndvi_mean_wt, #alternative minus baseline
      ndvi_diff_20 = ndvi_alt_20-ndvi_mean_wt
    ) %>%  
  dplyr::select(contains("fips"), contains("ndvi"), everything())


den_co_bg_ndvi %>% mapview(
  layer.name = "NDVI",
  zcol = "ndvi_mean_wt")

save(den_co_bg_ndvi, file = "den_co_bg_ndvi.RData")

## Visualize NDVI by census block group--------
pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
# Examine weighted NDVI by census block group
mv_den_co_bg_ndvi  = den_co_bg_ndvi %>% 
  dplyr::select(bg_fips, ndvi_mean_wt) %>% 
  mapview(
    layer.name = "NDVI, block group",
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain, 
    at = seq(-0.1, 1, 0.1)
  )
mv_den_co_bg_ndvi + mv_ndvi_den_co_20210704
mv_ndvi_pixel_bg = mv_den_co_bg_ndvi+mv_ndvi_den_co_20210704

#save for rendering elsewhere
save(mv_den_co_bg_ndvi, file = "mv_den_co_bg_ndvi.RData")
save(mv_ndvi_pixel_bg, file = "mv_ndvi_pixel_bg.RData")
save(mv_ndvi_den_co_20210704, file = "mv_ndvi_den_co_20210704.Rdata")

mv_den_co_bg_ndvi
mv_ndvi_pixel_bg@map %>% 
  addFullscreenControl()

n_distinct(den_co_bg_ndvi$row_id_int)
nrow(den_co_bg_no_wtr_geo) #very good. so it's not dividing up the bgs further.

#Examine block groups above/below native threshold
library(shades)
library(RColorBrewer)

RColorBrewer::brewer.pal(3, "RdYlGn")[c(1,3)]  %>%   swatch()
ndvi_below_thresh_pal = RColorBrewer::brewer.pal(3, "RdYlGn")[c(1,3)]  %>% rev()
ndvi_below_thresh_pal %>% swatch()
mv_bg_below_native_threshold =den_co_bg_ndvi %>% 
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
#save this for use in rmarkdown code
save(mv_bg_below_native_threshold, file = "mv_bg_below_native_threshold.RData")
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

## Link NDVI with GBD/age data and estimate number of deaths prevented---------
table(ihme_co_w_drf$drf_increment)
den_co_bg_s_by_a_gbd_long_wrangle
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
    attrib_d_alt_20 = paf_alt_20*(rate_per_100k_est/100000)*pop,
    attrib_d_alt_100 = paf_alt_100*(rate_per_100k_est/100000)*pop
  )


## summarize long-form estimates----------
### deaths prevented by age----------
#sc for scenario;
#DRR: use 18 and older
names(den_co_bg_s_by_a_gbd_ndvi_long_wrangle)
sc_all_bg_deaths_prev_by_age = den_co_bg_s_by_a_gbd_ndvi_long_wrangle %>% 
  #limit to areas with baseline NDVI below the native threshold.
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(age_group_acs) %>% 
  summarise(
    pop = sum(pop, na.rm=TRUE),
    attrib_d_alt_20 = sum(attrib_d_alt_20, na.rm=TRUE),
    attrib_d_alt_100 = sum(attrib_d_alt_100, na.rm=TRUE))
  
sc_all_bg_deaths_prev_by_age
#save this to Excel for easier copy/paste
setwd(here("results"))
writexl::write_xlsx(
  sc_all_bg_deaths_prev_by_age,
  "sc_all_bg_deaths_prev_by_age.xlsx"
)
setwd(here("data-processed"))


### deaths prevented by block group----------
sc_all_bg_deaths_prev_by_bg = den_co_bg_s_by_a_gbd_ndvi_long_wrangle %>% 
  #limit to areas with baseline NDVI below the native threshold.
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(bg_fips) %>% 
  summarise(
    pop = sum(pop, na.rm=TRUE),
    attrib_d_alt_20 = sum(attrib_d_alt_20, na.rm=TRUE),
    attrib_d_alt_100 = sum(attrib_d_alt_100, na.rm=TRUE)
  )

sc_all_bg_deaths_prev_by_bg
#visualize this
library(viridis)
setwd(here("data-processed"))
load("den_metro_bg_geo.RData")
mv_sc_all_bg_deaths_prev_by_bg=sc_all_bg_deaths_prev_by_bg %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_d_alt_20",
    col.regions = viridis_pal(direction=-1)
  )

mv_sc_all_bg_deaths_prev_by_bg_pp=sc_all_bg_deaths_prev_by_bg %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_d_alt_100",
    col.regions = viridis_pal(direction=-1)
  )

mv_sc_all_bg_deaths_prev_by_bg_pp

### deaths prevented, overall----------
#marg for marginal total
sc_all_bg_deaths_prev_marg = den_co_bg_s_by_a_gbd_ndvi_long_wrangle %>% 
  #limit to areas with baseline NDVI below the native threshold.
  filter(ndvi_below_native_threshold==1) %>% 
  group_by(ndvi_below_native_threshold) %>% 
  summarise(
    pop = sum(pop, na.rm=TRUE),
    attrib_d_alt_20 = sum(attrib_d_alt_20, na.rm=TRUE),
    attrib_d_alt_100 = sum(attrib_d_alt_100, na.rm=TRUE))

sc_all_bg_deaths_prev_marg

### Make all of those long-form-------
#write a function because same code x 3
pivot_longer_sc_all_bg = function(df){
  df %>% 
    mutate(scenario = "all-bg") %>% #all block groups
  pivot_longer(
    cols = starts_with("attrib"),
    names_to = "scenario_sub",
    values_to = "attrib_deaths"
  ) %>% 
    mutate(
      scenario_sub = case_when(
        scenario_sub == "attrib_d_alt_20" ~ "20-pct",
        scenario_sub == "attrib_d_alt_100" ~ "100-pct"
      )
    ) %>% 
    rename(pop_affected = pop)
}
#lf for long-form
sc_all_bg_deaths_prev_by_age_lf = sc_all_bg_deaths_prev_by_age %>% 
  pivot_longer_sc_all_bg()

sc_all_bg_deaths_prev_by_age_lf

sc_all_bg_deaths_prev_by_bg_lf =sc_all_bg_deaths_prev_by_bg %>% 
  pivot_longer_sc_all_bg()

sc_all_bg_deaths_prev_by_bg_lf
sc_all_bg_deaths_prev_marg_lf = sc_all_bg_deaths_prev_marg %>% 
  pivot_longer_sc_all_bg()

sc_all_bg_deaths_prev_marg_lf

# 2. Scenaro 2: waterways--------

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
#First, a simple 500 m buffer around the bodies of water,
#as this will represent the population exposed.
#alternatively, we could create 500 m buffers from the edge of the
#proposed intervention (e.g., 500 m from 200 ft), but that seems
#more confusing to me. it's possible there are people on the perimeter
#of such a buffer where the the greened zone would only represent say 
#1% of their exposed area.
den_co_osm_wtr %>% mapview()
den_co_osm_wtr_500m = den_co_osm_wtr %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
save(den_co_osm_wtr_500m, file = "den_co_osm_wtr_500m.RData")
den_co_osm_wtr_500m %>% mapview()
#union this as well so it's one geo
den_co_osm_wtr_500m_union = den_co_osm_wtr_500m %>% 
  st_union_dplyr_way()

den_co_osm_wtr_500m_union %>% mapview()
save(den_co_osm_wtr_500m_union, 
     file = "den_co_osm_wtr_500m_union.RData")

##Exclude bodies of water from buffer
#this is a little confusing, but we need this both for the buffers
#that we would be greening as well as the full 500 m buffer, as
#we will need to characterize the NDVI of both
#a version excluding the bodies of waters themselves around just
#the zone that we would be greening
st_crs(den_co_osm_wtr_500m_union)
st_crs(den_co_osm_wtr_union)
den_co_osm_wtr_500m_union_no_wtr = den_co_osm_wtr_500m_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_500m_union_no_wtr %>% mapview()

### 200 feet (ideal)--------
den_co_osm_wtr_200ft = den_co_osm_wtr %>% 
  st_buffer(200)
save(den_co_osm_wtr_200ft, file = "den_co_osm_wtr_200ft.RData")

den_co_osm_wtr_200ft_union = den_co_osm_wtr_200ft %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_wtr_200ft_union_no_wtr = den_co_osm_wtr_200ft_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_200ft_union_no_wtr %>% mapview()
save(den_co_osm_wtr_200ft_union_no_wtr, file = "den_co_osm_wtr_200ft_union_no_wtr.RData")


#Realization: ultimately we will be calculating a weighted average,
#where part of the exposure area in the 500 m will be greened and part wil
#not, so I need to measure the NDVI of the part that would never be greened
#in each scenario, i.e., the complement of the intervention area

#the 500 m buffer with 200 feet taken out of it.
den_co_osm_wtr_500m_200ft_comp = den_co_osm_wtr_500m_union %>% 
  st_difference(den_co_osm_wtr_200ft_union)
den_co_osm_wtr_500m_200ft_comp %>% mapview()

#500 m from the perimeter of the 200 foot buffer
#not currently using, but we may
den_co_osm_wtr_200ft_500m = den_co_osm_wtr_200ft %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
den_co_osm_wtr_200ft_500m %>% mapview()
save(den_co_osm_wtr_200ft_500m, file = "den_co_osm_wtr_200ft_500m.RData")

#union it for simpler spatial intersection
den_co_osm_wtr_200ft_500m_union = den_co_osm_wtr_200ft_500m %>% 
  st_union_dplyr_way()

den_co_osm_wtr_200ft_500m_union %>% mapview()
save(den_co_osm_wtr_200ft_500m_union, 
     file = "den_co_osm_wtr_200ft_500m_union.RData")


### 100 feet (realistic)---------
den_co_osm_wtr_100ft = den_co_osm_wtr %>% 
  st_buffer(100)
save(den_co_osm_wtr_100ft, file = "den_co_osm_wtr_100ft.RData")

den_co_osm_wtr_100ft_union = den_co_osm_wtr_100ft %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_wtr_100ft_union_no_wtr = den_co_osm_wtr_100ft_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_100ft_union_no_wtr %>% mapview()
save(den_co_osm_wtr_100ft_union_no_wtr, file = "den_co_osm_wtr_100ft_union_no_wtr.RData")

#the 500 m buffer with 100 feet taken out of it.
den_co_osm_wtr_500m_100ft_comp = den_co_osm_wtr_500m_union %>% 
  st_difference(den_co_osm_wtr_100ft_union)
den_co_osm_wtr_500m_100ft_comp %>% mapview()

#500 m from the perimeter of the 100 foot buffer
#not currently using, but we may
den_co_osm_wtr_100ft_500m = den_co_osm_wtr_100ft %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
den_co_osm_wtr_100ft_500m %>% mapview()
save(den_co_osm_wtr_100ft_500m, file = "den_co_osm_wtr_100ft_500m.RData")

#union it for simpler spatial intersection
den_co_osm_wtr_100ft_500m_union = den_co_osm_wtr_100ft_500m %>% 
  st_union_dplyr_way()

den_co_osm_wtr_100ft_500m_union %>% mapview()
save(den_co_osm_wtr_100ft_500m_union, 
     file = "den_co_osm_wtr_100ft_500m_union.RData")

### 50 feet (very realistic)---------
den_co_osm_wtr_50ft = den_co_osm_wtr %>% 
  st_buffer(50)
save(den_co_osm_wtr_50ft, file = "den_co_osm_wtr_50ft.RData")

den_co_osm_wtr_50ft_union = den_co_osm_wtr_50ft %>% 
  st_union_dplyr_way()

#remove the bodies of water
den_co_osm_wtr_50ft_union_no_wtr = den_co_osm_wtr_50ft_union %>% 
  st_difference(den_co_osm_wtr_union)
den_co_osm_wtr_50ft_union_no_wtr %>% mapview()
save(den_co_osm_wtr_50ft_union_no_wtr, file = "den_co_osm_wtr_50ft_union_no_wtr.RData")

#the 500 m buffer with 50 feet taken out of it.
den_co_osm_wtr_500m_50ft_comp = den_co_osm_wtr_500m_union %>% 
  st_difference(den_co_osm_wtr_50ft_union)
den_co_osm_wtr_500m_50ft_comp %>% mapview()

#500 m from the perimeter of the 100 foot buffer
#not currently using, but we may
den_co_osm_wtr_50ft_500m = den_co_osm_wtr_50ft %>% 
  st_buffer(500*3.28084) #500 meters, but we're in feet
den_co_osm_wtr_50ft_500m %>% mapview()
save(den_co_osm_wtr_50ft_500m, file = "den_co_osm_wtr_50ft_500m.RData")

#union it for simpler spatial intersection
den_co_osm_wtr_50ft_500m_union = den_co_osm_wtr_50ft_500m %>% 
  st_union_dplyr_way()

den_co_osm_wtr_50ft_500m_union %>% mapview()
save(den_co_osm_wtr_50ft_500m_union, 
     file = "den_co_osm_wtr_50ft_500m_union.RData")

## Measure NDVI of buffer summarized intersecting census block groups ------
#First, I'm going to intersect the buffers (with holes for water) 
#around the census block groups and then measure the NDVI in each intersecting chunk

#Best to start with the block-group geometry without water
#and intersect it with the full buffer (including the water; it will go away)
load("den_metro_bg_geo.RData") #from 0_import_manage_denver_acs.R
load("den_metro_bg_no_wtr_geo.RData")
st_crs(den_metro_bg_geo)

### Intersect block groups with buffers-------------
#Intersect block groups with both the complement buffers and the intervention buffers
#what proportion of the block group is covered by the intersection?
#multiply the resulting area covered by the pop. density 
#(assume uniform pop dens. in block group)
load("den_bg_acs5_2019_wrangle_geo.RData")
st_crs(den_co_osm_wtr_500m)
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
den_bg_int_wtr_500m = den_metro_bg_no_wtr_geo %>% 
  st_intersection(den_co_osm_wtr_500m_union) %>% #use union version
  mutate(
    #the piece of the block group overlapping the buffer
    area_ft2_bg_500m = as.numeric(st_area(geometry)),
    area_mi2_bg_500m = area_ft2_bg_500m/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_wtr_500m %>% mapview(
  col.regions = rainbow(n_distinct(den_bg_int_wtr_500m$bg_fips)),
  zcol = "bg_fips")


#### Then the complement of the intervention areas within the 500 m buffer-----
#These will be used to calculate the weighted average NDVI where no intervention occurred
den_bg_int_wtr_500m_200ft_comp = den_metro_bg_no_wtr_geo %>% 
  st_intersection(den_co_osm_wtr_500m_200ft_comp) %>% 
  mutate(
    area_ft2_bg_500m_200ft_comp = as.numeric(st_area(geometry)),
    area_mi2_bg_500m_200ft_comp = area_ft2_bg_500m_200ft_comp/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps() 

den_bg_int_wtr_500m_100ft_comp = den_metro_bg_no_wtr_geo %>% 
  st_intersection(den_co_osm_wtr_500m_100ft_comp) %>% #unioned version
  mutate(
    area_ft2_bg_500m_100ft_comp = as.numeric(st_area(geometry)),
    area_mi2_bg_500m_100ft_comp = area_ft2_bg_500m_100ft_comp/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_wtr_500m_50ft_comp = den_metro_bg_no_wtr_geo %>% 
  st_intersection(den_co_osm_wtr_500m_50ft_comp) %>% #unioned version
  mutate(
    area_ft2_bg_500m_50ft_comp = as.numeric(st_area(geometry)),
    area_mi2_bg_500m_50ft_comp = area_ft2_bg_500m_50ft_comp/(5280**2)
  ) %>% 
  bg_int_wrangle_last_steps()


#### Then the intervention areas themselves---------
den_bg_int_wtr_200ft = den_metro_bg_no_wtr_geo %>% 
  st_intersection(den_co_osm_wtr_200ft_union) %>% #unioned version
  mutate(
    area_ft2_bg_200ft = as.numeric(st_area(geometry)),
    area_mi2_bg_200ft = area_ft2_bg_200ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_wtr_100ft = den_metro_bg_no_wtr_geo %>% 
  #use the unary unioned version so you don't create overlapping pieces
  st_intersection(den_co_osm_wtr_100ft_union) %>% 
  mutate(
    area_ft2_bg_100ft = as.numeric(st_area(geometry)),
    area_mi2_bg_100ft = area_ft2_bg_100ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_wtr_50ft = den_metro_bg_no_wtr_geo %>% 
  st_intersection(den_co_osm_wtr_50ft_union) %>% #unioned version
  mutate(
    area_ft2_bg_50ft = as.numeric(st_area(geometry)),
    area_mi2_bg_50ft = area_ft2_bg_50ft*3.58701e-8
  ) %>% 
  bg_int_wrangle_last_steps()


## Measure NDVI on those intersected polygons--------
###  Measure NDVI on the 500 m residential buffer----------
#this will be the status quo scenario for the whole area.
st_crs(den_bg_int_wtr_500m)
den_bg_int_wtr_500m_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_500m) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), starts_with("ndvi_below"), contains("area")
  )


names(den_bg_int_wtr_500m_ndvi)
save(den_bg_int_wtr_500m_ndvi, 
     file = "den_bg_int_wtr_500m_ndvi.RData")
den_bg_int_wtr_500m_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m")
den_bg_int_wtr_500m_ndvi %>% 
  mapview(zcol = "ndvi_below_native_threshold")


### Measure NDVI on the complement polygons--------
#i.e., the sym_diff between a 500 m buffer and an x-ft buffer
#### 200 ft complement------
den_bg_int_wtr_500m_200ft_comp %>% mapview()
den_bg_int_wtr_500m_200ft_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_500m_200ft_comp) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_200ft_comp = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_200ft_comp = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_200ft_comp = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_wtr_500m_200ft_comp_ndvi
save(den_bg_int_wtr_500m_200ft_comp_ndvi, 
     file = "den_bg_int_wtr_500m_200ft_comp_ndvi.RData")
den_bg_int_wtr_500m_200ft_comp_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m_200ft_comp")

#### 100 ft complement------
den_bg_int_wtr_500m_100ft_comp %>% mapview()
den_bg_int_wtr_500m_100ft_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_500m_100ft_comp) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_100ft_comp = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_100ft_comp = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_100ft_comp = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_wtr_500m_100ft_comp_ndvi
save(den_bg_int_wtr_500m_100ft_comp_ndvi, 
     file = "den_bg_int_wtr_500m_100ft_comp_ndvi.RData")
den_bg_int_wtr_500m_100ft_comp_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m_100ft_comp")

#### 50 ft complement------
den_bg_int_wtr_500m_50ft_comp %>% mapview()
den_bg_int_wtr_500m_50ft_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_500m_50ft_comp) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_50ft_comp = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_50ft_comp = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_50ft_comp = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_wtr_500m_50ft_comp_ndvi
save(den_bg_int_wtr_500m_50ft_comp_ndvi, 
     file = "den_bg_int_wtr_500m_50ft_comp_ndvi.RData")
den_bg_int_wtr_500m_50ft_comp_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_500m_50ft_comp")

### Measure NDVI on the intervention polygons--------

#### 200 ft intervention------
den_bg_int_wtr_200ft %>% mapview()
den_bg_int_wtr_200ft_ndvi= ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_200ft) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_200ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_200ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_200ft = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_wtr_200ft_ndvi
save(den_bg_int_wtr_200ft_ndvi, 
     file = "den_bg_int_wtr_200ft_ndvi.RData")
den_bg_int_wtr_200ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_200ft")

#### 100 ft intervention------
den_bg_int_wtr_100ft %>% mapview()
den_bg_int_wtr_100ft_ndvi= ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_100ft) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_100ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_100ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_100ft = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_wtr_100ft_ndvi
save(den_bg_int_wtr_100ft_ndvi, 
     file = "den_bg_int_wtr_100ft_ndvi.RData")
den_bg_int_wtr_100ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_100ft")

#### 50 ft intervention------
den_bg_int_wtr_50ft %>% mapview()
den_bg_int_wtr_50ft_ndvi= ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_wtr_50ft) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_50ft = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_50ft = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_50ft = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )


den_bg_int_wtr_50ft_ndvi
save(den_bg_int_wtr_50ft_ndvi, 
     file = "den_bg_int_wtr_50ft_ndvi.RData")
den_bg_int_wtr_50ft_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt_50ft")

##  Compute weighted average NDVI under each scenario--------
### Visualize the elements that will be averaged-----
#save them as I will also present this in an rmarkdown doc
save(den_bg_int_wtr_500m_200ft_comp_ndvi, file = "den_bg_int_wtr_500m_200ft_comp_ndvi.RData")
save(den_bg_int_wtr_200ft_ndvi, file = "den_bg_int_wtr_200ft_ndvi.RData")
save(den_co_osm_wtr, file = "den_co_osm_wtr.RData")
#For example,
pal_terrain_col = rev(terrain.colors(100)) 
names(den_bg_int_wtr_500m_200ft_comp_ndvi)

mv_den_bg_int_wtr_500m_200ft_comp_ndvi= den_bg_int_wtr_500m_200ft_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft - 500 m",
    zcol = "ndvi_mean_wt_500m_200ft_comp")

mv_den_bg_int_wtr_200ft_ndvi= den_bg_int_wtr_200ft_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "NDVI, 200 ft",
    zcol = "ndvi_mean_wt_200ft")

mv_den_co_osm_wtr = den_co_osm_wtr %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_wtr$water_type)),
    zcol = "water_type")

#### Visualize all at once--------
mv_den_bg_int_wtr_200ft_ndvi+
  mv_den_bg_int_wtr_500m_200ft_comp_ndvi +
  mv_den_co_osm_wtr


#Summarizing, we have the following objects which can be linked together
### Remove geometry from NDVI datasets-----------
#Note: we need no-geo versions of all of them so they can be linked together.

#The full 500 m residential bufer
den_bg_int_wtr_500m_ndvi_nogeo = den_bg_int_wtr_500m_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

#The areas in the 500 m buffer that are the complement of the intervention areas.
den_bg_int_wtr_500m_200ft_comp_ndvi_nogeo =den_bg_int_wtr_500m_200ft_comp_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_wtr_500m_100ft_comp_ndvi_nogeo =den_bg_int_wtr_500m_100ft_comp_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_wtr_500m_50ft_comp_ndvi_nogeo =den_bg_int_wtr_500m_50ft_comp_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

#The intervention areas
den_bg_int_wtr_200ft_ndvi_nogeo = den_bg_int_wtr_200ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()
den_bg_int_wtr_100ft_ndvi_nogeo = den_bg_int_wtr_100ft_ndvi %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

den_bg_int_wtr_50ft_ndvi_nogeo = den_bg_int_wtr_50ft_ndvi %>%
  st_set_geometry(NULL) %>% 
  as_tibble()

### Link them together and set alternate NDVI values------------
#Begin with this, which is created above. Recall it has those NE restrictions.
load("lookup_bg_no_wtr_area.RData") #created 2_ndvi_tract_bg_park_den.R
#ripar for riparian; no_wtr to keep track that we've removed water from the geometry
names(den_bg_int_wtr_100ft_ndvi_nogeo)
names(den_bg_int_wtr_500m_100ft_comp_ndvi_nogeo)
names(lookup_bg_no_wtr_area)
names(den_bg_int_wtr_500m_ndvi_nogeo)
names(den_bg_int_wtr_50ft_ndvi_nogeo)
names(den_bg_int_wtr_200ft_ndvi_nogeo)
den_co_bg_no_wtr_rip_geo = den_co_bg_no_wtr_geo %>% 
  left_join(lookup_bg_no_wtr_area, by = "bg_fips") %>% #link no-water area
  left_join(den_bg_int_wtr_500m_ndvi_nogeo, by = "bg_fips") %>% 
  #the complement buffer areas
  left_join(den_bg_int_wtr_500m_200ft_comp_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_wtr_500m_100ft_comp_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_wtr_500m_50ft_comp_ndvi_nogeo, by = "bg_fips") %>% 
  #the intervention areas
  left_join(den_bg_int_wtr_200ft_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_wtr_100ft_ndvi_nogeo, by = "bg_fips") %>% 
  left_join(den_bg_int_wtr_50ft_ndvi_nogeo, by = "bg_fips") %>% 
  #drop the wt_area_ variables, as it will get confusing. they are the average
  dplyr::select(-contains("wt_area_")) %>% 
  mutate(
    #value of the area-based weights when the NDVI was calculated in each area.
    #see above for definition
    #the areal proportion of the full 500 m buffer
    prop_area_200ft = area_mi2_bg_200ft/area_mi2_bg_500m,
    prop_area_100ft = area_mi2_bg_100ft/area_mi2_bg_500m,
    prop_area_50ft = area_mi2_bg_50ft/area_mi2_bg_500m,
    
    #the proportion of the complement. just do 1 minus.
    prop_area_200ft_comp=1-prop_area_200ft,
    prop_area_100ft_comp=1-prop_area_100ft,
    prop_area_50ft_comp=1-prop_area_50ft,
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
    #the sym_diff NDVI values, and the areal proportions above
    #this naming convention is...here's the weighted average NDVI for the whole
    #500 m area under the alternate scenario
    #scenario where we green the 200 ft buffer 
    #Update 3/22/22 for parsimony, I'm removing the word "mean". We know it's a weighted mean.
    #I'm also removing the "500m" as we know it's within the 500 m buffer
    #3/22 update: set missings to status quo? no.

    ndvi_alt_200ft = ndvi_mean_wt_500m_200ft_comp*prop_area_200ft_comp+
      prop_area_200ft*ndvi_value_alt,
    ndvi_alt_100ft = ndvi_mean_wt_500m_100ft_comp*prop_area_100ft_comp+
      prop_area_100ft*ndvi_value_alt,
    ndvi_alt_50ft = ndvi_mean_wt_500m_50ft_comp*prop_area_50ft_comp+
      prop_area_50ft*ndvi_value_alt,
    
    #3/22: note! because of the imperfect resolution, the weighted average NDVI
    #over the whole area (ndvi_mean_wt_500m) does not necessarilly 
    #equal the weighted average in terms of the areal proportion.
    #conceptually, they should equal one another, but they might not.
    #that suggests we need to re-calculate
    #the weighted average at baseline and instead of using ndvi_mean_wt_500m for the status quo
    #calculate a new status-quo weighted average using the areal proportions
    #use quo for status quo; avoid sq because it could be squared
    ndvi_quo_200ft = ndvi_mean_wt_500m_200ft_comp*prop_area_200ft_comp+
      prop_area_200ft*ndvi_mean_wt_200ft,
    ndvi_quo_100ft = ndvi_mean_wt_500m_100ft_comp*prop_area_100ft_comp+
      prop_area_100ft*ndvi_mean_wt_100ft,
    ndvi_quo_50ft = ndvi_mean_wt_500m_50ft_comp*prop_area_50ft_comp+
      prop_area_50ft*ndvi_mean_wt_50ft,

    
    #now we can calculate the linear exposure difference between those values
    #and the baseline mean NDVI in the 500 m buffer. use ndvi_diff as done above
    ##alternative minus baseline
    ndvi_200ft_comp= ndvi_alt_200ft-ndvi_quo_200ft,
    ndvi_100ft_comp= ndvi_alt_100ft-ndvi_quo_200ft,
    ndvi_50ft_comp= ndvi_alt_50ft-ndvi_quo_200ft
  )

save(den_co_bg_no_wtr_rip_geo, file = "den_co_bg_no_wtr_rip_geo.RData")
names(den_co_bg_no_wtr_rip_geo)
den_co_bg_no_wtr_rip_geo %>% mapview(zcol = "ndvi_200ft_comp")
summary(den_co_bg_no_wtr_rip_geo$ndvi_200ft_comp)
summary(den_co_bg_no_wtr_rip_geo$ndvi_100ft_comp)
summary(den_co_bg_no_wtr_rip_geo$ndvi_50ft_comp)

#a look up for just the NDVI vars (riparian buffer = rip_buff)
lookup_bg_ndvi_rip_buff = den_co_bg_no_wtr_rip_geo %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(bg_fips, contains("ndvi")) %>% 
  distinct() %>% 
  as_tibble()

lookup_bg_ndvi_rip_buff

### Link with population data and estimate AF (sex by age)----------
#calculation population in each age group in each small area
#using the population density values in each age group and the
#area of each chunk

#look up just the area of the 500 m area from above
names(den_bg_int_wtr_500m)
lookup_bg_int_water_500m_area = den_bg_int_wtr_500m %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_500m) %>% 
  as_tibble()
  
lookup_bg_int_water_500m_area
#Begin with this, which we created above
names(den_co_bg_s_by_a_gbd_long_wrangle)
names(lookup_bg_ndvi_rip_buff)
#rip for riparian. this is the final long-form dataset for scenario 2.
den_co_bg_long_rip = den_co_bg_s_by_a_gbd_long_wrangle %>% 
  left_join(lookup_bg_int_water_500m_area, by = "bg_fips") %>% #link area of 500 m buffer
  left_join(lookup_bg_ndvi_rip_buff, by = "bg_fips") %>% #link NDVI data
  #because this is long form a simple multiplication will do to estimate
  #pop in each of these pieces in each age-sex group. recall, these areas
  #are estimated without the bodies of water there.
  mutate(
    pop_500m = area_mi2_bg_500m*pop_dens_mi2,
    #calculate the risk ratios based on the dose-response
    #function
    rr_alt_200ft = drf_est**(ndvi_200ft_comp/drf_increment),
    rr_alt_100ft = drf_est**(ndvi_100ft_comp/drf_increment),
    rr_alt_50ft = drf_est**(ndvi_50ft_comp/drf_increment),

    #population-attributable fraction
    paf_alt_200ft =(rr_alt_200ft  -1)/rr_alt_200ft  ,
    paf_alt_100ft =(rr_alt_100ft -1)/rr_alt_100ft  ,
    paf_alt_50ft =(rr_alt_50ft -1)/rr_alt_50ft  ,
    
    #attributable deaths
    attrib_d_200ft = paf_alt_200ft*(rate_per_100k_est/100000)*pop_500m,
    attrib_d_100ft = paf_alt_100ft*(rate_per_100k_est/100000)*pop_500m,
    attrib_d_50ft = paf_alt_50ft*(rate_per_100k_est/100000)*pop_500m
  )


save(den_co_bg_long_rip, file = "den_co_bg_long_rip.RData")

## summarize long-form estimates----------
names(den_co_bg_long_rip)
summary(den_co_bg_long_rip$pop_500m)
### deaths prevented by age----------

sc_rip_deaths_prev_by_age = den_co_bg_long_rip %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(age_group_acs) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_200ft = sum(attrib_d_200ft, na.rm=TRUE),
    attrib_d_100ft = sum(attrib_d_100ft, na.rm=TRUE),
    attrib_d_50ft = sum(attrib_d_50ft, na.rm=TRUE)
    ) %>% 
  ungroup()

sc_rip_deaths_prev_by_age
#save this to Excel for easier copy/paste
setwd(here("results"))
writexl::write_xlsx(
  sc_all_bg_deaths_prev_by_age,
  "sc_all_bg_deaths_prev_by_age.xlsx"
)
setwd(here("data-processed"))

### deaths prevented by block group----------
sc_rip_deaths_prev_by_bg = den_co_bg_long_rip %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(bg_fips) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_200ft = sum(attrib_d_200ft, na.rm=TRUE),
    attrib_d_100ft = sum(attrib_d_100ft, na.rm=TRUE),
    attrib_d_50ft = sum(attrib_d_50ft, na.rm=TRUE)
  ) %>% 
  ungroup()

sc_rip_deaths_prev_by_bg
#lf for long-form
sc_rip_deaths_prev_by_bg_lg = sc_rip_deaths_prev_by_bg %>% 
  pivot_longer(
    cols = starts_with("attrib"),
    names_to = "scenario_sub",
    values_to = "deaths_prev"
  )
sc_rip_deaths_prev_by_bg_lg

#visualize this
library(viridis)
load("den_metro_bg_geo.RData")
sc_rip_deaths_prev_by_bg %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_d_200ft",
    col.regions = viridis_pal(direction=-1)
  )

### deaths prevented, overall----------
#marg for marginal
sc_rip_deaths_prev_marg = den_co_bg_long_rip %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(ndvi_below_native_threshold) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_200ft = sum(attrib_d_200ft, na.rm=TRUE),
    attrib_d_100ft = sum(attrib_d_100ft, na.rm=TRUE),
    attrib_d_50ft = sum(attrib_d_50ft, na.rm=TRUE)
  ) %>% 
  ungroup()

sc_rip_deaths_prev_marg

### Make all of those long-form-------
#write a function because same code x 3
pivot_longer_sc_rip = function(df){
  df %>% 
    mutate(scenario = "riparian") %>% 
    pivot_longer(
      cols = starts_with("attrib"),
      names_to = "scenario_sub",
      values_to = "attrib_deaths"
    ) %>% 
    mutate(
      scenario_sub = case_when(
        scenario_sub == "attrib_d_200ft" ~ "200-ft",
        scenario_sub == "attrib_d_100ft" ~ "100-ft",
        scenario_sub == "attrib_d_50ft" ~ "50-ft",
      )
    ) %>% 
    rename(pop_affected = pop_500m)
}

sc_rip_deaths_prev_by_age_lf = sc_rip_deaths_prev_by_age %>% 
  pivot_longer_sc_rip()

sc_rip_deaths_prev_by_age_lf

sc_rip_deaths_prev_by_bg_lf =sc_rip_deaths_prev_by_bg %>% 
  pivot_longer_sc_rip()

sc_rip_deaths_prev_by_bg_lf
sc_rip_deaths_prev_marg_lf = sc_rip_deaths_prev_marg %>% 
  pivot_longer_sc_rip()

sc_rip_deaths_prev_marg_lf

# 3. Scenario 3. Stormwater management --------

## Per conversations with CB @ OGI, we consider 3 sub-scenarios.
#1. Regional projects
#2. OGI capital budget
#3. Stormwater regulations on new developments or re-developments.
#See this script (0_read_office_green_inf_data.R) for additional detail.


setwd(here("data-processed"))
load("ogi_proj.RData") 

## Prep buffers around regional OGI projects--------
# Data managed here: ~0_read_office_green_inf_data.R

### 500 m buffer around all projects--------
names(ogi_proj)
ogi_proj_marg  = ogi_proj %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>%
  summarise(
    area_ft2_ogi_proj = sum(area_ft2_ogi_proj),
    area_ac_ogi_proj  = sum(area_ac_ogi_proj),
    area_mi2_ogi_proj = sum(area_mi2_ogi_proj)  ,
  ) %>% 
  ungroup() %>% 
  st_simplify() %>% 
  st_as_sf() %>% 
  dplyr::select(-dummy)

ogi_proj_marg %>% mapview()
st_crs(ogi_proj_marg)
dist_500_m = 500*3.28084
#omit the _marg_ for the buffers. it gets to be too much nomenclature.
ogi_proj_500m = ogi_proj_marg %>% 
  st_union() %>% 
  st_buffer(dist_500_m) %>%  #500 meters, but we're in feet
  st_as_sf()

setwd(here("data-processed"))
save(ogi_proj_500m, file = "ogi_proj_500m.RData")
ogi_proj_500m %>% mapview()

### 500 m buffer by priority (short-term vs later); see docs--------
#updated with all 5 short-term priority projects
table(ogi_proj$short_term_proj)
ogi_proj %>% mapview(zcol = "short_term_proj")
ogi_proj_by_term  = ogi_proj %>% 
  group_by(short_term_proj) %>%
  summarise(
    area_ft2_ogi_proj = sum(area_ft2_ogi_proj),
    area_ac_ogi_proj  = sum(area_ac_ogi_proj),
    area_mi2_ogi_proj = sum(area_mi2_ogi_proj)  ,
  ) %>% 
  ungroup() %>% 
  st_simplify() %>% 
  st_as_sf() 

ogi_proj_by_term_500m = ogi_proj_by_term %>% 
  st_buffer(dist_500_m) %>%  #500 meters, but we're in feet
  st_as_sf() 

ogi_proj_by_term_500m %>% mapview(zcol = "short_term_proj")

#Remove water from those buffers
setwd(here("data-processed"))
load("den_co_osm_water_union.RData")
ogi_proj_500m_no_wtr = ogi_proj_500m %>%  
  st_difference(den_co_osm_water_union)
ogi_proj_500m_no_wtr %>% mapview()

#symmetric difference between project buffers and the projects themselves
#remove ogi projects from the ogi project buffer (i.e., the complement (comp))
ogi_proj_500m_comp = ogi_proj_500m  %>% 
  st_difference(ogi_proj_marg) %>% 
  st_as_sf()
ogi_proj_500m_comp %>% mapview(layer.name = "comp")

#remove water from the ogi-proj buffer
ogi_proj_500m_no_wtr_ogi_proj_comp = ogi_proj_500m_no_wtr %>% 
  st_difference(ogi_proj_marg)

ogi_proj_500m_no_wtr_ogi_proj_comp %>% 
  mapview(layer.name = "comp, no water")


## Intersect these polygons with tracts--------
### With the full 500 m buffer--------
#These polygons will serve as the baseline measurement.
load("den_metro_bg_no_wtr_geo.RData")
den_bg_int_ogi_proj_500m = den_metro_bg_no_wtr_geo %>% #we already dropped water
  st_intersection(ogi_proj_500m) %>% 
  mutate(
    #the full 500 m buffer 
    area_ft2_bg_500m = as.numeric(st_area(geometry)),
    area_mi2_bg_500m = area_ft2_bg_500m /(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

den_bg_int_ogi_proj_500m %>% mapview(zcol = "area_mi2_bg_500m")

### Complement within the buffer---------
#This polygon will be used to create a weighted average of the alternative scenario
st_crs(den_metro_bg_no_wtr_geo)
den_metro_bg_no_wtr_geo %>% mapview()
ogi_proj_500m_comp %>% mapview()
den_metro_bg_no_wtr_geo %>% mapview()
den_bg_int_ogi_proj_500m_comp = den_metro_bg_no_wtr_geo %>% #we already dropped water
  st_intersection(ogi_proj_500m_comp) %>% #use union version
  mutate(
    #the piece of the block group overlapping the buffer
    area_ft2_bg_ogi_proj_comp = as.numeric(st_area(geometry)),
    area_mi2_bg_ogi_proj_comp = area_ft2_bg_ogi_proj_comp/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

save(den_bg_int_ogi_proj_500m_comp, file = "den_bg_int_ogi_proj_500m_comp.RData")
den_bg_int_ogi_proj_500m_comp %>% mapview()

### Only the projects themselves----------
#Again, for the weighted average
ogi_proj_marg %>% mapview()
den_metro_bg_no_wtr_geo %>% mapview()
den_bg_int_ogi_proj_only = den_metro_bg_no_wtr_geo %>% #we already dropped water
  st_intersection(ogi_proj_marg) %>% #use union version
  dplyr::select(-contains("area")) %>%
  mutate(
    area_ft2_bg_ogi_proj_only = as.numeric(st_area(geometry)),
    area_mi2_bg_ogi_proj_only = area_ft2_bg_ogi_proj_only/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()
save(den_bg_int_ogi_proj_only, file = "den_bg_int_ogi_proj_only.RData")
names(den_bg_int_ogi_proj_only)
den_bg_int_ogi_proj_only %>% mapview(
  zcol = "bg_fips",
  col.regions = rainbow(n=n_distinct(den_bg_int_ogi_proj_only$bg_fips)))



### Ensure the area measurements are as you expect. -------
names(den_bg_int_ogi_proj_500m)
lookup_den_bg_int_ogi_proj_500m_area = den_bg_int_ogi_proj_500m %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_500m) %>% 
  as_tibble()


lookup_bg_no_wtr_area #from a different script
lookup_den_bg_int_ogi_proj_only_area = den_bg_int_ogi_proj_only %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_ogi_proj_only) %>% 
  as_tibble()

lookup_den_bg_int_ogi_proj_comp_area = den_bg_int_ogi_proj_500m_comp %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_ogi_proj_comp) %>% 
  as_tibble()

## Measure NDVI on those intersected polygons--------
###  Measure NDVI on the full 500 m buffer around the projects----------
#including the projects, but without water.
#use function again
names(den_bg_int_ogi_proj_500m)
den_bg_int_ogi_proj_500m_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_ogi_proj_500m) %>% #function created above.
  rename(    
    ndvi_mean_no_wt_500m = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area") #keep ndvi_below here
  )

names(den_bg_int_ogi_proj_500m_ndvi)
save(den_bg_int_ogi_proj_500m_ndvi, 
     file = "den_bg_int_ogi_proj_500m_ndvi.RData")
den_bg_int_ogi_proj_500m_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt_500m",
    col.regions = pal_terrain)
den_bg_int_ogi_proj_500m_ndvi %>% 
  mapview(
    zcol = "ndvi_below_native_threshold")


### Measure NDVI on the 500 m buffer excluding the projects-----------
#and excluding water.
names(den_bg_int_ogi_proj_500m_comp)
den_bg_int_ogi_proj_500m_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_ogi_proj_500m_comp) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_ogi_proj_comp = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_ogi_proj_comp = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_ogi_proj_comp = wt_area
  ) %>% 
  dplyr::select( #select unique aspects of this dataset; everything else can be linked.
    bg_fips, starts_with("ndvi_mean"),  contains("area") #keep ndvi_below here
  )

save(den_bg_int_ogi_proj_500m_comp_ndvi, 
     file = "den_bg_int_ogi_proj_500m_comp_ndvi.RData")
den_bg_int_ogi_proj_500m_comp_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt_500m_ogi_proj_comp",
    col.regions = pal_terrain)

### Measure NDVI on just the projects---------
den_bg_int_ogi_proj_only %>% mapview()
den_bg_int_ogi_proj_only_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_ogi_proj_only) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_ogi_proj_only = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_ogi_proj_only = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_ogi_proj_only = wt_area
  ) %>% 
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area")
  )

save(den_bg_int_ogi_proj_only_ndvi, 
     file = "den_bg_int_ogi_proj_only_ndvi.RData")
den_bg_int_ogi_proj_only_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt_ogi_proj_only",
    col.regions = pal_terrain)

##  Compute weighted average NDVI under each scenario--------
### Visualize the elements that will be averaged-----
#### Buffer excluding the project---------
names(den_bg_int_ogi_proj_500m_comp_ndvi)
mv_den_bg_int_ogi_proj_500m_comp_ndvi= den_bg_int_ogi_proj_500m_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "ndvi_mean_wt_500m_ogi_proj_comp",
    zcol = "ndvi_mean_wt_500m_ogi_proj_comp")

#### The project only----------
names(den_bg_int_ogi_proj_only_ndvi)
mv_den_bg_int_ogi_proj_only_ndvi= den_bg_int_ogi_proj_only_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "ndvi_mean_wt_ogi_proj_only",
    zcol = "ndvi_mean_wt_ogi_proj_only")

mv_den_bg_int_ogi_proj_only_ndvi
#### Bodies of water--------
load("den_co_osm_water_union.RData")
load("den_co_osm_water.RData")
mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")

#### Visualize all at once--------------
mv_den_bg_int_ogi_proj_500m_comp_ndvi+
  mv_den_bg_int_ogi_proj_only_ndvi +
  mv_den_co_osm_water

### Remove geometry from NDVI datasets-----------
den_bg_int_ogi_proj_500m_ndvi_nogeo = den_bg_int_ogi_proj_500m_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()
den_bg_int_ogi_proj_500m_comp_ndvi_nogeo = den_bg_int_ogi_proj_500m_comp_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

den_bg_int_ogi_proj_only_ndvi_nogeo = den_bg_int_ogi_proj_only_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

### Link them together and set alternate NDVI values------------
#Begin with this, which is created above. Recall it has those NE restrictions.
load("den_co_bg_no_wtr_geo.RData")
load("lookup_bg_no_wtr_area.RData") #created 2_ndvi_tract_bg_park_den.R
names(den_bg_int_ogi_proj_500m_ndvi_nogeo)
names(den_bg_int_ogi_proj_500m_comp_ndvi)
names(den_bg_int_ogi_proj_only_ndvi_nogeo)
den_co_bg_no_wtr_geo %>% mapview()
den_co_bg_no_wtr_ogi_proj_geo = den_co_bg_no_wtr_geo %>% 
  left_join(lookup_bg_no_wtr_area, by = "bg_fips") %>% #link no-water area
  left_join(den_bg_int_ogi_proj_500m_ndvi_nogeo, by = "bg_fips") %>%  #full 500 m buffer around project
  left_join(den_bg_int_ogi_proj_500m_comp_ndvi_nogeo, by = "bg_fips") %>% #buffer minus project 
  left_join(den_bg_int_ogi_proj_only_ndvi_nogeo, by = "bg_fips") %>% #project only only 
  dplyr::select(-contains("wt_area_")) %>% 
  mutate(
    prop_area_ogi_proj = area_mi2_bg_ogi_proj_only/area_mi2_bg_500m,
    prop_area_ogi_proj_comp=1-prop_area_ogi_proj  #proportion of complement. 1 minus
  ) %>% 

  dplyr::select(-contains("no_wt")) %>% #omit the mean without weight
  #intermediates
  #sort them so easier to see in mapview
  dplyr::select(contains("fips"), contains("area"), contains("ndvi"), everything()) %>% 
  ### Compute alternative NDVI under each scenario--------
#what proportion of the 500 m buffer is in the riparian area buffers? we need
#this to calculate a weighted average.
mutate(
  
  #Per CB, about 75% of the polygon would be native or adapted plants, so:
  ndvi_value_alt_75 = ndvi_native_threshold*.75+ndvi_mean_wt_ogi_proj_only*.25,

  #as in scenario 2, calculated weighted average;
  ndvi_alt_ogi_proj_75 = prop_area_ogi_proj_comp*ndvi_mean_wt_500m_ogi_proj_comp+
    prop_area_ogi_proj*ndvi_value_alt_75,

  #re-calculate the status quo using a weighted average, as per scenario 2
  #only need one, as the total area of each is not changing, as it does in scenario 2
  #how different is this from ndvi_mean_wt_500m?
  #3/25 here's my issue.
  ndvi_quo_ogi_proj = prop_area_ogi_proj_comp*ndvi_mean_wt_500m_ogi_proj_comp+
    prop_area_ogi_proj*ndvi_mean_wt_ogi_proj_only,
  
  #now we can calculate the linear exposure difference between those values
  #and the baseline mean NDVI in the 500 m buffer. use ndvi_diff as done above
  ##alternative minus baseline
  ndvi_diff_ogi_proj_75= ndvi_alt_ogi_proj_75-ndvi_quo_ogi_proj

)


#a look up for just the NDVI vars
lookup_bg_ndvi_ogi_proj = den_co_bg_no_wtr_ogi_proj_geo %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(bg_fips, contains("ndvi")) %>% 
  distinct() %>% 
  as_tibble()


### Link with population data and estimate AF (sex by age)----------
#calculation population in each age group in each small area
#using the population density values in each age group and the
#area of each chunk

#look up just the area of the 500 m area from above
names(den_bg_int_wtr_500m)
names(den_bg_int_ogi_proj_500m)
lookup_bg_int_ogi_proj_500m_area = den_bg_int_ogi_proj_500m %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_500m) %>% 
  as_tibble()
save(lookup_bg_int_ogi_proj_500m_area, file = "lookup_bg_int_ogi_proj_500m_area.RData")


names(den_co_bg_s_by_a_gbd_long_wrangle)
names(lookup_bg_ndvi_ogi_proj)
names(lookup_bg_int_ogi_proj_500m_area)
table(den_co_bg_s_by_a_gbd_ndvi_long_wrangle$age_group_acs)
den_co_bg_long_ogi_proj = den_co_bg_s_by_a_gbd_long_wrangle %>% 
  left_join(lookup_bg_int_ogi_proj_500m_area, by = "bg_fips") %>% #link area of 500 m buffer
  left_join(lookup_bg_ndvi_ogi_proj, by = "bg_fips") %>% #link NDVI data
  mutate(
    pop_500m = area_mi2_bg_500m*pop_dens_mi2,
    rr_alt_ogi_proj_75 = drf_est**(ndvi_diff_ogi_proj_75/drf_increment), #rr based on dose-resp function
    paf_alt_ogi_proj_75 =(rr_alt_ogi_proj_75 -1)/rr_alt_ogi_proj_75  , #paf
    attrib_d_ogi_proj_75 = paf_alt_ogi_proj_75*(rate_per_100k_est/100000)*pop_500m #attrib deaths
  )


save(den_co_bg_long_ogi_proj, file = "den_co_bg_long_ogi_proj.RData")

## summarize long-form estimates----------
names(den_co_bg_long_ogi_proj)
### deaths prevented by age----------
sc_ogi_proj_deaths_prev_by_age = den_co_bg_long_ogi_proj %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(age_group_acs) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_ogi_proj_75 = sum(attrib_d_ogi_proj_75, na.rm=TRUE)
  ) %>% 
  ungroup()

sc_ogi_proj_deaths_prev_by_age
#save this to Excel for easier copy/paste
setwd(here("results"))
writexl::write_xlsx(
  sc_all_bg_deaths_prev_by_age,
  "sc_all_bg_deaths_prev_by_age.xlsx"
)
setwd(here("data-processed"))

### deaths prevented by block group----------
load("den_bg_acs5_2019_wrangle_nogeo.RData")
den_bg_acs5_2019_wrangle_nogeo
sc_ogi_proj_deaths_prev_by_bg = den_co_bg_long_ogi_proj %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(bg_fips) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_ogi_proj_75 = sum(attrib_d_ogi_proj_75, na.rm=TRUE)
  ) %>% 
  ungroup() 

sc_ogi_proj_deaths_prev_by_bg
#visualize this
library(viridis)
load("den_metro_bg_geo.RData")
sc_ogi_proj_deaths_prev_by_bg %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_d_ogi_proj_75",
    col.regions = viridis_pal(direction=-1)
  )

### deaths prevented, overall----------
names(den_co_bg_long_ogi_proj)
sc_ogi_proj_deaths_prev_marg = den_co_bg_long_ogi_proj %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(ndvi_below_native_threshold) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_ogi_proj_75 = sum(attrib_d_ogi_proj_75, na.rm=TRUE)
  ) %>% 
  ungroup()

sc_ogi_proj_deaths_prev_marg

### Make all of those long-form-------
#write a function because same code x 3
pivot_longer_sc_ogi_proj = function(df){
  df %>% 
    mutate(scenario = "ogi_proj") %>% 
    pivot_longer(
      cols = starts_with("attrib"),
      names_to = "scenario_sub",
      values_to = "attrib_deaths"
    ) %>% 
    mutate(
      scenario_sub = case_when(
        scenario_sub == "attrib_d_ogi_proj_75" ~ "pct-ogi_proj-75",
      )
    ) %>% 
    rename(pop_affected = pop_500m)
}

#lf for long-form
sc_ogi_proj_deaths_prev_by_age_lf = sc_ogi_proj_deaths_prev_by_age %>% 
  pivot_longer_sc_ogi_proj()

sc_ogi_proj_deaths_prev_by_age_lf

sc_ogi_proj_deaths_prev_by_bg_lf =sc_ogi_proj_deaths_prev_by_bg %>% 
  pivot_longer_sc_ogi_proj()

sc_ogi_proj_deaths_prev_by_bg_lf

sc_ogi_proj_deaths_prev_marg_lf = sc_ogi_proj_deaths_prev_marg %>% 
  pivot_longer_sc_ogi_proj()

sc_ogi_proj_deaths_prev_marg_lf

## Other Office of Green Infrastructure scenarios -----------
### Green streets---------
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
load("den_osm_roads") #see for definitions
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

### Stormwater regulations---------
#How to operationalize? Randomly sample parcels based on size class
load("den_landuse_2018.RData")

# 4. Scenario 4: Parking -----------
## Prep parking buffers--------
#Parking data managed here: 0_read_denver_parking.R
setwd(here("data-processed"))
load("den_prkng_500m.RData") #Note rename from parking to prkng
load("den_prkng_sum_marg.RData") #I used to call this sum_union; marg for marginal is more clear to me
den_prkng_500m = den_prkng_500m %>% #for redundancy to be sure find + replace works.
  st_as_sf()
load("den_co_osm_wtr_union.RData")
load("den_metro_bg_no_wtr_geo.RData")
load("lookup_tracts_to_exclude.RData")
den_prkng_500m%>% mapview()
#Remove water from that buffer
den_prkng_500m_no_wtr = den_prkng_500m %>%  #prkng=parking; trying to reduce chars in object names
  st_difference(den_co_osm_water_union)
den_prkng_500m_no_wtr %>% mapview()

#remove parking from the parking buffer (i.e., the complement (comp))
den_prkng_500m_comp = den_prkng_500m  %>% 
  st_difference(den_prkng_sum_marg) %>% 
  st_as_sf()
den_prkng_500m_comp %>% mapview(layer.name = "comp")

#remove water and parking from the parking buffer
den_prkng_500m_no_wtr_prkng_comp = den_prkng_500m_no_wtr %>% 
  st_difference(den_prkng_sum_marg)

den_prkng_500m_no_wtr_prkng_comp %>% mapview(layer.name = "comp, no water")

## Intersect these parking polygons with tracts--------
### With the full 500 m buffer, including the parking lots--------
#These polygons will serve as the baseline measurement.
den_bg_int_prkng_500m = den_metro_bg_no_wtr_geo %>% #we already dropped water
  st_intersection(den_prkng_500m) %>% 
  mutate(
    #the full 500 m buffer, similar to the riparian interventions
    area_ft2_bg_500m = as.numeric(st_area(geometry)),
    area_mi2_bg_500m = area_ft2_bg_500m /(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

#this is basically just the block groups without water, since the buffer covers
#just about everything.
den_bg_int_prkng_500m %>% mapview(zcol = "area_mi2_bg_500m")

### Complement within the parking buffer---------
den_metro_bg_no_wtr_geo

#This polygon will be used to create a weighted average of the alternative scenario
st_crs(den_metro_bg_no_wtr_geo)
den_metro_bg_no_wtr_geo %>% mapview()
class(den_prkng_500m_comp)
den_prkng_500m_comp %>% mapview()
den_metro_bg_no_wtr_geo %>% mapview()
den_bg_int_prkng_500m_comp = den_metro_bg_no_wtr_geo %>% #we already dropped water
  st_intersection(den_prkng_500m_comp) %>% #use union version
  mutate(
    #the piece of the block group overlapping the buffer
    area_ft2_bg_prkng_comp = as.numeric(st_area(geometry)),
    area_mi2_bg_prkng_comp = area_ft2_bg_prkng_comp/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()

save(den_bg_int_prkng_500m_comp, file = "den_bg_int_prkng_500m_comp.RData")
den_bg_int_prkng_500m_comp %>% mapview()

### Only the parking lots themselves----------
#Again, for the weighted average
den_prkng_sum_marg %>% mapview()
den_metro_bg_no_wtr_geo %>% mapview()
den_bg_int_prkng_only = den_metro_bg_no_wtr_geo %>% #we already dropped water
  st_intersection(den_prkng_sum_marg) %>% #use union version
  #drop the existing area variables in den_prkng_sum_marg
  dplyr::select(-contains("area")) %>%
  #link in the block-group area measurements; out of curiosity, I want to know
  #how 
  mutate(
    area_ft2_bg_prkng_only = as.numeric(st_area(geometry)),
    area_mi2_bg_prkng_only = area_ft2_bg_prkng_only/(5280**2) #miles squared
  ) %>% 
  bg_int_wrangle_last_steps()
save(den_bg_int_prkng_only, file = "den_bg_int_prkng_only.RData")
den_bg_int_prkng_only %>% mapview()

### Ensure the area measurements are as you expect. -------
#That is, one adds up to the other.
names(den_bg_int_prkng_500m)
lookup_den_bg_int_prkng_500m_area = den_bg_int_prkng_500m %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_500m) %>% 
  as_tibble()

  
lookup_bg_no_wtr_area #from a different script
lookup_den_bg_int_prkng_only_area = den_bg_int_prkng_only %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_prkng_only) %>% 
  as_tibble()

lookup_den_bg_int_prkng_comp_area = den_bg_int_prkng_500m_comp %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_prkng_comp) %>% 
  as_tibble()

den_co_bg_no_wtr_geo
den_co_bg_no_wtr_area_confirm = den_co_bg_no_wtr_geo %>% 
  left_join(lookup_bg_no_wtr_area, by = "bg_fips") %>% 
  left_join(lookup_den_bg_int_prkng_500m_area, by = "bg_fips") %>% 
  left_join(lookup_den_bg_int_prkng_only_area, by = "bg_fips") %>% 
  left_join(lookup_den_bg_int_prkng_comp_area, by = "bg_fips") %>% 
  mutate(
    area_mi2_bg_500m_check =  area_mi2_bg_prkng_only + area_mi2_bg_prkng_comp,
    diff_area_mi2_bg_500m = area_mi2_bg_500m_check - area_mi2_bg_500m,
    #is there at least one parking lot in every census tract?
    bg_has_parking = case_when(
      area_mi2_bg_prkng_only> 0 ~1,
      TRUE ~0
    )
  ) 

summary(den_co_bg_no_wtr_area_confirm$diff_area_mi2_bg_500m) #good, zero
den_co_bg_no_wtr_area_confirm %>% mapview(zcol = "diff_area_mi2_bg_500m")
den_co_bg_no_wtr_area_confirm %>% mapview(zcol = "bg_has_parking")
## Measure NDVI on those intersected polygons--------
###  Measure NDVI on the full 500 m buffer around the parking lots----------
#including the parking lots, but without water.
names(den_bg_int_prkng_500m)
den_bg_int_prkng_500m_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_prkng_500m) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m = wt_area
  ) %>% 
  dplyr::select( #select what's unique about this df
    bg_fips, starts_with("ndvi_mean"), contains("ndvi_below"), contains("area") #note include ndvi_below here
  )

save(den_bg_int_prkng_500m_ndvi, 
     file = "den_bg_int_prkng_500m_ndvi.RData")
den_bg_int_prkng_500m_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt_500m",
    col.regions = pal_terrain)

### Measure NDVI on the 500 m buffer excluding the parking lots-----------
#and excluding water.
names(den_bg_int_prkng_500m_comp)
den_bg_int_prkng_500m_comp_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_prkng_500m_comp) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_500m_prkng_comp = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_500m_prkng_comp = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_500m_prkng_comp = wt_area
  ) %>% 
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area") #no ndvi below here
  )

save(den_bg_int_prkng_500m_comp_ndvi, 
     file = "den_bg_int_prkng_500m_comp_ndvi.RData")
den_bg_int_prkng_500m_comp_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt_500m_prkng_comp",
    col.regions = pal_terrain)

### Measure NDVI on just the parking lots---------
den_bg_int_prkng_only %>% mapview()
den_bg_int_prkng_only_ndvi = ndvi_den_co_20210704 %>% 
  extract_wrangle_ndvi_bg_int(df2=den_bg_int_prkng_only) %>% #function created above.
  rename(   #rename ndvi values in prep for a wide-form dataset by block group
    ndvi_mean_no_wt_prkng_only = ndvi_mean_no_wt, #mean agnostic to pixels covering water
    ndvi_mean_wt_prkng_only = ndvi_mean_wt, #weighted to account for pixels over water
    wt_area_prkng_only = wt_area
  ) %>% 
  dplyr::select(
    bg_fips, starts_with("ndvi_mean"), contains("area") #no ndvi below here
  )

save(den_bg_int_prkng_only_ndvi, 
     file = "den_bg_int_prkng_only_ndvi.RData")
den_bg_int_prkng_only_ndvi %>% 
  mapview(
    zcol = "ndvi_mean_wt_prkng_only",
    col.regions = pal_terrain)

##  Compute weighted average NDVI under each scenario--------
### Visualize the elements that will be averaged-----
#### Buffer excluding the parking---------
names(den_bg_int_prkng_500m_comp_ndvi)
mv_den_bg_int_prkng_500m_comp_ndvi= den_bg_int_prkng_500m_comp_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "ndvi_mean_wt_500m_prkng_comp",
    zcol = "ndvi_mean_wt_500m_prkng_comp")

#### The parking only----------
names(den_bg_int_prkng_only_ndvi)
mv_den_bg_int_prkng_only_ndvi= den_bg_int_prkng_only_ndvi %>% 
  mapview(
    col.regions = pal_terrain_col,
    at = seq(-.1, 1, 0.1), #0 to 1 by .1; define the breaks so consistent between layers
    layer.name = "ndvi_mean_wt_prkng_only",
    zcol = "ndvi_mean_wt_prkng_only")

mv_den_bg_int_prkng_only_ndvi
#### Bodies of water--------
load("den_co_osm_water_union.RData")
load("den_co_osm_water.RData")
mv_den_co_osm_water = den_co_osm_water %>% 
  st_intersection(study_area_2876) %>% 
  mapview(
    layer.name = "Water type",
    col.regions = rainbow(n_distinct(den_co_osm_water$water_type)),
    zcol = "water_type")

#### Visualize all at once--------------
mv_den_bg_int_prkng_500m_comp_ndvi+
  mv_den_bg_int_prkng_only_ndvi +
  mv_den_co_osm_water

### Remove geometry from NDVI datasets-----------
den_bg_int_prkng_500m_ndvi_nogeo = den_bg_int_prkng_500m_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()
den_bg_int_prkng_500m_comp_ndvi_nogeo = den_bg_int_prkng_500m_comp_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

den_bg_int_prkng_only_ndvi_nogeo = den_bg_int_prkng_only_ndvi %>% 
  st_set_geometry(NULL) %>%
  as_tibble()

### Link them together and set alternate NDVI values------------
#Begin with this, which is created above. Recall it has those NE restrictions.
load("den_co_bg_no_wtr_geo.RData")
load("lookup_bg_no_wtr_area.RData") #created 2_ndvi_tract_bg_park_den.R
names(den_bg_int_prkng_500m_ndvi_nogeo)
names(den_bg_int_prkng_500m_comp_ndvi)
names(den_bg_int_prkng_only_ndvi_nogeo)
den_co_bg_no_wtr_geo %>% mapview()
den_co_bg_no_wtr_prkng_geo = den_co_bg_no_wtr_geo %>% 
  left_join(lookup_bg_no_wtr_area, by = "bg_fips") %>% #link no-water area
  left_join(den_bg_int_prkng_500m_ndvi_nogeo, by = "bg_fips") %>%  #full 500 m buffer around parking
  left_join(den_bg_int_prkng_500m_comp_ndvi_nogeo, by = "bg_fips") %>% #buffer minus parking
  left_join(den_bg_int_prkng_only_ndvi_nogeo, by = "bg_fips") %>% #parking only 
  #drop the wt_area_ variables, as it will get confusing. they are the average
  dplyr::select(-contains("wt_area_")) %>% 
  mutate(
    # the proportion of the area covered by parking by census block group
    prop_area_prkng = area_mi2_bg_prkng_only/area_mi2_bg_500m,
    prop_area_prkng_comp=1-prop_area_prkng  #proportion of complement. 1 minus
  ) %>% 
  #also note that we will be using the NDVI values weighted to account for the proportion
  #the pixel overlapped the area, so omit the unweighted means:
  dplyr::select(-contains("no_wt")) %>% #omit the mean without weight
  #intermediates
  #sort them so easier to see in mapview
  dplyr::select(contains("fips"), contains("area"), contains("ndvi"), everything()) %>% 
  ### Compute alternative NDVI under each scenario--------
  #what proportion of the 500 m buffer is in the riparian area buffers? we need
  #this to calculate a weighted average.
  mutate(
    
    #What if we convert 100% of parking lots to native green levels?
    #50%?
    #20%?
    ndvi_value_alt_100 = ndvi_native_threshold*1+ndvi_mean_wt_prkng_only*0,
    ndvi_value_alt_50 = ndvi_native_threshold*.5+ndvi_mean_wt_prkng_only*.5,
    ndvi_value_alt_20 = ndvi_native_threshold*.2+ndvi_mean_wt_prkng_only*.8,
    
    #as in scenario 2, this is a weighted average, assuming we converted x%
    #of parking to green.
      #use case-when syntax to set missings to their original value
    ndvi_alt_prkng_100 = prop_area_prkng_comp*ndvi_mean_wt_500m_prkng_comp+
      prop_area_prkng*ndvi_value_alt_100,
    
    ndvi_alt_prkng_50 = prop_area_prkng_comp*ndvi_mean_wt_500m_prkng_comp+
      prop_area_prkng*ndvi_value_alt_50,
    
    ndvi_alt_prkng_20 = prop_area_prkng_comp*ndvi_mean_wt_500m_prkng_comp+
      prop_area_prkng*ndvi_value_alt_20,
    
    #re-calculate the status quo using a weighted average, as per scenario 2
    #only need one, as the total area of each is not changing, as it does in scenario 2
    #how different is this from ndvi_mean_wt_500m?
    #3/25 here's my issue.
    ndvi_quo_prkng = prop_area_prkng_comp*ndvi_mean_wt_500m_prkng_comp+
      prop_area_prkng*ndvi_mean_wt_prkng_only,
    
    
    #now we can calculate the linear exposure difference between those values
    #and the baseline mean NDVI in the 500 m buffer. use ndvi_diff as done above
    ##alternative minus baseline
    ndvi_diff_prkng_100= ndvi_alt_prkng_100-ndvi_quo_prkng,
    ndvi_diff_prkng_50= ndvi_alt_prkng_50-ndvi_quo_prkng,
    ndvi_diff_prkng_20= ndvi_alt_prkng_20-ndvi_quo_prkng

  )
save(den_co_bg_no_wtr_prkng_geo, file = "den_co_bg_prkng_geo.RData")
names(den_co_bg_no_wtr_prkng_geo)
den_co_bg_no_wtr_prkng_geo %>% mapview(zcol = "ndvi_diff_prkng_100")
den_co_bg_no_wtr_prkng_geo %>% mapview(zcol = "ndvi_diff_prkng_50")
den_co_bg_no_wtr_prkng_geo %>% mapview(zcol = "ndvi_diff_prkng_20")
den_co_bg_no_wtr_prkng_geo %>% mapview(zcol = "prop_area_prkng")
den_co_bg_no_wtr_prkng_geo %>% mapview(zcol = "ndvi_below_native_threshold")

#a look up for just the NDVI vars
lookup_bg_ndvi_prkng = den_co_bg_no_wtr_prkng_geo %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(bg_fips, contains("ndvi")) %>% 
  distinct() %>% 
  as_tibble()


### Link with population data and estimate AF (sex by age)----------
#calculation population in each age group in each small area
#using the population density values in each age group and the
#area of each chunk

#look up just the area of the 500 m area from above
names(den_bg_int_wtr_500m)
names(den_bg_int_prkng_500m)
lookup_bg_int_prkng_500m_area = den_bg_int_prkng_500m %>% 
  st_set_geometry(NULL) %>% 
  distinct(bg_fips, area_mi2_bg_500m) %>% 
  as_tibble()
save(lookup_bg_int_prkng_500m_area, file = "lookup_bg_int_prkng_500m_area.RData")


names(den_co_bg_s_by_a_gbd_long_wrangle)
names(lookup_bg_ndvi_prkng)
names(lookup_bg_int_prkng_500m_area)
table(den_co_bg_s_by_a_gbd_ndvi_long_wrangle$age_group_acs)
den_co_bg_long_prkng = den_co_bg_s_by_a_gbd_long_wrangle %>% 
  left_join(lookup_bg_int_prkng_500m_area, by = "bg_fips") %>% #link area of 500 m buffer
  left_join(lookup_bg_ndvi_prkng, by = "bg_fips") %>% #link NDVI data
  #because this is long form a simple multiplication will do to estimate
  #pop in each of these pieces in each age-sex group. recall, these areas
  #are estimated without the bodies of water there.
  mutate(
    pop_500m = area_mi2_bg_500m*pop_dens_mi2,
    #calculate the risk ratios based on the dose-response
    #function
    rr_alt_prkng_100 = drf_est**(ndvi_diff_prkng_100/drf_increment),
    rr_alt_prkng_50 = drf_est**(ndvi_diff_prkng_50/drf_increment),
    rr_alt_prkng_20 = drf_est**(ndvi_diff_prkng_20/drf_increment),
    
    #population-attributable fraction
    paf_alt_prkng_100 =(rr_alt_prkng_100  -1)/rr_alt_prkng_100  ,
    paf_alt_prkng_50 =(rr_alt_prkng_50 -1)/rr_alt_prkng_50  ,
    paf_alt_prkng_20 =(rr_alt_prkng_20 -1)/rr_alt_prkng_20  ,
    
    #attributable deaths
    attrib_d_prkng_100 = paf_alt_prkng_100*(rate_per_100k_est/100000)*pop_500m,
    attrib_d_prkng_50 = paf_alt_prkng_50*(rate_per_100k_est/100000)*pop_500m,
    attrib_d_prkng_20 = paf_alt_prkng_20*(rate_per_100k_est/100000)*pop_500m
  )


save(den_co_bg_long_prkng, file = "den_co_bg_long_prkng.RData")

## summarize long-form estimates----------
names(den_co_bg_long_prkng)
### deaths prevented by age----------
sc_prkng_deaths_prev_by_age = den_co_bg_long_prkng %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(age_group_acs) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_prkng_100 = sum(attrib_d_prkng_100, na.rm=TRUE),
    attrib_d_prkng_50 = sum(attrib_d_prkng_50, na.rm=TRUE),
    attrib_d_prkng_20 = sum(attrib_d_prkng_20, na.rm=TRUE)
  ) %>% 
  ungroup()

sc_prkng_deaths_prev_by_age
#save this to Excel for easier copy/paste
setwd(here("results"))
writexl::write_xlsx(
  sc_all_bg_deaths_prev_by_age,
  "sc_all_bg_deaths_prev_by_age.xlsx"
)
setwd(here("data-processed"))

### deaths prevented by block group----------
load("den_bg_acs5_2019_wrangle_nogeo.RData")
den_bg_acs5_2019_wrangle_nogeo
sc_prkng_deaths_prev_by_bg = den_co_bg_long_prkng %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(bg_fips) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_prkng_100 = sum(attrib_d_prkng_100, na.rm=TRUE),
    attrib_d_prkng_50 = sum(attrib_d_prkng_50, na.rm=TRUE),
    attrib_d_prkng_20 = sum(attrib_d_prkng_20, na.rm=TRUE)
  ) %>% 
  ungroup() 
  #link overall population data to calculate a rate



sc_prkng_deaths_prev_by_bg
#visualize this
library(viridis)
load("den_metro_bg_geo.RData")
sc_prkng_deaths_prev_by_bg %>% 
  ungroup() %>% 
  left_join(den_metro_bg_geo, by = "bg_fips") %>% 
  st_as_sf() %>% 
  mapview(
    layer.name = "Attributable deaths, all-cause",
    zcol = "attrib_d_prkng_100",
    col.regions = viridis_pal(direction=-1)
  )

### deaths prevented, overall----------
names(den_co_bg_long_prkng)
sc_prkng_deaths_prev_marg = den_co_bg_long_prkng %>% 
  filter(ndvi_below_native_threshold==1) %>% #limit to pieces w NDVI below native
  group_by(ndvi_below_native_threshold) %>% 
  summarise(
    pop_500m = sum(pop_500m, na.rm=TRUE),
    attrib_d_prkng_100 = sum(attrib_d_prkng_100, na.rm=TRUE),
    attrib_d_prkng_50 = sum(attrib_d_prkng_50, na.rm=TRUE),
    attrib_d_prkng_20 = sum(attrib_d_prkng_20, na.rm=TRUE)
  ) %>% 
  ungroup()


sc_prkng_deaths_prev_marg

### Make all of those long-form-------
#write a function because same code x 3
pivot_longer_sc_prkng = function(df){
  df %>% 
    mutate(scenario = "prkng") %>% 
    pivot_longer(
      cols = starts_with("attrib"),
      names_to = "scenario_sub",
      values_to = "attrib_deaths"
    ) %>% 
    mutate(
      scenario_sub = case_when(
        scenario_sub == "attrib_d_prkng_20" ~ "pct-prkng-20",
        scenario_sub == "attrib_d_prkng_50" ~ "pct-prkng-50",
        scenario_sub == "attrib_d_prkng_100" ~ "pct-prkng-100"
      )
    ) %>% 
    rename(pop_affected = pop_500m)
}

#lf for long-form
sc_prkng_deaths_prev_by_age_lf = sc_prkng_deaths_prev_by_age %>% 
  pivot_longer_sc_prkng()

sc_prkng_deaths_prev_by_age_lf

sc_prkng_deaths_prev_by_bg_lf =sc_prkng_deaths_prev_by_bg %>% 
  pivot_longer_sc_prkng()

sc_prkng_deaths_prev_by_bg_lf

sc_prkng_deaths_prev_marg_lf = sc_prkng_deaths_prev_marg %>% 
  pivot_longer_sc_prkng()

sc_prkng_deaths_prev_marg_lf

# Combine long-form datasets from all scenarios for easier summary--------
all_sc_by_age_lf = sc_all_bg_deaths_prev_by_age_lf %>% 
  bind_rows(
    sc_rip_deaths_prev_by_age_lf,
    sc_ogi_proj_deaths_prev_by_age_lf,
    sc_prkng_deaths_prev_by_age_lf,
  )


all_sc_by_bg_lf = sc_all_bg_deaths_prev_by_bg_lf %>% 
  bind_rows(
    sc_rip_deaths_prev_by_bg_lf,
    sc_ogi_proj_deaths_prev_by_bg_lf,
    sc_prkng_deaths_prev_by_bg_lf
  )

all_sc_marg_lf = sc_all_bg_deaths_prev_marg_lf %>% 
  bind_rows(
    sc_rip_deaths_prev_marg_lf,
    sc_ogi_proj_deaths_prev_marg_lf,
    sc_prkng_deaths_prev_marg_lf
  )

setwd(here("data-processed"))
save(all_sc_marg_lf, file = "all_sc_marg_lf.RData")
all_sc_marg_lf
