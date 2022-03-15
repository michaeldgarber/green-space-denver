#filename: 3_HIA_for_each_scenario
library(tidyverse)
library(sf)
library(mapview)
library(here)
library(terra)
library(raster)
setwd(here("data-processed"))

#This code should
#-extract baseline NDVI for each polygon
#-calculate the total number of adults in each polygon as necessary
#-make the appropriate assumptions about how much NDVI would change in a given scenario
#-compute mortality and cases averted by age group
#1. Scenario 1: homogenous greening in each census block group---------

## Summarize NDVI on one good day at the block-group level in the Denver area
#update 3/10/22
library(here)
library(tidyverse)
library(terra)
library(raster)
library(mapview)
library(sf)
setwd(here("data-processed"))
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
ndvi_den_co_20210704 = ndvi_den_metro_terr_5_yr$`20210704_NDVI` 
ndvi_den_metro_terr_5_yr$`20210704_NDVI`
#note this is is in 4326, and den_co_might be in something else
ndvi_den_co_20210704 = ndvi_den_metro_terr_5_yr$`20210704_NDVI` %>% 
  terra::trim() %>%    #remove NAs
  terra::crop(den_co_4326)  
    #This automatically converts to a bbox so it's a rectangle.
    #for some reason, this band is not getting the NE area (airport)
    #this takes about 30 seconds. probably not worth saving for that time.

pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
mv_ndvi_den_co_20210704= ndvi_den_co_20210704 %>% 
  raster::raster() %>% 
  mapview(
    layer.name = "NDVI",
    col.regions = pal_terrain, 
    at = seq(-0.4, 1, 0.1)
  )

mv_den_co_tract_geo = den_co_tract_geo %>% 
  mapview(zcol = "tract_fips")

mv_ndvi_den_co_20210704 + mv_den_co_tract_geo

### tracts and block groups-----------
#Limit administrative boundaries data to the Denver area only
#This can be done aspatially using the county field :)
#these are created in this script but load for convenience
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

load("den_metro_bg_no_water_geo.RData")
#note this excludes northeast tracts.
den_co_bg_no_water_geo = den_metro_bg_no_water_geo %>% 
  filter(county_fips == "031") %>% 
  #look up tract ids
  left_join(lookup_den_metro_bg_tract, by = "bg_fips") %>% 
  #same
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


## Extract NDVI in those block groups on that day--------
#define native threshold 
ndvi_native_threshold = .5
den_co_bg_ndvi_20210704 = ndvi_den_co_20210704 %>% 
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
    ndvi_mean = mean(ndvi, na.rm=TRUE), #as a check to make sure different.
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
      ndvi_diff_100 = ndvi_alt_100-ndvi_mean_wt,
      ndvi_diff_20 = ndvi_alt_20-ndvi_mean_wt
    ) %>%  
  left_join(den_co_bg_no_water_geo, by = "bg_fips") %>% 
  left_join(lookup_den_metro_bg_tract, by = "bg_fips") %>% #link tract ID
  st_as_sf() %>% 
  dplyr::select(contains("fips"), contains("ndvi"), everything())

names(den_co_bg_ndvi_20210704)
den_co_bg_ndvi_20210704

## Visualize NDVI by census block group--------
pal_terrain = terrain.colors(100) %>% rev()#reverse the order of the palette
# Examine weighted NDVI by census block group
mv_den_co_bg_ndvi_20210704  = den_co_bg_ndvi_20210704 %>% 
  mapview(
    layer.name = "ndvi (weighted)",
    zcol = "ndvi_mean_wt",
    col.regions = pal_terrain, 
    at = seq(-0.1, 1, 0.1)
  )
mv_den_co_bg_ndvi_20210704 + mv_ndvi_den_co_20210704

n_distinct(den_co_bg_ndvi_20210704$bg_id_row_number)
nrow(den_co_bg_no_water_geo) #very good. so it's not dividing up the bgs further.

#Examine block groups above/below native threshold
den_co_bg_ndvi_20210704 %>% 
  mapview(
    layer.name = "Below native threhsold",
    zcol = "ndvi_below_native_threshold",
    col.regions = rainbow(n=2)
  )

#where would gain the most, under the 20% scenario?
den_co_bg_ndvi_20210704 %>% 
  mapview(
    layer.name = "ndvi_diff_20",
    zcol = "ndvi_diff_20"  )

summary(den_co_bg_ndvi_20210704$ndvi_mean_wt)
summary(den_co_bg_ndvi_20210704$ndvi_diff_20)
summary(den_co_bg_ndvi_20210704$ndvi_diff_100)

## Link NDVI data with population data & incidence data (from GBD)------
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

#look up the mean weighted ndvi by block group and the other
#ndvi-derived values
lookup_bg_ndvi_20210704 = den_co_bg_ndvi_20210704 %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(bg_fips, contains("ndvi")) %>% 
  distinct()
nrow(lookup_bg_ndvi_20210704)
nrow(den_co_bg_ndvi_20210704)
names(lookup_bg_ndvi_20210704)
## Prep dataset to be linked to mortality rates
names(den_metro_bg_s_by_a_long_wrangle)
den_co_bg_long_w_ndvi_gbd_lookup = den_metro_bg_s_by_a_long_wrangle %>% 
  filter(county_fips == "031" ) %>% 
  left_join(lookup_acs_gbd_age, by = "age_group_acs") %>% 
  left_join(lookup_bg_ndvi_20210704, by = "bg_fips") %>%  #link baseline ndvis
  dplyr::select(-var_label) #drop this var
  
  #these should be done at the bg level. not here. 

names(lookup_bg_ndvi_20210704)
View(den_co_bg_long_w_ndvi_gbd_lookup)

## Estimate number of deaths prevented---------
names(den_co_bg_long_w_ndvi_gbd_lookup)
names(ihme_co_w_drf)
age_lowest_to_include = 30
den_co_bg_long_w_ndvi_gbd_rates = den_co_bg_long_w_ndvi_gbd_lookup %>% 
  left_join(ihme_co_w_drf,
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

View(den_co_bg_long_w_ndvi_gbd_rates)

#age check
table(den_co_bg_long_w_ndvi_gbd_rates$age_group_acs_lb,
      den_co_bg_long_w_ndvi_gbd_rates$age_group_gbd)
  
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
#load the waterway data here:

#count the population affected within 500 meters of waterways

# Waterway scenario---------------
load("den_bg_acs5_2019_wrangle_geo.RData") #block-group-level population data
load("den_co_osm_water.RData") #the waterways with the 10-foot buffer around lines
load("den_co_osm_water_500m.RData")
load("den_co_osm_water_500m_union.RData") #the unioned version. 
names(den_bg_acs5_2019_wrangle_geo)
nrow()
#intersect the block groups with the 500-m buffer
#what proportion of the block group is covered by the intersection?
#multiply the resulting area covered by the pop. density 
#(assume uniform pop dens. in block group)
st_crs(den_co_osm_water_500m)
st_crs(den_bg_acs5_2019_wrangle_geo)
names(den_bg_acs5_2019_wrangle_geo)
#I use these mutates twice, so make a function
mutate_intersected_pop = function(df){
  df %>% 
    mutate(
      #measure the piece of the intersection and then estimate population 
      #in that piece using pop density
      area_ft2_piece = as.numeric(st_area(geometry)),
      area_mi2_piece = area_ft2_piece*3.58701e-8,
      pop_piece = area_mi2_piece*pop_dens_mi2,
    )
}
den_bg_int_osm_water = den_bg_acs5_2019_wrangle_geo %>% 
  #use the unary unioned version so you don't create overlapping pieces
  st_intersection(den_co_osm_water_500m_union) %>% 
  mutate_intersected_pop()

save(den_bg_int_osm_water, file = "den_bg_int_osm_water.RData")

#checks
den_bg_int_osm_water %>% 
  dplyr::select(contains("fips"), contains("piece"), contains("pop_dens"), contains("prop")) %>% 
  mapview(zcol = "pop_piece")
den_bg_int_osm_water %>% 
  mapview(zcol = "hh_inc_med")

# Parking lot scenario-------
load("den_parking_500m.RData")
den_bg_int_parking = den_bg_acs5_2019_wrangle_geo %>% 
  st_intersection(den_parking_500m) %>% 
  mutate_intersected_pop()

den_bg_int_parking %>% 
  mapview(zcol = "hh_inc_med")



  