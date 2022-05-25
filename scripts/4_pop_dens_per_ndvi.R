#filename: 4_ndvi_per_pop

library(tidyverse)
library(sf)
library(mapview)
library(here)
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)
setwd(here("data-processed"))
load("lookup_den_co_bg_no_wtr_geo.RData") #created ~scripts/3_HIA_for_each_scenario.R
load("den_co_bg_ndvi_alt_all_nogeo.RData") #from  scripts/3_HIA_for_each_scenario.R
load("den_bg_acs5_wrangle_geo.RData") #from ~scripts/0_import_manage_denver_acs.R
load("lookup_den_co_bg_ndvi_mean_wt.RData")
load("lookup_tract_nbhd_northeast_exclude.RData")

#make sure that this has been run recently, and now use this function but over
den_co_bg_pop_per_ndvi =  den_bg_acs5_wrangle_geo %>% 
  filter(county_fips == "031") %>% 
  left_join(lookup_den_co_bg_ndvi_mean_wt, by = "bg_fips") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>% #exclude the northeast airport-adjacent tracts. nobody or very few people
  mutate(
    pop_dens_per_ndvi = case_when(
      is.na(ndvi_mean_wt)==TRUE ~ NA_real_,
      is.na(pop_dens_mi2_all)==TRUE ~ NA_real_,
      pop_dens_mi2_all <5 ~ NA_real_,
      TRUE ~ pop_dens_mi2_all/ndvi_mean_wt
  ),
  #try a ranking for both pop. density and NDVI
  #https://dplyr.tidyverse.org/reference/ranking.html
  pop_dens_mi2_all_rank = dense_rank(pop_dens_mi2_all),
  ndvi_mean_wt_rank = dense_rank(ndvi_mean_wt),
  
  #and then do a ratio of those two ranks
  pop_dens_per_ndvi_ratio_of_ranks = pop_dens_mi2_all_rank/ndvi_mean_wt_rank,
  pop_dens_per_ndvi_rank = dense_rank(pop_dens_per_ndvi)   #rank the ratio
  ) %>% 
  dplyr::select(
    ends_with("fips"), pop_tot, contains("pop_dens"), contains("ndvi"), contains("nbhd_northeast"))
summary(den_co_bg_pop_per_ndvi)
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_mi2_all_rank")
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_per_ndvi_ratio_of_ranks")
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_per_ndvi_rank")

den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_mi2_all")
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_per_ndvi")

den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_all<100) %>% 
  mapview(zcol = "pop_dens_mi2_all")


