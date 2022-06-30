#The purpose of this script is to calculate some
#overall measures (mortality, area) to be used in the appendix descriptive table

library(tidyverse)
setwd(here("data-processed"))




## Build mortality rate table section---------------------------#
# for use in tables, calculate an overall estimated mortality rate for 30 years and up

# load mortality rate to include in the table
setwd(here("data-processed"))
load("ihme_co.RData")
load("lookup_tract_nbhd_northeast_exclude.RData")
load("den_bg_acs5_wrangle_geo.RData")
load("den_co_bg_sex_age_gbd_wrangle.RData")
#all of Denver. the only difference from the main result will be the fact that it's 
#calculated with respect to the age distribution of denver
mortality_rate_per1k_18_plus_denver =den_co_bg_sex_age_gbd_wrangle %>% 
  filter(county_fips == "031") %>% 
  group_by(age_group_acs_18_plus) %>% #note all adults
  summarise(
    deaths_annual_est=sum(deaths_annual_est, na.rm=TRUE),
    pop_est = sum(pop_est, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(
    rate_est =deaths_annual_est/pop_est,
    rate_per_100k_est = rate_est*100000,
    rate_per_1k_est = rate_est*1000) %>% 
  dplyr::select(rate_per_1k_est)



#note i made the 30+ grouping here:
# scripts/0_import_manage_denver_acs.R
mortality_rate_per1k_30_plus_study_area = den_co_bg_sex_age_gbd_wrangle %>% 
  filter(county_fips == "031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>% #this version is specific to the study, so exclude these.
  group_by(age_group_acs_30_plus) %>% 
  summarise(
    deaths_annual_est=sum(deaths_annual_est, na.rm=TRUE),
    pop_est = sum(pop_est, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(
    rate_est =deaths_annual_est/pop_est,
    rate_per_100k_est = rate_est*100000,
    rate_per_1k_est = rate_est*1000) %>% 
  dplyr::select(rate_per_1k_est)

mortality_rate_per1k_18_plus_denver
mortality_rate_per1k_30_plus_study_area

save(mortality_rate_per1k_18_plus_denver, file = "mortality_rate_per1k_18_plus_denver.RData")
save(mortality_rate_per1k_30_plus_study_area, file = "mortality_rate_per1k_30_plus_study_area.RData")

#--------Average block-group size?------------------------------#
#both for the city of denver and for the study area for your table 1
bg_size_average_study_area = den_bg_acs5_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  filter(county_fips == "031") %>% 
  left_join(lookup_tract_nbhd_northeast_exclude, by = "tract_fips") %>% 
  filter(nbhd_northeast_exclude==0) %>% #Decide not to exclude, actually
  group_by(county_fips) %>% 
  summarise(
    area_mi2_bg_med  = median(area_mi2_bg),
    area_mi2_bg_mean = mean(area_mi2_bg),
    area_mi2_bg_sd = sd(area_mi2_bg)
  )


bg_size_average_denver = den_bg_acs5_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  filter(county_fips == "031") %>% 
  group_by(county_fips) %>% 
  summarise(
    area_mi2_bg_med  = median(area_mi2_bg),
    area_mi2_bg_mean = mean(area_mi2_bg),
    area_mi2_bg_sd = sd(area_mi2_bg)
  )

bg_size_average_study_area
bg_size_average_denver
save(bg_size_average_study_area, file = "bg_size_average_study_area.RData")
save(bg_size_average_denver, file = "bg_size_average_denver.RData")