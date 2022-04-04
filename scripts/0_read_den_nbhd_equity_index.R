#Revised March 11 2022
library(tidyverse)
library(mapview)
library(sf)
library(here)
# read neighborhood data-----------
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-statistical-neighborhoods
setwd(here("data-input", "city-of-denver-data"))
den_nbhd = st_read(dsn ="neighborhoods") %>%
  st_transform(2876) %>% #central colorado, feet
  st_make_valid() %>% 
  #measure area of neighborhoods
  mutate(
    area_ft2_nbhd = as.numeric(st_area(geometry)),
    area_m2_nbhd = area_ft2_nbhd/10.764, #meters squared, for informational purposes.
    area_mi2_nbhd = area_ft2_nbhd/(5280**2) #miles squared
  ) %>% 
  rename(#rename some vars for future joins
    nbhd_id = NBHD_ID,
    nbhd_name = NBHD_NAME,
    nbhd_notes = NOTES,
    nbhd_type = TYPOLOGY
  )
setwd(here("data-processed"))
save(den_nbhd, file = "den_nbhd.RData")
den_nbhd %>% mapview(zcol = "nbhd_id")

## neighborhood-neighborhood-id lookup------
lookup_den_nbhd_name_id = den_nbhd  %>% 
  distinct(nbhd_id, nbhd_name) %>% 
  as_tibble()


save(lookup_den_nbhd_name_id, file = "lookup_den_nbhd_name_id.RData")

#about how big (area) are neighborhoods?
den_nbhd %>% 
  ggplot(aes(area_mi2_nbhd))+
  geom_histogram()
options(scipen = 999)
summary(den_nbhd$area_mi2)
#so about 1-2 square miles each.
#okay, if people are on average exposed within a 500m buffer, that's
ft_in_500m = 500*3.28
#how big would a circle of that radius be in square miles?
circle_area_in_ft2_500_m = pi*ft_in_500m**2
circle_area_in_mi2_500_m= circle_area_in_ft2_500_m/(5280**2)
circle_area_in_mi2_500_m
den_nbhd %>%   mapview(
  col.regions = rainbow(n_distinct(den_nbhd$nbhd_name)),
  zcol = "nbhd_name")

## look-up table for neighborhoods and census tracts in Denver---------
#only variables unique to neighborhood; i.e., remove area measurements

den_nbhd_for_join  = den_nbhd %>% 
  dplyr::select(-contains("area")) 

lookup_den_nbhd_tract = den_co_tract_geo %>% 
  st_join(den_nbhd, largest=TRUE) %>% #ensure only largest
  st_set_geometry(NULL) %>% 
  distinct(tract_fips, nbhd_id) %>% 
  as_tibble()


setwd(here("data-processed"))
save(lookup_den_nbhd_tract, file = "lookup_den_nbhd_tract.RData")


# read Denver equity index------
#These are the neighborhood level. Neighborhoods are typically comprised
#of census tracts.
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-equity-index-2020-neighborhood
#note some of these measures are somewat dated
setwd(here("data-input", "city-of-denver-data"))
den_equity_ind_2020 = st_read(dsn ="equity-index-2020") %>%
  st_transform(2876) %>% #central colorado, feet
  st_make_valid()  %>% 
  rename(
    #clean up var names a bit in case we need to link to other data at tract/bg level
    nbhd_name = NBRHD_NAME, #make it match other neighborhood data so it will link
    nbhd_care_access_score = ACCESSTOCA, 
    nbhd_socioecon_score = SOCIOECON_,
    nbhd_morbidity_score = MORBIDITY_,
    nbhd_mortality_score = MORTALITY_,
    nbhd_built_env_score = BUILTENV_S,
    nbhd_overall_equity_score = OVERALLEQU
  )

setwd(here("data-processed"))
save(den_equity_ind_2020, file = "den_equity_ind_2020.RData")
mapview(den_equity_ind_2020)
den_equity_ind_2020 %>% mapview(zcol = "nbhd_overall_equity_score")

#it's by neighborhood. interesting.
mv_den_equity_ind_2020= den_equity_ind_2020 %>% mapview(
  col.regions = rainbow(n_distinct(den_equity_ind_2020$nbhd_name)),
  zcol = "nbhd_name")
mv_den_equity_ind_2020
#how do the neighborhoods compare with census tracts?
setwd(here("data-processed"))
load("den_co_tract_geo.RData")

#how big are most census tracts?
den_co_tract_wrangle_geo = den_co_tract_geo %>% 
  st_transform(2876) %>% #central colorado, feet
  mutate(
    area_ft2 = as.numeric(st_area(geometry)),
    area_m2 = area_ft2/10.764, #meters squared, for informational purposes.
    area_mi2 = area_ft2/(5280**2) #miles squared
  ) 
summary(den_co_tract_wrangle_geo$area_mi2)
mv_den_co_tract_geo = den_co_tract_wrangle_geo %>% 
  mapview(zcol = "tract_fips")
mv_den_equity_ind_2020+mv_den_co_tract_geo


