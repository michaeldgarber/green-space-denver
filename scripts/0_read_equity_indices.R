#filename: 0_read_equity_indices

#This script reads in the various equity indices for the City of Denver and the State of Colorado

# read Denver equity index from data portal------
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
#note we read in the denver neighborhoods here: scripts/0_read_den_nbhd.R
mv_den_equity_ind_2020= den_equity_ind_2020 %>% 
  mapview(
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

# read statewide equity definition from CDPHE---------------
#links for information
#overview: https://storymaps.arcgis.com/stories/46bf289f92bc4629a0a1266de4bb7f97
#more details: https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view
#data: https://drive.google.com/file/d/1iwUYHeHucpe9gaBMgb_UMOrS7mDGAftM/view
#load the statewide bg data
setwd(here("data-processed"))
load("colorado_bg_geo.RData") #uses 2020 acs
load("colorado_bg_2019_geo.RData") #uses 2019 ACS
names(colorado_bg_geo)

setwd(here("data-input", "climate-equity-co-statewide"))
cdphe_equity = read_csv("CEDV_Scores.csv") %>% 
  rename(
    bg_fips = BGFIPS,
    climate_equity_score = CE_Score)

names(cdphe_equity)
#link with geometry and visualize
cdphe_equity_geo = cdphe_equity %>% 
  left_join(colorado_bg_2019_geo, by = "bg_fips") %>% 
  st_as_sf()

cdphe_equity_geo %>% mapview(zcol = "climate_equity_score")

#create a lookup for the CE score
#I'm including the 2019 to remind myself that it's based on the 2019 bgs
lookup_cdphe_equity_bg_2019 = cdphe_equity %>%
  rename(bg_fips_2019 = bg_fips) %>% 
  distinct(bg_fips_2019, )
  
