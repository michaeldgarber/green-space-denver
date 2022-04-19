#filename: 0_read_equity_indices

#This script reads in the various equity indices for the City of Denver and the State of Colorado

# read Denver equity index from data portal------
#These are the neighborhood level. Neighborhoods are typically comprised
#of census tracts.
#https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-equity-index-2020-neighborhood
#note some of these measures are somewat dated
setwd(here("data-processed"))
load("lookup_den_nbhd_name_id.RData") #link neighborhood ID, created 0_read_den_nbhd.R
setwd(here("data-input", "city-of-denver-data"))
den_equity_ind_2020 = st_read(dsn ="equity-index-2020") %>%
  st_transform(2876) %>% #central colorado, feet
  st_make_valid()  %>% 
  rename(
    #clean up var names a bit in case we need to link to other data at tract/bg level
    nbhd_name = NBRHD_NAME, #make it match other neighborhood data so it will link
    nbhd_care_access = ACCESSTOCA, 
    nbhd_socioecon = SOCIOECON_,
    nbhd_morbidity = MORBIDITY_,
    nbhd_mortality = MORTALITY_,
    nbhd_built_env = BUILTENV_S,
    nbhd_overall_equity = OVERALLEQU #lower score is worse off
  ) %>% #link a neighborhood id
  mutate(
    #change the name of stapleton to central park, per 2020 change
    nbhd_name_pre_2020 = nbhd_name,
    nbhd_name = case_when(
      nbhd_name_pre_2020 == "Stapleton" ~ "Central Park",
      TRUE ~nbhd_name_pre_2020
    )
  ) %>% 
  left_join(lookup_den_nbhd_name_id, by ="nbhd_name") %>% 
  dplyr::select(starts_with("nbhd_n"), starts_with("nbhd_i"), everything())

names(den_equity_ind_2020)
setwd(here("data-processed"))
save(den_equity_ind_2020, file = "den_equity_ind_2020.RData")
mapview(den_equity_ind_2020)
den_equity_ind_2020 %>% mapview(zcol = "nbhd_overall_equity")
den_equity_ind_2020 %>% mapview(zcol = "nbhd_id")

#lookup the overall equity score
den_equity_ind_2020
lookup_nbhd_overall_equity = den_equity_ind_2020 %>% 
  distinct(nbhd_id, nbhd_overall_equity) %>% 
  as_tibble()
save(lookup_nbhd_overall_equity, file = "lookup_nbhd_overall_equity.RData")
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

# read state equity definition from CDPHE---------------
#links for information
#overview: https://storymaps.arcgis.com/stories/46bf289f92bc4629a0a1266de4bb7f97
#more details: https://drive.google.com/file/d/1iytdPG5iK2VBNpIy8k6oT6lU6-QKMLOa/view
#data: https://drive.google.com/file/d/1iwUYHeHucpe9gaBMgb_UMOrS7mDGAftM/view
#load the state bg data
setwd(here("data-processed"))
load("colorado_bg_geo.RData") #uses 2020 acs
load("colorado_bg_2019_geo.RData") #uses 2019 ACS
names(colorado_bg_geo)

setwd(here("data-input", "climate-equity-co-statewide"))
cdphe_equity = read_csv("CEDV_Scores.csv") %>% 
  rename(
    bg_fips = BGFIPS,
    climate_equity_score = CE_Score) %>% 
  #categorize climate equity into quartiles
  mutate(
    climate_equity_quartile_state = cut_number(climate_equity_score, 4)
  )

#what about a climate equity quartile for denver, only?
lookup_climate_equity_quartile_state = cdphe_equity %>% 
  filter(COUNTY_NAME == "DENVER") %>% 
  mutate(
    climate_equity_quartile_den = cut_number(climate_equity_score, 4)
  ) %>% 
  distinct(bg_fips, climate_equity_quartile_den)

lookup_climate_equity_cdphe = cdphe_equity %>% 
  left_join(lookup_climate_equity_quartile_state, by = "bg_fips") %>% 
  distinct(bg_fips, climate_equity_score, climate_equity_quartile_state, climate_equity_quartile_den)
save(lookup_climate_equity_cdphe, file = "lookup_climate_equity_cdphe.RData")
table(lookup_climate_equity_quartile_state$climate_equity_quartile_den)
table(cdphe_equity$climate_equity_quartile_state)
summary(cdphe_equity$climate_equity_score)
cdphe_equity %>% ggplot()+
  geom_histogram(aes(climate_equity_score))

names(cdphe_equity)
#link with geometry and visualize
cdphe_equity_geo = lookup_climate_equity_cdphe %>% 
  left_join(colorado_bg_2019_geo, by = "bg_fips") %>% 
  st_as_sf()

cdphe_equity_geo %>% mapview(zcol = "climate_equity_score")
cdphe_equity_geo %>% mapview(zcol = "climate_equity_quartile_den")
#create a lookup for the CE score
#I'm including the 2019 to remind myself that it's based on the 2019 bgs
lookup_cdphe_equity_bg_2019 = lookup_climate_equity_cdphe %>%
  rename(bg_fips_2019 = bg_fips) %>% 
  distinct(bg_fips_2019, climate_equity_score, climate_equity_quartile_state, climate_equity_quartile_den)
  
lookup_cdphe_equity_bg_2019
setwd(here("data-processed"))
save(lookup_cdphe_equity_bg_2019, file = "lookup_cdphe_equity_bg_2019.RData")
