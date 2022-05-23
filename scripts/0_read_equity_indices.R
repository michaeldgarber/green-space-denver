#filename: 0_read_equity_indices
library(here)
library(tidyverse)
library(mapview)
library(sf)
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
    #rename this to specify it's the denver city version
    #nbhd for neighborhood level and denver to denote that it's just a city-level score
    equity_nbhd_denver  = OVERALLEQU #lower score means worse
  ) %>% #link a neighborhood id
  mutate(
    #change the name of stapleton to central park, per 2020 change
    nbhd_name_pre_2020 = nbhd_name,
    nbhd_name = case_when(
      nbhd_name_pre_2020 == "Stapleton" ~ "Central Park",
      TRUE ~nbhd_name_pre_2020),
      #make it into quartiles; cut_number is a quantile function from ggplot2
      equity_nbhd_denver_quartile  = cut_number(equity_nbhd_denver, 4),
      equity_nbhd_denver_tertile  = cut_number(equity_nbhd_denver, 3)
  ) %>% 
  left_join(lookup_den_nbhd_name_id, by ="nbhd_name") %>% 
  dplyr::select(starts_with("nbhd_n"), starts_with("nbhd_i"), everything())

names(den_equity_ind_2020)
setwd(here("data-processed"))
save(den_equity_ind_2020, file = "den_equity_ind_2020.RData")
den_equity_ind_2020 %>% mapview(zcol = "equity_nbhd_denver_quartile")
den_equity_ind_2020 %>% mapview(zcol = "equity_nbhd_denver_tertile")
den_equity_ind_2020 %>% mapview(zcol = "equity_nbhd_denver") #lower, worse off
den_equity_ind_2020 %>% mapview(zcol = "nbhd_id")
den_equity_ind_2020 %>% ggplot()+
  geom_histogram(aes(equity_nbhd_denver))

#lookup the overall equity score
lookup_equity_nbhd_denver = den_equity_ind_2020 %>% 
  distinct(
    nbhd_id, equity_nbhd_denver, 
    equity_nbhd_denver_quartile,
    equity_nbhd_denver_tertile) %>% 
  as_tibble()
save(lookup_equity_nbhd_denver, file = "lookup_equity_nbhd_denver.RData")
#it's by neighborhood. interesting.
#note we read in the denver neighborhoods here: scripts/0_read_den_nbhd.R
mv_den_equity_ind_2020_nbhd_name= den_equity_ind_2020 %>% 
  mapview(
    col.regions = rainbow(n_distinct(den_equity_ind_2020$nbhd_name)),
  zcol = "nbhd_name")
mv_den_equity_ind_2020_nbhd_name
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
    #bg for block-group level, and state to denote that it's colorado wide, done by CDPHE
    equity_bg_cdphe = CE_Score) %>% #rename so it parallels above naming conventions
  #categorize climate equity into quartiles
  mutate(
    equity_bg_cdphe_quartile_state = cut_number(equity_bg_cdphe, 4),
    equity_bg_cdphe_tertile_state = cut_number(equity_bg_cdphe, 3)
  )

#what about a climate equity quartile for denver, only?
lookup_equity_bg_cdphe_quartile_state = cdphe_equity %>% 
  filter(COUNTY_NAME == "DENVER") %>% 
  mutate(
    equity_bg_cdphe_quartile_den = cut_number(equity_bg_cdphe, 4),
    equity_bg_cdphe_tertile_den = cut_number(equity_bg_cdphe, 3)
  ) %>% 
  distinct(bg_fips, equity_bg_cdphe_quartile_den, equity_bg_cdphe_tertile_den)

lookup_equity_bg_cdphe = cdphe_equity %>% 
  left_join(lookup_equity_bg_cdphe_quartile_state, by = "bg_fips") %>% 
  distinct(bg_fips, equity_bg_cdphe, 
           equity_bg_cdphe_quartile_state, 
           equity_bg_cdphe_tertile_state,
           equity_bg_cdphe_quartile_den,
           equity_bg_cdphe_tertile_den)
save(lookup_equity_bg_cdphe, file = "lookup_equity_bg_cdphe.RData")
table(lookup_equity_bg_cdphe_quartile_state$equity_bg_cdphe_quartile_den)
table(cdphe_equity$equity_bg_cdphe_quartile_state)
summary(cdphe_equity$equity_bg_cdphe)
cdphe_equity %>% ggplot()+
  geom_histogram(aes(equity_bg_cdphe))

names(cdphe_equity)
#link with geometry and visualize
cdphe_equity_geo = lookup_equity_bg_cdphe %>% 
  left_join(colorado_bg_2019_geo, by = "bg_fips") %>% 
  st_as_sf()

save(cdphe_equity_geo, file = "cdphe_equity_geo.RData")
cdphe_equity_geo %>% mapview(zcol = "equity_bg_cdphe")
cdphe_equity_geo %>% mapview(zcol = "equity_bg_cdphe_quartile_den")
cdphe_equity_geo %>% mapview(zcol = "equity_bg_cdphe_tertile_den")
cdphe_equity_geo %>% 
  filter(county_fips == "031") %>% 
  mapview(zcol = "equity_bg_cdphe_tertile_den")
#create a lookup for the CE score
#I'm including the 2019 to remind myself that it's based on the 2019 bgs
names(lookup_equity_bg_cdphe)
lookup_equity_bg_cdphe_2019 = lookup_equity_bg_cdphe %>%
  rename(bg_fips_2019 = bg_fips) %>% 
  distinct(
    bg_fips_2019, equity_bg_cdphe,
    equity_bg_cdphe_quartile_state, 
    equity_bg_cdphe_quartile_den,
    equity_bg_cdphe_tertile_state,
    equity_bg_cdphe_tertile_den
    )
  
lookup_equity_bg_cdphe_2019
setwd(here("data-processed"))
save(lookup_equity_bg_cdphe_2019, file = "lookup_equity_bg_cdphe_2019.RData")

# Compare tertiles for the CDPHE definition vs the Denver city definition----------

library(scales)
mv_cdphe_equity_den_tertile =  cdphe_equity_geo %>% 
  filter(county_fips == "031") %>% 
  mapview(
    layer.name = "Equity Index, CDPHE, tertile",
    zcol = "equity_bg_cdphe_tertile_den",
    col.regions = viridis_pal(direction = -1) #flip scale
    )

mv_den_equity_ind_tertile = den_equity_ind_2020 %>% 
  mapview(
    layer.name = "Equity Index, Denver, tertile",
    zcol = "equity_nbhd_denver_tertile",
    col.regions = viridis_pal(direction = 1) #don't flip scale
    
    )
mv_den_equity_ind_tertile + mv_cdphe_equity_den_tertile

#to link 2020 block groups with 2020 census tracts
load("den_bg_acs5_wrangle_geo.RData")
load("lookup_colorado_bg_tract.RData")
load("lookup_den_nbhd_tract.RData") #neighborhood ids in denver linked to tracts
#run a chi square of the two indices

#to keep consistent, using exact same function used in 3_hia_for_each_scenario
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

names(den_bg_acs5_wrangle_geo)
compare_tertiles = den_bg_acs5_wrangle_geo %>% 
  dplyr::select(bg_fips, county_fips, geometry) %>% 
  link_equity_indices() %>% 
  filter(county_fips == "031") %>% 
  dplyr::select(contains("fips"), contains("equity")) %>% 
  st_set_geometry(NULL)

names(compare_tertiles)

compare_tertiles %>% 
  group_by(equity_bg_cdphe_tertile_den, equity_nbhd_denver_tertile) %>% 
  summarise(n=n())

compare_tertiles_chisq = chisq.test(table(
  compare_tertiles$equity_bg_cdphe_tertile_den, compare_tertiles$equity_nbhd_denver_tertile)
)

compare_tertiles_chisq
compare_equity_tertiles_den %>% mapview(zcol = "equity_bg_cdphe_tertile_state")

names(compare_equity_tertiles_den)
  
