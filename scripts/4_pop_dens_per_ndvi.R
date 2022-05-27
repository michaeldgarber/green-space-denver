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
  pop_dens_per_ndvi_rank = dense_rank(pop_dens_per_ndvi) ,  #rank the ratio
  
  #categorize population density and explore NDVI within category?
  pop_dens_mi2_quartile = cut_number(pop_dens_mi2_all, 4),
  pop_dens_mi2_tertile = cut_number(pop_dens_mi2_all, 3),
  ndvi_tertile_overall = cut_number(ndvi_mean_wt, 3)
  ) %>% 
  group_by(pop_dens_mi2_tertile) %>% #tried to convert to character to fix weird mapview factor issue
  mutate(
    ndvi_tertile_within_pop_dens_tertile = cut_number(ndvi_mean_wt, 3),
    ndvi_tertile_within_pop_dens_tertile_label = cut_number(
      ndvi_mean_wt, 3, labels = c("1", "2", "3"))
    ) %>% 
  ungroup() %>% 
  group_by(pop_dens_mi2_tertile) %>% #tried to convert to character to fix weird mapview factor issue
  mutate(
    ndvi_tertile_within_pop_dens_quartile = cut_number(ndvi_mean_wt, 3),
    ndvi_tertile_within_pop_dens_quartile_label = cut_number(
      ndvi_mean_wt, 3, labels = c("1", "2", "3"))) %>% 
  ungroup() %>% 
  dplyr::select(
    ends_with("fips"), pop_tot, contains("pop_dens"), contains("ndvi"), contains("nbhd_northeast")) %>% 
  #maximum NDVI tertile within pop. density tertile.
  #Note I can do this all in the same dataset! no need to summarise. just use group_by() and mutate()
  #I want the median NDVI within the highest of the three NDVI tertiles within
  #population density tertile
  group_by(
    pop_dens_mi2_tertile,
    ndvi_tertile_within_pop_dens_quartile_label) %>% 
  mutate(
    ndvi_med_pop_dens_ndvi_tert = median(ndvi_mean_wt, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  #and now the max of that median value grouped by pop dens
  group_by(pop_dens_mi2_tertile) %>%
  mutate(
    ndvi_max_med_pop_dens_ndvi_tert = max(ndvi_med_pop_dens_ndvi_tert),
    ratio_ndvi_med_pop_dens_ndvi_tert = ndvi_med_pop_dens_ndvi_tert/ndvi_max_med_pop_dens_ndvi_tert,
    diff_ndvi_med_pop_dens_ndvi_tert = ndvi_med_pop_dens_ndvi_tert-ndvi_max_med_pop_dens_ndvi_tert,
  ) %>% 
  ungroup()

save(den_co_bg_pop_per_ndvi, file = "den_co_bg_pop_per_ndvi.RData")
names(den_co_bg_pop_per_ndvi)
class(den_co_bg_pop_per_ndvi)
table(den_co_bg_pop_per_ndvi$ndvi_tertile_within_pop_dens_quartile)
table(den_co_bg_pop_per_ndvi$ndvi_tertile_within_pop_dens_tertile)
table(den_co_bg_pop_per_ndvi$ndvi_tertile_within_pop_dens_quartile_label)
class(den_co_bg_pop_per_ndvi$ndvi_tertile_within_pop_dens_quartile_label)

#get a map of just the pop-density tertiles
pop_density_tertile_den_co_bg = den_co_bg_pop_per_ndvi %>% 
  group_by(pop_dens_mi2_tertile) %>% 
  summarise(pop_dens_mi2_med = median(pop_dens_mi2_all, na.rm=TRUE))
save(pop_density_tertile_den_co_bg, file = "pop_density_tertile_den_co_bg.RData")

library(RColorBrewer)
library(shades)
set2 = RColorBrewer::brewer.pal(n=3, "Set2")
set2 %>% swatch()
greys = RColorBrewer::brewer.pal(n=3, "Greys")
mv_pop_density_tertile = pop_density_tertile_den_co_bg %>% 
  mutate(
    pop_dens_mi2_med_round = round(pop_dens_mi2_med)
  ) %>% 
  mapview(
    layer.name = "Median pop. density within pop. dens. tertile",
    col.regions = greys,
  zcol = "pop_dens_mi2_med_round")

den_co_bg_pop_per_ndvi %>% 
  dplyr::select(bg_fips, pop_dens_mi2_tertile, ndvi_tertile_within_pop_dens_quartile_label, 
                contains("ndvi_med_pop_dens_ndvi_tert")) 

table(den_co_bg_pop_per_ndvi$ndvi_med_pop_dens_ndvi_tert)

library(viridis)
mv_ratio_ndvi_med_pop_dens_ndvi_tert=  den_co_bg_pop_per_ndvi %>% 
  mapview(
    col.regions = viridis_pal(option = "plasma", direction=-1),
    layer.name = "Ratio of NDVI with median of highest NDVI tertile",
    zcol = "ratio_ndvi_med_pop_dens_ndvi_tert")
mv_diff_ndvi_med_pop_dens_ndvi_tert=  den_co_bg_pop_per_ndvi %>% 
  mapview(
    col.regions = viridis_pal(option = "plasma", direction=-1),
    layer.name = "Difference of NDVI with median of highest NDVI tertile",
    zcol = "diff_ndvi_med_pop_dens_ndvi_tert")

mv_ratio_ndvi_med_pop_dens_ndvi_tert+mv_pop_density_tertile
mv_diff_ndvi_med_pop_dens_ndvi_tert + mv_pop_density_tertile
#are pop density and ndvi inversely correlated as expected?
den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_all>10) %>% 
  ggplot(aes(x=pop_dens_mi2_all, y=ndvi_mean_wt )) + 
    geom_point(aes(colour=pop_dens_mi2_tertile ), size = 1) +
    ylab("NDVI") +
    xlab("Pop. density") +
    theme_bw(base_size = 14) +
  scale_color_hue(
    name = "Pop. density tertile"
  )

den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_all>10) %>% 
  ggplot(aes(x=pop_dens_mi2_all, y=ndvi_mean_wt )) + 
  geom_point(aes(colour=pop_dens_mi2_quartile ), size = 1) +
  ylab("NDVI") +
  xlab("Pop. density") +
  theme_bw(base_size = 14) +
  scale_color_hue(
    name = "Pop. density quartile"
  )

summary(den_co_bg_pop_per_ndvi)
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "ndvi_mean_wt")
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_mi2_quartile")
den_co_bg_pop_per_ndvi %>% 
  mapview(zcol = "pop_dens_mi2_tertile")

#NDVI quantiles within pop. dens quantiles

pop_dens_mi2_tertile_1 = den_co_bg_pop_per_ndvi %>% 
  st_set_geometry(NULL) %>% 
  group_by(pop_dens_mi2_tertile) %>% 
  summarise(n=n()) %>% 
  dplyr::select(pop_dens_mi2_tertile) %>% 
  ungroup() %>% 
  dplyr::slice(1) %>% 
  pull()
pop_dens_mi2_tertile_2 = den_co_bg_pop_per_ndvi %>% 
  st_set_geometry(NULL) %>% 
  group_by(pop_dens_mi2_tertile) %>% 
  summarise(n=n()) %>% 
  dplyr::select(pop_dens_mi2_tertile) %>% 
  ungroup() %>% 
  dplyr::slice(2) %>% 
  pull()

pop_dens_mi2_tertile_3 = den_co_bg_pop_per_ndvi %>% 
  st_set_geometry(NULL) %>% 
  group_by(pop_dens_mi2_tertile) %>% 
  summarise(n=n()) %>% 
  dplyr::select(pop_dens_mi2_tertile) %>% 
  ungroup() %>% 
  dplyr::slice(3) %>% 
  pull()


pop_dens_mi2_tertile_1
pop_dens_mi2_tertile_2
pop_dens_mi2_tertile_3
table(den_co_bg_pop_per_ndvi$pop_dens_mi2_tertile)
table(den_co_bg_pop_per_ndvi$ndvi_tertile_within_pop_dens_tertile)
class(den_co_bg_pop_per_ndvi$ndvi_tertile_within_pop_dens_tertile)
library(RColorBrewer)
pal_terrain = terrain.colors(4)  #reverse the order of the palette
library(shades)
pal_terrain_3_cat = rev(terrain.colors(4)[1:3])
pal_terrain_3_cat %>% swatch()
## these strangely return erroneous results. see
# https://github.com/r-spatial/mapview/issues/425
den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_1) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens tertile 1",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_tertile")

den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_2) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens. tertile 2",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_tertile")

den_co_bg_pop_per_ndvi %>% #doesn't render correctly.
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_3) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens. tertile 3",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_tertile")

## try the labeled factor way?-----
den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_1) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens. tertile 1",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_tertile_label")

den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_2) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens. tertile 2",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_tertile_label")

den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_3) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens. tertile 3",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_tertile_label")

den_co_bg_pop_per_ndvi %>% 
  filter(pop_dens_mi2_tertile== pop_dens_mi2_tertile_3) %>% 
  mapview(
    layer.name = "NDVI tertile within pop. dens. tertile 3",
    col.regions = pal_terrain_3_cat, 
    zcol = "ndvi_tertile_within_pop_dens_quartile_label")

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


