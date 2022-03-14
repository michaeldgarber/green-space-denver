#-----load IHME data for Colorado--------#
#Revised 3/10/22

#outcomes:
#1. all-cause mortality
#2. incidence of dementia and stroke

#note rates are per 100k
#stratified by sex and by age group

library(here)
library(sf)
library(tidyverse)
here()

#data downloaded from http://ghdx.healthdata.org/gbd-results-tool and saved locally
#consider getting county-level data from https://hdpulse.nimhd.nih.gov/data/deathrates/index.php

setwd(here("data-input", "GBD data-CO"))
ihme_co <- read_csv("IHME-GBD_2019_DATA-96be8737-1.csv") %>% 
  #restrict to rates only
  dplyr::filter(metric == "Rate") %>% 
  #can drop colorado because we know
  dplyr::select(-location) %>% 
  #re-name age group to begin corresponding with ACS
  #Update 3/10/22 I thought about renaming the age groups to correspond better with
  #ACS, but I don't think that's necessary. Just pick the few age groups that you want.
  rename(age_group_gbd = age)  %>% 
  #make it wide-form by measure &
    

setwd(here("data-processed"))
save(ihme_co, file = "ihme_co.RData")
table(ihme_co$measure,
      ihme_co$cause
      )
#looks good
table(ihme_co$age_group_gbd)
class(ihme_co$age_group_gbd)
View(ihme_co)

#make them match these categories
load("lookup_acs_2019_var_s_by_a_label.RData")
table(lookup_acs_2019_var_s_by_a_label$age_group_acs) #good

