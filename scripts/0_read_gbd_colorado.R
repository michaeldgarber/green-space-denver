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
#see here on what the confidence intervals are. they're posterior quantiles (2.5th and 97.5th). good
#https://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_LMICS_U5M_2000_2017_INFO_SHEET_Y2019M10D16.PDF

setwd(here("data-input", "GBD data-CO"))
ihme_co <- read_csv("IHME-GBD_2019_DATA-96be8737-1.csv") %>% 
  #restrict to rates only
  dplyr::filter(metric == "Rate") %>% 
  #can drop colorado because we know
  dplyr::select(-location) %>% 
  #re-name age group to begin corresponding with ACS
  #Update 3/10/22 I thought about renaming the age groups to correspond better with
  #ACS, but I don't think that's necessary.
  #Use a look-up table insted later on.
  rename(age_group_gbd = age)  %>% 
  mutate(
    cause_short = case_when(
      cause == "All causes" ~ "all",
      cause == "Alzheimer's disease and other dementias" ~ "alzheimers-dementia",
      cause == "Stroke" ~ "stroke"
  ),
  #make sex lowercase for linking with ACS data
  sex = case_when(
    sex == "Both" ~ "all", #just use all across the board for age/sex
    sex == "Female" ~ "female",
    sex == "Male" ~ "male"
  ),
  measure = tolower(measure)#make lowercase
  ) %>% 
  #also lowercase
  #these are all rates, so rename the val and upper and lower to be more explicit
  rename(
    rate_per_100k_est = val,
    rate_per_100k_ul = upper, #upper limit
    rate_per_100k_ll = lower  #upper limit
  ) %>% 
  #drop a few
  #again, we know they're rates, and we have a shorter cause
  dplyr::select(-metric, -cause) 

setwd(here("data-processed"))
save(ihme_co, file = "ihme_co.RData")
#looks good
table(ihme_co$cause_short)
table(ihme_co$cause_short)
table(ihme_co$age_group_gbd)
table(ihme_co$measure)
summary(ihme_co$rate_per_100k_est)
class(ihme_co$age_group_gbd)
ihme_co

