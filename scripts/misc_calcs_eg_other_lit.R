#Philadelphia sample calculations to compare rates 
# from Kondo et al., 2020, Lancet Planetary Health, Health impact assessment of Philadelphia’s 2025 tree canopy cover goals
#
#Following age distribution here: https://censusreporter.org/profiles/16000US4260000-philadelphia-pa/
philly_adult_pop = 0.76*1557306 #76% adults and pop reported in paper above
library(tidyverse)

philly_deaths = c("low", "moderate", "ambitious") %>% 
  as_tibble() %>% 
  rename(scenario = value) %>% 
  mutate(
    deaths_prevented = case_when(
      scenario == "low" ~ 302,
      scenario == "moderate" ~ 376,
      scenario == "ambitious" ~ 403
    ),
    deaths_prevented_rate = deaths_prevented/philly_adult_pop,
    deaths_prevented_rate_per100k = deaths_prevented_rate*100000
  )

philly_deaths
#rates between 25.5 per 100k and 34.1 per 100k


#compare baseline mortality rates with philadelphia
#load ihme data from colorado
library(here)
setwd(here("data-processed"))
load("ihme_co.RData")


# what's the area of a circle of 500 meters squared?
area_circle_500m2 = pi*(500**2)
area_circle_500m2_in_mi2 = area_circle_500m2*3.86102e-7
area_circle_500m2_in_mi2
