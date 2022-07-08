#Philadelphia sample calculations to compare rates 
# from Kondo et al., 2020, Lancet Planetary Health, Health impact assessment of Philadelphia’s 2025 tree canopy cover goals
#
library(tidyverse)

#Following age distribution here: https://censusreporter.org/profiles/16000US4260000-philadelphia-pa/
philly_adult_pop = 0.76*1557306 #76% adults and pop reported in paper above
philly_adult_pop 

philly_deaths = c("low", "moderate", "ambitious") %>% 
  as_tibble() %>% 
  rename(scenario = value) %>% 
  mutate(
    deaths_prevented = case_when(
      scenario == "low" ~ 302,
      scenario == "moderate" ~ 376,
      scenario == "ambitious" ~ 403
    ),
    deaths_prevented_ll = case_when(
      scenario == "low" ~ 223,
      scenario == "moderate" ~ 279,
      scenario == "ambitious" ~ 298
      
    ),
    deaths_prevented_ul = case_when(
      scenario == "low" ~ 461,
      scenario == "moderate" ~ 575,
      scenario == "ambitious" ~ 618
    ),
    deaths_prevented_rate = deaths_prevented/philly_adult_pop,
    deaths_prevented_rate_ll = deaths_prevented_ll/philly_adult_pop,
    deaths_prevented_rate_ul = deaths_prevented_ul/philly_adult_pop,
    deaths_prevented_rate_per100k = deaths_prevented_rate*100000,
    deaths_prevented_rate_per100k_ll = deaths_prevented_rate_ll*100000,
    deaths_prevented_rate_per100k_ul = deaths_prevented_rate_ul*100000
  )

philly_deaths %>% 
  dplyr::select(scenario, contains("rate_per100k"))
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

# Calculate population-standardized mortality rates in Europe study--------#
setwd(here("data-processed"))
load("ihme_co.RData")
table(ihme_co$age_group_gbd)
library(here)
library(readxl)

setwd(here("data-input", "other-studies-misc"))
#note these were typed into a spreadsheet copying from
#table s3 in the appendix of 
#Green Space and Mortality in European cities: a health impact assessment study

europe_mortality_by_age <- read_csv("europe-mortality-by-age-table-s3.csv") 
europe_mortality_by_age_wrangle =  europe_mortality_by_age%>% 
  mutate(
    sum_of_props = sum(age_group_prop),
    wt = age_group_prop/sum_of_props,
    dummy=1,
    rate_per_10k = rate_per_100k/100, #I report this in the paper
    wt_int = rate_per_10k*wt #weight times rate in each group
  ) %>% 
  group_by(dummy) %>% 
  summarise(rate_per_10k = sum(wt_int)) %>% #sum weighted intermediate
  ungroup() 

europe_mortality_by_age_wrangle

# e-value calculation--------
#e-value = RR + sqrt(RR*(RR-1))
#note that this formula applies to a formula greater than 1.
#for a risk ratio less than 1, first take the inverse of the observed RR
#and then apply the formula.
rr_inv_pt = 1/.96
rr_inv_ll = 1/0.94
rr_inv_ul = 1/0.97
rr_inv_pt
rr_inv_ll
rr_inv_ul

e_val_pt = (rr_inv_pt+sqrt(rr_inv_pt*(rr_inv_pt-1)))
e_value_ll = (rr_inv_ll+sqrt(rr_inv_ll*(rr_inv_ll-1)))
e_value_ul = (rr_inv_ul+sqrt(rr_inv_ul*(rr_inv_ul-1)))

e_val_pt
e_value_ll
e_value_ul

# Denver vs Colorado Mortality rate
colorado_mort_rate_age_adjust = 738.74
denver_mort_rate_age_adjust = 806.77

denver_mort_rate_age_adjust/colorado_mort_rate_age_adjust