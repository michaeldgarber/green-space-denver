#Philadelphia sample calculations to compare rates 
# from Kondo et al., 2020, Lancet Planetary Health, Health impact assessment of Philadelphia’s 2025 tree canopy cover goals
#
#Following age distribution here: https://censusreporter.org/profiles/16000US4260000-philadelphia-pa/
philly_adult_pop = 0.76*1557306 #76% adults and pop reported in paper above

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
  