test = 1:10 %>% 
  as_tibble() %>% 
  rename(id = value) %>% 
  mutate(
    RR_dose_resp = rnorm(n=n(), mean = .95, sd=.1),
    exp_change = rnorm(n=n(), mean=2, sd=.5),
    RR_result_way1 =RR_dose_resp**exp_change, 
    RR_result_way2 =exp(log(RR_dose_resp)*exp_change)
    )
test

#they're identical. interesting.
test$RR_result_way1==test$RR_result_way2

