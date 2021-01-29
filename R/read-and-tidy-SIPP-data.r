df_sipp_full <-
  # Raw SIPP file for waves 1-3 of 2014 SIPP.
  data.table::fread("./input/sipp/00_raw_SIPP_w1-to3.csv") %>% 
  janitor::clean_names() %>% 
  as_tibble()  %>% 
  rename(fips = tehc_st) %>% 
  inner_join(state_xw,"fips") %>% 
  mutate(himth = as.integer(rhimth==1 | rcdmth ==1 | ecrmth ==1),
         privhimth = as.integer(rhimth == 1),
         medcarmth = as.integer(ecrmth == 1),
         medcaidmth = as.integer(rcdmth == 1), 
         milmth = as.integer(emlmth == 1) 
  ) %>% 
  #EHEMPLY: 1=Employer or job, 2=Former Employer, 3=Union or Association, 
  # 4=Bought it directly, 5=School, or 6=something else)
  mutate(ephi_t = as.integer(ehemply1 %in% c(1,2,3) | ehemply2 %in% c(1,2,3))) %>% 
  mutate(own_cov = as.integer(rhiowner==1),
         oth_cov = as.integer(rhiowner==2),
         both_cov = as.integer(rhiowner==3))  %>% #  plan, someone else's, both or neither. 
  mutate(ownboth = as.integer(own_cov==1 | both_cov==1)) %>% 
  mutate(re_ephi = as.integer(ephi_t==1 & ownboth==1)) %>% 
  mutate(year_in_survey = swave)  %>% 
  mutate(id = paste0(ssuid,"-",pnum)) %>% 
  mutate(sex = as.factor(esex),
         race = as.factor(erace),
         state = as.factor(state),
         age = tage) %>% 
  mutate(inc_pov = pmin(50,pmax(0,tftotinc / rfpov)))

write_rds(df_sipp_full, path = here("input/sipp/01_sipp-tidy_v1-0.rds"))

# TK CHECK IF WE ACTUALLY NEED THIS .. 
# set.seed(123) 
# sample_ids <- df_sipp_full %>% 
#   filter(age>18 & age<63) %>% 
#   pull(id) %>% 
#   unique() %>% 
#   sample(1000)
# 
# df_sipp_full_sample <- 
#   df_sipp_full %>% filter(id %in% sample_ids)
# 
# #write_rds(df_sipp_full_sample, path = here("input/sipp/01_sample_sipp-tidy_v1-0.rds"))
# write_rds(df_sipp_full_sample, path = here("input/sipp/01_sample_sipp-tidy_v3-0.rds"))
