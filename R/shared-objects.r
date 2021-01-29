other_expansion_dates <-  
  ### NOTE: UPDATED 2020-12-03 by Kyle Gavulic
  c("AK" = lubridate::as_date(zoo::as.yearmon("09/2014","%m/%Y")), 
    "IN" = lubridate::as_date(zoo::as.yearmon("02/2015","%m/%Y")), 
    "MI" = lubridate::as_date(zoo::as.yearmon("04/2014","%m/%Y")), 
    "MT" = lubridate::as_date(zoo::as.yearmon("01/2016","%m/%Y")), 
    "NH" = lubridate::as_date(zoo::as.yearmon("08/2014","%m/%Y")), 
    "PA" = lubridate::as_date(zoo::as.yearmon("01/2015","%m/%Y")), 
    "CA" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")), 
    "CT" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")), 
    "HI" = lubridate::as_date(zoo::as.yearmon("01/1994","%m/%Y")), 
    "MN" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")), 
    "WI" = lubridate::as_date(zoo::as.yearmon("01/2009","%m/%Y")), 
    "DE" = lubridate::as_date(zoo::as.yearmon("01/1996","%m/%Y")), 
    "DC" = lubridate::as_date(zoo::as.yearmon("01/2010","%m/%Y")), 
    "MA" = lubridate::as_date(zoo::as.yearmon("01/2006","%m/%Y")), 
    "NY" = lubridate::as_date(zoo::as.yearmon("01/2001","%m/%Y")), 
    "VT" = lubridate::as_date(zoo::as.yearmon("01/1996","%m/%Y")), 
    "LA" = lubridate::as_date(zoo::as.yearmon("01/2016","%m/%Y")), 
    "ID" = lubridate::as_date(zoo::as.yearmon("01/2020","%m/%Y")), 
    "ME" = lubridate::as_date(zoo::as.yearmon("01/2019","%m/%Y")), 
    "MO" = lubridate::as_date(zoo::as.yearmon("07/2021","%m/%Y")), 
    "NE" = lubridate::as_date(zoo::as.yearmon("10/2020","%m/%Y")), 
    "OK" = lubridate::as_date(zoo::as.yearmon("07/2021","%m/%Y")), 
    "UT" = lubridate::as_date(zoo::as.yearmon("01/2020","%m/%Y")), 
    "VA" = lubridate::as_date(zoo::as.yearmon("01/2019","%m/%Y"))) 


# Note the state groupings are based on Black et al. 
# (NBER working paper "THE EFFECT OF HEALTH INSURANCE ON MORTALITY: POWER ANALYSIS AND WHAT WE CAN LEARN FROM THE AFFORDABLE CARE ACT COVERAGE EXPANSIONS")
#
state_xw <- read.csv(here::here("input/state_xwalk.csv")) %>% 
  mutate(full_expansion_state = as.integer(
    state %in%   c("AK","AZ","AR","CO","IL","IN","KY","IA",
                   "MD","MI","MT","NV","NH","NJ","NM",
                   "ND","OH","OR","PA","RI","WA","WV","ID","LA","ME","MO","NE","OK","UT","VA")
  ), 
  substantial_expansion_state = as.integer(
    state %in% c("CA","CT","HI","MN","WI") 
  ),
  mild_expansion_state = as.integer(
    state %in% c("DE","DC","MA","NY","VT")
  ),
  non_expansion_state = as.integer(
    state %in% c("AL","GA","FL","KS",
                 "MS","NE","NC","SC","SD",
                 "TN","TX","WY")
  )) %>% 
  mutate(expansion_date = ifelse(non_expansion_state ==0 , 
                                 lubridate::as_date(zoo::as.yearmon("01/2014","%m/%Y")),
                                 NA)) %>% 
  mutate(expansion_date = ifelse(state %in% names(other_expansion_dates), 
                                 other_expansion_dates[as.character(state)],expansion_date)) %>% 
  mutate(expansion_date = lubridate::as_date(expansion_date)) %>% 
  mutate(expansion_year = lubridate::year(expansion_date))


# scale_fill_Publication <- function(...){
#   library(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }
# 
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
# 
# 
theme_tufte_revised <- function(base_size = 11, base_family = "Gill Sans", ticks = TRUE) {

  ret <- ggplot2::theme_bw(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = 'black'),
      axis.title.x = ggplot2::element_text(vjust = -0.3),
      axis.title.y = ggplot2::element_text(vjust = 0.8),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face="plain"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
  }

  ret
}

add_binary_indicators <- function(df, variable) {
  var <- enquo(variable)
  df_tmp <-
    df %>%
    as_tibble() %>%
    # select(!!var) %>%
    mutate(!!quo_name(var) := factor(!!var))
  var_levels <- unique(df_tmp[, quo_name(var)]) %>% pull(!!var) %>% levels()
  newvars <- c()
  for (vv in var_levels) {
    newvar <- paste0(quo_name(var), "_", vv)
    newvars <- c(newvars, newvar)
    df_tmp <-
      df_tmp %>%
      mutate(!!newvar := case_when(
        !!var %in% vv ~ 1,
        !(!!var %in% vv) ~ 0
      ))
  }
  attr(df_tmp, "binary_variables") <- newvars
  df_tmp
}



# insurance_sipp_lut <- c(
#   "cat_1" = "01_esi_own",       
#   "cat_2" = "02_priv_oth",
#   "cat_3" = "03_public",
#   "cat_4" = "04_uninsured"
# )
# 
# insurance_lut <-
#   c("01_esi_own" = "Employer Sponsored",
#     "02_priv_oth" = "Privately Purchased",
#     "03_public" = "Public",
#     "04_uninsured" = "Uninsured")

# get_twoper_result <- function(df) {
#   df %>% 
#     select(term,starts_with("marginal")) %>% 
#     na.omit() %>% 
#     gather(estimate, value, -term) %>% 
#     filter(term=="post_expansion:expansion_state") %>% 
#     mutate(group = insurance_sipp_lut)  %>% 
#     select(group,value)
# }
# 
# get_quantile_ranking <- function(obs,inf) {
#   inf %>% 
#     group_by(replicate) %>% 
#     bind_rows(
#       obs %>% mutate(replicate=0) 
#     ) %>% 
#     group_by(group) %>% 
#     mutate(quantile_rank = percent_rank(value)) %>% 
#     filter(replicate == 0) %>% 
#     select(group,quantile_rank)
# }
# 
# get_pvalue <- function(obs,inf) {
#   inf %>% 
#     group_by(replicate) %>% 
#     left_join(obs,"group") %>% 
#     mutate(obs_lt_est = as.integer(abs(value.y) < abs(value.x))) %>% 
#     group_by(group) %>% 
#     summarise(p_value = mean(obs_lt_est)) 
# }
# 
# get_transprob_result <- function(df) {
#   df %>% 
#     select(ex_ante,starts_with("DD")) %>% 
#     na.omit() %>% 
#     gather(estimate, value, -ex_ante) %>% 
#     mutate(ex_post = estimate) %>% 
#     mutate(ex_post = gsub("^DD_","",ex_post)) %>% 
#     select(ex_ante,ex_post,value)
# }
# 
# get_dd_marginal <- function(p1,p0,R1,DD,C,D) ((p1 %*% (C * R1) - p1*D) - (p0 %*% (C * (R1-DD)) - p0*D))
# 
# get_marginal_estimates <- 
#   function(df, C = matrix(1,nrow =4, ncol = 4), D = rep(1,4)) {
#     
#     # Regression Parameters (not going to output this now as decompositions won't match)
#     regression_parameters <- 
#       df %>% 
#       select(term,starts_with("marginal")) %>% 
#       filter(term == "post_expansion:expansion_state") %>% 
#       gather(ex_ante,beta,-term) %>% 
#       select(-term)
#     
#     p1 <- df %>% filter(!is.na(ex_ante)) %>% pull(p_Z1_T0) #%>% as.matrix()
#     p0 <- df %>% filter(!is.na(ex_ante)) %>% pull(p_Z0_T0) #%>% as.matrix()
#     tmp <- df %>% filter(!is.na(ex_ante)) %>% select(ex_ante,starts_with("DD")) 
#     DD <- tmp[,-1] %>% as.matrix()
#     rownames(DD) <- tmp$ex_ante
#     tmp2 <- df %>% filter(!is.na(ex_ante)) %>% select(ex_ante,starts_with("R1"))
#     R1 <- tmp2[,-1] %>% as.matrix()
#     rownames(R1) <- tmp2$ex_ante
#     
#     get_dd_marginal(p1 = p1, p0 = p0, R1= R1, DD = DD, D = D, C = C ) %>% 
#       data.frame() %>% 
#       gather(estimate,value) %>% 
#       mutate(group = gsub("R1_","",estimate)) %>% 
#       select(-estimate)
#   }
# 
# get_decomp_est <- function(df,df_ri,C,D) {
#   
#   # Pooled (Transition) Model Marginal Result
#   pooled_marginal_result_obs <- 
#     df %>% 
#     do(get_marginal_estimates(df = .,C = C, D = D)) 
#   pooled_marginal_result_inf <- 
#     df_ri %>% 
#     group_by(replicate) %>% 
#     do(get_marginal_estimates(df = .,C = C, D = D)) 
#   pooled_marginal_result <- 
#     pooled_marginal_result_obs %>% 
#     left_join(get_quantile_ranking(obs = pooled_marginal_result_obs, inf = pooled_marginal_result_inf),"group") %>% 
#     left_join(get_pvalue(obs = pooled_marginal_result_obs, inf = pooled_marginal_result_inf),"group")
#   
#   pooled_marginal_result_formatted <- 
#     pooled_marginal_result %>% 
#     select(group, a_pooledl = value, b_p_value = p_value) %>% 
#     gather(row,value,-group) %>% 
#     arrange(group) %>% 
#     mutate(value = ifelse(grepl("^a_",row),format(round(value,4),nsmall=3),
#                           ifelse(value>0.01,paste0("(p=",format(round(value,2),nsmall=1),")"),
#                                  paste0("(p< 0.01)")))) %>% 
#     mutate(group = ifelse(grepl("^a_",row),insurance_lut[group],"")) %>% 
#     select(group,pooled=value)
#   
#   out <- 
#     list(marginal_result = pooled_marginal_result, 
#          marginal_result_formatted = pooled_marginal_result_formatted)
#   
#   return(out)
#   
# }
# 
# # Overall population totals (01_construct-and-calibrate-model.Rmd and R/estimate-acs.R)
# hicov_lut <- c("1" = "ESI",
#                "3" = "NG",
#                "2" = "PUB",
#                "4" = "UNIN")
# 
# wrap_string <- function(x,width) paste0(strwrap(x,width = width),collapse = "\n")
# qnorm01 <- function(x,...) pmax(0,pmin(1,qnorm(...)))
