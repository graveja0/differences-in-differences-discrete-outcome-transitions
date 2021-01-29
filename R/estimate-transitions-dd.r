# outcome <- quo(insurance_type)
# post = quo(post)
# z = quo(expansion_state)
# id = quo(idnumber)
# ref_cat = "UNIN"

estimate_DD <- function(df, id, outcome, post , z ,ref_cat ) {

   outcome <- enquo(outcome)
   post <- enquo(post)
   z <- enquo(z)
   id <- enquo(id) 
   
  category_names <- 
    df %>% 
    ungroup() %>% 
    select({{outcome}}) %>% 
    unique() %>% 
    pull({{outcome}})
  
  xx_orig <- xx <- 
    df  %>% 
    group_by({{id}}) %>% 
    filter(n()==2) %>% 
    add_binary_indicators({{outcome}}) %>% 
    #select(-{{outcome}})  %>% 
    rename_at(vars(starts_with(quo_name(outcome))),~gsub(paste0(quo_name(outcome),"_"),"",.)) %>% 
    mutate(z = {{z}},
           post = {{post}}, 
           post_z = {{post}} * {{z}}) %>% 
    left_join(
      df %>% 
        filter({{post}}==0) %>% 
        select({{id}},{{outcome}}) %>% 
        rename(pre = {{outcome}}) %>% 
        mutate(pre = factor(pre)), quo_name(id)
    )

  # Outcome matrix (binary indicators for each coverage category)
  Y <- 
    xx_orig[,category_names] %>% 
    as.matrix()
  
  # Define J-1 outcomes and structure the data with pre-period indicators
  Y_ <- 
    xx_orig %>% 
    filter({{post}} == 1) %>% 
    select_at(all_of(category_names[-grep(ref_cat,category_names)]))  %>% 
    as.matrix()

  y_pre <- 
    xx %>% 
    filter({{post}} == 0) %>% 
    select_at(vars({{id}},all_of(category_names)))

  y_pre_z <- 
    xx %>% 
    select(-post_z) %>% 
    filter({{post}} == 0 ) %>% 
    mutate(z = {{z}}) %>% 
    mutate_at(all_of(category_names),~as.integer((. & .data$z))) %>% 
    rename_at(vars(all_of(category_names)),~(paste0(.,"_z"))) %>% 
    select({{id}},ends_with("_z"))

  xx_ <- 
    y_pre %>% 
    inner_join(y_pre_z,quo_name(id))  %>% 
    select(-quo_name(id))
  
  ###----------------
  ### FIT MODELS
  ###----------------
  m_marginal <- 
    lm(Y ~ xx$z + xx$post + xx$post_z)
  
  # m1 <-
  #   lm(Y~xx$z + xx$post + xx$post_z + xx$z*xx$pre + xx$post*xx$pre + xx$post_z*xx$pre)

  m_transitions <-
    lm(Y_ ~ . -1, data = xx_)
  
  ###------------------
  ### PREDICTED VALUES
  ###------------------
  
  xx <- 
    category_names %>% 
    map_df(~({
      data.frame(post = c(1,1), z = c(1,0)) %>% 
        mutate(post_z = post * z)  %>% 
        mutate(pre = .x)
    })) %>% 
    mutate(pre = factor(pre)) 
  
  p_hat <- 
    xx_orig %>% 
    filter(post==0) %>% 
    select(z,{{outcome}}) %>% 
    add_binary_indicators({{outcome}}) %>% 
    select(-{{outcome}}) %>% 
    rename_at(vars(starts_with(paste0(quo_name(outcome),"_"))),~gsub(paste0(quo_name(outcome),"_"),"",.)) %>% 
    group_by(z) %>% 
    summarise_all(mean) %>% 
    gather(pre,p,-z)
  
  # predicted_values_m1 <-
  #   cbind(xx,predict(m1,newdata =xx)) %>%
  #   left_join(p_hat,c("pre","z")) %>%
  #   data.frame() %>%
  #   mutate(pre = factor(pre,levels = c(category_names))) %>%
  #   arrange(z,pre) %>%
  #   select(-post,-post_z) %>%
  #   select(z,pre,p,all_of(category_names))
  # hat_R_0_m1 <-
  #   predicted_values_m1 %>%
  #   filter(z==0) %>%
  #   data.frame() %>%
  #   select(-z,-p) %>%
  #   column_to_rownames(var = "pre") %>%
  #   as.matrix
  # hat_R_1_m1 <-
  #   predicted_values_m1 %>%
  #   filter(z==1) %>%
  #   data.frame() %>%
  #   select(-z,-p) %>%
  #   column_to_rownames(var = "pre") %>%
  #   as.matrix()
  # hat_p_1_m1 <-
  #   predicted_values_m1 %>%
  #   filter(z==1) %>%
  #   data.frame() %>%
  #   pull(p) %>%
  #   set_names(category_names)
  # hat_p_0_m1 <-
  #   predicted_values_m1 %>%
  #   filter(z==0) %>%
  #   data.frame() %>%
  #   pull(p) %>%
  #   set_names(category_names)

  xx_ <- 
    category_names %>% 
    map_df(~({
      data.frame(z = c(1,0)) %>% 
        mutate(pre = .x)
    })) %>% 
    add_binary_indicators(pre) %>% 
    rename_at(vars(starts_with("pre_")),~gsub("pre_","",.)) %>% 
    mutate_at(vars(all_of(category_names)),list(z = ~(.data$z * .))) 
  
  predicted_values_m_transitions <- 
    cbind(xx_[,c("z","pre")],predict(m_transitions,newdata =xx_)) %>% 
    left_join(p_hat,c("pre","z")) %>% 
    data.frame() %>% 
    mutate(pre = factor(pre,levels = c(category_names))) %>% 
    arrange(z,pre) %>% 
    select(z,pre,p,everything())
  predicted_reference = 1-Matrix::rowSums(predicted_values_m_transitions[,category_names[-which(category_names==ref_cat)]])
  predicted_values_m_transitions[[ref_cat]] <-  predicted_reference
  
  hat_R_0_m_transitions <- 
    predicted_values_m_transitions %>% 
    filter(z==0) %>% 
    data.frame() %>% 
    select(-z,-p) %>% 
    column_to_rownames(var = "pre") %>% 
    as.matrix
  hat_R_0_m_transitions <- hat_R_0_m_transitions[category_names,category_names]
  
  hat_R_1_m_transitions <- 
    predicted_values_m_transitions %>% 
    filter(z==1) %>% 
    data.frame() %>% 
    select(-z,-p) %>% 
    column_to_rownames(var = "pre") %>% 
    as.matrix()
  hat_R_1_m_transitions <-   hat_R_1_m_transitions[category_names,category_names]
  
  hat_p_1_m_transitions <-
    predicted_values_m_transitions %>% 
    filter(z==1) %>% 
    data.frame() %>% 
    pull(p) %>% 
    set_names(category_names)
  hat_p_0_m_transitions <-
    predicted_values_m_transitions %>% 
    filter(z==0) %>% 
    data.frame() %>% 
    pull(p) %>% 
    set_names(category_names)
  
  ### CONSTRUCT ESTIMANDS
  (att_marginal <-  coef(m_marginal)["xx$post_z",])
  #(att_A_m1 <- (hat_p_1_m1 %*% hat_R_1_m1 - hat_p_1_m1) - (hat_p_0_m1 %*% hat_R_0_m1 - hat_p_0_m1))
  (att_marginal_transitions <- (hat_p_1_m_transitions %*% hat_R_1_m_transitions - hat_p_1_m_transitions) - (hat_p_0_m_transitions %*% hat_R_0_m_transitions - hat_p_0_m_transitions) %>% as.vector())
  
  #R_DD_m1 <- hat_R_1_m1 - hat_R_0_m1
  att_transitions <- hat_R_1_m_transitions - hat_R_0_m_transitions
  
  #att_C_m_transitions <- hat_p_1_m_transitions %*% (R_DD_m_transitions)
  
  out <- 
    list(
      att_marginal = t(att_marginal) %>% data.frame() %>% set_rownames("marginal"),
      att_marginal_transitions = att_marginal_transitions %>% data.frame() %>% set_rownames("marginal_transitions"),
      att_transtiions = att_transitions %>% data.frame()
    )
  out %>% 
    bind_rows() %>% 
    rownames_to_column(var = "model")
  
}
