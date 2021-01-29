#' Structure data for cluster permutation-based inference.
#'
#' This function takes an input datase indexes it m times
#' according to randomly permuting treatment status at a defined cluster
#' level.  For example, the cluster level could be set to the individual, which
#' would correspond to standard randomization-based inference. Alternatively,
#' the cluster could be defined at some higher level of intervention (e.g., the state).
#'
#' In cases with a small number of overall (N) and treated clusters (N_Tx) it could be
#' the case that the number of permutations (m) is greater than (N choose Tx).  This functionality
#' has not yet been built into the function, but will be a focus of future work.
#'
#' @param df The input dataset (from structure_data_for_dd()).
#' @param m The number of permutations.
#' @param treatment The treatment variable.
#' @param cluster The cluster variable (treatment status will be randomly permuted at this level).
#' @param idvar Unique identifier.
#'
#' @return A grouped data from with attributes that guide the permutation procedure.
#' @export
#'

ri_permute <- function(df , m, treatment , cluster , idvar,  outcome, quietly=TRUE)  {
  
  n_df <- nrow(df)
  tx <- enquo(treatment)
  yy <- enquo(outcome)
  tx_orig <- paste0(quo_name(tx),"_orig")
  yy_orig <- paste0(quo_name(yy),"_orig")
  cc <- enquo(cluster)
  id <- enquo(idvar)
  
  
  df <- df %>%
    # The original version of the Tx variable.
    mutate(!!tx_orig := !!tx)
  
  # Overall number of clusters
  n <- df %>% select(!!tx , !!cc) %>% unique() %>% nrow()
  n_times_2 <- n * 2
  n_tx <- df %>% select(!!tx,!!cc) %>% filter(!!tx == 1) %>% unique() %>% nrow()
  
  # Ultimate output is a stacked data frame with indexed attributes
  # This is the ultmate output -- a stacked longitudinal data frame.
  df_0 <- df %>%
    # The original version of the Tx variable.
    mutate(!!tx_orig := !!tx) %>%
    mutate(!!yy_orig := !!yy) %>% 
    # In this half of the data, all observations assigned to Cx.
    mutate(!!quo_name(tx) := 0) %>% 
    mutate(!!quo_name(yy) := !!yy)
  
  df_1 <- df %>%
    # The original version of the Tx variable.
    mutate(!!tx_orig := !!tx) %>%
    mutate(!!yy_orig := !!yy) %>% 
    # In this half of the data, all observations assigned to Tx.
    mutate(!!quo_name(tx) := 1) %>% 
    mutate(!!quo_name(yy) := !!yy )
  
  # We then stack the data, obtaining a dataset of N*2
  df_stacked <- bind_rows(df_1,df_0) %>%
    as_tibble() %>%
    # This is used to track individuals -- see comment below.
    mutate(row_number = row_number())
  
  # Now we need a new dataset, at the level of the cluster, specifying which
  # rows of the df_stacked correspond to each cluster.
  df_row_mapping <-
    df_stacked %>%
    select(!!id,!!cc,!!tx,row_number)
  
  # Now a cluster-level dataset
  df_c_0 = df %>% select(!!cc,!!tx) %>%
    unique() %>%
    # The original version of the Tx variable.
    mutate(!!tx_orig := !!tx) %>%
    # In this half of the data, all observations assigned to Cx.
    mutate(!!quo_name(tx) := 0)
  
  df_c_1 = df %>% select(!!cc,!!tx) %>%
    unique() %>%
    # The original version of the Tx variable.
    mutate(!!tx_orig := !!tx) %>%
    # In this half of the data, all observations assigned to Cx.
    mutate(!!quo_name(tx) := 1)
  
  df_c_stacked <-
    bind_rows(df_c_1,df_c_0) %>%
    as_tibble()
  
  sampled_index <- function(N=n,N_tx=n_tx,N_Times_2 = n_times_2)
  {
    
    sampled_tx_observations_c <- sample(1:N,N_tx,replace=FALSE)
    sampled_cx_observations_c <- seq(N+1,N_Times_2,1)[-sampled_tx_observations_c]
    
    sampled_tx_observations <-
      df_c_stacked[sampled_tx_observations_c,]  %>% select(!!cc,!!tx) %>%
      left_join(df_row_mapping,c(quo_name(cc),quo_name(tx))) %>%
      pull(row_number)
    
    sampled_cx_observations <-
      df_c_stacked[sampled_cx_observations_c,]  %>% select(!!cc,!!tx) %>%
      left_join(df_row_mapping,c(quo_name(cc),quo_name(tx))) %>%
      pull(row_number)
    
    out <- c(sampled_tx_observations,sampled_cx_observations)
    out
  }
  
  sampled_index_exact <- function(N = n , N_tx = n_tx , N_Times_2 = n_times_2) {
    tx_vec <- c(rep(1,N_tx),rep(0,N - N_tx))
    possible <- ri::genperms(tx_vec)
    
    sampled_index_exact_tmp <- function(i) {
      sampled_tx_observations_c = which(possible[,i]==1) %>% unname()
      sampled_cx_observations_c = seq(N+1,N_Times_2,1)[-sampled_tx_observations_c]
      
      sampled_tx_observations <-
        df_c_stacked[sampled_tx_observations_c,]  %>% select(!!cc,!!tx) %>%
        left_join(df_row_mapping,c(quo_name(cc),quo_name(tx))) %>%
        pull(row_number)
      
      sampled_cx_observations <-
        df_c_stacked[sampled_cx_observations_c,]  %>% select(!!cc,!!tx) %>%
        left_join(df_row_mapping,c(quo_name(cc),quo_name(tx))) %>%
        pull(row_number)
      out <- c(sampled_tx_observations,sampled_cx_observations) - 1
      out
    }
    if (!quietly) cat(paste0("Permutating based on ",ncol(possible)," possible treatment combinations\n"))
    out <- 1:ncol(possible) %>% map(~sampled_index_exact_tmp(.x))
    out
  }
  
  blah <- df_stacked
  
  attr(blah,"groups") <- list(.rows = replicate(m, sampled_index() - 1, simplify = FALSE))
  
  if (m != "exact") {
    # #attr(df_stacked, "indices") <- replicate(m, sampled_index() - 1, simplify = FALSE)
    # attr(df_stacked, "drop") <- TRUE
    # attr(df_stacked, "group_sizes") <- rep(n_df, m)
    # attr(df_stacked, "biggest_group_size") <- n_df
    # attr(df_stacked, "labels") <- data.frame(replicate = 1:m)
    # attr(df_stacked, "vars") <- list(quote(replicate))
    # class(df_stacked) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
    
    attr(df_stacked,"groups") <- tbl_df(list(.rows = replicate(m, sampled_index() - 1, simplify = FALSE)) ) %>% mutate(replicate = 1:m) %>% 
      select(replicate,.rows)
    attr(df_stacked, "drop") <- TRUE
    class(df_stacked) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
    
    
  } else {
    foo <- sampled_index_exact()
    #attr(df_stacked, "indices") <-  foo
    attr(df_stacked,"groups") <- foo
    attr(df_stacked, "drop") <- TRUE
    attr(df_stacked, "group_sizes") <- rep(n_df, choose(n,n_tx))
    attr(df_stacked, "biggest_group_size") <- n_df
    attr(df_stacked, "labels") <- data.frame(replicate = 1:choose(n,n_tx))
    attr(df_stacked, "vars") <- list(quote(replicate))
    class(df_stacked) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  
  
  
  
  df_stacked
}

