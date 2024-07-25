
rm(list = ls())
library(magrittr)

# set constant ----
num_agent <-
  20
num_process <-
  50
num_category <-
  10
num_simulation <-
  100
num_process_list <-
  c(
    10,
    20,
    30,
    40,
    50,
    100,
    500
  )
list_worker_id <-
  c(1:num_agent)
list_category_id <-
  c(1:num_category)
list_process <-
  c(1:num_process)
list_simulation <-
  c(1:num_simulation)

compute_bias <-
  function(
    match_effect_monte_carlo_data,
    target_simulation_id,
    target_estimated_coefficients
  ){
    match_effect_list <-
      match_effect_monte_carlo_data %>% 
      dplyr::filter(
        simulation_id == target_simulation_id
      ) %>% 
      dplyr::distinct(
        worker_id,
        category_id,
        match_effect
      )
    match11 <-
      match_effect_list[1,3]$match_effect
    match_effect_list_minus_match11 <-
      match_effect_list %>% 
      dplyr::mutate(
        match_effect_minus_match11 = 
          match_effect - match11 
      ) 
    match_effect_list_minus_match11_worker1 <-
      match_effect_list_minus_match11 %>% 
      dplyr::filter(
        worker_id == 1 &
          category_id != 1
      )
    match_effect_list_minus_match11_category1 <-
      match_effect_list_minus_match11 %>% 
      dplyr::filter(
        worker_id != 1 &
          category_id == 1
      )
    match_effect_list_minus_match11 <-
      match_effect_list_minus_match11 %>% 
      dplyr::left_join(
        match_effect_list_minus_match11_worker1 %>% 
          dplyr::select(
            category_id, 
            match_effect_minus_match11
          ) %>% 
          dplyr::rename(
            worker_relative_match_effect = match_effect_minus_match11
          ),
        by = c("category_id" = "category_id")
      ) %>% 
      dplyr::left_join(
        match_effect_list_minus_match11_category1 %>% 
          dplyr::select(
            worker_id, 
            match_effect_minus_match11
          ) %>% 
          dplyr::rename(
            category_relative_match_effect = match_effect_minus_match11
          ),
        by = c("worker_id" = "worker_id")
      )
    
    match_effect_list_minus_match11_drop_worker1_category1 <-
      match_effect_list_minus_match11 %>% 
      dplyr::filter(
        worker_id != 1 &
          category_id != 1
      )
    
    bias <-
      as.numeric(target_estimated_coefficients) -
      #as.numeric(estimation_result[[3]]$coefficients) -
      c(
        # constant = worker 1 + category 1 + match11
        1 + 1 + match11,
        # worker 2 relative FE = (worker 2 - worker 1) + (match21 - match11)
        (list_worker_id[2:num_agent] - 1) + 
          match_effect_list_minus_match11_category1$match_effect_minus_match11,
        # category 2 relative FE = (category 2 - category 1) + (match12 - match11)
        (list_category_id[2:num_category] - 1) + 
          match_effect_list_minus_match11_worker1$match_effect_minus_match11,
        # (worker2,category2)-match relative FE = (match22 + match11 - match12 - match21)
        match_effect_list_minus_match11_drop_worker1_category1$match_effect_minus_match11 -
          match_effect_list_minus_match11_drop_worker1_category1$category_relative_match_effect -
          match_effect_list_minus_match11_drop_worker1_category1$worker_relative_match_effect
      )
    return(bias)
  }

# load, estimate, compute bias, save ----
## benchmark data ----
for(nn in 1:length(num_process_list)){
  temp_nn <-
    num_process_list[nn]
  filename <-
    paste(
      "match_effect_monte_carlo_data_",
      "num_process_",
      temp_nn,
      #".rds",
      sep = ""
    )
  cat(filename,"\n")
  # load 
  target_data <-
    readRDS(
      file = 
        here::here(
          paste(
            "output/",
            filename,
            ".rds",
            sep = ""
          )
        )
    )
  # estimate 
  res <-
    target_data %>% 
    split(
      .$simulation_id
    ) %>% 
    purrr::map(
      ~ lfe::felm(
        formula = 
          as.formula(
            paste(
              "y ~", 
              "factor(worker_id)*factor(category_id)",
              "|",
              "0|",
              "0|",
              "0"
            )
          ),
        data = .x)
    ) %>% 
    purrr::map(summary)
  
  for(ss in 1:num_simulation){
    bias <-
      compute_bias(
        target_data,
        target_simulation_id = ss,
        target_estimated_coefficients =
          as.numeric(res[[ss]]$coefficients[,1])
      )
    t_stats <-
      res[ss][[1]]$coefficients[,3]
    beta_hat <-
      res[ss][[1]]$coefficients[,1]
    if(ss == 1){
      bias_table <-
        bias
      t_stats_table <-
        t_stats
      beta_hat_table <-
        beta_hat
    }else{
      bias_table <-
        rbind(
          bias_table,
          bias
        ) %>% 
        tibble::as_tibble()
      t_stats_table <-
        rbind(
          t_stats_table,
          t_stats
        ) %>% 
        tibble::as_tibble()
      beta_hat_table <-
        rbind(
          beta_hat_table,
          beta_hat
        ) %>% 
        tibble::as_tibble()
    }
  }
  colnames(bias_table) <-
    rownames(res[[ss]]$coefficients)
  colnames(t_stats_table) <-
    rownames(res[[ss]]$coefficients)
  colnames(beta_hat_table) <-
    rownames(res[[ss]]$coefficients)
  # save 
  saveRDS(
    bias_table,
    file = 
      paste(
        "output/",
        "bias_table_",
        "num_process_",
        temp_nn,
        ".rds",
        sep = ""
      )
  )
  saveRDS(
    t_stats_table,
    file = 
      paste(
        "output/",
        "t_stats_table_",
        "num_process_",
        temp_nn,
        ".rds",
        sep = ""
      )
  )
  saveRDS(
    beta_hat_table,
    file = 
      paste(
        "output/",
        "beta_hat_table_",
        "num_process_",
        temp_nn,
        ".rds",
        sep = ""
      )
  )
}

