rm(list = ls())
library(magrittr)

# set constant ----
# monte carlo simulation to get nice sample size identifying match effect
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
set.seed(1)

temp_data <-
  expand.grid(
    list_worker_id,
    list_category_id
  ) %>% 
  tibble::as_tibble() 
colnames(temp_data) <-
  c(
    "worker_id",
    "category_id"
  )

  
# generate data ----

generate_data <-
  function(
    list_worker_id,
    list_category_id,
    list_process,
    list_simulation
    ){
    data <-
      expand.grid(
        list_worker_id,
        list_category_id,
        list_process,
        list_simulation
      ) %>% 
      tibble::as_tibble() 
    data$error <-
      rnorm(dim(data)[1])
    colnames(data) <-
      c(
        "worker_id",
        "category_id",
        "process_id",
        "simulation_id",
        "error"
      )
    data <-
      data %>% 
      dplyr::group_by(
        worker_id,
        category_id
      ) %>% 
      dplyr::mutate(
        match_effect =
          #rnorm(1) + 10
          worker_id*category_id/3
      ) %>% 
      dplyr::ungroup()
    
    data <-
      data %>% 
      # worker FE = worker id
      # category FE = category id
      # match effect = worker_id*category_id/3
      dplyr::mutate(
        y = 
          worker_id + 
          category_id + 
          match_effect +
          error
      )
    return(data)
  }

## benchmark data ----
for(nn in 1:length(num_process_list)){
  temp_nn <-
    num_process_list[nn]
  list_process <-
    c(1:temp_nn)
  data <-
    generate_data(
      list_worker_id,
      list_category_id,
      list_process,
      list_simulation
    )
  filename <-
    paste(
      "output/match_effect_monte_carlo_data_",
      "num_process_",
      temp_nn,
      ".rds",
      sep = ""
    )
  cat(filename,"\n")
  saveRDS(
    data,
    file = filename)
}



