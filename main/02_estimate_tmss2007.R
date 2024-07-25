rm(list = ls())
gc()
gc()

library(magrittr)
#library(dplyr)
library(tidyverse)
library(stringr)
library(restriktor)

# load ----
timss_data_n_teacher_1_2007 <-
  readRDS(
    file = 
      here::here(
        paste(
          "cleaned/",
          "timss_data_n_teacher_1_2007",
          ".rds",
          sep = ""
        )
      )
  )
timss_data_2007 <-
  readRDS(
    file = 
      here::here(
        paste(
          "cleaned/",
          "timss_data_2007",
          ".rds",
          sep = ""
        )
      )
  )
# make constraint ----
make_constraint <-
  function(target_data){
    constraints_teacher <- FALSE
    for (x in target_data %>% select(teacher_id) %>% unique() %>% pull()){
      const <- FALSE
      for (y in target_data %>% filter(teacher_id == x) %>% select(student_id) %>% unique() %>% pull()){
        if (const != FALSE){
          const <- paste(const, sprintf("student_teacher%s_%s", y, x), sep = " + ")
        } else{
          const <- sprintf("student_teacher%s_%s", y, x)
        }
      }
      const <- paste(const, "= 0")
      if (constraints_teacher != FALSE){
        constraints_teacher <- paste(constraints_teacher, const, sep = " & ")
      } else {
        constraints_teacher <- const
      }
    }
    
    constraints_student <- FALSE
    for (x in target_data %>% select(student_id) %>% unique() %>% pull()){
      const <- FALSE
      for (y in target_data %>% filter(student_id == x) %>% select(teacher_id) %>% unique() %>% pull()){
        if (const != FALSE){
          const <- paste(const, sprintf("student_teacher%s_%s", x, y), sep = " + ")
        } else{
          const <- sprintf("student_teacher%s_%s", x, y)
        }
      }
      const <- paste(const, "= 0")
      if (constraints_student != FALSE){
        constraints_student <- paste(constraints_student, const, sep = " & ")
      } else {
        constraints_student <- const
      }
    }
    
    constraints <- 
      paste(
        constraints_teacher,
        constraints_student, 
        sep = " & "
      )
    return(constraints)
  }
## ----
# transform ----
## timss_data_n_teacher_1_2007 ----
target_data_n_teacher_1 <- 
  timss_data_n_teacher_1_2007 %>% 
  pivot_longer(
    starts_with("score")
  ) %>% 
  mutate(
    subfield = 
      as.factor(
        ifelse(
          str_detect(name,"physics"), 
          "physics", 
          ifelse(
            str_detect(name, "chemistry"), 
            "chemistry", 
            ifelse(
              str_detect(name,"biology"),
              "biology", 
              "earth"
              )
            )
          )
        ),
    major_match = 
      ifelse(
        (
          (str_detect(name,"physics"))&(major_physics=="YES")|
            (str_detect(name,"chemistry"))&(major_chemistry=="YES")|
            (str_detect(name,"biology"))&(major_bioligy=="YES")|
            (str_detect(name,"earth"))&(major_earth=="YES")
          ),
        1,
        0
        ),
    student_teacher_id = 
      as.factor(
        paste(
          as.character(country_id),
          as.character(student_id),
          as.character(teacher_id),
          sep="_"
          )
        )
  )
## timss_data_2007 ----
target_data <- 
  timss_data_2007 %>% 
  pivot_longer(
    starts_with("score")
  ) %>% 
  mutate(
    subfield = 
      as.factor(
        ifelse(
          str_detect(name,"physics"), 
          "physics", 
          ifelse(
            str_detect(name, "chemistry"), 
            "chemistry", 
            ifelse(
              str_detect(name,"biology"),
              "biology", 
              "earth"
            )
          )
        )
      ),
    major_match = 
      ifelse(
        (
          (str_detect(name,"physics"))&(major_physics=="YES")|
            (str_detect(name,"chemistry"))&(major_chemistry=="YES")|
            (str_detect(name,"biology"))&(major_bioligy=="YES")|
            (str_detect(name,"earth"))&(major_earth=="YES")
        ),
        1,
        0
      ),
    student_teacher_id = 
      as.factor(
        paste(
          as.character(country_id),
          as.character(student_id),
          as.character(teacher_id),
          sep="_"
        )
      ),
    student_teacher = 
      as.factor(
        paste(
          student_id, 
          teacher_id,
          sep = "_"
        )
      )
  ) 



temp <-
  target_data %>% 
  dplyr::group_by(
    school_id,
    student_id
  ) %>% 
  dplyr::summarise(
    #unique_student_id_count = n_distinct(student_id),
    unique_teacher_id_count = n_distinct(teacher_id)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(unique_teacher_id_count >= 5) %>% 
  dplyr::group_by(
    school_id
  ) %>% 
  dplyr::mutate(
    unique_student_id_count = n_distinct(student_id)
  ) %>% 
  dplyr::ungroup()
temp
temp %>% 
  dplyr::filter(
    school_id == 228
  )
# focus on n_teacher >= 5
target_data_school_id_228 <-
  target_data %>% 
  dplyr::filter(
    school_id == 228
  ) 

# make formula ----
covariate_interest_list <-
  paste(
    c(
      "major_match",
      "factor(subfield)",
      "factor(country_id)"
      ),
    collapse = "+"
    )
covariate_interest_list_subsample <-
  paste(
    c(
      "major_match",
      "factor(subfield)"#,
      #"factor(country_id)"
    ),
    collapse = "+"
  )
covariate_student_list <-
  paste(
    c(
    "factor(student_sex)",
    "factor(parent_education)"
  ),
  collapse = "+"
  )
covariate_teacher_list <-
  paste(
    c(
      "enrollment_grade8",
      "I(enrollment_grade8^2)",
      "I(enrollment_grade8^3)",
      "teacher_education_year",
      "I(teacher_education_year^2)",
      "num_student",
      "I(num_student^2)",
      "I(num_student^3)"
    ),
    collapse = "+"
  )
## whole sample ----
formula_list_whole_sample <-
  c(
    #standard errors are clustered at class level
    # Table 2 (1)
    as.formula(
      paste(
        "value ~ -1 +", 
        covariate_interest_list,
        "|",
        "class_id|",
        "0|",
        "0"
      )
    ),
    # # Table 2 (2)
    as.formula(
      paste(
        "value ~ -1 +",
        covariate_interest_list,
        "+",
        covariate_student_list,
        "|",
        "class_id|",
        "0|",
        "0"
      )
    ),
    # Table 2 (3)
    as.formula(
      paste(
        "value ~ -1 +",
        covariate_interest_list,
        "+",
        covariate_student_list,
        "+",
        covariate_teacher_list,
        "|",
        "class_id|",
        "0|",
        "0"
      )
    )#,
    # # Table 2 (4)
    # as.formula(
    #   paste(
    #     "value ~ -1 +", 
    #     covariate_interest_list,
    #     "+",
    #     "student_teacher_id",
    #     "|",
    #     "class_id|",
    #     "0|",
    #     "0"
    #   )
    # )
  )
## subsample ----
formula_list_subsample <-
  c(
    #standard errors are clustered at class level
    # Table 2 (1)
    as.formula(
      paste(
        "value ~ -1 +", 
        covariate_interest_list_subsample,
        "|",
        "class_id|",
        "0|",
        "0"
      )
    ),
    # # Table 2 (2)
    as.formula(
      paste(
        "value ~ -1 +",
        covariate_interest_list_subsample,
        "+",
        covariate_student_list,
        "|",
        "class_id|",
        "0|",
        "0"
      )
    ),
    # Table 2 (3)
    as.formula(
      paste(
        "value ~ -1 +",
        covariate_interest_list_subsample,
        "+",
        covariate_student_list,
        "+",
        covariate_teacher_list,
        "|",
        "class_id|",
        "0|",
        "0"
      )
    ),
    # Table 2 (4)
    as.formula(
      paste(
        "value ~ -1 +",
        covariate_interest_list_subsample,
        "+",
        "factor(student_id):factor(teacher_id)",
        "|",
        "class_id|",
        "0|",
        "0"
      )
    ),
    # for minimal model
    as.formula(
      paste(
        "value ~ -1 +",
        "major_match",
        "+",
        "factor(student_id):factor(teacher_id)",
        "|",
        "class_id|",
        "0|",
        "0"
      )
    )
  )
### with constraint ----
formula_for_constraint = as.formula(
  paste(
    "value ~ -1 + major_match",
    "+",
    "student_teacher"
  )
)

# estimate ----
## estimate whole sample like Inoue and Tanaka ----
### n_teacher = 1 ----

result_benchmark_n_teacher_1_list <-
  formula_list_whole_sample %>% 
  purrr::map(
    ~ lfe::felm(
      formula = .,
      data = target_data_n_teacher_1)
  )
modelsummary::modelsummary(result_benchmark_n_teacher_1_list)

### n_teacher >= 1 ----
result_benchmark_list <-
  formula_list_whole_sample %>% 
  purrr::map(
    ~ lfe::felm(
      formula = .,
      data = target_data)
  )
modelsummary::modelsummary(result_benchmark_list)

## estimate subsample ----
### n_teacher >= 1 ----
result_benchmark_list_school_id_228 <-
  formula_list_subsample %>% 
  purrr::map(
    ~ lfe::felm(
      formula = .,
      data = target_data_school_id_228)
  )
modelsummary::modelsummary(result_benchmark_list_school_id_228)

### with restriction ----
constraints <-
  make_constraint(
    target_data = target_data_school_id_228
    )

fit.unrestricted <- 
  lm(
    formula = formula_for_constraint, 
    data = target_data_school_id_228 
    )
fit.constrained <- 
  restriktor(
    fit.unrestricted, 
    constraints = constraints
    )
summary(fit.constrained)

# save ----
saveRDS(
  result_benchmark_n_teacher_1_list,
  file = here::here(paste("output/","result_benchmark_n_teacher_1_list",".rds",sep = ""))
)
saveRDS(
  result_benchmark_list,
  file = here::here(paste("output/","result_benchmark_list",".rds",sep = ""))
)
saveRDS(
  result_benchmark_list_school_id_228,
  file = here::here(paste("output/","result_benchmark_list_school_id_228",".rds",sep = ""))
)
saveRDS(
  fit.constrained,
  file = here::here(paste("output/","result_school_id_228_constrained",".rds",sep = ""))
)
