rm(list = ls())
gc()
gc()
library(magrittr)
library(tidyverse)
library(dplyr)

# library(intsvy)
# Variable: Path to .exp file

# load ----
school_background <- list()
student_background <- list()
student_teacher_linkage <- list()
teacher_background <- list()
for (filename in list.files(here::here(paste("input","T07_SPSS_G8_1",sep="/")))){
  if (startsWith(filename, "bcg")){
    bcg_tmp <- foreign::read.spss(here::here(paste("input","T07_SPSS_G8_1",filename,sep="/"))) %>% tibble::as.tibble()
    school_background <- school_background %>% bind_rows(bcg_tmp)
  } else if (startsWith(filename, "bsg")){
    bsg_tmp <- foreign::read.spss(here::here(paste("input","T07_SPSS_G8_1",filename,sep="/"))) %>% tibble::as.tibble()
    student_background <- student_background %>% bind_rows(bsg_tmp)
  } else if (startsWith(filename, "bst")){
    bst_tmp <- foreign::read.spss(here::here(paste("input","T07_SPSS_G8_1",filename,sep="/"))) %>% tibble::as.tibble()
    student_teacher_linkage <- student_teacher_linkage %>% bind_rows(bst_tmp)
  } else if (startsWith(filename, "bts")){
    bts_tmp <- foreign::read.spss(here::here(paste("input","T07_SPSS_G8_1",filename,sep="/"))) %>% tibble::as.tibble()
    teacher_background <- teacher_background %>% bind_rows(bts_tmp)
  }
}

rm(bcg_tmp,bsg_tmp,bst_tmp,bts_tmp)

# extract variable for replication ----

student_teacher_linkage <- student_teacher_linkage %>% 
  select(
    IDCNTRY, IDSCHOOL, IDCLASS, IDSTUD, IDTEACH, IDLINK, ITCOURSE, NSTEACH,
    BSSPHY01, BSSPHY02, BSSPHY03, BSSPHY04, BSSPHY05, # scores for physics
    BSSCHE01, BSSCHE02, BSSCHE03, BSSCHE04, BSSCHE05, # scores for chemistry
    BSSBIO01, BSSBIO02, BSSBIO03, BSSBIO04, BSSBIO05, # scores for biology
    BSSEAR01, BSSEAR02, BSSEAR03, BSSEAR04, BSSEAR05 # scores for earch science
  ) %>% 
  mutate(NSTEACH = as.numeric(NSTEACH)) %>% 
  rename(
    country_id = IDCNTRY, school_id = IDSCHOOL, class_id = IDCLASS, student_id = IDSTUD, 
    teacher_id = IDTEACH, linkage_id = IDLINK, course_name = ITCOURSE, n_teacher = NSTEACH,
    score_physics01 = BSSPHY01, score_physics02 = BSSPHY02, score_physics03 = BSSPHY03, score_physics04 = BSSPHY04, score_physics05 = BSSPHY05,
    score_chemistry01 = BSSCHE01, score_chemistry02 = BSSCHE02, score_chemistry03 = BSSCHE03, score_chemistry04 = BSSCHE04, score_chemistry05 = BSSCHE05,
    score_biology01 = BSSBIO01, score_biology02 = BSSBIO02, score_biology03 = BSSBIO03, score_biology = BSSBIO04, score_biology05 = BSSBIO05,
    score_earth01 = BSSEAR01, score_earth02 = BSSEAR02, score_earth03 = BSSEAR03, score_earth04 = BSSEAR04, score_earth = BSSEAR05
  )

teacher_background <- teacher_background %>% 
  select(
    IDCNTRY, IDSCHOOL, IDTEACH, ITCOURSE, IDLINK, BT4GSEX, BT4GTAUT, BT4GFEDC, BT4SSTUD,
    BT4SPSPH, BT4SPSCH, BT4SPSBI, BT4SPSES, # major
    BT4STT01, BT4STT02, BT4STT03, BT4STT04, BT4STT05, BT4STT06, BT4STT07, # preparation for bio
    BT4STT08, BT4STT09, BT4STT10, BT4STT11, BT4STT12, # preparation for chemistry
    BT4STT13, BT4STT14, BT4STT15, BT4STT16, BT4STT17, BT4STT18, # preparation for physics
    BT4STT19, BT4STT20, BT4STT21, BT4STT22, BT4STT23, # preparation for earth science
    BT4SCLSC, BT4SCCHE, BT4SCPHY, BT4SCESC, # time allocation for Bio, Che, Phy, Earth
    BT4STP01, BT4STP02, BT4STP03, BT4STP04, BT4STP05, BT4STP06, BT4STP07, BT4STP08, BT4STP09, BT4STP10, BT4STP11, BT4STP12, BT4STP13, BT4STP14, # topics taught in biology
    BT4STP15, BT4STP16, BT4STP17, BT4STP18, BT4STP19, BT4STP20, BT4STP21, BT4STP22, # taught topics in chemistry
    BT4STP23, BT4STP24, BT4STP25, BT4STP26, BT4STP27, BT4STP28, BT4STP29, BT4STP30, BT4STP31, BT4STP32, # taught topics in pysics
    BT4STP33, BT4STP34, BT4STP35, BT4STP36, BT4STP37, BT4STP38, BT4STP39, BT4STP40, BT4STP41, BT4STP42, BT4STP43, BT4STP44, BT4STP45, BT4STP46, # taught topics in earth sci
  ) %>% 
  mutate(
    majors_one = as.factor(ifelse(rowSums(across(c(BT4SPSPH, BT4SPSCH, BT4SPSBI, BT4SPSES), ~(as.numeric(ifelse(.x == "YES", 1, 0))))) == 1, "YES", "NO")),
    majors_two = as.factor(ifelse(rowSums(across(c(BT4SPSPH, BT4SPSCH, BT4SPSBI, BT4SPSES), ~(as.numeric(ifelse(.x == "YES", 1, 0))))) == 2, "YES", "NO")),
    majors_three = as.factor(ifelse(rowSums(across(c(BT4SPSPH, BT4SPSCH, BT4SPSBI, BT4SPSES), ~(as.numeric(ifelse(.x == "YES", 1, 0))))) == 3, "YES", "NO")),
    majors_four = as.factor(ifelse(rowSums(across(c(BT4SPSPH, BT4SPSCH, BT4SPSBI, BT4SPSES), ~(as.numeric(ifelse(.x == "YES", 1, 0))))) == 4, "YES", "NO")),
  ) %>% 
  mutate(teacher_education_year = as.numeric(BT4GTAUT)) %>% 
  mutate(across(starts_with("BT4STP"), ~(ifelse(as.numeric(.x)<=2, 1, 0)))) %>% 
  mutate(
    range_topics_biology = rowMeans(across(c(BT4STP01, BT4STP02, BT4STP03, BT4STP04, BT4STP05, BT4STP06, BT4STP07, BT4STP08, BT4STP09, BT4STP10, BT4STP11, BT4STP12, BT4STP13, BT4STP14))),
    range_topics_chemistry = rowMeans(across(c(BT4STP15, BT4STP16, BT4STP17, BT4STP18, BT4STP19, BT4STP20, BT4STP21, BT4STP22))),
    range_topics_physics = rowMeans(across(c(BT4STP23, BT4STP24, BT4STP25, BT4STP26, BT4STP27, BT4STP28, BT4STP29, BT4STP30, BT4STP31, BT4STP32))),
    range_topics_earth = rowMeans(across(c(BT4STP33, BT4STP34, BT4STP35, BT4STP36, BT4STP37, BT4STP38, BT4STP39, BT4STP40, BT4STP41, BT4STP42, BT4STP43, BT4STP44, BT4STP45, BT4STP46)))
  ) %>% 
  mutate(across(starts_with("BT4STT"), ~(as.factor(ifelse(as.numeric(.x)==2, 3, ifelse(as.numeric(.x)==3, 2, ifelse(as.numeric(.x)==1, 1, NA))))))) %>% 
  mutate(
    prepare_biology = rowMeans(across(c(BT4STT01, BT4STT02, BT4STT03, BT4STT04, BT4STT05, BT4STT06, BT4STT07), ~(as.numeric(.x)))),
    prepare_chemistry = rowMeans(across(c(BT4STT08, BT4STT09, BT4STT10, BT4STT11, BT4STT12), ~(as.numeric(.x)))),
    prepare_physics = rowMeans(across(c(BT4STT13, BT4STT14, BT4STT15, BT4STT16, BT4STT17, BT4STT18), ~(as.numeric(.x)))),
    prepare_earth = rowMeans(across(c(BT4STT19, BT4STT20, BT4STT21, BT4STT22, BT4STT23), ~(as.numeric(.x))))
  ) %>% 
  mutate(
    time_physics = as.numeric(BT4SCPHY)*0.01,
    time_chemistry = as.numeric(BT4SCCHE)*0.01,
    time_biology = as.numeric(BT4SCLSC)*0.01,
    time_earth = as.numeric(BT4SCESC)*0.01,
  ) %>% 
  mutate(graduate_degree = as.factor(ifelse(as.numeric(BT4GFEDC)==6, "YES", "NO"))) %>%
  mutate(num_student = as.numeric(BT4SSTUD)) %>% 
  select(
    IDCNTRY, IDSCHOOL, IDTEACH, ITCOURSE, IDLINK, BT4GSEX, teacher_education_year, num_student,
    graduate_degree, time_physics, time_chemistry, time_biology, time_earth,
    majors_one, majors_two, majors_three, majors_four,
    BT4SPSPH, BT4SPSCH, BT4SPSBI, BT4SPSES,
    range_topics_physics, range_topics_chemistry, range_topics_biology, range_topics_earth,
    prepare_physics, prepare_chemistry, prepare_biology, prepare_earth
  ) %>% 
  rename(
    country_id = IDCNTRY, school_id = IDSCHOOL, teacher_id = IDTEACH, course_id = ITCOURSE, linkage_id = IDLINK, teacher_sex = BT4GSEX, 
    major_physics = BT4SPSPH, major_chemistry = BT4SPSCH, major_bioligy = BT4SPSBI, major_earth = BT4SPSES
  )

school_background <- school_background %>% 
  select(
    IDCNTRY, IDSCHOOL, BC4GEENR
  ) %>%
  mutate(enrollment_grade8 = as.numeric(BC4GEENR)) %>% 
  rename(
    country_id = IDCNTRY, school_id = IDSCHOOL
  ) %>% 
  select(country_id, school_id, enrollment_grade8)

student_background <- student_background %>% 
  select(
    IDCNTRY, IDSCHOOL, IDCLASS, IDSTUD, ITSEX, BSDGEDUP
  ) %>% 
  mutate(
    across(c("IDCNTRY", "IDSCHOOL", "IDCLASS"), ~(as.numeric(as.character(.x)))),
  ) %>% 
  mutate(
    parent_education = as.factor(ifelse(as.numeric(BSDGEDUP) <= 2, "POST UNIV", "BELOW UNIV"))
  ) %>% 
  select(
    IDCNTRY, IDSCHOOL, IDCLASS, IDSTUD, ITSEX, parent_education
  ) %>% 
  rename(
    country_id = IDCNTRY, school_id = IDSCHOOL, class_id = IDCLASS, student_id = IDSTUD, student_sex = ITSEX
  )


data <- 
  student_teacher_linkage %>% 
  left_join(student_background, by = c("country_id", "student_id"), suffix=c("",".y")) %>% 
  left_join(school_background, by = c("country_id", "school_id"), suffix=c("",".y")) %>% 
  left_join(teacher_background, by = c("country_id", "teacher_id", "linkage_id"), suffix=c("",".y")) %>% 
  select(!ends_with(".y")) %>% 
  drop_na()

## filter for n_teacher == 1 for replication of Inoue and Tanaka (2023)----
data_n_teacher_1 <-
  data %>% 
  dplyr::filter(n_teacher == 1)

# save ----
saveRDS(
  data,
  file = here::here(paste("cleaned/","timss_data_2007",".rds",sep = ""))
)
saveRDS(
  data_n_teacher_1,
  file = here::here(paste("cleaned/","timss_data_n_teacher_1_2007",".rds",sep = ""))
)
