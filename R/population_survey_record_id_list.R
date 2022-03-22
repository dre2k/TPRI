# =========================================================================== #
# 'fix' pop_record_id
#
# 1) main question -- how are pop_record_ids assigned???
# 
# the problem: sometimes the same individual has different pop_record_id 
# between waves? 
# 
# I think when prioritizing which duplicate entry to keep, sometimes in between waves
# a different pop_record_id entry would be inadvertently chosen 
# for example - wave 1 i choose the earlier entry, wave 2 the later. 
# So although these are the same individual, they would have different pop_record_id
#
#
# One solution i came up with is 'fixing' pop_record_id such that for duplicate 
# individuals, I would account for repeats and only chose one as the master pop_record_id for that person
# ============================================================================ #

library(tidyverse)
library(data.table)
library(glue)
library(janitor)
library(stringr)
library(lubridate)

date <- "2022_03_02"
date <- "2022_03_21"

# population survey 
export_population_survey <- function(category = c("student", "staff"), report_id) {
  match.arg(category)
  
  url = "https://redcap.med.usc.edu/api/"
  token = scan(glue("data/population_survey_{category}_tkn.txt"), what = 'character')
  formData = list("token"=token, content='report', format='csv', report_id=report_id, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')
  response = httr::POST(url, body = formData, encode = "form")
  httr::content(response, guess_max = 10000) %>% 
    mutate(usc_email = str_to_lower(usc_email), 
           name = str_to_title(name), 
           dob = paste(birth_month, birth_day, birth_year, sep = "/"),
           dob = as.Date(dob, format = "%m/%d/%Y"),) # %>% 
  # rename_with( ~ 'pop_uscid', any_of(c("pop_student_id", "pop_usc_id"))) %>%
  # rename_with( ~ 'tpri_health_survey_timestamp', any_of(c("consent_timestamp")))
}


# --------------------------------------------------------------------------- #
# student ----
# --------------------------------------------------------------------------- #

population_student <- export_population_survey("student", 17173)
population_student <- population_student %>% 
  # dplyr::select(contains("email"))
  mutate(pop_email = str_to_lower(coalesce(hipaa_email, hipaa_email_v2))) %>% 
  filter(grepl('usc.edu', pop_email))

# ---- get a list of pop_record_id vectors, where each list item is an individual ---- 

# duplicate uscid 
a <- get_dupes(population_student %>% filter(!is.na(student_id)), student_id) %>% 
  dplyr::select(student_id, record_id) %>% 
  group_by(student_id) %>% 
  mutate(row = letters[row_number()]) %>% 
  pivot_wider(values_from = record_id, names_from = row)

alist <- a %>% 
  rowwise %>%
  mutate(C = list(c(a,b,c,d,e))) %>% 
  ungroup() %>% pull(C)

a1 <- do.call(c, alist)
a1 <- a1[!is.na(a1)]


# duplicate email
b <- get_dupes(population_student %>% 
                 filter(!is.na(pop_email), 
                        !record_id %in% a1), pop_email) %>% 
  dplyr::select(pop_email, record_id) %>% 
  group_by(pop_email) %>% 
  mutate(row = letters[row_number()]) %>% 
  pivot_wider(values_from = record_id, names_from = row)


blist <- b %>% 
  rowwise %>%
  mutate(C = list(c(a,b,c,d))) %>% 
  ungroup() %>% pull(C)

b1 <- do.call(c, blist)
b1 <- b1[!is.na(b1)]



# duplicate name + dob (?) 
# (don't worry about the issue of choosing the right pop_record_id based on 
# availability of uscid since they're supposed to enter that info again in the health survey)
cc <- get_dupes(population_student %>% 
                 filter(!is.na(name),
                        !is.na(dob),
                        !record_id %in% a1, 
                        !record_id %in% b1), name, dob) %>% 
  dplyr::select(name, dob, record_id) %>% 
  group_by(name, dob) %>% 
  mutate(row = letters[row_number()]) %>% 
  pivot_wider(values_from = record_id, names_from = row)


clist <- cc %>% 
  rowwise %>%
  mutate(C = list(c(a,b))) %>% 
  ungroup() %>% pull(C)

c1 <- do.call(c, clist)
c1 <- c1[!is.na(c1)]


remain <- filter(population_student, 
                 !record_id %in% a1, 
                 !record_id %in% b1, 
                 !record_id %in% c1)


# so can you create a list of every 'unique' individual vectors of record_id .. .

pop_survey_student_record_id_list <- append(alist, blist) %>% 
  append(clist) %>% 
  append(as.list(remain$record_id))
pop_survey_student_record_id_list <- sapply(pop_survey_student_record_id_list, function(x) x[!is.na(x)])

saveRDS(pop_survey_student_record_id_list, file = glue("data/pop_survey_student_record_id_list_{date}.rds"))











# --------------------------------------------------------------------------- #
# staff ----
# --------------------------------------------------------------------------- #
export_population_survey <- function(category = c("student", "staff"), report_id) {
  match.arg(category)
  
  url = "https://redcap.med.usc.edu/api/"
  token = scan(glue("data/population_survey_{category}_tkn.txt"), what = 'character')
  formData = list("token"=token, content='report', format='csv', report_id=report_id, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')
  response = httr::POST(url, body = formData, encode = "form")
  httr::content(response, guess_max = 10000) %>% 
  mutate(usc_email = str_to_lower(usc_email),
         name = str_to_title(name),
         dob = paste(birth_month, birth_day, birth_year, sep = "/"),
         dob = as.Date(dob, format = "%m/%d/%Y"),) # %>%
  # rename_with( ~ 'pop_uscid', any_of(c("pop_student_id", "pop_usc_id"))) %>%
  # rename_with( ~ 'tpri_health_survey_timestamp', any_of(c("consent_timestamp")))
}




population_staff <- export_population_survey("staff", 17174)
population_staff <- population_staff %>% 
  # dplyr::select(contains("email"))
  mutate(pop_email = str_to_lower(coalesce(hipaa_email, hipaa_email_v2))) %>% 
  filter(grepl('usc.edu', pop_email)) %>% 
  mutate(usc_id = case_when(usc_id == "000000000" ~ NA_character_, 
                            usc_id == "N/A" ~ NA_character_, 
                            TRUE ~ usc_id))

# ---- get a list of pop_record_id vectors, where each list item is an individual ---- 

# duplicate uscid 
a <- get_dupes(population_staff %>% filter(!is.na(usc_id)), usc_id) %>% 
  dplyr::select(usc_id, record_id) %>% 
  group_by(usc_id) %>% 
  mutate(row = letters[row_number()]) %>% 
  pivot_wider(values_from = record_id, names_from = row)

alist <- a %>% 
  rowwise %>%
  mutate(C = list(c(a,b,c,d,e,f,g,h,i,j,k))) %>% 
  ungroup() %>% pull(C)

a1 <- do.call(c, alist)
a1 <- a1[!is.na(a1)]


# duplicate email
b <- get_dupes(population_staff %>% 
                 filter(!is.na(pop_email), 
                        !record_id %in% a1), pop_email) %>% 
  dplyr::select(pop_email, record_id) %>% 
  group_by(pop_email) %>% 
  mutate(row = letters[row_number()]) %>% 
  pivot_wider(values_from = record_id, names_from = row)


blist <- b %>% 
  rowwise %>%
  mutate(C = list(c(a,b,c))) %>% 
  ungroup() %>% pull(C)

b1 <- do.call(c, blist)
b1 <- b1[!is.na(b1)]



# duplicate name + dob (?) 
# (don't worry about the issue of choosing the right pop_record_id based on 
# availability of uscid since they're supposed to enter that info again in the health survey)
cc <- get_dupes(population_staff %>% 
                  filter(!is.na(name),
                         !is.na(dob),
                         !record_id %in% a1, 
                         !record_id %in% b1), name, dob) %>% 
  dplyr::select(name, dob, record_id) %>% 
  group_by(name, dob) %>% 
  mutate(row = letters[row_number()]) %>% 
  pivot_wider(values_from = record_id, names_from = row)


clist <- cc %>% 
  rowwise %>%
  mutate(C = list(c(a,b))) %>% 
  ungroup() %>% pull(C)

c1 <- do.call(c, clist)
c1 <- c1[!is.na(c1)]


remain <- filter(population_staff, 
                 !record_id %in% a1, 
                 !record_id %in% b1, 
                 !record_id %in% c1)


# so can you create a list of every 'unique' individual vectors of record_id .. .

pop_survey_staff_record_id_list <- append(alist, blist) %>% 
  append(clist) %>% 
  append(as.list(remain$record_id))
pop_survey_staff_record_id_list <- sapply(pop_survey_staff_record_id_list, function(x) x[!is.na(x)])

saveRDS(pop_survey_staff_record_id_list, file = glue("data/pop_survey_staff_record_id_list_{date}.rds"))











# - testing - #
# # so now... let's take example w1 and try to 'fix' ids. 
# 
# w1_test <- data.frame(record_id = w1$pop_record_id, 
#                       fakevar = rnorm(nrow(w1), mean = 0, 1))
# 
# #summary record_id
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 1    2614    6456    5854    8777   11616
# 
# tmp <- function(num, ele) {
#   if(num %in% ele) {min(ele, na.rm = T)} else {0}
# }
# 
# tmp2 <- function(num2) {
#   max(sapply(zz, tmp, num = num2))
# }
# 
# dude <- max(sapply(zz, tmp, num = 1114))
# 
# 
# tmp2(1114)
# 
# map_dbl(w1_test$record_id[1:100], ~ tmp2(.x))
# w1_test$record_id2 <- map_dbl(w1_test$record_id, ~ tmp2(.x))
# 
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 0    2548    6326    5764    8728   11616

