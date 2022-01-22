# -------------------------------------------------------------------------------- #
# 01/20/2022
#
# Export data from RedCAP into R
#
# Goals:
# - download data through API
# - identify duplicate responses, keep only single instance (see below for algorithm)
# - combine waves into a single wide format file
#   
#
# Notes:
# - pop_email OK to use as email field, ignore all else
# - use full_join to combine waves 1-3 (for example, some only completed wave 2)
# - full_join using pop_record_id
# - wave3 rename uscid to pop_usc_id (from pop_student_id)
# - combine (coalesce - dplyr) only the following variables between waves: 
#   pop_record_id
#   pop_email
#   pop_full_name
#   dob (create separate)
#   student_id (pop_student_id, pop_usc_id)
#   
#
# For distribution to collaborators - remove identifers like name and id, 
# use pop_record_id as the master ID
# -------------------------------------------------------------------------------- #
library(tidyverse)
library(data.table)
library(glue)
library(janitor)
library(stringr)
library(lubridate)

# convenience functions
export_population_survey <- function(category = c("student", "staff"), report_id) {
  match.arg(category)
  
  url = "https://redcap.med.usc.edu/api/"
  token = scan(glue("data/population_survey_{category}_tkn.txt"), what = 'character')
  formData = list("token"=token, content='report', format='csv', report_id=report_id, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')
  response = httr::POST(url, body = formData, encode = "form")
  httr::content(response, guess_max = 10000)
}

export_health_survey <- function(category = c("student", "staff"), wave, report_id) {
  match.arg(category)
  
  url = "https://redcap.med.usc.edu/api/"
  token = scan(glue("data/health_survey_{category}_wave{wave}_tkn.txt"), what = 'character')
  formData = list("token"=token, content='report', format='csv', report_id=report_id, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')
  response = httr::POST(url, body = formData, encode = "form")
  
  # make name and email consistent, create dob, and rename common variables to match between waves
  httr::content(response, guess_max = 10000) %>% 
    mutate(pop_email = str_to_lower(pop_email), 
           pop_full_name = str_to_title(pop_full_name), 
           dob = paste(pop_birth_month, pop_birth_day, pop_birth_year, sep = "/"),
           dob = as.Date(dob, format = "%m/%d/%Y"),) %>% 
    rename_with( ~ 'pop_uscid', any_of(c("pop_student_id", "pop_usc_id"))) %>%
    rename_with( ~ 'tpri_health_survey_timestamp', any_of(c("consent_timestamp")))
}


# export pop survey data from RedCAP
population_student <- export_population_survey("student", 17173)
population_staff <- export_population_survey("staff", 17174)

# export health survey data from RedCAP
health_student_w1 <- export_health_survey("student", 1, 17166)
health_student_w2 <- export_health_survey("student", 2, 17167)
health_student_w3 <- export_health_survey("student", 3, 17169)
health_staff_w1 <- export_health_survey("staff", 1, 17170)
health_staff_w2 <- export_health_survey("staff", 2, 17171)
health_staff_w3 <- export_health_survey("staff", 3, 17172)



# ------------------------------------------------------ #
# Data cleaning step 
# ------------------------------------------------------ #

# - filter consent_complete == 2
# - usc.edu email (pop_email)
# - filter duplicates
# - filter duplicate responses
#   - algorithm - compare dates. if 2+ response took place > 30 days since the first, keep earlier response. Otherwise keep latest response (to capture do-overs)
#   - there are a handful of duplicate responses on same date, keep the later record_id



# convenience function

clean_health_survey <- function(dat, wave) {
  
  tmp <- dat %>% 
    # filter consent_complete
    # filter duplicated rows (unlikely)
    {if(wave == 1) 
      dplyr::filter(., consent_complete == 2, !duplicated(.)) else 
        dplyr::filter(., !duplicated(.))} %>%
    # for individuals who provided USCID once but not in subsequent responses - fill NAs with the correct value
    group_by(pop_full_name, pop_email) %>% fill(pop_uscid, .direction = 'up') %>% ungroup()
 

  # duplicate names, remove based on timestamp and record id
  # - duplicate names + email
  # - duplicate names + usc id, different email  
  # need to cover two situations - duplicate name and email, duplicate name and student id (when available)
  tmp_out <- tmp %>% 
    mutate(tpri_health_survey_timestamp = as.Date(tpri_health_survey_timestamp),
           tpri_health_survey_timestamp = if_else(is.na(tpri_health_survey_timestamp), as.Date("1900-01-01"), tpri_health_survey_timestamp)) %>%
    group_by(pop_email) %>%
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) > 30)
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup()  %>%
    group_by(pop_full_name, pop_email) %>%
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) > 30)
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup()  %>%
    group_by(pop_full_name, pop_uscid) %>% 
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) > 30) 
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else 
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup() %>%
    mutate(wave = wave) %>% 
    rename_with( ~ paste0("w", wave, "_", .x), .cols = c(-pop_record_id, -pop_full_name, -pop_email, -dob, -pop_uscid, -wave))
  
  return(tmp_out)
}




# ---- student wave 1 ---- 

w1 <- clean_health_survey(health_student_w1, 1)

# checks
sum(duplicated(w1$pop_record_id))
sum(is.na(w1$w1_date))
sum(is.na(w1$pop_full_name))
sum(is.na(w1$pop_email))

check <- get_dupes(w1, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(w1, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(w1, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)


# ---- student wave 2 ---- 
w2 <- clean_health_survey(health_student_w2, 2)
  
# check (wave 2 looks much cleaner. However, there are a lot of missing responses)
sum(duplicated(w2_tmp$pop_record_id))
sum(duplicated(w2_tmp$pop_full_name))
sum(is.na(w2_tmp$pop_full_name))
sum(is.na(w2_tmp$pop_email))

check <- get_dupes(w2, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(w2, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(w2, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)
  
# how complete is wave 2 responses 
tmp <- w2 %>% 
  summarise(across(.cols = contains("complete"), ~ sum(.x == 2)))





# ---- student wave 3 ---- 
w3 <- clean_health_survey(health_student_w3, 3) %>% 
  filter(!is.na(pop_record_id)) # lol kush

sum(duplicated(w3$pop_record_id))
sum(duplicated(w3$pop_full_name))
sum(is.na(w3$pop_full_name))
sum(is.na(w3$pop_email))

check <- get_dupes(w3, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(w3, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(w3, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# how complete is wave 2 responses 
tmp <- w3 %>% 
  summarise(across(.cols = contains("complete"), ~ sum(.x == 2)))






# ---- staff wave 1 ---- 

staff_w1 <- clean_health_survey(health_staff_w1, 1)

# checks
sum(duplicated(staff_w1$pop_record_id))
sum(is.na(staff_w1$w1_date))
sum(is.na(staff_w1$pop_full_name))
sum(is.na(staff_w1$pop_email))

check <- get_dupes(staff_w1, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(staff_w1, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(staff_w1, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)





# ---- staff wave 2 ---- 
staff_w2 <- clean_health_survey(health_staff_w2, 2) %>% 
  filter(!is.na(pop_record_id))

# checks
sum(duplicated(staff_w2$pop_record_id))
# sum(is.na(staff_w2$w2_date))
sum(is.na(staff_w2$pop_full_name))
sum(is.na(staff_w2$pop_email))

check <- get_dupes(staff_w2, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(staff_w2, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(staff_w2, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)



# ---- staff wave 2 ---- 
staff_w3 <- clean_health_survey(health_staff_w3, 3) %>% 
  filter(!is.na(pop_record_id))

# checks
sum(duplicated(staff_w3$pop_record_id))
# sum(is.na(staff_w3$w3_date))
sum(is.na(staff_w3$pop_full_name))
sum(is.na(staff_w3$pop_email))

check <- get_dupes(staff_w3, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(staff_w3, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(staff_w3, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)







# ---- MERGE DATASETS ---- #

tpri_student <- full_join(w1, w2, 'pop_record_id') %>% 
  full_join(w3, 'pop_record_id') %>% 
  tidyr::unite('wave', c(wave.x, wave.y, wave)) %>% 
  mutate(pop_full_name = coalesce(pop_full_name, pop_full_name.x, pop_full_name.y), 
         pop_uscid = coalesce(pop_uscid, pop_uscid.x, pop_uscid.y), 
         pop_email = coalesce(pop_email, pop_email.x, pop_email.y), 
         dob = coalesce(dob, dob.x, dob.y)) %>% 
  dplyr::select(-contains(c(".x", ".y"))) %>% 
  mutate(across(.cols = contains("timestamp"), ~ as.Date(.x)),
         # change some num to char
         across(.cols = c(w2_adults_household,
                          w2_rooms, 
                          w2_campus_days,
                          w2_nodes_infected,
                          pop_uscid), ~ as.character(.x))) %>% 
  mutate(pop_record_id = paste0("student_", pop_record_id))

tpri_staff <- full_join(staff_w1, staff_w2, "pop_record_id") %>%
  full_join(staff_w3, 'pop_record_id') %>% 
  tidyr::unite('wave', c(wave.x, wave.y, wave)) %>% 
  mutate(pop_full_name = coalesce(pop_full_name, pop_full_name.x, pop_full_name.y), 
         pop_uscid = coalesce(pop_uscid, pop_uscid.x, pop_uscid.y), 
         pop_email = coalesce(pop_email, pop_email.x, pop_email.y), 
         dob = coalesce(dob, dob.x, dob.y)) %>% 
  dplyr::select(-contains(c(".x", ".y"))) %>% 
  mutate(across(.cols = contains("timestamp"), ~ as.Date(.x)),
         across(.cols = c(w2_personal_degree,
                          w2_nodes_vaccinated,
                          w3_behaviours_23,
                          w3_behaviours_24), ~ as.character(.x))) %>% 
  mutate(pop_record_id = paste0("staff_", pop_record_id))


# some issues with data type differences between student and staff

str_help <- function(word) {
  str(tpri_student %>% dplyr::select(contains(word)))
  str(tpri_staff %>% dplyr::select(contains(word)))
}

str_help('household')
str_help( 'rooms')
str_help('campus_days')
str_help('personal_degree')
str_help('nodes_infected')
str_help('nodes_vaccinated')
str_help('behaviours_23')
str_help('pop_uscid')

tpri <- bind_rows(tpri_student, tpri_staff)

# checks
sum(duplicated(tpri$pop_record_id))
sum(is.na(tpri$pop_full_name))
sum(is.na(tpri$pop_email))

# several people who filled out both student and survey questionnaires
check <- get_dupes(tpri, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)
check <- get_dupes(tpri, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# no way to know if these are duplicated names or not, keep in database
# (some cases are obvious, but will defer to analysts)
check <- get_dupes(tpri, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)


saveRDS(tpri, file = "data/tpri_merged_w1_w2_w3_2022_01_14.rds")

# no identifiers

names(tpri)[grepl("id", names(tpri))]
names(tpri)[grepl("name", names(tpri))]
names(tpri)[grepl("identifier", names(tpri))]
names(tpri)[grepl("email", names(tpri))]
names(tpri)[grepl("birth", names(tpri))]


tpri_deid <- tpri %>% 
  dplyr::select(-w1_record_id, -w2_record_id, -w3_record_id, 
                -w1_name, -pop_full_name, -pop_uscid, -pop_email, -dob,
                -contains(c("identifier", "birth")))

names(tpri_deid)

write.csv(tpri_deid, file = "data/tpri_merged_w1_w2_w3_2022_01_14_noid.csv", quote = T, row.names = F)
