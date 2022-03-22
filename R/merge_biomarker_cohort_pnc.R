# -------------------------------------------------------------------------------- #
# 01/26/2022
#
# Export data from RedCAP into R
#
# Goals:
# - download data through API
# - identify duplicate responses
# - summarize demographic and electronic health record information 
#   (we don't need to )
#
# Notes:
# - pop_record_id is incomplete with biomarker cohort. Either create a new ID, 
#   use the recordid, or USCID
#   
#
# For distribution to collaborators - remove identifers like name and id, 
# -------------------------------------------------------------------------------- #
library(tidyverse)
library(data.table)
library(glue)
library(janitor)
library(stringr)
library(lubridate)

url = "https://redcap.med.usc.edu/api/"
token = scan(glue("data/biomarker_scheduling_tracking_tkn.txt"), what = 'character')
formData = list("token"=token, content='report', format='csv', report_id=17318, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')

bm <- httr::content(response, guess_max = 10000) %>% 
  mutate(email = str_to_lower(email), 
         name = str_to_title(name), 
         dob = paste(dob_month, dob_day, dob_year, sep = "/"),
         dob = as.Date(dob, format = "%m/%d/%Y")) %>% 
  rename_with( ~ 'uscid', any_of(c("pop_student_id", "pop_usc_id", "usc_id"))) %>%
  rename_with( ~ 'tpri_biomarker_timestamp', any_of(c("consent_timestamp")))

bm$pop_record_id <- map_dbl(w1$pop_record_id_old, ~ tmp2(.x))





# ------------------------------------------------------ #
# Data cleaning step 
# ------------------------------------------------------ #

# - filter consent_complete == 2
# - usc.edu email (pop_email)
# - filter duplicates
# - filter duplicate responses
#   - algorithm - compare dates. if 2+ response took place > 30 days since the first, keep earlier response. Otherwise keep latest response (to capture do-overs)
#   - there are a handful of duplicate responses on same date, keep the later record_id



# Cleaning function
# dat <- health_student_w1
# wave = 1

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
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) >= 30)
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup()  %>%
    group_by(pop_full_name, pop_email) %>%
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) >= 30)
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup()  %>%
    group_by(pop_full_name, pop_uscid) %>%
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) >= 30)
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup() %>%
    group_by(pop_full_name, dob) %>%
    {if (max(.$tpri_health_survey_timestamp) - min(.$tpri_health_survey_timestamp) >= 30)
      filter(., tpri_health_survey_timestamp == min(tpri_health_survey_timestamp) & record_id == max(record_id)) else
        filter(., tpri_health_survey_timestamp == max(tpri_health_survey_timestamp) & record_id == max(record_id))} %>% ungroup() %>%
    mutate(wave = wave) %>% 
    rename_with( ~ paste0("w", wave, "_", .x), .cols = c(-pop_record_id, -pop_full_name, -pop_email, -dob, -pop_uscid, -wave)) %>% 
    mutate(pop_record_id_old = pop_record_id)
  
  return(tmp_out)
}



# function to 'clean' pop_record_id
tmp <- function(num, ele) {
  if(num %in% ele) {min(ele, na.rm = T)} else {0}
}

tmp2 <- function(num2) {
  max(sapply(pop_survey_record_id_list, tmp, num = num2))
}

pop_survey_record_id_list <- readRDS("data/pop_survey_student_record_id_list.rds")




# ---- student wave 1 ---- 

w1 <- clean_health_survey(health_student_w1, 1)
w1$pop_record_id <- map_dbl(w1$pop_record_id_old, ~ tmp2(.x))

# checks
sum(duplicated(w1$pop_record_id))
sum(is.na(w1$w1_date))
sum(is.na(w1$pop_full_name))
sum(is.na(w1$pop_email))

check <- get_dupes(w1, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(w1, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, pop_record_id_old)

# no way to know if these are duplicated names or not, keep in database
# (different DOBs)
check <- get_dupes(w1, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, dob)

w1 <- filter(w1, pop_record_id != 0)


# ---- student wave 2 ---- 
w2 <- clean_health_survey(health_student_w2, 2)
w2$pop_record_id <- map_dbl(w2$pop_record_id_old, ~ tmp2(.x))




# check (wave 2 looks much cleaner. However, there are a lot of missing responses)
sum(duplicated(w2$pop_record_id))
sum(duplicated(w2$pop_full_name))
sum(is.na(w2$pop_full_name))
sum(is.na(w2$pop_email))

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
  filter(!is.na(pop_record_id))
w3$pop_record_id <- map_dbl(w3$pop_record_id_old, ~ tmp2(.x))



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
pop_survey_record_id_list <- readRDS("data/pop_survey_staff_record_id_list.rds")
staff_w1 <- clean_health_survey(health_staff_w1, 1)
staff_w1$pop_record_id <- map_dbl(staff_w1$pop_record_id_old, ~ tmp2(.x))



# checks
sum(duplicated(staff_w1$pop_record_id))
sum(is.na(staff_w1$w1_date))
sum(is.na(staff_w1$pop_full_name))
sum(is.na(staff_w1$pop_email))

check <- get_dupes(staff_w1, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

# need to remove these (prio uscid available)
# this also choose one of the dup pairs correctly
check <- get_dupes(staff_w1, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, dob, w1_record_id) %>% 
  filter(pop_record_id != 0) %>% arrange(pop_record_id, pop_full_name, pop_uscid) %>% 
  filter(duplicated(pop_record_id))

staff_w1 <- staff_w1 %>% 
  dplyr::filter(pop_record_id != 0, 
                !w1_record_id %in% check$w1_record_id)

check <- get_dupes(staff_w1, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)





# ---- staff wave 2 ---- 
staff_w2 <- clean_health_survey(health_staff_w2, 2) %>% 
  filter(!is.na(pop_record_id))
staff_w2$pop_record_id <- map_dbl(staff_w2$pop_record_id_old, ~ tmp2(.x))



# checks
sum(duplicated(staff_w2$pop_record_id))
# sum(is.na(staff_w2$w2_date))
sum(is.na(staff_w2$pop_full_name))
sum(is.na(staff_w2$pop_email))

check <- get_dupes(staff_w2, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(staff_w2, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, pop_record_id_old, w2_record_id) %>% 
  filter(pop_record_id != 0) %>% arrange(pop_record_id, pop_full_name, pop_uscid) %>% 
  filter(duplicated(pop_record_id))

staff_w2 <- staff_w2 %>% 
  dplyr::filter(pop_record_id != 0, 
                !w2_record_id %in% check$w2_record_id)

# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(staff_w2, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, dob)



# ---- staff wave 2 ---- 
staff_w3 <- clean_health_survey(health_staff_w3, 3) %>% 
  filter(!is.na(pop_record_id))
staff_w3$pop_record_id <- map_dbl(staff_w3$pop_record_id_old, ~ tmp2(.x))


# checks
sum(duplicated(staff_w3$pop_record_id))
# sum(is.na(staff_w3$w3_date))
sum(is.na(staff_w3$pop_full_name))
sum(is.na(staff_w3$pop_email))

check <- get_dupes(staff_w3, pop_full_name, pop_email) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id)

check <- get_dupes(staff_w3, pop_record_id) %>%
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, pop_record_id_old, w3_record_id) %>% 
  filter(pop_record_id != 0) %>% arrange(pop_record_id, pop_full_name, pop_uscid) %>% 
  filter(duplicated(pop_record_id))

staff_w3 <- staff_w3 %>% 
  dplyr::filter(pop_record_id != 0,
                !w3_record_id %in% check$w3_record_id)



# no way to know if these are duplicated names or not, keep in database
check <- get_dupes(staff_w3, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, dob)







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
sum(duplicated(tpri$pop_uscid))
sum(is.na(tpri$pop_full_name))
sum(is.na(tpri$pop_email))

idvars <- c("pop_full_name", "pop_email", "pop_uscid", "pop_record_id")

# several people who filled out both student and survey questionnaires
check <- get_dupes(tpri, pop_uscid) %>%
  dplyr::select(all_of(idvars))
check <- get_dupes(tpri, pop_full_name) %>%
  dplyr::select(all_of(idvars))
check <- get_dupes(tpri, pop_full_name, pop_email) %>%
  dplyr::select(all_of(idvars))
check <- get_dupes(tpri, pop_record_id) %>%
  dplyr::select(all_of(idvars))

# no way to know if these are duplicated names or not, keep in database
# (some cases are obvious, but will defer to analysts)
# also filled both student and staff, as mentioned above
check <- get_dupes(tpri, pop_full_name) %>% 
  dplyr::select(pop_full_name, pop_email, pop_uscid, pop_record_id, dob)


saveRDS(tpri, file = glue("data/{date}_out/tpri_merged_w1_w2_w3_2022_01_14.rds"))

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

write.csv(tpri_deid, file = glue("data/{date}_out/tpri_merged_w1_w2_w3_2022_01_14.csv"), quote = T, row.names = F)
