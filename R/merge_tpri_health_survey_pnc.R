# -------------------------------------------------------------------------------- #
# 01/20/2022
#
# TPRI subset that match PnC
# 
# use the following variables to match: 
# - uscid
# - email
# - first/last name + dob
#
#
# use pop_record_id to link back to TPRI
# use pop_uscid to match between different PnC reports
# -------------------------------------------------------------------------------- #
library(tidyverse)
library(data.table)
library(glue)
library(janitor)
library(stringr)
library(lubridate)

date <- "2022_03_02"
date <- "2022_03_21"


# TPRI merged data
tpri <- readRDS(glue("data/{date}_out/tpri_merged_w1_w2_w3_{date}.rds")) %>% 
  dplyr::select(pop_record_id, pop_full_name, pop_uscid, pop_email, dob, wave) %>% 
  mutate(pop_record_id = str_trim(pop_record_id, side = 'both'), 
         pop_uscid = str_trim(pop_uscid, side = 'both'), 
         pop_email = str_trim(pop_email, side = 'both')) %>% 
  mutate(pop_full_name = str_trim(str_to_upper(pop_full_name), side = 'both'), 
         firstn = str_trim(str_split(pop_full_name, "\\s+", simplify = T)[,1],side = 'both'),
         lastn = str_trim(sapply(str_split(pop_full_name, "\\s+"), tail, 1), side = 'both'),
         firstn_init = str_sub(firstn, start = 1, end = 1), 
         subn = paste(firstn_init, lastn, sep = ". "), 
         age = floor(time_length(difftime(Sys.Date(), dob), 'years')),
         age_2021 = floor(time_length(difftime(as.Date("2021-01-01"), dob), 'years')), 
         dob = as.Date(dob, format = "%m/%d/%Y"),
         newid = paste(subn, dob, sep = ":")) %>% 
  dplyr::select(-firstn, -lastn, -firstn_init, -subn) #%>% 
  # mutate(merge_flag_uscid = pop_uscid %in% populationData$usc_pop_uscid, 
  #        merge_flag_email = pop_email %in% populationData$usc_pop_email, 
  #        merge_flag_newid = newid %in% populationData$usc_newid) %>% 
  # tidyr::unite('merge_flag', c(merge_flag_uscid, merge_flag_email, merge_flag_newid))




# population compliance report (USC Census)
# only need email, uscid, and newid for matching
populationData <- fread(glue("data/{date}/SARS-CoV-2_USC_Population_Compliance_Report.csv"), encoding = "Latin-1") %>%
  rename(pop_uscid = PatientNumber,
         pop_email = Email1, 
         sex_ehr = Sex) %>%
  mutate(PatientName = str_to_upper(PatientName), 
         PatientFirst_tmp = str_trim(str_split(PatientName, ',', simplify = TRUE)[,2], side = 'both'),
         PatientFirst = str_split(PatientFirst_tmp, '\\s+', simplify = T)[,1],
         PatientLast_tmp = str_trim(str_split(PatientName, ',', simplify = TRUE)[,1], side = 'both'),
         PatientLast = str_trim(sapply(str_split(PatientLast_tmp, ','), tail, 1), side = 'both'), 
         PatientFirstInit = str_sub(PatientFirst, start = 1, end = 1),
         PatientSub = paste(PatientFirstInit, PatientLast, sep = ". "),
         dob = as.Date(DOB, format = "%m/%d/%Y"),
         newid = paste(PatientSub, dob, sep = ":"),
         pop_email = str_to_lower(str_trim(pop_email, side = 'both')), 
         pop_uscid = str_trim(pop_uscid, side = 'both'))%>%
  dplyr::select(pop_uscid, pop_email, newid, PatientName, everything(), -PatientFirst, -PatientFirst_tmp, -PatientLast_tmp, -PatientFirstInit, -PatientLast, -PatientSub, -DOB)
names(populationData) <- paste0("usc_", str_to_lower(names(populationData)))
names(populationData)


# ---------------------------------------------------------------------------- #
# match data ---- 
# ---------------------------------------------------------------------------- #

# ---- Filtering Survey Data ---- #

# quick tally of matches by each of the 3 variables
tpri %>% 
  mutate(merge_flag_uscid = pop_uscid %in% populationData$usc_pop_uscid, 
         merge_flag_email = pop_email %in% populationData$usc_pop_email, 
         merge_flag_newid = newid %in% populationData$usc_newid) %>% 
  group_by(merge_flag_uscid, merge_flag_email, merge_flag_newid) %>% 
  summarise(n = n())



# ------------- start here ----------- #
tmp1 <- tpri 
tmp2 <- populationData

# 1) match uscid
merge_uscid <- inner_join(tmp1, tmp2, by = c('pop_uscid' = 'usc_pop_uscid'), na_matches = 'never') %>% 
  mutate(pop_email = usc_pop_email)
  
# 2) match email
tmp1 <- filter(tmp1, !pop_uscid %in% merge_uscid$pop_uscid)
tmp2 <- filter(tmp2, !usc_pop_uscid %in% merge_uscid$pop_uscid)

merge_email <- inner_join(tmp1, tmp2, by = c("pop_email" = "usc_pop_email"), na_matches = "never") %>% 
  mutate(pop_uscid = usc_pop_uscid)

merge_uscid_email = bind_rows(merge_uscid, merge_email)
  
# 3) match newid
tmp1 <- filter(tmp1, !pop_email %in% merge_uscid_email$pop_email)
tmp2 <- filter(tmp2, !usc_pop_email %in% merge_uscid_email$pop_email)

merge_newid <- inner_join(tmp1, tmp2, by = c('newid' = 'usc_newid'), na_matches = "never") %>% 
  # assume newid matches are good (note: not necessarily true)
  mutate(pop_uscid = usc_pop_uscid,
         pop_email = usc_pop_email) %>%
  dplyr::select(newid, pop_full_name, usc_patientname, dob, usc_dob, pop_uscid, usc_pop_uscid, pop_email, usc_pop_email)
  

output_tmp <- bind_rows(merge_uscid_email, merge_newid) %>% 
  arrange(pop_uscid) %>% 
  filter(!duplicated(.), 
         !is.na(pop_record_id), 
         !is.na(dob))


# duplicates expected at this stage:
sum(duplicated(output_tmp$pop_uscid))
sum(duplicated(output_tmp$pop_email))
sum(duplicated(output_tmp$newid))





# ---------------------------------------------------------------------------- #
# vaccination ---- 
# ---------------------------------------------------------------------------- #

# vaccination data.frame (to prioritize individuals when dropping duplicates)
source("R/import_pnc_vac.R")
# output<-left_join(output, vac, 'USCID')


# ---------------------------------------------------------------------------- #
# testing ---- 
# ---------------------------------------------------------------------------- #
source("R/import_pnc_lab_panel.R")


# ---------------------------------------------------------------------------- #
# identify and remove duplicates ---- 
# ---------------------------------------------------------------------------- #

output <- output_tmp 

# ---- pop_record_id duplicates ----
# prioritize uscid in vaccination report
dups <- get_dupes(output, pop_record_id) %>% 
  mutate(vac = ifelse(pop_uscid %in% vac_wide$USCID, 'vac', NA))

remove <- dups %>% 
  filter(is.na(vac))

output_poprecordid <- output %>% 
  filter(!pop_uscid %in% remove$pop_uscid)
  

# ---- USCID Duplicates ---- #
#
# prioritize records that responded to multiple waves
# if both staff student surveys = keep student survey
dups <- get_dupes(output_poprecordid, pop_uscid)

# check <- get_dupes(output, pop_uscid) %>%
#   separate(wave, into = c('a', 'b', 'c'), remove = T) %>%
#   mutate(across(.cols = a:c, ~ as.numeric(.x))) %>%
#   group_by(pop_record_id) %>%
#   mutate(wave = sum(a,b,c, na.rm = T)) %>% ungroup() %>%
#   arrange(wave) %>%
#   group_by(pop_uscid) %>%
#   filter(!duplicated(pop_uscid))

output_poprecordid_uscid <- output_poprecordid %>%
  filter(!pop_record_id %in% c('staff_1721', 'staff_40'))
  
# ---- Email duplicates ---- #
dups <- get_dupes(output_poprecordid_uscid, pop_email) # no duplicates

# ---- Name/DOB Duplicates - start with POP_NEWID ---- #
dups <- get_dupes(output_poprecordid_uscid, newid)






# ---- Final dataset ---- #
output_final = output_poprecordid_uscid

# final check
any(duplicated(output_final$pop_record_id)); any(duplicated(output_final$pop_uscid)); any(duplicated(output_final$pop_email)); any(duplicated(output_final$newid))


# -------------------------------------------------------------------- #
# provide separate dataset (linked with pop_record_id) with variables from 
# 1) pop compliance report
# 2) testing dataset (wide format)
# 3) vaccination dataset (wide format)

vac_info <- left_join(output_final[, c('pop_record_id', 'pop_uscid')], 
                       vac_wide %>% mutate(pop_uscid = as.character(USCID)) %>% 
                        dplyr::select(-USCID, -PatientNumber), 
                       by = 'pop_uscid') %>% 
  dplyr::select(-pop_uscid) %>% 
  rename_with( ~ paste0("vac_", .x), -pop_record_id)

lab_info <- left_join(output_final[, c('pop_record_id', 'pop_uscid')], 
                      lab_panel_wide %>% ungroup() %>% mutate(pop_uscid = as.character(USCID)) %>% 
                        dplyr::select(-USCID),
                      by = 'pop_uscid') %>% 
  dplyr::select(-pop_uscid) %>% 
  rename_with( ~ paste0("lab_", .x), -pop_record_id)


usc_info <- output_final %>% 
  dplyr::select(-pop_full_name, -pop_uscid, -pop_email, -dob, -wave, 
                -newid, 
                -usc_newid, -usc_pop_email, -usc_patientname, -usc_dob, -usc_pop_uscid)


ehr_out <- inner_join(usc_info, vac_info, 'pop_record_id') %>% 
  inner_join(lab_info, 'pop_record_id') %>% 
  dplyr::select(where(~ !all(is.na(.x))))

saveRDS(ehr_out, file = glue("data/{date}_out/tpri_ehr_subset_{date}.rds"))
write.csv(ehr_out, file = glue("data/{date}_out/tpri_ehr_subset_{date}.csv"), quote = T, row.names = F)
