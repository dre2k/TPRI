# ============================================================================ #
# Merge/Match:
# - Population survey (USCVaccineStudyStude/staff)
# - PnC population compliance report (usc_population_w_uid.csv)
# - PnC Vaccine report (COVID_Vaccine_1st_and_2nd_Dose_Received_Including_Historic_Documentation)
# ============================================================================ #
library(tidyverse)
library(tidyr)
library(stringr)
library(readxl)
library(magrittr)
library(mice)
library(Hmisc)
library(lubridate)
library(glue)
rm(list = ls())

# directories and files
date <- "2021_12_01"
data_directory <- "E:/USCVaccineStudy/data/"
file_pnc_compliance_report <- "SARS-CoV-2_USC_Population_Compliance_Report.csv"
# file_pnc_vaccine_report <- "COVID_Vaccine_1st_and_2nd_Dose_Received_Including_Historic_Documentation.csv"
# file_pnc_vaccine_report <- "covid19_vaccination_events_ind.csv"
source("ehr_merge/functions.R")



# ---------------------------------------------------------------------------- #
# Import population survey data (students) ---- 
# ---------------------------------------------------------------------------- #
# this script needs to be edited every time
# - remove line that deletes objs
# - edit path for data file
source(glue("{data_directory}/{date}/USCVaccineStudyStude_R.r"))

studentData <- data %>% 
  dplyr::select(-redcap_survey_identifier) %>%
  mutate(name = str_to_upper(name),
         pop_email = str_to_lower(hipaa_email), 
         DOB = paste(birth_month, birth_day, birth_year, sep = "/"),
         DOB = as.Date(DOB, format = "%m/%d/%Y"),
         AGE_AT_ENTRY = floor((as.Date(consent_timestamp) - DOB) / 365))
names(studentData) <- paste0("PSS_", toupper(names(studentData)))


# ---- Filtering Survey Data ---- #
# FYI Kush uses demographics complete .. i'll leave it full for now, can filter during analysis

# Complete survey responses, with Email
completeStudentData <- studentData %>% 
  # dplyr::filter(PSS_VACCINE_STUDY_COMPLETE.FACTOR == "Complete") %>%
  mutate(PSS_NAME = str_trim(PSS_NAME, side = "both"),
         PSS_FIRST_NAME = word(PSS_NAME, 1),
         PSS_LAST_NAME = word(PSS_NAME, -1),
         PSS_FIRST_INIT = str_sub(PSS_FIRST_NAME, start = 1, end = 1),
         PSS_NAME_SUB = paste(PSS_FIRST_INIT, PSS_LAST_NAME, sep = ". "),
         PSS_NEWID = paste(PSS_NAME_SUB, PSS_DOB, sep = ":"),
         tmp = ifelse(PSS_HIPAA_EMAIL == "", PSS_HIPAA_EMAIL_V2, PSS_HIPAA_EMAIL)) %>%
  # make sure emails don't have empty ""
  mutate(PSS_EMAIL1 = ifelse(tmp == "", NA, tmp)) %>% 
  rename(PSS_SEX_survey = PSS_SEX) %>% 
  dplyr::select(PSS_NAME, PSS_FIRST_NAME, PSS_LAST_NAME, PSS_FIRST_INIT, PSS_NAME_SUB, 
                PSS_DOB, PSS_NEWID, PSS_BIRTH_MONTH, PSS_BIRTH_DAY, PSS_BIRTH_YEAR, everything(), -tmp)




# ---------------------------------------------------------------------------- #
# import pnc data ---- 
# ---------------------------------------------------------------------------- #
source("ehr_merge/import_pnc_data.R")


# ---------------------------------------------------------------------------- #
# match data with pnc population data ---- 
# ---------------------------------------------------------------------------- #

# initial matches based on USCID, email, and NEWID (name:DOB)
output <- match_survey_pop(survey_data = completeStudentData, 
                           pnc_data = populationData,
                           survey_uscid = PSS_STUDENT_ID, 
                           pnc_uscid = POP_USCID,
                           survey_email = PSS_EMAIL1, 
                           pnc_email = POP_EMAIL1,
                           survey_newid = PSS_NEWID, 
                           pnc_newid = POP_NEWID,
                           survey_timestamp = PSS_CONSENT_TIMESTAMP, 
                           survey_recordid = PSS_RECORD_ID)


# ---------------------------------------------------------------------------- #
# merge with vaccination data ---- 
# ---------------------------------------------------------------------------- #
# output <- left_join(output, vaccinationData, by = 'USCID', na_match = "never") %>%
#   dplyr::arrange(USCID) %>%
#   dplyr::filter(!duplicated(.))



# ---------------------------------------------------------------------------- #
# identify and remove duplicates ---- 
# ---------------------------------------------------------------------------- #

# ---- Email duplicates ---- #
dups_email <- output[which(duplicated(output$POP_EMAIL1)), "POP_EMAIL1"] 

check <- filter(output, POP_EMAIL1 == dups_email) %>%
  dplyr::select(USCID, PSS_FIRST_NAME, POP_PATIENTFIRST, PSS_LAST_NAME, POP_PATIENTLAST,
                POP_DOB, PSS_EMAIL1, POP_EMAIL1, PSS_RECORD_ID, POP_DIVISION,
                starts_with("VAC")) %>%
  dplyr::arrange(desc(USCID)) %>%
  dplyr::filter(!duplicated(PSS_RECORD_ID))

output <- output %>%
  dplyr::filter(! USCID %in% check$USCID)


# ---- Name/DOB Duplicates ---- #
dups_newid <- output[which(duplicated(output$POP_NEWID)), "POP_NEWID"]

check <- output %>% 
  dplyr::filter(POP_NEWID %in% dups_newid) %>% 
  dplyr::arrange(PSS_NAME_SUB) %>% 
  dplyr::select(USCID, PSS_FIRST_NAME, POP_PATIENTFIRST, PSS_LAST_NAME, 
                POP_PATIENTLAST, POP_DOB, PSS_EMAIL1, POP_EMAIL1, PSS_RECORD_ID) %>%
  dplyr::mutate(PSS_EMAIL1 = tolower(PSS_EMAIL1), 
                POP_EMAIL1 = tolower(POP_EMAIL1)) %>% 
  dplyr::filter(PSS_EMAIL1 != POP_EMAIL1)

output <- dplyr::filter(output, ! USCID %in% check$USCID)



# ---- survey record id duplicates ---- #
# e.g. same first name initial + DOB but different USCID + EMAIL
dups_record_id <- output[which(duplicated(output$PSS_RECORD_ID)), "PSS_RECORD_ID"]

check <- output %>%
  dplyr::filter(PSS_RECORD_ID %in% dups_record_id) %>%
  dplyr::select(USCID, PSS_FIRST_NAME, POP_PATIENTFIRST, PSS_LAST_NAME,
                POP_PATIENTLAST, POP_DOB, PSS_EMAIL1, POP_EMAIL1, PSS_RECORD_ID) %>%
  dplyr::filter(PSS_EMAIL1 != POP_EMAIL1)

output <- dplyr::filter(output, ! USCID %in% check$USCID)

# final check
# duplicated POP_NEWID are twins with first name initial (assuming)
any(duplicated(output$USCID)); any(duplicated(output$POP_EMAIL)); any(duplicated(output$POP_NEWID))


# ---------------------------------------------------------------------------- #
# vaccination data ---- 
# ---------------------------------------------------------------------------- #
source("ehr_merge/import_vac_data.R")

output<-left_join(output, vac, 'USCID')


# ---------------------------------------------------------------------------- #
# output data ---- 
# ---------------------------------------------------------------------------- #

saveRDS(output, file = glue("E:/USCVaccineStudy/output/{date}/population_survey_student_{date}.rds"))

output_deidentified <- output %>% 
  dplyr::select(-contains(c("EMAIL", "BIRTH_DAY", "BIRTH_MONTH", "HIPAA", "STUDENT_ID", "USCID", "NEWID", "BIRTH", "DOB", "ZIPCODE", "CELL_PHONE", "CONTACT_RELATION", "CONTACT_NUMBER")), -c("POP_PATIENTFIRST", "POP_PATIENTFIRSTINIT" ,"POP_PATIENTLAST"  ,"POP_PATIENTFULL"  ,"POP_PATIENTSUB"  ,"POP_NEWID"), 
                -c("PSS_NAME", "PSS_FIRST_NAME", "PSS_LAST_NAME", "PSS_NAME_SUB", 
                    "PSS_SIS_FNAME", "PSS_SIS_MNAME", 
                   "PSS_SIS_LNAME", "PSS_SIS_CONTACT_NAME", "POP_PATIENTNAME"))

saveRDS(output_deidentified, file = glue("E:/USCVaccineStudy/output/{date}/population_survey_student_deidentified_{date}.rds"))
write.csv(output_deidentified, file = glue("E:/USCVaccineStudy/output/{date}/population_survey_student_deidentified_{date}.csv"))

