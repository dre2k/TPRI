#==============================================================================#
# 01/14/2022
# explore biomarker survey data once again
# 
# keep in mind (2022/01/14):
# - Kush requested positivity data (whether they were ever positive
#   how often, etc)
# - Also try to provide school year information for each individual
# - Lastly, you owe them demographic info, self reported or otherwise
#   - sources will include PnC and questionnaire fields
#
#
#
# Goal - match individuals to PnC and TPRI questionnaire responses 
# 1) summarise demographics (PnC and TPRI responses)
# 2) summarise positivity over time
#
# where as in the health survey I cleaned pop_record_id etc, 
# in here - prioritize matching to PnC only. treat is as an entirely separate study
#==============================================================================#
library(data.table)
library(tidyverse)
library(glue)
library(stringi)
date <- "2022_03_02"


# ---------------------------------------------------------------------------- #
# biomarker data ---- 
# ---------------------------------------------------------------------------- #

url = "https://redcap.med.usc.edu/api/"
token = scan("data/biomarker_scheduling_tracking_tkn.txt", what = 'character')
# formData = list("token"=token, content='report', format='csv', report_id=11600, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')
formData = list("token"=token, content='report', format='csv', report_id=17318, csvDelimiter='', rawOrLabel='raw', rawOrLabelHeaders='raw', exportCheckboxLabel='false', returnFormat='csv')
response = httr::POST(url, body = formData, encode = "form")
student_p = httr::content(response, guess_max = 10000)


# redcap export "Biomarker Scheduling & Tracking" (version 1/14/2022)
bioData <- fread(glue("data/{date}/BiomarkerSchedulingT_DATA.csv")) %>% 
  dplyr::select(contains(c("email", "id", "name", "dob", "record", "date", "consent_timestamp")), 
                -starts_with(c("appt", "collection")), 
                -name_2, 
                -redcap_survey_identifier) %>%
  mutate(name = str_to_upper(name),
         email = str_to_lower(email), 
         DOB = paste(dob_month, dob_day, dob_year, sep = "/"),
         DOB = as.Date(DOB, format = "%m/%d/%Y"),
         AGE_AT_ENTRY = floor((as.Date(consent_timestamp) - DOB) / 365))
names(bioData) <- paste0("BM_", toupper(names(bioData)))



# ---------------------------------------------------------------------------- #
# vaccination data ---- 
# ---------------------------------------------------------------------------- #
source("R/import_pnc_vac.R")
# output<-left_join(output, vac, 'USCID')


# ---------------------------------------------------------------------------- #
# import pnc data ---- 
# ---------------------------------------------------------------------------- #

# specific to 2022_02_02
# a <- fread(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_a.csv"))
# b <- fread(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_b.csv"))
# c <- fread(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_c.csv"))
# d <- fread(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_d.csv"))
# e <- fread(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_e.csv"))
# 
out <- bind_rows(a,b,c,d,e) %>% 
  mutate(Result.Date = `Result Date`)
saveRDS(out, file = glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats.rds"))

source("R/import_pnc_lab_panel.R")


#------------------------------------------------------------------------------#
# start with goal #2
# 
# compliance report first (list of all USC population)
# based on uscid, name:dob, and email, match data with PnC
#------------------------------------------------------------------------------#


# ---- pnc population compliance report ---- #
# PnC Population Compliance Report
# (obtain valid USCIDs for as many survey respondents as possible)
#
# uses "NEWID" - combination of name and DOB
# -- possible issue is non-matches between non UTF-8 characters (accents, etc)
# -- although using USCID, as of 1/14 this doesn't seem to be an issue
# 05/19/21: Create age variable ( in years) based on CURRENT DATE - DOB (and round down).

populationData <- fread(glue("data/{date}/SARS-CoV-2_USC_Population_Compliance_Report.csv"), encoding = "Latin-1") %>%
  # select(-X) %>%
  rename(USCID = PatientNumber) %>%
  mutate(PatientName = str_to_upper(PatientName), 
         PatientFirst = str_trim(str_split(PatientName, ",", simplify = TRUE)[, 2], side = "both"),
         PatientLast = str_trim(str_split(PatientName, ",", simplify = TRUE)[, 1], side = "both"),
         PatientFull = paste(PatientFirst, PatientLast),
         PatientFirstInit = str_sub(PatientFirst, start = 1, end = 1),
         PatientSub = paste(PatientFirstInit, PatientLast, sep = ". "),
         DOB = as.Date(DOB, format = "%m/%d/%Y"),
         AGE = floor(time_length(difftime(Sys.Date(), DOB), 'years')),
         # AGE = floor((Sys.Date() - DOB)),
         # AGE2021 = floor((as.Date("2021-01-01") - DOB)),
         AGE2021 = floor(time_length(difftime(as.Date("2021-01-01"), DOB), 'years')),
         NEWID = paste(PatientSub, DOB, sep = ":")) %>%
  rename(SEX_pnc = Sex) %>% 
  dplyr::select(USCID, PatientName, PatientFirst, PatientFirstInit, PatientLast, PatientFull, PatientSub, NEWID, AGE, everything())
names(populationData) <- paste0("POP_", str_to_upper(names(populationData)))


# revert to USCID variable
# you will end up joining based on USCID
vaccinationData <- mutate(vaccinationData, USCID = as.numeric(VAC_USCID)) %>% 
  dplyr::select(-VAC_USCID)





# ---------------------------------------------------------------------------- #
# match data ---- 
# ---------------------------------------------------------------------------- #

# ---- Filtering Survey Data ---- #

# Complete survey responses, with Email
completeBioData <- bioData %>% 
  mutate(BM_NAME = str_trim(BM_NAME, side = "both"),
         BM_FIRST_NAME = word(BM_NAME, 1),
         BM_LAST_NAME = word(BM_NAME, -1),
         BM_FIRST_INIT = str_sub(BM_FIRST_NAME, start = 1, end = 1),
         BM_NAME_SUB = paste(BM_FIRST_INIT, BM_LAST_NAME, sep = ". "),
         BM_NEWID = paste(BM_NAME_SUB, BM_DOB, sep = ":"),
         BM_EMAIL1 = BM_EMAIL) %>%
  # make sure emails don't have empty ""
  mutate(BM_EMAIL1 = ifelse(BM_EMAIL1 == "", NA, BM_EMAIL1)) %>% 
  # Create a FLAG to know who we have linked with compliance report
  mutate(BM_MERGE_FLAG_ID = BM_USC_ID %in% populationData$POP_USCID,
         BM_MERGE_FLAG_EMAIL = BM_EMAIL1 %in% populationData$POP_EMAIL1,
         BM_MERGE_FLAG_NAME = BM_NEWID %in% populationData$POP_NEWID)

# Tabulate match success
completeBioData %>% 
  group_by(BM_MERGE_FLAG_ID, BM_MERGE_FLAG_EMAIL, BM_MERGE_FLAG_NAME) %>%
  summarise(n = n())


# ---- Merging Vaccine Survey Data w/ PnC Population Compliance Data ---- #
# (to obtain USCID + PnC variables)
# N = 391

# 1) Match based on USCID if available
tmp1 <- completeBioData %>% 
  dplyr::mutate(USCID = as.numeric(BM_USC_ID)) 
tmp2 <- populationData %>% 
  dplyr::mutate(USCID = as.numeric(POP_USCID))
survey_pop_merge_uscid <- inner_join(tmp1, tmp2, 'USCID', na_matches = 'never') %>% 
  dplyr::select(-POP_USCID, -BM_USC_ID)


# 2) Match based on USC Email address:
# recode blank email entries from both data sources to 'NA'
# create one common variable for matching and merging
# N = 285
tmp1 <- completeBioData %>% 
  dplyr::mutate(EMAIL1 = BM_EMAIL1)
tmp2 <- populationData %>% 
  dplyr::mutate(EMAIL1 = POP_EMAIL1)

survey_pop_merge_email <- inner_join(tmp1, tmp2, by = "EMAIL1", na_matches = "never") %>% 
  dplyr::mutate(USCID = as.numeric(POP_USCID)) %>% 
  # remove patients already matched by USCID
  dplyr::filter(!USCID %in%  survey_pop_merge_uscid$USCID) %>% 
  dplyr::select(-POP_USCID, -BM_USC_ID, -EMAIL1)




# 3) Match individuals based on Name/DOB (NEWID):
# this needs care
# N = 37
any(is.na(completeBioData$BM_NEWID))
any(is.na(populationData$PSS_NEWID))

tmp1 <- completeBioData %>% 
  dplyr::mutate(NEWID = BM_NEWID)
tmp2 <- populationData %>% 
  dplyr::mutate(NEWID = POP_NEWID)

survey_pop_merge_newid <- inner_join(tmp1, tmp2, by = 'NEWID', na_matches = "never") %>% 
  dplyr::mutate(USCID = as.numeric(POP_USCID)) %>% 
  dplyr::filter(!USCID %in% survey_pop_merge_uscid$USCID, 
                !USCID %in% survey_pop_merge_email$USCID,
                !BM_EMAIL1 %in% survey_pop_merge_email$BM_EMAIL1, 
                !POP_EMAIL1 %in% survey_pop_merge_email$POP_EMAIL1) %>% 
  dplyr::select(-POP_USCID, -BM_USC_ID, -NEWID)

check <- survey_pop_merge_newid %>% 
  dplyr::select(BM_FIRST_NAME, POP_PATIENTFIRST, BM_LAST_NAME, POP_PATIENTLAST, BM_DOB, POP_DOB)


# 4) Bind rows from data sets from parts 1,2,3, remove duplicate rows
# N = 713
output_tmp <- bind_rows(survey_pop_merge_uscid, survey_pop_merge_email, survey_pop_merge_newid) %>% 
  arrange(USCID) %>% 
  filter(!duplicated(.))



# 5) Merge with vaccine report
# output <- left_join(output_tmp, vaccinationData, by = 'USCID', na_match = "never") %>% 
#   dplyr::arrange(USCID) %>% 
#   dplyr::filter(!duplicated(.))



# ---------------------------------------------------------------------------- #
# identify and remove duplicates ---- 
# ---------------------------------------------------------------------------- #

output <- output_tmp

any(duplicated(output$USCID)) # no duplicates


# ---- USCID Duplicates ---- #
# (keep latest PSS_RECORD_ID)
dups_uscid <- output[which(duplicated(output$USCID)), "USCID"]

# create data.frame of rows to REMOVE - use PSS_RECORD_ID
check <- output %>% 
  dplyr::filter(USCID %in% dups_uscid) %>% 
  dplyr::select(USCID, POP_PATIENTNAME, BM_RECORD_ID, BM_CONSENT_TIMESTAMP) %>% 
  arrange(USCID, desc(BM_RECORD_ID))

remove_uscid <- output %>% 
  dplyr::filter(USCID %in% dups_uscid) %>% 
  # arrange by USCID and descending PSS_RECORD_ID
  dplyr::arrange(USCID, desc(BM_RECORD_ID)) %>% 
  # only keep first instance (latest PSS_RECORD_ID)
  filter(duplicated(USCID)) %>% 
  # list of PSS_RECORD_IDs to remove
  pull(BM_RECORD_ID)

output_filter_uscid <- dplyr::filter(output, ! BM_RECORD_ID %in% remove_uscid)


# ---- Email duplicates ---- #
# due to multiple entries in PnC, NOT because of multiple survey responses
# e.g. same email used but person has more than 1 USCID
#
# (there won't be USCID duplicates because of the step above)
# prioritize USCIDs found in VAC report (e.g. vaccinated)
# otherwise, keep smaller USCID


dups_email <- output_filter_uscid[which(duplicated(output_filter_uscid$POP_EMAIL1)), "POP_EMAIL1"]

check <- output_filter_uscid %>% 
  dplyr::filter(POP_EMAIL1 %in% dups_email) 

tmp <- filter(vac, USCID %in% check$USCID) 

remove_uscid <- output_filter_uscid %>% # in this very specific case, keep the larger USCID
  dplyr::filter(POP_EMAIL1 %in% dups_email) %>% 
  dplyr::arrange(POP_EMAIL1, BM_RECORD_ID, desc(USCID)) %>% 
  filter(duplicated(POP_EMAIL1)) %>% 
  pull(USCID)

output_filter_uscid_email <- dplyr::filter(output_filter_uscid, ! USCID %in% remove_uscid)



# ---- Name/DOB Duplicates - start with POP_NEWID ---- #
# need care, you can accidentally introduce duplicates
# e.g. no USCID or email available, same first initial + last name + DOB.. 
dups_newid <- output_filter_uscid_email[which(duplicated(output_filter_uscid_email$POP_NEWID)), "POP_NEWID"]

# # create data.frame of rows to REMOVE
# # individuals have different USCIDs but same name and email. keep earlier instance of USCID
# check <- output_filter_uscid_email %>% 
#   dplyr::filter((POP_NEWID %in% dups_newid)) %>% 
#   dplyr::arrange(BM_NAME_SUB) %>% 
#   dplyr::select(USCID, BM_FIRST_NAME, POP_PATIENTFIRST, POP_DOB, BM_EMAIL1, POP_EMAIL1, BM_RECORD_ID, starts_with("VAC"))
# 
# remove_uscid <- output_filter_uscid_email %>% 
#   dplyr::filter(POP_NEWID %in% dups_newid) %>% 
#   # remove individuals for whom PSS_FIRST_NAME is NOT in POP_PATIENTFIRST
#   dplyr::filter(!grepl(BM_FIRST_NAME, POP_PATIENTFIRST)) %>% 
#   pull(USCID)

# output_filter_uscid_email_newid <- dplyr::filter(output_filter_uscid_email, ! USCID %in% remove_uscid)
output_filter_uscid_email_newid <- output_filter_uscid_email



# ---- Final dataset ---- #
output = output_filter_uscid_email_newid

# final check
any(duplicated(output$USCID)); any(duplicated(output$POP_EMAIL)); any(duplicated(output$POP_NEWID))


output <- output %>% 
  filter(!is.na(USCID))










# ======================================================================== #
# merge vac and testing information
# delete null columns
# create summaries
# ======================================================================== #

# Vaccination information
work_vac <- filter(vac, USCID %in% output$USCID)


# Testing information
work_lab <- filter(lab_panel_wide, USCID %in% output$USCID)



# for Kush - give him two datasets
# 1) complete N = 711, but exclude pop info except for age, division, sub-division, acdemic yeear
# 2) subset of individuals and every instance they tested positive


output1 <- output %>% 
  left_join(vac_wide, 'USCID') %>% 
  left_join(lab_panel_wide, 'USCID') %>% 
  dplyr::select(-starts_with(c("ProcedureDate_", "VaccineType_", "Result.Date_", "Result_", "Panel_")),
                -contains("MERGE_FLAG"), 
                -(BM_FNAME:BM_DOB_YEAR), 
                -(BM_FIRST_NAME:BM_NEWID), 
                -(POP_PATIENTFIRST:POP_NEWID))
                # Panel, Result.Date, Result, PatientType, AcademicLevel)
names(output1)
saveRDS(output1, file = "data/2022_01_14/bm_pnc_match_711.rds")

# first - identify positive test results (any) 
table(lab_panel_long$Result)
positives <- c("Detected", 
               "DETECTED", 
               "positive",
               "Positive",
               "Positive SARS-CoV-2", 
               "Presumptive Positive")
output2 <- left_join(output, lab_panel_long, "USCID") %>% 
  filter(Result %in% positives)
saveRDS(output2, file = "data/2022_01_14/bm_pnc_match_711_pos.rds")






head(output)

out_final <- left_join(output, vac, 'USCID') %>% 
  left_join(lab_panel, 'USCID') %>% 
  dplyr::select(where(~ !all(is.na(.x))))

saveRDS(out_final, glue("data/{date}/out_final.rds"))


# experiment making data long format .. get positives etc


work <- out_final %>% 
  pivot_longer(
    cols = starts_with("Result.Date_"), 
    names_to = "result_count", 
    names_prefix = "Result.Date",
    values_to = "result_date")



work2 <- out_final %>% 
  pivot_longer(
    cols = starts_with("Result_"), 
    names_to = "result_count", 
    names_prefix = "Result.Date",
    values_to = "result_date")

work3 <- out_final %>% 
  pivot_longer(
    cols = starts_with("Panel_"), 
    names_to = "result_count", 
    names_prefix = "Result.Date",
    values_to = "result_date")





