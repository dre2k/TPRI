#==============================================================================#
# Read in vaccination data (COVID-19 vaccine events report)
#==============================================================================#
library(tidyverse)
library(data.table)
library(haven)
library(dplyr)
library(stringr)
library(readxl)
library(tableone)
library(ggalluvial)
library(ggpubr)
library(tidyr)
# library(clipr)


# -------------------- #
# make vax report wide
# -------------------- #

# dat0 <- fread(glue("E:/USCVaccineStudy/data/{date}/covid19_vaccination_events.csv"))
# WHOapproved <- c("Pfizer", "Moderna", "J&J", "AstraZeneca",
#                  "CoviShield", "Sinopharm-BIBP", "Sinovac-CoronaVac")

dat0 <- fread(glue("data/{date}/covid19_vaccination_events.csv")) %>% 
  dplyr::filter(IsDeclined != "True") %>%
  # dplyr::select(-c("PatientName", "LastName", "FirstName")) %>%
  dplyr::rename(ImmunizationName = Name) %>% 
  # Remove Duplicates
  group_by(PatientNumber, ProcedureDate, ImmunizationName) %>%
  slice(1)

names(dat0)


vac_long <- dat0 %>% 
  mutate(ProcedureDate = as.Date(ProcedureDate, "%m/%d/%Y")) %>%
  mutate(USCID = as.numeric(PatientNumber)) %>% 
  filter(!is.na(USCID)) %>% 
  group_by(USCID) %>%
  arrange(USCID, ProcedureDate) %>%
  mutate(ndoses = n(),
         dose = row_number(),
         VaccineType = case_when(
           ImmunizationName == "COVID-19 (AstraZeneca)" ~ "AstraZeneca",
           ImmunizationName == "COVID-19 (Bharat-Covaxin)" ~ "BharatCovaxin",
           ImmunizationName == "COVID-19 (CanSino-Convidecia)" ~ "CanSino-Convidecia",
           ImmunizationName == "COVID-19 (Covishield)" ~ "CoviShield",
           ImmunizationName == "COVID-19 (CoviVac)" ~ "CoviVac",
           ImmunizationName == "COVID-19 (EpiVacCorona)" ~ "EpiVacCorona",
           ImmunizationName %in% c("COVID-19 (J&J)", "COVID-19 Vaccine (J&J)")  ~ "J&J",
           ImmunizationName == "COVID-19 (Longcom-RBD-Dimer)" ~ "Longcom-RBD-Dimer",
           ImmunizationName %in% c("COVID-19 (Moderna)", "COVID-19 Vaccine (Moderna)") ~ "Moderna",
           ImmunizationName == "COVID-19 (Novavax)" ~ "Novavax",
           ImmunizationName %in% c("COVID-19 (Pfizer)", "COVID-19 Vaccine (Pfizer)") ~ "Pfizer",
           ImmunizationName == "COVID-19 (Sinopharm-BIBP)" ~ "Sinopharm-BIBP",
           ImmunizationName == "COVID-19 (Sinopharm-WIBP)" ~ "Sinopharm-WIBP",
           ImmunizationName == "COVID-19 (Sinovac-CoronaVac)" ~ "Sinovac-CoronaVac",
           ImmunizationName == "COVID-19 (Sputnik V)" ~ "Sputnik V",
           TRUE ~ "Unknown"),
         WHOapproved = VaccineType %in% c("Pfizer", "Moderna", "J&J", "AstraZeneca",
                                          "CoviShield", "Sinopharm-BIBP", "Sinovac-CoronaVac")
  ) 

# test <- filter(dat, grepl("N00", PatientNumber))
# 
# test <- filter(dat, dose == 1) %>% 
#   group_by(VaccineType) %>% 
#   summarise(n = n())
# 
# test <- filter(dat, dose == 1) %>% 
#   group_by(ImmunizationName) %>% 
#   summarise(n = n())


vac_wide <- vac_long %>% 
  pivot_wider(
    id_cols = "PatientNumber",
    names_from = "dose", 
    values_from = c("ProcedureDate", "VaccineType")) %>% 
  mutate(USCID = as.numeric(PatientNumber)) %>% 
  filter(!is.na(USCID))

rm(dat0)
