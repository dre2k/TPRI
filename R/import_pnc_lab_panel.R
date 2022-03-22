# ---------------------------------------------------------------------------- #
# ---- COVID19 testing information ---- 
# ---------------------------------------------------------------------------- #

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

# make testing information wide .. 
# prioritize only certain fields

# testing + covid positivity information 
tmp_a <- read.csv(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_a.csv"))
tmp_b <- read.csv(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_b.csv"))
tmp_c <- read.csv(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_c.csv"))
tmp_d <- read.csv(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_d.csv"))
tmp_e <- read.csv(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats_e.csv"))
populationData <- bind_rows(tmp_a, tmp_b, tmp_c, tmp_d, tmp_e)
saveRDS(populationData, file = glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats.rds"))

dat0 <- readRDS(glue("data/{date}/SARS-CoV-2_Viral_Lab_Panel_Detail_Biostats.rds")) %>% 
  mutate(USCID = as.numeric(USCID)) %>%
  filter(!duplicated(.), 
         !is.na(USCID))


lab_panel_long <- dat0 %>% 
  dplyr::select(USCID, Panel, Result.Date, Result, PatientType, AcademicLevel) %>% 
  dplyr::filter(!USCID == "") %>% 
  mutate(Result.Date = as.Date(Result.Date, "%m/%d/%Y")) %>% 
  group_by(USCID) %>%
  arrange(USCID, Result.Date) %>% 
  mutate(tmp = row_number())


lab_panel_wide <- lab_panel_long %>% 
  pivot_wider(
    id_cols = c("USCID", PatientType, AcademicLevel),
    names_from = "tmp", 
    values_from = c("Result.Date", "Result", "Panel"))

rm(dat0)
