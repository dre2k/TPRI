x <- fread("E:/Vaccine events 09.23.21 deduped.csv") %>% 
  dplyr::select(-PatientName, -LastName, -FirstName, -Comment, -Inclusion, -ProcedureNote) %>% 
  arrange(PatientNumber) %>% 
  dplyr::filter(IsDeclined != T, 
                !duplicated(.))


table(x$ImmunizationName)

# actually, remove dups
x <- fread("E:/Vaccine events 09.23.21 deduped.csv") %>%
  # dplyr::filter(IsDeclined != T) %>% 
  dplyr::select(PatientNumber, ImmunizationName, ProcedureDate) %>% 
  filter(!duplicated(.)) %>% 
  separate(ProcedureDate, into = c("a", "b"), sep = " ") %>% 
  mutate(ProcedureDate = mdy(a)) %>% dplyr::select(-a, -b)

x <- fread("E:/Vaccine events 09.23.21 deduped.csv") %>%
  # dplyr::filter(IsDeclined != T) %>% 
  dplyr::select(PatientNumber, ImmunizationName, ProcedureDate) %>% 
  separate(ProcedureDate, into = c("a", "b"), sep = " ") %>% 
  mutate(ProcedureDate = mdy(a)) %>% dplyr::select(-a, -b) %>% 
  filter(!duplicated(.)) 
  


any(is.na(x$PatientNumber))
any(is.na(x$ImmunizationName))
any(is.na(x$ProcedureDate))


# I want all individuals who only got 1 vaccine

y <- x %>% 
  dplyr::select(ImmunizationName, PatientNumber, ProcedureDate) %>% 
  count(PatientNumber)

# patients who only got single vaccine, what brand is it
tmp_one <- filter(y, n == 1)
one <- filter(x, PatientNumber %in% unique(tmp_one$PatientNumber))

out <- data.frame(table(one$ImmunizationName))
write.csv(out, file = "E:/one.csv", quote = F)




# patients who got two doses..
tmp_two <- filter(y, n == 2)
two <- filter(x, PatientNumber %in% unique(tmp_two$PatientNumber)) %>% 
  arrange(PatientNumber, ProcedureDate) %>%
  dplyr::select(-ProcedureDate) %>% 
  group_by(PatientNumber) %>% 
  summarise(ImmunizationName = paste(ImmunizationName, collapse = ","))


out <- data.frame(table(two$ImmunizationName))
write.csv(out, file = "E:/two.csv", quote = F)



# Patients who got more than two
tmp_twomore <- filter(y, n > 2)
more <- filter(x, PatientNumber %in% unique(tmp_twomore$PatientNumber)) %>% 
  arrange(PatientNumber, ProcedureDate) %>%
  dplyr::select(-ProcedureDate) %>% 
  group_by(PatientNumber) %>% 
  summarise(ImmunizationName = paste(ImmunizationName, collapse = ","))

out <- data.frame(table(more$ImmunizationName))
write.csv(out, file = "E:/more.csv", quote = F)
