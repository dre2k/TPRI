
date <- "2021_09_27"




# STAFF STAFF STAFFF
# wave 1 .. 

input_data <- readRDS(glue("{wdir}/health_survey_staff_w1_w2_deidentified_{date}.rds")) %>% 
  filter(wave_flag %in% c("1", "1,2")) %>% 
  # remove unnecessary variabless
  dplyr::select(-contains(c("TIMESTAMP", "delete", "W2", "ATTEMPT", ".FACTOR", "ASIAN")), 
                -contains(c("RECORD", "SIGNATURE", "NAME", "INIT", "SEX", "GIFT", "wave")), 
                -starts_with("POP"), 
                -starts_with("VAC"))
# duplicate variable cleaning .. 

# let's visualize missingness by column .. 
p1 <- input_data[, 1:100]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 6))


ggsave(p1, filename = "~/Desktop/vaccine/health_survey_staff_wave1_missingness_part1.png", width = 9, height = 8)


p2 <- input_data[, 101:ncol(input_data)]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 6))

ggsave(p2, filename = "~/Desktop/vaccine/health_survey_staff_wave1_missingness_part2.png", width = 9, height = 8)





# wave 2 .. 


input_data <- readRDS(glue("{wdir}/health_survey_staff_w1_w2_deidentified_{date}.rds")) %>% 
  dplyr::filter(wave_flag %in% c("1,2", "2")) %>% 
  #HSS_W2_TPRI_HEALTH_SURVEY_COMPLETE == 2) %>% 
  # remove unnecessary variabless
  dplyr::select(-contains(c("TIMESTAMP", "delete", "ATTEMPT", ".FACTOR", "ASIAN")), 
                -contains(c("RECORD", "SIGNATURE", "NAME", "INIT", "SEX", "GIFT", "wave")), 
                -starts_with("POP"), 
                -starts_with("VAC")) %>% 
  dplyr::select(contains("HSS_W2"))
# duplicate variable cleaning .. 





# let's visualize missingness by column .. 
p1 <- input_data[, 1:90]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))


ggsave(p1, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part1.png", width = 9, height = 8)


p2 <- input_data[, 91:180]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))

ggsave(p2, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part2.png", width = 9, height = 8)


p3 <- input_data[, 181:270]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))

ggsave(p3, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part3.png", width = 9, height = 8)


p4 <- input_data[, 271:ncol(input_data)]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))

ggsave(p4, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part4.png", width = 9, height = 8)








# COMPLETE ONLY
# wave 2 .. 


input_data <- readRDS(glue("{wdir}/health_survey_staff_w1_w2_deidentified_{date}.rds")) %>% 
  dplyr::filter(wave_flag %in% c("1,2", "2"), 
                HSS_W2_TPRI_HEALTH_SURVEY_COMPLETE == 2) %>%
  # remove unnecessary variabless
  dplyr::select(-contains(c("TIMESTAMP", "delete", "ATTEMPT", ".FACTOR", "ASIAN")), 
                -contains(c("RECORD", "SIGNATURE", "NAME", "INIT", "SEX", "GIFT", "wave")), 
                -starts_with("POP"), 
                -starts_with("VAC")) %>% 
  dplyr::select(contains("HSS_W2"))
# duplicate variable cleaning .. 





# let's visualize missingness by column .. 
p1 <- input_data[, 1:90]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))


ggsave(p1, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part1_c.png", width = 9, height = 8)


p2 <- input_data[, 91:180]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))

ggsave(p2, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part2_c.png", width = 9, height = 8)


p3 <- input_data[, 181:270]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))

ggsave(p3, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part3_c.png", width = 9, height = 8)


p4 <- input_data[, 271:ncol(input_data)]  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(size = 7))

ggsave(p4, filename = "~/Desktop/vaccine/health_survey_staff_wave2_missingness_part4_c.png", width = 9, height = 8)

