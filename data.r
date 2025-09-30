

AthleteData <- read_csv("testdata.csv") %>% 
  select(practitioner:date,comments,matches("value")) %>% 
  rename_with(~str_replace_all(.x,"_cm_value|_mm_value|_kg_value","")) %>% 
  rename(reporter=practitioner,
         Date=date,
         AthleteName=athlete) %>% 
  rowwise() %>% 
  mutate(lmi=weight/sum(triceps,subscap,biceps,supraspinale,abdomen,
                        ifelse(is.na(r_calf),l_calf,ifelse(is.na(l_calf),r_calf,mean(r_calf,l_calf))),
                        ifelse(is.na(r_thigh),l_thigh,ifelse(is.na(l_thigh),r_thigh,mean(r_thigh,l_thigh))))^0.14,
         so8=sum(triceps,subscap,biceps,illiac,supraspinale,abdomen,
                 ifelse(is.na(r_calf),l_calf,ifelse(is.na(l_calf),r_calf,mean(r_calf,l_calf))),
                 ifelse(is.na(r_thigh),l_thigh,ifelse(is.na(l_thigh),r_thigh,mean(r_thigh,l_thigh))))) %>% 
  ungroup() %>% 
  arrange(desc(Date)) %>% 
  slice_head(n=10) %>% 
  mutate_if(is.numeric, ~ round(., 2))

