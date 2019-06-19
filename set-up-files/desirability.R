require(xlsx)
require(dplyr)

# Create list of special schools to remove, as these will skew attainment results
special_schools <- c("ACCS", "ACS", "CYS", "FD", "FDS", "INDSPEC", "NMSS")

# Read in ks2 data
ks2_final_1718 <- read.csv(file = "england_ks2final.csv", stringsAsFactors = FALSE)
# Select columns we need
ks2_final_1718 <- ks2_final_1718 %>% 
                  filter(!(NFTYPE %in% special_schools), !(PTKS1GROUP_H %in% c('','SUPP'))) %>%
                  select(LEA, URN, SCHNAME, PTKS1GROUP_H) %>% 
                                     rename(LA.code = LEA,
                                            percentage.high.KS1 = PTKS1GROUP_H,
                                            School.name = SCHNAME) 

# Remove and create metric 
ks2_final_1718$percentage.high.KS1 <- as.numeric(lapply(ks2_final_1718$percentage.high.KS1, function(x){(gsub("%", "", x))})) / 100


ks4_final_1718 <- read.csv(file = "england_ks4final.csv", stringsAsFactors = FALSE)

# Filter columns selected
ks4_final_1718 <- ks4_final_1718 %>% 
                  filter(!(NFTYPE %in% special_schools), !(ATT8SCR %in% c('','SUPP','NE'))) %>%
                  select(LEA, URN, SCHNAME, ATT8SCR) %>%
                  rename(LA.code = LEA,
                         avg.attainment.per.pupil = ATT8SCR,
                         School.name = SCHNAME) %>%
                  mutate(avg.attainment.per.pupil = as.numeric(avg.attainment.per.pupil) / 100)


cfr_full_1718 <- read.csv(file = "cfrfull.csv", stringsAsFactors = FALSE)
cfr_full_1718 <- cfr_full_1718 %>% 
                 filter(!grepl("special", Establishment.type)) %>%
                 select(LA.code,	LA.name,	URN,	School.name, Phase.of.education, Number.of.Pupils..FTE., 
                        E19.Learning.resources..not.ICT.equipment., E20..ICT.learning.resources)

cfr_full_1718$E19.Learning.resources..not.ICT.equipment. <- as.numeric(lapply(cfr_full_1718$E19.Learning.resources..not.ICT.equipment., function(x){(gsub(",", "", x))}))
cfr_full_1718$E20..ICT.learning.resources <- as.numeric(lapply(cfr_full_1718$E20..ICT.learning.resources, function(x){(gsub(",", "", x))}))
cfr_full_1718$Number.of.Pupils..FTE. <- as.numeric(lapply(cfr_full_1718$Number.of.Pupils..FTE., function(x){(gsub(",", "", x))}))

cfr_full_1718 <- cfr_full_1718 %>% 
                 mutate(total.spend.learning.resources = E19.Learning.resources..not.ICT.equipment. + E20..ICT.learning.resources) %>%
                 mutate(spend.per.pupil = total.spend.learning.resources / Number.of.Pupils..FTE.)

# Find min and max value of spend per pupil
min_spend_value <- min(cfr_full_1718$spend.per.pupil)
max_spend_value <- max(cfr_full_1718$spend.per.pupil)

# Create metric from 0 to 1
cfr_full_1718$spend.metric <- (cfr_full_1718$spend.per.pupil - min_spend_value) / (max_spend_value - min_spend_value)

staff_numbers_17 <- read.csv("Staff number 2017.csv")
staff_numbers_17 <- staff_numbers_17 %>%
                    filter(!grepl("special", School.Type.Description)) %>%
                    select(LA.Number, URN, Total.Number.of.Classroom.Teachers..Headcount.,
                           Total.Number.of.Classroom.Teachers..Full.time.Equivalent., Total.Number.of.Teachers..Headcount.) %>%
                    rename(LA.code = LA.Number)

# Join staff numbers to cfr_full for pupil numbers
cfr_full_1718 <- cfr_full_1718 %>% select(LA.code, LA.name, URN, School.name, Phase.of.education, Number.of.Pupils..FTE., spend.metric)

desirability <- left_join(cfr_full_1718, staff_numbers_17, by = c("LA.code", "URN"))
desirability$Total.Number.of.Teachers..Headcount. <- as.numeric(as.character(desirability$Total.Number.of.Teachers..Headcount.))
desirability$Total.Number.of.Classroom.Teachers..Full.time.Equivalent. <- as.numeric(as.character(desirability$Total.Number.of.Classroom.Teachers..Full.time.Equivalent.))
desirability$pupil.per.staff <- desirability$Number.of.Pupils..FTE. / desirability$Total.Number.of.Teachers..Headcount.

desirability$pupil.per.staff[is.infinite(desirability$pupil.per.staff)]<-NA

min_pupil_per_staff <- min(desirability$pupil.per.staff, na.rm = TRUE)
max_pupil_per_staff <- max(desirability$pupil.per.staff, na.rm = TRUE)

desirability$staff.metric <-  1- ((desirability$pupil.per.staff - min_pupil_per_staff) / (max_pupil_per_staff - min_pupil_per_staff))

desirability <- left_join(desirability, ks2_final_1718)
desirability <- left_join(desirability, ks4_final_1718)

## Spend per pupil by LA
#spend_per_pupil <- read.csv("C:/Users/HR612KP.UK/Documents/Loneliness challenge/Spend per pupil.csv")

### Ofsted 2018
ofsted_scores <- read.xlsx("england_ofsted-schools.xlsx", sheetName = "DFE_data_31_Mar_2019")
ofsted_scores <- ofsted_scores %>%
                 filter(!grepl("Special", Ofsted.phase)) %>%
                 mutate(Ofsted.metric = 1 - ((Overall.effectiveness - 1)/ 3))
  
desirability <- left_join(desirability, ofsted_scores, by = c("URN", "School.name"))
ks2_schools <- c("Infant and junior", "Junior")

desirability <- desirability %>%
                filter(Phase.of.education != "Infant")%>%
                mutate(attainment.metric = ifelse(Phase.of.education %in% ks2_schools, percentage.high.KS1, avg.attainment.per.pupil)) %>%
                select(LA.code, LA.name, URN, School.name, Phase.of.education, spend.metric, staff.metric,
                       attainment.metric, Ofsted.metric) # remove infant schools as they have no KS1 metric
                
desirability <- na.omit(desirability)
desirability_by_school <- desirability %>%
                mutate(desirability.metric = (attainment.metric + spend.metric + staff.metric + Ofsted.metric) / 4)

desirability <- desirability_by_school %>%
                group_by(LA.code, LA.name) %>%
                summarise(desirability.metric = mean(desirability.metric))
  
saveRDS(desirability, file = "desirability.RDS")

#To add school type later?
#school_type <- read.csv("C:/Users/HR612KP.UK/Documents/Loneliness challenge/2017-2018/england_spine.csv")

### KS4 destinations - unsure if we need
#destinations_ks4 <- read.csv("C:/Users/HR612KP.UK/Documents/Loneliness challenge/Destinations.csv")

