#Script IBD_Biologics comorbidities
#Date initiated: 24/03/2020
#S. Zeki

# Raw data query
#Get the raw data from hospital pharmacy as a list of hospital numbers.
#Then use STHAnalytics::PatientComorbiditiesAndDemog to get the comorbidities and then ?furher filter here???

library(here)
library(dplyr)
library(tidyverse)

#Import csv with comorbidities from SQL query above:
my_biologics_comorbidities_data<-read.csv(here("DevFiles","IBD","IBD_Biologics_Comorbidities3.csv"),stringsAsFactors = F)

#Select the high risk patients based on age and comorbidity
high_risk_comorbidity_patients<-my_biologics_comorbidities_data %>% filter(
  (AgeInYearsCurrent>69)|grepl("Angina|COPD|Diabete|(High blood pressure)|ertension|IHD|asthma|Bronchiectasis|heart|leukaemia|leukemia|atrial|^Cardio|pulmon|Emphysema|lymphoma|transplant|Obesity",DiagnosisDSC,ignore.case = T))


#Collapse all the rows for a single patient together:
library(dplyr)
library(tidyverse)
high_risk_comorbidity_patients2<-high_risk_comorbidity_patients %>%
  group_by(PatientID) %>%
  summarize_all(paste, collapse=",")


#Get the unique hospital numbers for the patient:
high_risk_comorbidity_patients2$ADDRESS_L1<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$ADDRESS_L1, ","), unique), paste, character(1L), collapse = ",")

#Make other columns unique too- get this into a function eventually:
high_risk_comorbidity_patients2$ADDRESS_L2<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$ADDRESS_L2, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$ADDRESS_L3<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$ADDRESS_L3, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$ADDRESS_L4<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$ADDRESS_L4, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$PostCode<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$PostCode, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$Home_Phone<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$Home_Phone, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$DAY_PHONE<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$DAY_PHONE, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$NHSNBR<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$NHSNBR, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$PatientFullNM<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$PatientFullNM, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$FirstNM<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$FirstNM, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$SurNM<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$SurNM, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$DiagnosisDSC<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$DiagnosisDSC, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$DiagnosisCD<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$DiagnosisCD, ","), unique), paste, character(1L), collapse = ",")

high_risk_comorbidity_patients2$BirthDTS<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$BirthDTS, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$AgeInYearsCurrent<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$AgeInYearsCurrent, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$MostRecentCoderNM<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$MostRecentCoderNM, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$MostRecentCodingDTS<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$MostRecentCodingDTS, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$BindingNM<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$BindingNM, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$LastLoadDTS<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$LastLoadDTS, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$BindingID<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$BindingID, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$TotalAdmissionsCNT<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$TotalAdmissionsCNT, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$PtSpellsWithThisSubcategoryCNT<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$PtSpellsWithThisSubcategoryCNT, ","), unique), paste, character(1L), collapse = ",")
high_risk_comorbidity_patients2$CodePerAdmissionsPCT<-vapply(lapply(strsplit(high_risk_comorbidity_patients2$CodePerAdmissionsPCT, ","), unique), paste, character(1L), collapse = ",")

#Merge the addres
high_risk_comorbidity_patients2$FullAddress<-paste0(high_risk_comorbidity_patients2$ADDRESS_L1,",",
                                                    high_risk_comorbidity_patients2$ADDRESS_L2,",",
                                                    high_risk_comorbidity_patients2$ADDRESS_L3,",",
                                                    high_risk_comorbidity_patients2$ADDRESS_L4,",",
                                                    high_risk_comorbidity_patients2$PostCode)
#Clean up:
high_risk_comorbidity_patients2$ADDRESS_L1<-gsub("NULL","",high_risk_comorbidity_patients2$ADDRESS_L1)
high_risk_comorbidity_patients2$ADDRESS_L2<-gsub("NULL","",high_risk_comorbidity_patients2$ADDRESS_L2)
high_risk_comorbidity_patients2$ADDRESS_L3<-gsub("NULL","",high_risk_comorbidity_patients2$ADDRESS_L3)
high_risk_comorbidity_patients2$ADDRESS_L4<-gsub("NULL","",high_risk_comorbidity_patients2$ADDRESS_L4)

high_risk_comorbidity_patients2$FullAddress<-gsub(",NULL","",high_risk_comorbidity_patients2$FullAddress)

write.csv(high_risk_comorbidity_patients2,here("DevFiles","IBD","outputHighRiskForCovidBasedOnHC_Comorbidities.csv"))

