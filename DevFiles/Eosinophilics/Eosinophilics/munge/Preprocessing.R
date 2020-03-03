# Example preprocessing script.
library(dplyr)
library(stringr)
library(here)
library(EndoMineR)
library(readxl)
library(tidyr)
library(PhysiMineR)
library(finalfit)
library(knitr)
library(kableExtra)
library(cutpointr)
library(ggbeeswarm)
library(ggplot2)
library(gridExtra)
library(gtsummary)
library(gt)
library(ggpubr)



##### Import ####
## @knitr dataImport

oldw <- getOption("warn")

options(warn = -1)

#Get this from the data folder for PhysiMineR-  just click on the RData to load into
#the global environment

#Or if using the local file system

BravoDayOneAndTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/BravoDay1And2.xls"))
BravoDayThreeAndFour <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/BravoDay3And4.xls"))
BRAVOTotal <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/BRAVOTotal.xls"))
Diag <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Diag.xls"))
HRMImportMain <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportMain.xls"))
HRMImportSwallows <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/HRMImportSwallows.xls"))
Imp_Symp <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Imp_Symp.xls"))
ImpedanceTwo <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/Impedance2.xls"))
Procs <- read_excel(here::here("inst/Projects/BRAVOStudies/NegImpPredictorsOfAllPosBRAVO/data/ProcByHospitalNumbers.xls"))

options(warn = oldw)




##### Clean ####################################################################################################################
## @knitr dataClean
#Data cleaning
#Use after PhysiData_Acq
#Clean HRM
#Clean up the HRM swallows:
HRMImportSwallows<-HRMImportSwallows[!is.na(HRMImportSwallows$panesophagealpressurizationMapSwallowsNum8),]





##### IntraTestMergeAndClean ########################################################################################
## @knitr dataIntraTestMerge

# Get the whole impedance dataset
#You will need to re-clean the merged dataImpSympImpedance as cleaning function needed to be fixed
AllImpedance<-merge(ImpedanceTwo,Imp_Symp,by="Imp_Id", all = TRUE)
ImpedanceThree<-dataImpClean(AllImpedance)
ImpAll<-dataImpSymptoms(ImpedanceThree)
ImpAll<-GORD_AcidImp(ImpAll)





Diag<-dataDiagClean(Diag)





#Merge the HRM results together (swallows and Main)
AllHRM<-merge(HRMImportMain,HRMImportSwallows,by="HRM_Id",all=TRUE)
#Get rid of the swallows for now as using the whole HRM dataset makes things slow:
AllHRM<-AllHRM[,!grepl("Num\\d+",colnames(AllHRM))]
HRMImportMainTwo<-AllHRM %>%
  group_by(HospNum_Id,DistalLESfromnarescm)%>%
  summarise_all(.funs = function(x) paste(unique(c(dplyr::lag(x, default = NULL), x)), collapse = ":"))
#Clean Up the main HRM
HRMImportMainTwo<-HRMCleanUp1(HRMImportMainTwo)



#Merge the BRAVO results together:
AllBravo<-merge(BravoDayOneAndTwo,BravoDayThreeAndFour,by="BravoID",all=TRUE)
AllBravo<-merge(AllBravo,BRAVOTotal,by="BravoID",all=TRUE)
AllBravo<-dataBRAVOClean(AllBravo)
AllBravo<-dataBRAVODayLabeller(AllBravo,"HospNum_Id","VisitDate")
AllBravo<-dataBRAVOSymptoms(AllBravo)
AllBravo<-GORD_AcidBRAVO(AllBravo)
AllBravo<-GORD_BravoWDAAndAverage(AllBravo)






#Get the BRAVO procedures

Procs<-data.frame(Procs,stringsAsFactors = FALSE)
Procs$DATEOFPROCEDURE<-as.Date(Procs$DATEOFPROCEDURE)

names(Procs)<-c("HospNum_Id","VisitDate.x","Findings","Oesophagitis")
Procs<-data.frame(Procs,stringsAsFactors = FALSE)








##### CrossTestMerge ########################################################################################
## @knitr dataCrossTestMerge


#Merge all HRM with AllBravo to get the full dataset
#This merges the tests so that the nearest hrm to the BRAVO is chosen to avoid replication
#Note that a lot of Bravo's do not have HRM as may not have been requested by outside referrers





BravoAnd_diag <- Diag %>% inner_join(AllBravo, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id) %>%



BravoAndDiag_HRM <- HRMImportMainTwo %>% inner_join(BravoAnd_diag, by ="HospNum_Id") %>%
  mutate(Date_ABS_Diff = abs(VisitDate.x - VisitDate.y)) %>%
  arrange(HospNum_Id, Date_ABS_Diff) %>%
  group_by(HospNum_Id)

