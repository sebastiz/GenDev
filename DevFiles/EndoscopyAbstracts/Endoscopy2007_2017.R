library(readxl)
library(EndoMineR)
library(stringr)
library(dplyr)
library(lubridate)
library(psych)
library(zoo)




#######################################????????????????Data acquisiton######################################### 
#This is where the data is imported
Mydata<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/AdHoc/GSTT_GastroscopyProcedure2007_2017.xlsx"))
names(Mydata)<-c("PatientID","NHSno","PatientName","birthdaynum","birthmonthnum","birthyearnum","Endo_ResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText")

#######################################????????????????Data merging######################################### 
#This is where external datasets are merged with the current dataset. This can be done after cleaning as well especially if you need to clean dates etc.
#######################################????????????????Data cleaning######################################### 


RenameCols<-function(x){
  names(x)<-c("PatientID","NHSno","PatientName","birthdaynum","birthmonthnum","birthyearnum","Endo_ResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText")
  x<-x[-1,]
  return(x) 
}



Mydata2<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Apr17).xlsx"))
Mydata2<-RenameCols(Mydata2)
Mydata11<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Aug17).xlsx"))
Mydata11<-RenameCols(Mydata11)
Mydata3<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Dec17).xlsx"))
Mydata3<-RenameCols(Mydata3)
Mydata4<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Feb17).xlsx"))
Mydata4<-RenameCols(Mydata4)
Mydata5<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(July17).xlsx"))
Mydata5<-RenameCols(Mydata5)
Mydata6<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(June17).xlsx"))
Mydata6<-RenameCols(Mydata6)
Mydata7<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Mar17).xlsx"))
Mydata7<-RenameCols(Mydata7)
Mydata8<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(May17).xlsx"))
Mydata8<-RenameCols(Mydata8)
Mydata9<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Nov17).xlsx"))
Mydata9<-RenameCols(Mydata9)
Mydata10<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Oct17).xlsx"))
Mydata10<-RenameCols(Mydata10)


FinalData<-rbind(Mydata,Mydata2)
FinalData<-rbind(FinalData,Mydata3)
FinalData<-rbind(FinalData,Mydata4)
FinalData<-rbind(FinalData,Mydata5)
FinalData<-rbind(FinalData,Mydata6)
FinalData<-rbind(FinalData,Mydata7)
FinalData<-rbind(FinalData,Mydata8)
FinalData<-rbind(FinalData,Mydata9)
FinalData<-rbind(FinalData,Mydata10)
FinalData<-rbind(FinalData,Mydata11)



Mydata<-FinalData

Mydata$Endo_ResultEntered<-as.Date(Mydata$Endo_ResultEntered,origin="1899-12-30")
Mydata$Endo_ResultPerformed<-as.Date(Mydata$Endo_ResultPerformed,origin="1899-12-30")
Mydata$Histo_ResultPerformed<-as.Date(Mydata$Histo_ResultPerformed,origin="1899-12-30")
Mydata$Histo_ResultEntered<-as.Date(Mydata$Histo_ResultEntered,origin="1899-12-30")

HistolTree<-c("NATURE OF SPECIMEN:","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY",
                 "DIAGNOSIS","")
for(i in 1:(length(HistolTree)-1)) {
  Mydata<-Extractor(Mydata,"Histo_ResultText",as.character(HistolTree[i]),
                    as.character(HistolTree[i+1]),as.character(HistolTree[i]))
}


Mydata$Endo_ResultText<-gsub("2nd Endoscopist","Second Endoscopist",Mydata$Endo_ResultText)
EndoTree<-list("Patient Name:","Date of Birth:","Hospital Number:","Date of Procedure:","Endoscopist:","Second Endoscopist:",
               "Referring Physician:","General Practicioner:","Nurses:",
               "Medications:","Instrument:","Extent of Exam:","Visualization:","Tolerance:","Complications:","Co-morbidity:",
               "INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED",
               "FINDINGS","ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","BIOPSIES","OPCS4 Code:","Signature:","")
for(i in 1:(length(EndoTree)-1)) {
  Mydata<-Extractor(Mydata,"Endo_ResultText",as.character(EndoTree[i]),
                    as.character(EndoTree[i+1]),as.character(EndoTree[i]))
}


Mydata<-EndoscEndoscopist(Mydata,'Endoscopist')
Mydata$Endoscopist<-gsub("Second","",Mydata$Endoscopist)
Mydata<-EndoscMeds(Mydata,'Medications')
Mydata<-EndoscInstrument(Mydata,'Instrument')
Mydata<-EndoscIndications(Mydata,'INDICATIONSFOREXAMINATION')
Mydata<-EndoscProcPerformed(Mydata,'PROCEDUREPERFORMED')


Mydata<-HistolrHistol(Mydata,'HISTOLOGY')
Mydata<-HistolrDx(Mydata,"DIAGNOSIS")
Mydata<-HistolExtrapolDx(Mydata,"DIAGNOSIS")



######################################################????????????????Data forking (filtering and subsetting)######################################### 



#To do: Decide on retrospective cohort question types that often get asked as generic questions then code that up


############                Adenomas         ############

dataframe <- Self[grepl("Colonoscopy", Self$PROCEDUREPERFORMED),]
MyAdenomaDataFrame <- dataframe[grep(".*[Aa]denom.*", dataframe$Dx),]



############                Eosinophil         ############
#Get rid of 'no evidence of'
MydataDx<-Mydata[grep("osinop",Mydata$DIAGNOSIS),]
MydataDx$DIAGNOSIS<-gsub("\n","-",MydataDx$DIAGNOSIS,fixed=TRUE)
MydataDx<-MydataDx[!grepl(".*[Nn]o\\s*evidence\\s*of\\s*[Ee]osinop.*",MydataDx$DIAGNOSIS),]
MydataDx<-MydataDx[!grepl(".*histological evidence\\s+of\\s+.*[Ee]osinop.*",MydataDx$Histo_ResultText),]
EoEDx<-MydataDx

############                Barretts         ############
MyBarrettsData<-Mydata[grepl("Barr",Mydata$FINDINGS),]




#######################################????????????????Data accordionisation ############################################################


############                Adenomas         ############
#Remove the previous polyp rows
MyAdenomaDataFrame<-MyAdenomaDataFrame[!grepl("Surveillance- Previous Polyps",MyAdenomaDataFrame$INDICATIONSFOREXAMINATION),]

# Get the number of adenomas diagnosed by year
MyAdenomaDataFrame%>%arrange(as.Date(Endo_ResultPerformed)) %>% group_by(
  year = year(as.Date(Endo_ResultPerformed))
) %>% summarise(Number = n())




############                Eosinophil         ############


EoEDx$HPF<-as.numeric(sapply(str_extract_all(EoEDx$Histo_ResultText, "[0-9]+(?=(\\s+[Ii]ntraepithelial)?(\\s+[Ee]osinophils)?\\s+(per|in one|in a|\\/)?.+([Hh][Pp][Ff]|[Hh]igh.+power.+field|[Hh]ighpower\\s+field))"), 
                             function(x) x[which.max(as.numeric(x))][1]))


#These are all the endoscopies with high hpf
PatientsWithEoE<-subset(EoEDx,EoEDx$HPF>=15)


############                Barretts         ############
em<-SurveillanceCapacity(MyBarrettsData,"Endo_ResultPerformed")
how<-HowManyTests(MyBarrettsData,'INDICATIONSFOREXAMINATION','Endo_ResultPerformed','Surv')
b1<-BarrettsDataAccord_Prague(MyBarrettsData,'FINDINGS')
b2<-BarrettsDataAccord_PathStage(b1,'HISTOLOGY')
b3<-BarrettsDataAccord_Event(b2,'HISTOLOGY','PROCEDUREPERFORMED','Endo_ResultText','FINDINGS')
b4<-BarrettsDataAccord_FUGroup(b3,'FINDINGS')
Rule<-BarrettsPatientTracking_UniqueHospNum(b4,'Rule1','HospitalNumber')
BarrettsQuality_AnalysisDocumentation(b4,"FINDINGS")
BarrettsSurveillance_PathDetection(b4,'Myplot')
Therapeutics<-b4[!grepl("nothing",b4$EVENT),]



#######################################????????????????Subset merging with other datasets ################ 



#######################################????????????????Cohort Question Type 1 ################ 
#######################################????????????????Cohort Question Type 2 ################ 
#######################################????????????????Cohort Question Type 3 ################ 
#######################################????????????????Cohort Question Type 4 ################ 
#######################################????????????????Cohort Question Type 5 ################ 
#######################################????????????????Cohort Question Type 6 ################ 



#######################################????????????????Demographics for subset ################ 
#M:F ratio (extract from the title in the Patient name column
Mydata$Gender<-ifelse(grepl("MRS|MS|MISS", Mydata$PatientName), "Female","Male")

genderStats<-Mydata%>%group_by(Gender)%>%summarise(n=n())

MFRatio<-genderStats[2,2]/genderStats[1,2]
#Age and range of age (age at diagnosis)
Mydata$DiagnosisYear<-lubridate::year(Mydata$Endo_ResultEntered)
Mydata$AgeAtDiagnosis<-Mydata$DiagnosisYear-Mydata$birthyearnum
summ<-describe(Mydata$AgeAtDiagnosis) 

summ$median
summ$mean
summ$range
summ$max
summ$min
summ$sd
summ$n
BarrettsTherapeuticsRFA_ByCatheter(Therapeutics,"FINDINGS","ENDOSCOPICDIAGNOSIS")





