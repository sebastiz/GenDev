library(readxl)
library(EndoMineR)
Mydata<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/AdHoc/GSTT_GastroscopyProcedure2007_2017.xlsx"))
names(Mydata)<-c("PatientID","NHSno","PatientName","birthdaynum","birthmonthnum","birthyearnum","Endo_ResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText")



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


#Get rid of 'no evidence of'
MydataDx<-Mydata[grep("osinop",Mydata$DIAGNOSIS),]
MydataDx$DIAGNOSIS<-gsub("\n","-",MydataDx$DIAGNOSIS,fixed=TRUE)
MydataDx<-MydataDx[!grepl(".*[Nn]o\\s*evidence\\s*of\\s*[Ee]osinop.*",MydataDx$DIAGNOSIS),]
MydataDx<-MydataDx[!grepl(".*histological evidence\\s+of\\s+.*[Ee]osinop.*",MydataDx$Histo_ResultText),]
EoEDx<-MydataDx

library(zoo)
library(stringr)
EoEDx$HPF<-as.numeric(sapply(str_extract_all(EoEDx$Histo_ResultText, "[0-9]+(?=(\\s+[Ii]ntraepithelial)?(\\s+[Ee]osinophils)?\\s+(per|in one|in a|\\/)?.+([Hh][Pp][Ff]|[Hh]igh.+power.+field|[Hh]ighpower\\s+field))"), 
                             function(x) x[which.max(as.numeric(x))][1]))


#These are all the endoscopies with high hpf
PatientsWithEoE<-subset(EoEDx,EoEDx$HPF>=15)



#Export as excel speadsheet

writeWorksheetToFile("~\\AnothEoEForIona.xlsx",data=PatientsWithEoE,sheet="blabla",startRow=3,startCol=4)



Mydata<-EndoscChopperEndoscopist(Mydata,'Endoscopist')
Mydata$Endoscopist<-gsub("Second","",Mydata$Endoscopist)
Mydata<-EndoscChopperMeds(Mydata,'Medications')
Mydata<-EndoscChopperInstrument(Mydata,'Instrument')
Mydata<-EndoscChopperIndications(Mydata,'INDICATIONSFOREXAMINATION')
Mydata<-EndoscChopperProcPerformed(Mydata,'PROCEDUREPERFORMED')


Mydata<-HistolChopperHistol(Mydata,'HISTOLOGY')
Mydata<-HistolChopperDx(Mydata,"DIAGNOSIS")
Mydata<-HistolChopperExtrapolDx(Mydata,"DIAGNOSIS")



MyBarrettsData<-Mydata[grepl("Barr",Mydata$FINDINGS),]
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

BarrettsTherapeuticsRFA_ByCatheter(Therapeutics,"FINDINGS","ENDOSCOPICDIAGNOSIS")