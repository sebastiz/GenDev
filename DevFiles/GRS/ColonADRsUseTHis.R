library(ggplot2)
library(lattice)
library(compare)
library(grid)
library(reshape2)
library(gtools)
library(dplyr)
library(lubridate)
library(openxlsx)
library(knitr)
library(xtable)
library(stringr)


#If you have to cobble the data togather from monthly forms then use TCR2232 spreadsheet reports.
library(EndoMineR)
MyColonData<-read.xlsx("/home/rstudio/GenDev/DevFiles/GRS/July19GSTT_TCR2231_ColonoscopyProcedure_5_8_19.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
#names(MyColonData)<-gsub("\\.","",names(MyColonData))
MyColonData<-EndoPaste(MyColonData)
MyColonData<-data.frame(MyColonData[[1]],stringsAsFactors = FALSE)
MyColonData$Endo_ResultText<-gsub("2nd","Second",MyColonData$Endo_ResultText)

mywords<-c("Patient.ID","NHS.no","Patient.Name","Birthday.Num","Birthmonth.Num","Birthyear.Num","Endo_Result.Name","Endo_Result.Performed",
           "Endo_Result.Entered","Endo_Result.Text","Date.of.Birth","General.Practicioner","Referring.Physician","Hospital Number",
           "Date of Procedure","Endoscopist","2nd Endoscopist","Trainee","Referring Physician","Nurses","Medications","Instrument",
           "Extent of Exam","Complications","Comorbidity","INDICATIONS.FOR.EXAMINATION","PROCEDURE.PERFORMED","Withdrawal Time",
           "Quality of Bowel Preparation","FINDINGS","ENDOSCOPIC DIAGNOSIS","2ww","RECOMMENDATIONS","COMMENTS","FOLLO UP","OPCS4 Code",
           "Signature","Histo_Result.Name","Histo_Result.Performed","Histo_Result.Entered","Histo_Result.Text","Histopathology Report","Report Collected on",
           "Accession","Received on","Clinical Information","Macroscopic Description","Submitted by","Microscopic Description","Diagnosis",
           "Reported by","Verified")


MyColonData<-textPrep(MyColonData$X1_X2_X3,mywords)
MyColonData$dateofprocedure<-as.Date(MyColonData$dateofprocedure,format="%d/%m/%Y", origin = "1960-10-01")
MyColonData$histo_resultperformed<-as.Date(MyColonData$histo_resultperformed,format="%d/%m/%Y", origin = "1960-10-01")

#Select only the colonoscopies:
MyColonData$procedureperformed<-as.character(MyColonData$procedureperformed)
MyColonData<-MyColonData[grepl("colonoscop",MyColonData$procedureperformed),]


######################################################### by endoscopist for adenomas ####################  #############################################  #############################################  
#Get the adenomas vs the number of colons done by endoscopist:
MyColonDataAdenomaDetectionByEndoscopist<-MyColonData[grepl("denoma|serrate|denoca",paste(MyColonData$microscopicdescription,MyColonData$histo_resultname,MyColonData$diagnosis,MyColonData$macroscopicdescription,MyColonData$histo_resulttext)),] 
MyColonDataAdenomaDetectionByEndoscopist<-MyColonDataAdenomaDetectionByEndoscopist%>% 
  group_by(endoscopist) %>% 
  do(data.frame(NumAdenomas=nrow(.)))

MyColonDataColonoscopiesByEndoscopist<-MyColonData %>% 
  group_by(endoscopist) %>% 
  do(data.frame(NumColons=nrow(.)))

#Merge the two above by column to get proportion:
MyColonDataADR<-full_join(MyColonDataAdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("endoscopist"))
MyColonDataADR$PropAdenomas<-(MyColonDataADR$NumAdenomas/ MyColonDataADR$NumColons)*100


######################################################### by endoscopist for adenocarcinomas (without adenomas) ####################  #############################################  #############################################  
MyColonDataAdenoCarcinomaDetectionByEndoscopist<-MyColonData[grepl(".*denoca.*",MyColonData$Histo_ResultTextForFindings)&!grepl(".*denom.*",MyColonData$Histo_ResultTextForFindings),] 
MyColonDataAdenoCarcinomaDetectionByEndoscopist<-MyColonDataAdenoCarcinomaDetectionByEndoscopist%>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumAdenocarcinomas=nrow(.)))

MyColonDataAdenocarcinomas<-full_join(MyColonDataAdenoCarcinomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
MyColonDataAdenocarcinomas$PropAdenocarcinomas<-(MyColonDataAdenocarcinomas$NumAdenocarcinomas/ MyColonDataAdenocarcinomas$NumColons)*100

####################################################################################################################################################################################
######################################################### by endoscopist for dysplastic grade of adenomas
MyColonData_HG_AdenomaDetectionByEndoscopist<-MyColonData[grepl(".*denoma.*",MyColonData$Histo_ResultTextForFindings)&grepl(".*[Hh]igh [Gg]rade.*",MyColonData$Histo_ResultTextForFindings),] 
MyColonData_HG_AdenomaDetectionByEndoscopist<-MyColonData_HG_AdenomaDetectionByEndoscopist%>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumHighGradeAdenomas=nrow(.)))

MyColonData_LG_AdenomaDetectionByEndoscopist<-MyColonData[grepl(".*denoma.*",MyColonData$Histo_ResultTextForFindings)&grepl(".*[Ll]ow [Gg]rade.*",MyColonData$Histo_ResultTextForFindings),] 
MyColonData_LG_AdenomaDetectionByEndoscopist<-MyColonData_LG_AdenomaDetectionByEndoscopist%>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumLowGradeAdenomas=nrow(.)))

MyColonDataHGD_Adenomas<-full_join(MyColonData_HG_AdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
MyColonDataHGD_Adenomas$PropHGAdenomas<-(MyColonDataHGD_Adenomas$NumHighGradeAdenomas/ MyColonDataHGD_Adenomas$NumColons)*100

MyColonDataLGD_Adenomas<-full_join(MyColonData_LG_AdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
MyColonDataLGD_Adenomas$PropLGAdenomas<-(MyColonDataLGD_Adenomas$NumLowGradeAdenomas/ MyColonDataLGD_Adenomas$NumColons)*100

MyColonData_Serr_AdenomaDetectionByEndoscopist<-MyColonData[grepl(".*[Ss]errated.*",MyColonData$Histo_ResultTextForFindings),] 
MyColonData_Serr_AdenomaDetectionByEndoscopist<-MyColonData_Serr_AdenomaDetectionByEndoscopist%>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumSerrAdenomas=nrow(.)))

MyColonDataSerr_Adenomas<-full_join(MyColonData_Serr_AdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
MyColonDataSerr_Adenomas$PropSerrAdenomas<-(MyColonDataSerr_Adenomas$NumSerrAdenomas/ MyColonDataSerr_Adenomas$NumColons)*100

#############################################  ######## hyperplastic detection rate by endoscopist (from whole dataset)  ####################  #############################################  #############################################  

MyColonDataHyperplasticDetectionByEndoscopist<-MyColonData[grepl(".*yperplastic.*",MyColonData$Histo_ResultTextForFindings),] %>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumHyperplastics=nrow(.)))

MyColonDataColonoscopiesByEndoscopist<-MyColonData %>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumColons=nrow(.)))

#Merge the two above by column to get proportion:
MyColonDataHDR<-full_join(MyColonDataHyperplasticDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
MyColonDataHDR$PropHyperplastic<-(MyColonDataHDR$NumHyperplastics/ MyColonDataHDR$NumColons)*100

#### The final table #####
FinalTable<-full_join(MyColonDataADR,MyColonDataHDR,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataAdenocarcinomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataLGD_Adenomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataHGD_Adenomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataSerr_Adenomas,by=c("Endo_Endoscopist"))
FinalTable$HyperplasticToAdenomaRatio<-FinalTable$PropAdenomas/FinalTable$PropHyperplastic

library(writexl)
write_xlsx(FinalTable,"ADRTable.xlsx")

TBB<- ggplot(FinalTable)+
  geom_point(aes(NumColons.y, PropHyperplastic, color="red"))+
  xlab("Number of colons")+
  ylab("Proportion hyperplastic")+
  theme(axis.text.x=element_text(angle=-90))+
  theme(legend.position="top")


