library(lattice)
library(compare)
library(grid)
library(reshape2)
#library(plyr)
library(gtools)
library(lubridate)
#library(knitr)
library(xtable)
library(stringr)
library(anytime)
library(dplyr)


mysummary <- function(x) {
MyColonData<-x

#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
############################################# ////////////////////////////////////////////////////////////////#######################################  #############################################  
#############################################  DATA CLEANING ###############################  #############################################  #############################################  
#############################################////////////////////////////////////////////////////////////////#############################  #############################################  
#############################################  #############################################  #############################################  #############################################
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  

#Bit of a tidy up

MyColonData$Age=as.numeric(format(Sys.Date(),"%Y"))-MyColonData$birthyearnum


MyColonData$Endo_ResultEntered<-as.Date(anytime(MyColonData$Endo_ResultEntered))
MyColonData$Endo_ResultPerformed<-as.Date(anytime(MyColonData$Endo_ResultPerformed))

MyColonData$Endo_ResultText <- gsub("\\\\", "",MyColonData$Endo_ResultText) 
MyColonData$Endo_ResultText= sub("\\\\.*\\\\","",MyColonData$Endo_ResultText)


MyColonData$Endo_Endoscopist<-str_extract(MyColonData$Endo_ResultText, 'Endoscopist:(.*?)2nd Endoscopist')
MyColonData$Endo_Endoscopist <-gsub("Endoscopist:", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("?", "",MyColonData$Endo_Endoscopist,fixed=TRUE)


MyColonData$Endo_Endoscopist<-ifelse(is.na(MyColonData$Endo_Endoscopist),str_extract(MyColonData$Endo_ResultText,"Signature.*"),MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("  This procedure was electronically.* ", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("Signature:_________________________________", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("2nd Endoscopist.*", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("Dr\\.|Dr|Mr\\.|Mr|X[A-Z].*|X[0-9].*", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("_x000D_", " ",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("  ", " ",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist<-ifelse(is.na(MyColonData$Endo_Endoscopist),str_extract(MyColonData$Endo_ResultText,"Signature.*"),MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist<-trimws(MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("([a-z])([A-Z])", "\\1 \\2",MyColonData$Endo_Endoscopist,perl=TRUE)

#2nd Endoscopist:
MyColonData$SecondEndo_Endoscopist<-str_extract(MyColonData$Endo_ResultText, '2nd Endoscopist(.*?)Trainee')
MyColonData$SecondEndo_Endoscopist <-gsub("2nd Endoscopist:|2nd Endoscopist", "",MyColonData$Endo_Endoscopist)
MyColonData$SecondEndo_Endoscopist <-gsub("Trainee:|Trainee", "",MyColonData$Endo_Endoscopist)
#Nurses:
MyColonData$Nurses<-str_extract(MyColonData$Endo_ResultText, 'Nurses(.*)?Medication')
MyColonData$Nurses <-gsub("Nurses:|Nurses", "",MyColonData$Nurses)
MyColonData$Nurses <-gsub("Medication:|Medication", "",MyColonData$Nurses)
#Medications:
MyColonData$Medications<-str_extract(MyColonData$Endo_ResultText, 'Medication(.*)Instrument')
#How much fentanyl is given by endoscopist:
MyColonData$Fent<-str_extract(MyColonData$Medications,"Fentanyl \\d*")
MyColonData$Fent<-gsub("Fentanyl","",MyColonData$Fent)
MyColonData$Fent<-as.double(MyColonData$Fent)

#How much midazolam is given by endoscopist:
MyColonData$Midaz<-str_extract(MyColonData$Medications,"Midazolam \\d+")
MyColonData$Midaz<-gsub("Midazolam","",MyColonData$Midaz)
MyColonData$Midaz<-as.double(MyColonData$Midaz)
#Instrument:
MyColonData$Instrument<-str_extract(MyColonData$Endo_ResultText, 'Instrument(.*)Extent of Exam')
MyColonData$Instrument <- gsub("X.*|Instrument:|Instrument|Extent of Exam|LOAN|Loan Scope \\(|Loan Scope \\(specify serial no:|Loan Scope \\(specify\\s*serial no|\\)|-.*", "",MyColonData$Instrument) 
MyColonData$Instrument<- gsub(",.*|:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,","",MyColonData$Instrument)
MyColonData$Instrument<- gsub("(^.*)?\\s+.*","\\1",MyColonData$Instrument)
MyColonData$Instrument<- gsub("FC ","FC",MyColonData$Instrument)
MyColonData$Instrument<- gsub("^\\s*([1-9])","A\\1",MyColonData$Instrument)
MyColonData$Instrument<- gsub("^\\s+","",MyColonData$Instrument)
MyColonData$Instrument<-toupper(MyColonData$Instrument)
MyColonData$Instrument<-trimws(MyColonData$Instrument)

#Extent of Exam:
MyColonData$ExtentofExam<-str_extract(MyColonData$Endo_ResultText, 'Extent of Exam(.*)Complication')
MyColonData$ExtentofExam <- gsub("Extent of Exam:|Extent of Exam|Complication:|Complication", "",MyColonData$ExtentofExam) 

MyColonData$Endo_ResultText<-gsub("\n|\r","",MyColonData$Endo_ResultText)
#IndicationsForExamination:
MyColonData$IndicationsFroExamination<-str_extract(MyColonData$Endo_ResultText, 'INDICATIONS FOR EXAMINATION.*?PROCEDURE PERFORMED')
MyColonData$IndicationsFroExamination <- gsub("INDICATIONS FOR EXAMINATION|PROCEDURE PERFORMED", "",MyColonData$IndicationsFroExamination) 

#PROCEDURE PERFORMED
MyColonData$ProcPerformed<-str_extract(MyColonData$Endo_ResultText, 'PROCEDURE PERFORMED.*?Quality')
MyColonData$ProcPerformed <- gsub("PROCEDURE PERFORMED|Quality:|Quality|Withdrawal.*", "",MyColonData$ProcPerformed) 

#Withdrawal Time where available
MyColonData$WithdrawalTime<-str_extract(MyColonData$Endo_ResultText, 'Withdrawal Time.*?Quality')
MyColonData$WithdrawalTime<-gsub("Withdrawal Time:|Quality","",MyColonData$WithdrawalTime)
MyColonData$WithdrawalTime<-period_to_seconds(hms(MyColonData$WithdrawalTime))
MyColonData$WithdrawalTime<-as.numeric(MyColonData$WithdrawalTime)

#FINDINGS
MyColonData$Findings<-str_extract(MyColonData$Endo_ResultText, 'FINDINGS.*?ENDOSCOPIC DIAGNOSIS')
MyColonData$Findings<-gsub("FINDINGS|ENDOSCOPIC DIAGNOSIS","",MyColonData$Findings)
#ENDOSCOPIC DIAGNOSIS
MyColonData$EndoscopDiagn<-str_extract(MyColonData$Endo_ResultText, 'ENDOSCOPIC DIAGNOSIS.*?RECOMMENDATIONS')
MyColonData$EndoscopDiagn<-gsub("ENDOSCOPIC DIAGNOSIS|RECOMMENDATIONS","",MyColonData$EndoscopDiagn)
#RECOMMENDATIONS
MyColonData$Recommend<-str_extract(MyColonData$Endo_ResultText, 'RECOMMENDATIONS.*?COMMENTS')
MyColonData$Recommend<-gsub("RECOMMENDATIONS|COMMENTS","",MyColonData$Recommend)

#COMMENTS
MyColonData$Comments<-str_extract(MyColonData$Endo_ResultText, 'COMMENTS*?COMMENTS')
MyColonData$Comments<-gsub("|COMMENTS|COMMENTS","",MyColonData$Comments)
#FOLLOW UP
MyColonData$FollowUp<-str_extract(MyColonData$Endo_ResultText, 'FOLLOW UP.*?FURTHER ENDOSCOPIC PROCEDURES')
#FURTHER ENDOSCOPIC PROCEDURES (IF ANY)
MyColonData$FurtherEndo<-str_extract(MyColonData$Endo_ResultText, 'FURTHER ENDOSCOPIC PROCEDURES.*')

#From histology

#MyColonData$Histo_ResultPerformed<-as.Date(anytime(MyColonData$Histo_ResultPerformed))
#MyColonData$Histo_ResultEntered<-as.Date(anytime(MyColonData$Histo_ResultEntered))
MyColonData$Histo_ResultText<-gsub("\n|\r","",MyColonData$Histo_ResultText)
MyColonData$NatureOfSpec<-str_extract(MyColonData$Histo_ResultText, 'NATURE OF SPECIMEN(.*?)CLIN')

#CLINICAL DETAILS
MyColonData$ClinDet<-str_extract(MyColonData$Histo_ResultText, 'CLINICAL DETAILS.*?MAC')

#MACROSCOPICAL DESCRIPTION
MyColonData$MacDescrip<-str_extract(MyColonData$Histo_ResultText, 'MACROSCOPICAL DESCRIPTION.*?HISTOLOGY')
MyColonData$MacDescrip<-gsub(". ","\n",MyColonData$MacDescrip,fixed=TRUE)
MyColonData$MacDescrip<-gsub("^\\s+","",MyColonData$MacDescrip)
MyColonData$MacDescrip<-gsub("Dictated by.*","",MyColonData$MacDescrip)

#HISTOLOGY
MyColonData$Histol<-str_extract(MyColonData$Histo_ResultText, '[HISTOLOGY.*?DIAGNOSIS')
MyColonData$Histol<-gsub(". ","\n",MyColonData$Histol,fixed=TRUE)
MyColonData$Histol<-gsub("^\\s+","",MyColonData$Histol)

#DIAGNOSIS
MyColonData$Dx<-str_extract_all(MyColonData$Histo_ResultText, '[Dd][Ii][Aa][Gg][Nn][Oo][Ss][Ii][Ss].*')
MyColonData$Dx<-gsub("       ","",MyColonData$Dx)
MyColonData$Dx<-gsub(":","",MyColonData$Dx)
MyColonData$Dx<-gsub("[Nn]o .*?\n","",MyColonData$Dx)
MyColonData$Dx<-gsub("[Nn]ormal.*\n","",MyColonData$Dx)
return(MyColonData)
}

EndoscChopper<-function(x){
  
  
  
  
  
  x$Endo_ResultText<-gsub("    ","\n",x$Endo_ResultText)
  
   #x$Endo_ResultEntered<-as.Date(anytime(x$Endo_ResultEntered))
   #x$Endo_ResultPerformed<-as.Date(anytime(x$Endo_ResultPerformed))
 
  if(class(x$Endo_ResultEntered)!="Date"){
  x$Endo_ResultEntered<-as.Date(x$Endo_ResultEntered<-as.Date(dmy_hms(x$Endo_ResultEntered)))
  x$Endo_ResultPerformed<-as.Date(x$Endo_ResultPerformed<-as.Date(dmy_hms(x$Endo_ResultPerformed)))
  }
  #x$Histo_ResultPerformed<-as.Date(x$Histo_ResultPerformed<-as.Date(dmy_hms(x$Histo_ResultPerformed)))
  #x$Histo_ResultEntered<-as.Date(x$Histo_ResultEntered<-as.Date(dmy_hms(x$Histo_ResultEntered)))
  
  
#   x$Endo_ResultEntered<-as.Date(as.character(Self$Endo_ResultEntered),origin="1899-12-30")
#   x$Endo_ResultPerformed<-as.Date(x$Endo_ResultPerformed,origin="1899-12-30")
#   x$Histo_ResultPerformed<-as.Date(x$Histo_ResultPerformed,origin="1899-12-30")
#   x$Histo_ResultEntered<-as.Date(x$Histo_ResultEntered,origin="1899-12-30")
#   x$Endo_ResultEntered<-as.Date(x$Endo_ResultEntered,"%Y-%m-%d")
#   
  
 #  x$Endo_ResultEntered<-if(!is.null(grep("/",x$Endo_ResultEntered))){
#     strptime(x$Endo_ResultEntered,format="%d/%m/%Y")
 #  }
#   
#   
#   
 #  x$Endo_ResultEntered<-if(!is.null(grep("-",x$Endo_ResultEntered))){
#     strptime(x$Endo_ResultEntered,format="%Y-%m-%d")
 #  }
#   
#   
#   x$Endo_ResultEntered<-ifelse(grep("/",x$Endo_ResultEntered),strptime(x$Endo_ResultEntered,format="%d/%m/%Y"),
#                               strptime(x$Endo_ResultEntered,format="%Y-%m-%d"))
                                      
  #x$Endo_ResultEntered<-strptime(x$Endo_ResultEntered,format="%d/%m/%Y") 
  #x$Endo_ResultPerformed<-strptime(x$Endo_ResultPerformed,format="%d/%m/%Y") 
  #x$Endo_ResultEntered<-as.Date(x$Endo_ResultEntered,origin="1899-12-30")
  #x$Endo_ResultPerformed<-as.Date(x$Endo_ResultPerformed,origin="1899-12-30")
  
  
  x$Endo_Endoscopist<-str_extract(x$Endo_ResultText, '(?<=Endoscopist:)(.*)')
  x$Endo_Endoscopist<-gsub("0\\\\XF9\\\\\\\\XBB\\\\\\\\X06\\\\\\\\XEC\\\\\\\\XF8\\\\\\\\XBB\\\\\\\\X06\\\\\\\\X10\\\\","",x$Endo_Endoscopist)
  x$Endo_Endoscopist<-gsub("\\\\.*|_x000D_","",x$Endo_Endoscopist)
  x$Endo_Endoscopist<-trimws(x$Endo_Endoscopist,which=c("both"))
  x$Endo_Endoscopist<-gsub(":","",x$Endo_Endoscopist)
  x$Endo_Endoscopist <-gsub("?", "",x$Endo_Endoscopist,fixed=TRUE)
  #2nd Endoscopist:
  x$SecondEndo_Endoscopist<-str_extract(x$Endo_ResultText, '(?<=2nd Endoscopist:)(.*)')
  #Nurses:
  x$Nurses<-str_extract(x$Endo_ResultText, '(?<=Nurses:)(.*)')
  x$Nurses<-gsub("\\\\.*","",x$Nurses)
  #Medications:
  x$Medications<-str_extract(x$Endo_ResultText, regex('(?<=Medications:)(.*)Instrument',dotall = TRUE))
  x$Medications<-gsub("Instrument","",x$Medications)
  x$Medications<-gsub("\\\\.*\\\\","",x$Medications)
  x$Medications <- lapply(x$Medications, str_trim)

#Extract the fentanyl
x$Fent <- str_extract(x$Medications, 'Fentanyl (\\d+)mcg')
x$Fent<-gsub("Fentanyl ","",x$Fent)
x$Fent<-gsub("mcg","",x$Fent)
x$Fent<-as.character(as.numeric(x$Fent))

#Extract the midazolam
x$Midaz <- str_extract(x$Medications, 'Midazolam (\\d+)mg')
x$Midaz<-gsub("Midazolam ","",x$Midaz)
x$Midaz<-gsub("mg","",x$Midaz)
x$Midaz<-as.character(as.numeric(x$Midaz))

  #Instrument:
  x$Instrument<-str_extract(x$Endo_ResultText, '(?<=Instrument:)(.*)')
  x$Instrument<-gsub("-.*","",x$Instrument)
  x$Instrument <- lapply(x$Instrument, str_trim)
x$Instrument <- gsub("X.*|Instrument:|Instrument|Extent of Exam|LOAN|Loan Scope \\(|Loan Scope \\(specify serial no:|Loan Scope \\(specify\\s*serial no|\\)|-.*", "",x$Instrument) 
x$Instrument<- gsub(",.*|:|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,","",x$Instrument)
x$Instrument<- gsub("(^.*)?\\s+.*","\\1",x$Instrument)
x$Instrument<- gsub("FC ","FC",x$Instrument)
x$Instrument<- gsub("^\\s*([1-9])","A\\1",x$Instrument)
x$Instrument<- gsub("^\\s+","",x$Instrument)
  x$Instrument<-gsub("\\\\.*","",x$Instrument)
  x$Instrument<-gsub("Loan Scope \\(specify serial no\\)\\s*","",x$Instrument)
  x$Instrument<-gsub("Loan Scope \\(specify serial no:\\)\\s*","",x$Instrument)

x$Instrument<-toupper(x$Instrument)
x$Instrument<-trimws(x$Instrument)



  #Extent of Exam:
  x$ExtentofExam<-str_extract(x$Endo_ResultText, '(?<=Extent of Exam:)(.*)')
  x$Endo_ResultText<-gsub("(\n|\r){2,8}","\\.",x$Endo_ResultText)
  x$Endo_ResultText<-gsub("(\n|\r)","\\.",x$Endo_ResultText)
  #IndicationsForExamination:
  x$IndicationsFroExamination<-str_extract(x$Endo_ResultText, '(?<=INDICATIONS FOR EXAMINATION).*?PROCEDURE PERFORMED')
  x$IndicationsFroExamination<-gsub("\r\n","\n",x$IndicationsFroExamination)
  x$IndicationsFroExamination<-ColumnCleanUp(x,"IndicationsFroExamination")
  x$IndicationsFroExamination<-gsub("\\.\n\\.\n|\\.\r\\.\r","\\.",x$IndicationsFroExamination)
  x$IndicationsFroExamination<-gsub("^\\.","",x$IndicationsFroExamination)
  
  #PROCEDURE PERFORMED
  x$ProcPerformed<-str_extract(x$Endo_ResultText, 'PROCEDURE PERFORMED(.*)(FINDINGS)')
  x$ProcPerformed<-gsub("PROCEDURE PERFORMED","",x$ProcPerformed)
  x$ProcPerformed<-gsub("Withdrawal.*","",x$ProcPerformed)
  x$ProcPerformed<-gsub("Quality.*","",x$ProcPerformed)
  x$ProcPerformed<-gsub("Adequate.*|Good.*|Poor.*|None.*","",x$ProcPerformed)
  x$ProcPerformed<-gsub("FINDINGS","",x$ProcPerformed)
  x$ProcPerformed<-gsub("-\\s*$|-$|-\\s+$","",x$ProcPerformed)
  x$ProcPerformed<-gsub("([A-Z])-","\\1 -",x$ProcPerformed)
  x$ProcPerformed<-gsub("\\.","",x$ProcPerformed)
  x$ProcPerformed<-gsub("-([A-Z])","-\\1",x$ProcPerformed)
  x$ProcPerformed<-gsub(")-",") -",x$ProcPerformed)
  x$ProcPerformed<-gsub("ENDOSCOPIC APPEARANCE.*","",x$ProcPerformed)
  x$ProcPerformed <- lapply(x$ProcPerformed, str_trim)
  
  
  #FINDINGS
  x$Endo_ResultTextForFindings<-gsub("\n|\r", " ", x$Endo_ResultText)
  x$Findings<-str_extract(x$Endo_ResultTextForFindings, '(?<=FINDINGS).*?ENDOSCOPIC DIAGNOSIS')
  x$Findings<-gsub("       ","",x$Findings)
  
  x$Endo_ResultTextForFindings<-NULL  
  x$Findings<-ColumnCleanUp(x,"Findings") 
  #x$Findings<-NegativeRemove(x,"Findings")
  x$Findings<-gsub("cm\\s+[A-Z]|cm.+\\)","cm\n",x$Findings)
  
  #ENDOSCOPIC DIAGNOSIS
  x$EndoscopDiagn<-str_extract(x$Endo_ResultText, '(?<=ENDOSCOPIC DIAGNOSIS).*?RECOMMENDATIONS')
  x$EndoscopDiagn<-ColumnCleanUp(x,"EndoscopDiagn")
  x$EndoscopDiagn<-NegativeRemove(x,"EndoscopDiagn") 
  
  #RECOMMENDATIONS
  x$Recommend<-str_extract(x$Endo_ResultText, '(?<=RECOMMENDATIONS).*?FOLLOW UP')
  x$Recommend<-gsub("       ","",x$Recommend)
  x$Recommend<-ColumnCleanUp(x,"Recommend") 
  
  
  #FOLLOW UP
  x$FollowUp<-str_extract(x$Endo_ResultText, '(?<=FOLLOW UP).*?FURTHER ENDOSCOPIC PROCEDURES')
  x$FollowUp<-ColumnCleanUp(x,"FollowUp") 
  
  #FURTHER ENDOSCOPIC PROCEDURES (IF ANY)
  x$FurtherEndo<-str_extract(x$Endo_ResultText, '(?<=FURTHER ENDOSCOPIC PROCEDURES).*')
  
  
  #Withdrawal Time where available
   x$WithdrawalTime<-str_extract(x$Endo_ResultText, 'Withdrawal Time.*?Quality')
#   x$WithdrawalTime<-ifelse(!is.na(x$WithdrawalTime),gsub("Withdrawal Time:|Quality","",x$WithdrawalTime),"NA")
#   #x$WithdrawalTime<-gsub("Withdrawal Time:|Quality","",x$WithdrawalTime)
#   x$WithdrawalTime<-period_to_seconds(hms(x$WithdrawalTime))
#   x$WithdrawalTime<-as.numeric(x$WithdrawalTime)
return(x)
}

NegativeRemove<-function(x,y){
  x[,y]<-gsub('No .*?(\\.|:)',"",x[,y])
  x[,y]<-gsub('.*[Nn]ormal.*?\\.|:',"",x[,y])
  x[,y]<-gsub('.*[Nn]either .*?(\\.|:)',"",x[,y])
  x[,y]<-gsub('.*[Tt]here is no .*?(\\.|:)',"",x[,y])
  x[,y]<-gsub('.*[Tt]here are no .*?(\\.|:)',"",x[,y])
  x[,y]<-gsub(".*[Nn]egative.*?(\\.|:)","",x[,y])
  x[,y]<-gsub(".*[Ww]ithin normal .*?(\\.|:)","",x[,y])
  x[,y]<-gsub(".*with no.*?(\\.|:)","",x[,y])
  x[,y]<-gsub(".*[Nn]o significant.*?(\\.|:)","",x[,y])
}
ColumnCleanUp<-function(x,y){
  x[,y]<-gsub(".",".\n",x[,y],fixed=TRUE)
  x[,y]<-gsub("^\\s+","",x[,y])
}





HistolChopper<-function(x){
  x$Histo_ResultText<-gsub("(\n|\r){2,8}","\\.",x$Histo_ResultText)
  x$Histo_ResultText<-gsub("(\n|\r)",":",x$Histo_ResultText)
 
  #From histology
  
  #x$Histo_ResultEntered<-as.Date(anytime(x$Endo_ResultEntered))
  #x$Histo_ResultPerformed<-as.Date(anytime(x$Endo_ResultPerformed))
 
  x$NatureOfSpec<-str_extract(x$Histo_ResultText, '(?<=NATURE OF SPECIMEN).*?CL')
#   x$Histo_ResultPerformed<-strptime(x$Histo_ResultPerformed,format="%d/%m/%Y") 
#   x$Histo_ResultPerformed<-as.Date(x$Histo_ResultPerformed,origin="1899-12-30")
#   x$Histo_ResultEntered<-strptime(x$Histo_ResultEntered,format="%d/%m/%Y") 
#   x$Histo_ResultEntered<-as.Date(x$Histo_ResultEntered,origin="1899-12-30")
  
  
  #CLINICAL DETAILS
  x$ClinDet<-str_extract(x$Histo_ResultText, "(Clinical|CLINICAL).*?(Macroscopic|MACROSCOPIC)")
  #Column specific cleanup
  x$ClinDet<-ColumnCleanUp(x,"ClinDet")
  x$ClinDet<-gsub("^\\.\n","",x$ClinDet)
  #print(x$ClinDet)
  
  #MACROSCOPICAL DESCRIPTION
  x$MacDescrip<-str_extract(x$Histo_ResultText, '(Macroscopic|MACROSCOPIC).*?(Microscopic|MICROSCOPIC|HISTOLOGY)')
  x$MacDescrip<-gsub("\\.","\\.\n",x$MacDescrip,fixed=TRUE)
  x$MacDescrip<-gsub("^\\s+","",x$MacDescrip)
  #Column specific cleanup
  x$MacDescrip<-gsub("Dictated by.*","",x$MacDescrip)
  #x$MacDescrip<-ColumnCleanUp(x,"MacDescrip")

  #HISTOLOGY
  x$Histo_ResultTextForFindings<-gsub("\n|\r", " ", x$Histo_ResultText)
  x$Histol<-str_extract(x$Histo_ResultTextForFindings, '(?<=[Mm][Ii][Cc][Rr][oo][Ss][Cc][Oo][Pp][Ii][Cc]|HISTOLOGY).*?[Dd][Ii][Aa][Gg][Nn][Oo][Ss][Ii][Ss]')
  x$Histol<-gsub("       ","",x$Histol)
  x$Histol<-gsub("Harriet Deere","",x$Histol)
  x$Histol<-gsub("Michael Green","",x$Histol)
  #x$Histo_ResultTextForFindings<-NULL 
  
  x$Histol<-ColumnCleanUp(x,"Histol")
  x$Histol<-NegativeRemove(x,"Histol")
  x$Histol_Simplified<-x$Histol

x$Histol_Simplified<-gsub("       ","",x$Histol_Simplified)
x$Histol_Simplified<-gsub("- ","\n",x$Histol_Simplified,fixed=TRUE)
x$Histol_Simplified<-gsub("-[A-Z]","\n",x$Histol_Simplified,fixed=TRUE)
x$Histol_Simplified<-gsub("[Nn]egative.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Ww]ithin normal .*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub(".*biopsies.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub(".*biopsy.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub(":","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Nn]o .*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub(".*[Nn]ormal.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("^[Nn]ormal.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Ww]ithin normal histol.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Nn]egative for.*\n","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Nn]egative for [Dd]ysplasia","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Nn]o [Dd]ysplasia.*?\\.","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("[Nn]egative for.*?\\.","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("Neither dysplasia.*?\\.","",x$Histol_Simplified,perl=T)
x$Histol_Simplified<-gsub("Neither dysplasia nor malignancy is seen","",x$Histol_Simplified,perl=T)

  #Accession number samples
  x$AccessionNumber<-str_extract(x$Histo_ResultText,"SP-\\d{2}-\\d{7}")
  #Without the negative extractor which needs some improvement. Only Capital D included (not lower case d) to make sure picks up subtitle header as opposed to mentioning 'diagnosis' as part of a sentence.
  x$DxRaw<-str_extract_all(x$Histo_ResultText, 'D[Ii][Aa][Gg][Nn][Oo][Ss][Ii][Ss].*')
  #DIAGNOSIS
  x$Dx<-str_extract_all(x$Histo_ResultText, '(?<=[Dd][Ii][Aa][Gg][Nn][Oo][Ss][Ii][Ss]).*')
  #Column specific cleanup
  x$Dx<-gsub("Dr.*","",x$Dx,perl=T)
  x$Dx<-gsub("[Rr]eported.*","",x$Dx)
  #Column-generic cleanup
  x$Dx<-ColumnCleanUp(x,"Dx")
  x$Dx<-NegativeRemove(x,"Dx")

  x$Dx_Simplified<-x$Dx
  x$Dx_Simplified<-gsub("       ","",x$Dx_Simplified)
  x$Dx_Simplified<-gsub("- ","\n",x$Dx_Simplified,fixed=TRUE)
  x$Dx_Simplified<-gsub("-[A-Z]","\n",x$Dx_Simplified,fixed=TRUE)
  x$Dx_Simplified<-gsub(".*biopsies.*\n","",x$Dx_Simplified,perl=T)
  x$Dx_Simplified<-gsub(".*biopsy.*\n","",x$Dx_Simplified,perl=T)
  x$Dx_Simplified<-gsub(":","",x$Dx_Simplified,perl=T)
  x$Cancer<-str_extract(x$Dx_Simplified, '[Cc]arcin|[Cc]ance|[Ll]ymphoma|[Tt]umour')
  x$Dysplasia<-str_extract(x$Dx_Simplified, '[Dd]yspla')
  x$GIST<-str_extract(x$Dx_Simplified, 'G[Ii][Ss][Tt]|[Ss]tromal|[Ll]eio')
  
  
  x$MacDescrip<-gsub("One","1",x$MacDescrip)
  x$MacDescrip<-gsub("[Ss]ingle","1",x$MacDescrip)
  x$MacDescrip<-gsub("Two","2",x$MacDescrip)
  x$MacDescrip<-gsub("Three","3",x$MacDescrip)
  x$MacDescrip<-gsub("Four","4",x$MacDescrip)
  x$MacDescrip<-gsub("Five","5",x$MacDescrip)
  x$MacDescrip<-gsub("Six","6",x$MacDescrip)
  x$MacDescrip<-gsub("Seven","7",x$MacDescrip)
  x$MacDescrip<-gsub("Eight","8",x$MacDescrip)
  x$NumbOfBx<-str_extract_all(x$MacDescrip, "([A-Za-z]*|[0-9]) piece.*?(([0-9]).*?x.*?([0-9]).*?x.*?([0-9])).*?([a-z]\\.)")
  x$NumbOfBx<-sapply(x$NumbOfBx, function(x) sum(as.numeric(unlist(str_extract_all(x, "^\\d+")))))
  
  
  
  
  x$NumbOfBxs<-unlist(x$NumbOfBx)
  x$NumbOfBx<-as.numeric(str_extract(x$NumbOfBx,"^.*?\\d"))
  #How many biopsies have I taken this month?
  sum(x$NumbOfBx,na.rm = T)
  #What's the average biopsy size this month?
  x$BxSize<-str_extract(x$MacDescrip, "the largest.*?mm")
  x$BxSize<-gsub("the largest measuring ","",x$BxSize)
  x$BxSize<-gsub("mm","",x$BxSize)
  x$BxSize<-gsub("less than","",x$BxSize)
  x$BxSize<-as.numeric(str_match(x$BxSize, "([0-9]+).*?([0-9])+.*?([0-9])")[, 2])*as.numeric(str_match(x$BxSize, "([0-9]+).*?([0-9])+.*?([0-9])")[, 3])*as.numeric(str_match(x$BxSize, "([0-9]+).*?([0-9])+.*?([0-9])")[, 4])
  
  x$Endo_ResultText<-NULL
  x$Histo_ResultText<-NULL
  return(x)
}

Symptoms<-function(x,column){
  x$Dysphagia<- ifelse(grepl("[Dd]ysphagia",x[[column]],perl=TRUE)|
                         grepl("[Ss]wallow",x[[column]],perl=TRUE)|
                         grepl("[Ss]tick",x[[column]],perl=TRUE)|
                         grepl("[Oo]dynoph",x[[column]],perl=TRUE)|
                                 grepl("[Bb]olus",x[[column]],perl=TRUE)|
                                         grepl("[Oo]ro?-pharyn",x[[column]],perl=TRUE),"Yes","NO")
  
  x$Reflux<-ifelse(grepl("[Rr]eflux",x[[column]],perl=TRUE),"Yes","NO")
  x$Hoarse<-ifelse(grepl("[Hh]oarse",x[[column]],perl=TRUE),"Yes","NO")
  x$Cough<-ifelse(grepl("[Cc]ough",x[[column]],perl=TRUE),"Yes","NO")
  x$StomachPain<-ifelse(grepl("[Ss]tomach",x[[column]],perl=TRUE)|
                          grepl("[Aa]bdom",x[[column]],perl=TRUE)|
                          grepl("[Ee]pigas",x[[column]],perl=TRUE),"Yes","NO")
  x$Nausea<-ifelse(grepl("[Nn]ausea",x[[column]],perl=TRUE),"Yes","NO")
  x$Vomiting<-ifelse(grepl("[Vv]omit",x[[column]],perl=TRUE),"Yes","NO")
  x$Heartburn<-ifelse(grepl("[Hh]eartbu",x[[column]],perl=TRUE),"Yes","NO")
  x$Regurgitation<-ifelse(grepl("[Rr]egurg",x[[column]],perl=TRUE),"Yes","NO")
  x$Globus<-ifelse(grepl("[Gg]lobus",x[[column]],perl=TRUE),"Yes","NO")
  x$Throat<-ifelse(grepl("[Tt]hroat",x[[column]],perl=TRUE),"Yes","NO")
  x$Belch<-ifelse(grepl("[Bb]elch",x[[column]],perl=TRUE),"Yes","NO")
  x$Chest<-ifelse(grepl("[Cc]hest",x[[column]],perl=TRUE),"Yes","NO")
  return(x)
}

SymptomsInColNames<-function(x) {
  x$Dysphagia<- ifelse(grepl("[Nn]ausea",names(x),perl=TRUE),"It is","No its not")
  
                       
                       #If the name of any column contains 'Nausea' and the row does not contain NA then say Yes in a column
                       
                       
                       
  
}





# Multiple plot function # # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects) # - cols: Number of columns in layout # - layout: A matrix specifying the layout. If present, 'cols' is ignored. # # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), # then plot 1 will go in the upper left, 2 will go in the upper right, and # 3 will go all the way across the bottom. # 
multiplot <- function(..., plotlist=NULL, file, cols=4, layout=NULL) { library(grid)
              # Make a list from the ... arguments and plotlist 
              plots <- c(list(...), plotlist) 
              numPlots = length(plots) 
              # If layout is NULL, then use 'cols' to determine layout 
              if (is.null(layout)) { 
                # Make the panel 
                # ncol: Number of columns of plots 
                # nrow: Number of rows needed, calculated from # of cols 
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
                                 ncol = cols, nrow = ceiling(numPlots/cols)) 
              } 
              
              if (numPlots==1) { 
                print(plots[[1]]) 
              } else { 
                # Set up the page 
                grid.newpage() 
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) 
                # Make each plot, in the correct location 
                for (i in 1:numPlots) { 
                  # Get the i,j matrix positions of the regions that contain this subplot 
                  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE)) 
                  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                                  layout.pos.col = matchidx$col)) 
                } 
              } 
}

#Date conversion function

Dateconv<-function(x){
  #if Fileconversion date
  #if date is in the column name(as it should be)
  
  #and if"d{4}-d{2}-d{2}T.*?z" as the regular expression , then convert factor to character then gsub "(d{4}-d{2}-d{2})T.*?z" for $1
  #then convert to Date as.Date(format="%Y-%m-%d")
  
 # and if / then convert to Date as.Date(format="%Y/%m/%d")
  
  #and if / then convert to Date as.Date(format="%Y_%m_%d")
}



StThomasEndoscopy10YearFunction<-function(){
final.df<-read_excel("S:\\Gastroenterology\\Seb\\R\\Data\\AdHoc//GSTT_GastroscopyProcedure2007_2017.xlsx",col_names=F)
names(final.df)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")



final.df$Endo_ResultText<-gsub("2nd Endoscopist:","Second endoscopist:",final.df$Endo_ResultText)
EndoscTree<-list("Name:","Date of Birth:","General Practicioner:","Hospital Number:",
                 "Date of procedure:","Endoscopist:","Second endoscopist:","Nurses:","Medications",
                 "Instrument","Extent of Exam:","INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED","FINDINGS","ENDOSCOPIC DIAGNOSIS",
                 "RECOMMENDATIONS","FOLLOW UP","")
for(i in 1:(length(EndoscTree)-1)) {
  final.df<-Extractor(final.df,"Endo_ResultText",as.character(EndoscTree[i]),
                      as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}
res<-final.df



HistolTree<-list("NATURE OF SPECIMEN:","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY",
                 "DIAGNOSIS","")
for(i in 1:(length(HistolTree)-1)) {
  final.df<-Extractor(final.df,"Histo_ResultText",as.character(HistolTree[i]),
                      as.character(HistolTree[i+1]),as.character(HistolTree[i]))
}
res<-final.df
return(res)
}
