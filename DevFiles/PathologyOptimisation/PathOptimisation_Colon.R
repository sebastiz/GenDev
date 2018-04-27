library(ggplot2)
library(lattice)
library(compare)
library(grid)
library(reshape2)
library(plyr)
library(gtools)
library(dplyr)
library(lubridate)
library(openxlsx)
library(knitr)
library(xtable)
library(stringr)
library(gplots)




#Past 14 month data
MyColonData<-read.xlsx("S:\\Gastroenterology\\Seb\\R\\Data\\GRS\\Colonscopy_HistoReport.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
MyColonData$Endo_ResultEntered<-as.Date(MyColonData$Endo_ResultEntered,origin="1899-12-30")
MyColonData$Endo_ResultPerformed<-as.Date(MyColonData$Endo_ResultPerformed,origin="1899-12-30")
MyColonData$Histo_ResultPerformed<-as.Date(MyColonData$Histo_ResultPerformed,origin="1899-12-30")
MyColonData$Histo_ResultEntered<-as.Date(MyColonData$Histo_ResultEntered,origin="1899-12-30")
#Restrict to the last 6 months:
MyColonData<-MyColonData[order(MyColonData$Endo_ResultPerformed),]
MyColonData$Endo_ResultEntered<-as.Date(MyColonData$Endo_ResultEntered,"%Y-%m-%d")
MyColonData<-subset(MyColonData,MyColonData$Endo_ResultPerformed>"2015-12-31")
MyColonData<-subset(MyColonData,MyColonData$Endo_ResultPerformed<"2016-07-01")
MyColonData$Endo_ResultText<-gsub("\n"," ",MyColonData$Endo_ResultText)
MyColonData$Histo_ResultText<-gsub("\n"," ",MyColonData$Histo_ResultText)


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

MyColonData$Endo_ResultText <- gsub("\\\\", "",MyColonData$Endo_ResultText) 
MyColonData$Endo_ResultText= sub("\\\\.*\\\\","",MyColonData$Endo_ResultText)
MyColonData$Endo_Endoscopist<-str_extract(MyColonData$Endo_ResultText, 'Endoscopist:(.*?)2nd Endoscopist')
MyColonData$Endo_Endoscopist <-gsub("Endoscopist:", "",MyColonData$Endo_Endoscopist)


MyColonData$Endo_Endoscopist<-ifelse(is.na(MyColonData$Endo_Endoscopist),str_extract(MyColonData$Endo_ResultText,"Signature.*"),MyColonData$Endo_Endoscopist)

MyColonData$Endo_Endoscopist <-gsub("  This procedure was electronically.* ", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("Signature:_________________________________", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("2nd Endoscopist.*", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist <-gsub("Dr\\.|Dr|Mr\\.|Mr|X[A-Z].*|X[0-9].*", "",MyColonData$Endo_Endoscopist)
MyColonData$Endo_Endoscopist<-trimws(MyColonData$Endo_Endoscopist)

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
#Instrument:
MyColonData$Instrument<-str_extract(MyColonData$Endo_ResultText, 'Instrument(.*)Extent of Exam')
MyColonData$Instrument <- gsub("X.*|Instrument:|Instrument|Extent of Exam|LOAN|Loan Scope \\(|Loan Scope \\(specify serial no:|Loan Scope \\(specify\\s*serial no|\\)|-.*", "",MyColonData$Instrument) 

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
MyColonData$Histol<-str_extract(MyColonData$Histo_ResultText, 'HISTOLOGY.*?DIAGNOSIS')
MyColonData$Histol<-gsub(". ","\n",MyColonData$Histol,fixed=TRUE)
MyColonData$Histol<-gsub("^\\s+","",MyColonData$Histol)

#DIAGNOSIS
MyColonData$Dx<-str_extract_all(MyColonData$Histo_ResultText, 'DIAGNOSIS.*')
MyColonData$Dx<-gsub("       ","",MyColonData$Dx)
MyColonData$Dx<-gsub(":","",MyColonData$Dx)
MyColonData$Dx<-gsub("[Nn]o .*?\n","",MyColonData$Dx)
MyColonData$Dx<-gsub("[Nn]ormal.*\n","",MyColonData$Dx)
MyColonData$Dx<-gsub("Reported.*","",MyColonData$Dx)
MyColonData$Dx<-gsub("   ([A-Z]|[1-9])","\n\\1",MyColonData$Dx)

#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ######Extraction by presenting complaint to the endoscopist##############  ################################################################  
#############################################  #############################################  #############################################  #############################################  




#Then group so get an individual list of indications
PClst<-split(MyColonData,MyColonData$IndicationsFroExamination)
#Clean up the data
names(PClst)<-paste0('df',gsub("\\s+","",names(PClst)))
names(PClst)<-paste0('df',gsub("\\.","",names(PClst)))

#Then filter so just extracting those lists where nrow is >20
PClst<-PClst[sapply(PClst,nrow)>20]

#Gives the basic output of the number for each indication
numPC<-data.frame(lapply(PClst,nrow))
numPC<-t(numPC)
numPC<-data.frame(numPC)
#Then you will need to organise the histology result so that it gives a quantifiable output
#Remove any line with Duodenum or oesophagus.
ChronicDiarrhoea<-data.frame(PClst[["dfdfAlteredbowelhabit-CHRONICDIARRHOEA"]])

lapply(PClst, function(x) gsub("^.*uodenum.*\\n","",x$Dx))
lapply(PClst, function(x) gsub("^.*sophag.*\\n","",x$Dx))

Adenoma<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*denoma.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Adenoma)<-c("Adenoma")


MicroscopColi<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*icroscopic [Cc]olit.",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(MicroscopColi)<-c("MicroscopColi")
TotalHistopDiags<-rbind(Adenoma,MicroscopColi)

Inflam<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*inflamm.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Inflam)<-c("Inflam")
TotalHistopDiags<-rbind(TotalHistopDiags,Inflam)

Hyperplastic<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*yperplast.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Hyperplastic)<-c("Hyperplastic")
TotalHistopDiags<-rbind(TotalHistopDiags,Hyperplastic)

Collagenou<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*ollage.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Collagenou)<-c("Collagenou")
TotalHistopDiags<-rbind(TotalHistopDiags,Collagenou)

Lymphocy<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*ymphocyt.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Lymphocy)<-c("Lymphocy")
TotalHistopDiags<-rbind(TotalHistopDiags,Lymphocy)

Melanosis<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*elanosi.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Melanosis)<-c("Melanosis")
TotalHistopDiags<-rbind(TotalHistopDiags,Melanosis)


Normal<-as.data.frame(lapply(PClst, function(x) (sum(grepl(".*inflamm.*",x$Dx,perl=TRUE)
                                 &!grepl(".*denom.*",x$Dx,perl=TRUE)
                                 &!grepl(".*icroscopic colit.*",x$Dx,perl=TRUE)
                                 &!grepl(".*yperplast.*",x$Dx,perl=TRUE)
                                 &grepl(".*ormal.*",x$Dx))/sum(grepl(".*",x$Dx)))*100))
row.names(Normal)<-c("Normal")
TotalHistopDiags<-rbind(TotalHistopDiags,Normal)
TotalHistopDiagsMatrix<-as.matrix(TotalHistopDiags)

heatmap.2(TotalHistopDiagsMatrix,trace="none", density.info="none",dendrogram='none', Rowv=FALSE, Colv=FALSE,cexRow=0.8,cexCol=0.8)
levelplot(TotalHistopDiagsMatrix,scale=list(x=list(rot=45)))
print(TotalHistopDiagsMatrix)


#Then do a stacked barchart with the indication subdivided into the histology or a DiagrammR if too overwhelmed by normal results