#+ setup, include=FALSE
knitr::opts_chunk$set(warning=FALSE,message=FALSE,dev = "svg",fig.ext = ".svg")


library(ggplot2)
library(lattice)
library(compare)
library(grid)
library(reshape2)
#library(plyr)
library(gtools)
library(dplyr)
library(lubridate)
library(openxlsx)
library(knitr)
library(xtable)
local({
  hook_plot = knit_hooks$get('plot')
  knit_hooks$set(plot = function(x, options) {
    x = paste(x, collapse = '.')
    if (!grepl('\\.svg', x)) return(hook_plot(x, options))
    # read the content of the svg image and write it out without <?xml ... ?>
    paste(readLines(x)[-1], collapse = '\n')
    paste("<figure><img src=\"", opts_knit$get("base.url"), paste(x, collapse = "."), 
          "\"><figcaption>", options$fig.cap, "</figcaption></figure>", sep = "")
  })
})


library(stringr)



MyColonData<-read.xlsx("S:\\Gastroenterology\\Seb\\R\\Data\\GRS\\DataJan18ToJuneEnd18.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
#MyColonData<-MyColonData[-1]
#names(MyColonData)<-c("PatientID","NHSNumber","PatientName","EndoResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText")
#Have to do this as imports as numbers
MyColonData$Endo_ResultEntered<-as.Date(MyColonData$Endo_ResultEntered,format="%d-%m-%Y", origin = "1960-10-01")
MyColonData$Endo_ResultPerformed<-as.Date(MyColonData$Endo_ResultPerformed,format="%d-%m-%Y", origin = "1960-10-01")
#Restrict to the last 6 months:
#MyColonData<-MyColonData[order(MyColonData$Endo_ResultPerformed),]

#MyColonData<-subset(MyColonData,MyColonData$Endo_ResultPerformed>"2017-01-01")
#MyColonData<-subset(MyColonData,MyColonData$Endo_ResultPerformed<"2016-06-01")
#MyColonData$Endo_ResultText<-gsub("\n"," ",MyColonData$Endo_ResultText)
#MyColonData$Histo_ResultText<-gsub("\n"," ",MyColonData$Histo_ResultText)


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
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
#MyColonData<-as.data.frame(mysummary(MyColonData))
MyColonData<-as.data.frame(EndoscChopper(MyColonData))
MyColonData<-as.data.frame(HistolChopper(MyColonData))

MyColonData<-data.frame(apply(MyColonData,2,function(y) gsub("_x000D_","",y)))

#Select diagnostic endoscopies only
MyColonData$ProcPerformed<-as.character(MyColonData$ProcPerformed)
#MyColonData<-MyColonData[grepl("Colonoscopy$",MyColonData$ProcPerformed),]
#To make therapeutic only:
Therapeutic
MyColonData<-MyColonData[!grepl("herapeuti",MyColonData$IndicationsFroExamination),]
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ######## Adenoma detection rates  ###########  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ######## by endoscopist  ####################  #############################################  #############################################  
#############################################  ######## by instrument   ####################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  

######################################################### by endoscopist for adenomas ####################  #############################################  #############################################  
#Get the adenomas vs the number of colons done by endoscopist:
MyColonDataAdenomaDetectionByEndoscopist<-MyColonData[grepl(".*denoma.*",MyColonData$Histo_ResultTextForFindings),] 
MyColonDataAdenomaDetectionByEndoscopist<-MyColonDataAdenomaDetectionByEndoscopist%>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumAdenomas=nrow(.)))
####################################################################################################################################################################################
####################################################################################################################################################################################
# 
# #Interim bit to compare with Sabina's- TBB
# 
# 
# AdenomasSeb<-MyColonData[grep(".*denom.*",MyColonData$Histo_ResultText),]
# AdenomasSebNum<-data.frame(AdenomasSeb$PatientID)
# names(AdenomasSebNum)<-c("HospNum")
# AdenomasSebNum$Owner<-"Seb"
# 
# MyADR<-read.xlsx("S:\\Gastroenterology\\Seb\\R\\Scripts\\GRS\\ADR.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
# MyADRAdenomas<-MyADR[grepl("^a$",MyADR$Histology)|grepl("^a\\+",MyADR$Histology),]
# MyADRNum<-data.frame(MyADRAdenomas$PATIENTRECORDNUMBER)
# #grep out the adenomas
# 
# names(MyADRNum)<-c("HospNum")
# MyADRNum$Owner<-"De Martino"
# Discrepantdf<-rbind(MyADRNum,AdenomasSebNum)
# View(Discrepantdf[unique(Discrepantdf$HospNum),])

####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


MyColonDataColonoscopiesByEndoscopist<-MyColonData %>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumColons=nrow(.)))

#Merge the two above by column to get proportion:
MyColonDataADR<-full_join(MyColonDataAdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
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




#############################################  ######## by instrument   ####################  #############################################  #############################################  
#Adenoma detection rate based on endoscope used:
MyColonData$Completeness<-ifelse(grepl("*.erminal.*|Caecum",MyColonData$ExtentofExam),"Complete","Incomplete")

MyColonDataAdenomaDetectionByInstrument<-MyColonData[grep(".*denoma.*",MyColonData$Histo_ResultTextForFindings),] %>% 
  group_by(Instrument) %>% 
  do(data.frame(NumAdenomas=nrow(.)))
#To get all the endoscopies
MyColonDataColonoscopiesByInstrument<-MyColonData %>% 
  group_by(Instrument) %>% 
  do(data.frame(NumColons=nrow(.)))
#Merge the two above by column to get proportion:
MyColonDataADRByInstrument<-full_join(MyColonDataAdenomaDetectionByInstrument,MyColonDataColonoscopiesByInstrument,by=c("Instrument"))
MyColonDataADRByInstrument$PropAdenomas<-(MyColonDataADRByInstrument$NumAdenomas/ MyColonDataADRByInstrument$NumColons)*100

########Further Assessment by Instrument################################################
MyColonDataADREndoscopistsOver20PCent<-subset(MyColonDataADR,MyColonDataADR$PropAdenomas>15)
MyColonDataAdenomaDetectionByInstrument<-MyColonData[MyColonDataADREndoscopistsOver20PCent$Endo_Endoscopist %in% MyColonData,]
detach(package:plyr)

MyColonDataAdenomaDetectionByInstrument<-MyColonData[grepl(".*denoma.*",MyColonData$Histo_ResultTextForFindings),] %>% 
  group_by(Instrument) %>% 
  do(data.frame(NumAdenomas=nrow(.)))

#To get all the endoscopies
MyColonDataColonoscopiesByInstrument<-MyColonData %>% 
  group_by(Instrument) %>% 
  do(data.frame(NumColons=nrow(.)))

MyColonData$Dx_Simplified<-as.character(MyColonData$Histo_ResultTextForFindings)


#Get the average amount of sedation for each scope used and plot on the same graph as the PropAdenomas by scope graph:

MyColonData$Instrument<-as.character(MyColonData$Instrument)
Completeness<-MyColonData%>% 
  group_by(Instrument,Completeness) %>% 
  summarise(n=n())

MyColonData$Fent<-as.numeric(as.character(MyColonData$Fent))
Fent<-MyColonData %>% 
  group_by(Instrument) %>% 
  summarise(meanFent=mean(Fent,na.rm=T))

MyColonData$Midaz<-as.numeric(as.character(MyColonData$Midaz))
Midaz<-MyColonData %>% 
  group_by(Instrument) %>% 
  summarise(meanMidaz=mean(Midaz,na.rm=T),sdMidaz=sd(Midaz,na.rm=T))

CompleteByScopeNumber<-as.data.frame.matrix(table(MyColonData$Instrument,MyColonData$Completeness))
CompleteByScopeNumber$Instrument<-row.names(CompleteByScopeNumber)
CompleteByScopeNumber$PropFailed<-(CompleteByScopeNumber$Incomplete/(CompleteByScopeNumber$Incomplete+CompleteByScopeNumber$Complete))*100


#Merge the two above by column to get proportion:
MyColonDataAdenomaDetectionByInstrument<-data.frame(MyColonDataAdenomaDetectionByInstrument)
MyColonDataColonoscopiesByInstrument<-data.frame(MyColonDataColonoscopiesByInstrument)
Fent<-data.frame(Fent)
Midaz<-data.frame(Midaz)
CompleteByScopeNumber<-data.frame(CompleteByScopeNumber)
MyColonDataADRByInstrument<-full_join(MyColonDataAdenomaDetectionByInstrument,MyColonDataColonoscopiesByInstrument,by=c("Instrument"))
MyColonDataADRByInstrument<-full_join(MyColonDataADRByInstrument,Fent,by=c("Instrument"))
MyColonDataADRByInstrument<-full_join(MyColonDataADRByInstrument,Midaz,by=c("Instrument"))
MyColonDataADRByInstrument<-full_join(MyColonDataADRByInstrument,CompleteByScopeNumber,by=c("Instrument"))
MyColonDataADRByInstrument$PropAdenomas<-(MyColonDataADRByInstrument$NumAdenomas/ MyColonDataADRByInstrument$NumColons)*100

#Limit the output to scopes used >20 times
MyColonDataADRByInstrument<-MyColonDataADRByInstrument[MyColonDataADRByInstrument$NumColons>5,]


#Get rid of stragglers:
MyColonDataADRByInstrument$Instrument<-as.character(MyColonDataADRByInstrument$Instrument)
MyColonDataADRByInstrument<-MyColonDataADRByInstrument[!grepl("FC$",MyColonDataADRByInstrument$Instrument),]
MyColonDataADRByInstrument<-MyColonDataADRByInstrument[-1,]

MyColonData$ExtentofExam<-as.character(MyColonData$ExtentofExam)

#Get the ordering sorted:
MyColonDataADRByInstrument$Composite<-MyColonDataADRByInstrument$PropAdenomas+(1/MyColonDataADRByInstrument$PropFailed)*100
########################################################################################################################################################################
########################################################################################################################################################################
#Need to compare old to the new Fuji and Olympus and Guys scopes--Old =pre 2010
oldFuji<-c("FC1","FC2","FC3","FC4","FC5","FC10")
newFuji<-c("FC7","FC8","FC9","FC6","FC11","FC12","FC13","FC14","FC15","FC16")

newGuys<-c("GUYS1","GUYS2","GUYS3","GUYS4","GUYS5","GUYS6","GUYS7","GUYS8","GUYS9","GUYS10","GUYS11","GUYS12")
oldGuys<-c("Guys13")

oldOlympus<-c("C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C12")
newOlympus<-c("C16","C17")

MyColonDataADRByInstrument$Instrument<-as.character(MyColonDataADRByInstrument$Instrument)
MyColonDataADRByInstrument$Instrument<-as.character(MyColonDataADRByInstrument$Instrument)
MyColonDataADRByInstrument$ScopeType<-ifelse(grepl(paste(oldFuji, collapse='|'), MyColonDataADRByInstrument$Instrument,perl=TRUE),"oldFuji",
                                             ifelse(grepl(paste(newFuji, collapse='|'), MyColonDataADRByInstrument$Instrument,perl=TRUE),"newFuji",
                                                    ifelse(grepl(paste(oldGuys, collapse='|'), MyColonDataADRByInstrument$Instrument,perl=TRUE),"oldGuys",
                                                           ifelse(grepl(paste(newGuys, collapse='|'), MyColonDataADRByInstrument$Instrument,perl=TRUE),"newGuys",
                                                                  ifelse(grepl(paste(oldOlympus, collapse='|'), MyColonDataADRByInstrument$Instrument,perl=TRUE),"oldOlympus",
                                                                         ifelse(grepl(paste(newOlympus, collapse='|'), MyColonDataADRByInstrument$Instrument,perl=TRUE),"newOlympus",
                                                                                "unregistered"))))))

MyColonDataADRByInstrumentGrouped<-MyColonDataADRByInstrument%>%group_by(ScopeType)%>%summarise(ADR=mean(PropAdenomas),Failed=mean(PropFailed))
MyColonDataADRByInstrumentGrouped$Composite<-MyColonDataADRByInstrumentGrouped$ADR+(1/MyColonDataADRByInstrumentGrouped$Failed)*100
########################################################################################################################################################################
########################################################################################################################################################################
#Need to compare Olympus to Fuji overall:
MyColonDataADRByInstrumentGrouped$FujiOrOlympus<-ifelse(grepl("oldFuji", MyColonDataADRByInstrumentGrouped$ScopeType)|grepl("newFuji", MyColonDataADRByInstrumentGrouped$ScopeType),"Fuji",
                                                        ifelse(grepl("oldOlympus", MyColonDataADRByInstrumentGrouped$ScopeType)|grepl("newOlympus", MyColonDataADRByInstrumentGrouped$ScopeType),"Olympus",
                                                               ifelse(grepl("oldGuys", MyColonDataADRByInstrumentGrouped$ScopeType)|grepl("newGuys", MyColonDataADRByInstrumentGrouped$ScopeType),"Fuji",
                                                                      "Unregistered")))
MyColonDataADRByInstrumentGroupedFujiVSOlympus<-MyColonDataADRByInstrumentGrouped%>%group_by(FujiOrOlympus)%>%summarise(ADR=mean(ADR),Failed=mean(Failed))
MyColonDataADRByInstrumentGroupedFujiVSOlympus$Composite<-MyColonDataADRByInstrumentGroupedFujiVSOlympus$ADR+(1/MyColonDataADRByInstrumentGroupedFujiVSOlympus$Failed)*100
########################################################################################################################################################################
########################################################################################################################################################################
MyColonDataADRByInstrument$Instrument <-factor(MyColonDataADRByInstrument$Instrument, levels=MyColonDataADRByInstrument[order(MyColonDataADRByInstrument$Composite), "Instrument"]) 

#Graph for individual scopes
Instruments<-ggplot(MyColonDataADRByInstrument) + 
  geom_bar(aes(Instrument,PropAdenomas),fill="red",stat="identity") +
  geom_line(aes(x=Instrument,y=PropFailed, group = 1),color="green") +
  geom_point(aes(Instrument,PropFailed),color="green",stat="identity") +
  geom_line(aes(x=Instrument,y=Composite, group = 1),color="black") +
  geom_point(aes(Instrument,Composite),color="black",stat="identity") +
  labs(title="Metrics by Instrument number \nFiltered for colnoscopists with ADR>15% and \nInstruments used >5 times in date range (June to Dec31st 2016)\nADR (red bars)\nProportion of failed caecal intubation (green dots)\nComposite Score (ADR+%Complete); black dots)") +
  xlab("Instrument") + 
  ylab("ADR(%;red),Composite score(% complete;black),Failed intubation (%green)") +
  theme(axis.text.x=element_text(angle=90)) +
  theme(legend.position="top") 

#Graph for old vs new scopes

InstrumentsGrouped<-ggplot(MyColonDataADRByInstrumentGrouped) + 
  geom_bar(aes(ScopeType,ADR),fill="red",stat="identity") +
  geom_line(aes(x=ScopeType,y=Failed, group = 1),color="green") +
  geom_point(aes(ScopeType,Failed),color="green",stat="identity") +
  geom_line(aes(x=ScopeType,y=Composite, group = 1,color="black")) +
  geom_point(aes(ScopeType,Composite),color="black",stat="identity") +
  labs(title="Metrics by ScopeType ") +
  xlab("ScopeType") + 
  ylab("ADR(%;red),Composite score(% complete;black),Failed intubation (%green)") +
  theme(axis.text.x=element_text(angle=90)) +
  theme(legend.position="none") 

#Graph for Fuji vs Olympus scopes
InstrumentsGroupedFujiVSOlympus<-ggplot(MyColonDataADRByInstrumentGroupedFujiVSOlympus) + 
  geom_bar(aes(FujiOrOlympus,ADR),fill="red",stat="identity") +
  geom_point(aes(FujiOrOlympus,Failed),color="green",stat="identity",size=6) +
  geom_point(aes(FujiOrOlympus,Composite),color="black",stat="identity",size=6) +
  xlab("Instrument") + 
  ylab("ADR(%;red),Composite score(% complete;black),Failed intubation (%green)") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(title="Metrics by Fuji vs Olympus ") +
  theme(legend.position="none") 


source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
multiplot(Instruments,InstrumentsGrouped,InstrumentsGroupedFujiVSOlympus,cols=2)


#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ######## Sedation  rates  ###################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ######## by endoscopist  ####################  #############################################  #############################################  
#############################################  ######## by instrument   ####################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  



MyColonDataFentByEndoscopist<-MyColonData %>% 
  group_by(Endo_Endoscopist) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

MyColonDataMidazByEndoscopist<-MyColonData %>% 
  group_by(Endo_Endoscopist) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

#Merge the two above by column to get proportion:
MyColonDataMeds<-full_join(MyColonDataFentByEndoscopist,MyColonDataMidazByEndoscopist,by=c("Endo_Endoscopist"))
#Get the withdrawal time before restricting the dataframe
MyColonWithdrawalTime<-MyColonDataMeds
MyColonWithdrawalTime<-data.frame(MyColonWithdrawalTime["Endo_Endoscopist"],MyColonWithdrawalTime["WithdrawalTime.y"])

#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  ##########  Withdrawal Time  ################  #############################################  #############################################  
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
#############################################  #############################################  #############################################  #############################################  
#############################################  ######## Merge all per endoscopist for final table  ###########  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  

FinalTable<-full_join(MyColonWithdrawalTime,MyColonDataHDR,by=c("Endo_Endoscopist"))

FinalTable<-full_join(FinalTable,MyColonDataADR,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataAdenocarcinomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataLGD_Adenomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataHGD_Adenomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataSerr_Adenomas,by=c("Endo_Endoscopist"))
FinalTable$HyperplasticToAdenomaRatio<-FinalTable$PropAdenoma/FinalTable$PropHyperplastic



TBB<- ggplot(FinalTable)+
  geom_point(aes(NumColons.y, PropHyperplastic, color="red"))+
  xlab("Number of colons")+
  ylab("Proportion hyperplastic")+
  theme(axis.text.x=element_text(angle=-90))+
  theme(legend.position="top")



write.table(FinalTable,"S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Outputs\\out.csv")
sink("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Outputs\\out.html")
#print(xtable(FinalTable),type="html")
#sink()



#' ---
#' author: Sebastian Zeki
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'      St Thomas' Adenoma Detection Rates for GRS
#' ---

#+results='asis', echo=FALSE
knitr::kable(FinalTable, digits = 2)
#'
#'
#'Table 1: Adenoma detection rates 
#'
