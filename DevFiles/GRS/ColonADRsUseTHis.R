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


MyColonData<-read.xlsx("/home/rstudio/GenDev/DevFiles/GRS/CTT004772_DataJan18.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
MyColonData$Endo_ResultEntered<-as.Date(MyColonData$Endo_ResultEntered,format="%d-%m-%Y", origin = "1960-10-01")
MyColonData$Endo_ResultPerformed<-as.Date(MyColonData$Endo_ResultPerformed,format="%d-%m-%Y", origin = "1960-10-01")

#############################################  DATA CLEANING ###############################  #############################################  #############################################  

#Bit of a tidy up
source("/home/rstudio/GenDev/DevFiles/Generics/CleanUp.R")
MyColonData<-as.data.frame(EndoscChopper(MyColonData))
MyColonData<-as.data.frame(HistolChopper(MyColonData))
MyColonData<-data.frame(apply(MyColonData,2,function(y) gsub("_x000D_","",y)))

#Select diagnostic endoscopies only
MyColonData$ProcPerformed<-as.character(MyColonData$ProcPerformed)
MyColonData<-MyColonData[grepl("Colonoscopy$",MyColonData$ProcPerformed),]
#############################################  #############################################  #############################################  #############################################  

######################################################### by endoscopist for adenomas ####################  #############################################  #############################################  
#Get the adenomas vs the number of colons done by endoscopist:
MyColonDataAdenomaDetectionByEndoscopist<-MyColonData[grepl(".*denoma.*",MyColonData$Histo_ResultTextForFindings),] 
MyColonDataAdenomaDetectionByEndoscopist<-MyColonDataAdenomaDetectionByEndoscopist%>% 
  group_by(Endo_Endoscopist) %>% 
  do(data.frame(NumAdenomas=nrow(.)))

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

#### The final table #####
FinalTable<-full_join(MyColonDataADR,MyColonDataHDR,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataAdenocarcinomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataLGD_Adenomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataHGD_Adenomas,by=c("Endo_Endoscopist"))
FinalTable<-full_join(FinalTable,MyColonDataSerr_Adenomas,by=c("Endo_Endoscopist"))
FinalTable$HyperplasticToAdenomaRatio<-FinalTable$PropAdenomas/FinalTable$PropHyperplastic



TBB<- ggplot(FinalTable)+
  geom_point(aes(NumColons.y, PropHyperplastic, color="red"))+
  xlab("Number of colons")+
  ylab("Proportion hyperplastic")+
  theme(axis.text.x=element_text(angle=-90))+
  theme(legend.position="top")


