
library(XLConnect)
library(dplyr)

Self = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_HistoReport_run%2004-01-2017(Mar17).xlsx")
Self2 = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_HistoReport_run%2004-01-2017(1).xlsx")
Self3 = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_HistoReport_run%2004-01-2017(Feb17).xlsx")
Self4 = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_HistoReport_run%2004-01-2017(Apr17).xlsx")
Self5 = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_HistoReport_run%2004-01-2017(May17).xlsx")
Self = XLConnect::readWorksheet(Self, sheet="Data",header=TRUE)
Self2 = XLConnect::readWorksheet(Self2, sheet="Data",header=TRUE)
Self3 = XLConnect::readWorksheet(Self3, sheet="Data",header=TRUE)
Self4 = XLConnect::readWorksheet(Self4, sheet="Data",header=TRUE)
Self5 = XLConnect::readWorksheet(Self5, sheet="Data",header=TRUE)



names(Self)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")
names(Self2)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")
names(Self3)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")
names(Self4)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")
names(Self5)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")

#Remove the first row if necessary
Self<-Self[2:nrow(Self),]
Self2<-Self2[2:nrow(Self2),]
Self3<-Self3[2:nrow(Self3),]
Self4<-Self4[2:nrow(Self4),]
Self5<-Self5[2:nrow(Self5),]
Self<-rbind(Self2,Self)
Self<-rbind(Self3,Self)
Self<-rbind(Self4,Self)
Self<-rbind(Self5,Self)


#Get rid of everything without an endoscopy report (because pre 2007 there won't be one, just histopath)
Self<-subset(Self,!(is.na(Self["Endo_ResultName"])))


source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
Self<-EndoscChopper(Self)
Self<-HistolChopper(Self)
#Label the endoscopist as surgical or physician

Self$Endo_EndoscopistType<-ifelse(grepl("^Mr.*|Amir Darakhshan|Andrew Cowie|Mitesh Sharma|C Baker",Self$Endo_Endoscopist),"Surgeon",
                                         ifelse(grepl("Harriet Watson|Fiona Hibberts",Self$Endo_Endoscopist),"Nurse",
                                                  "Physician"))


#Label each endoscopy type with the necessary units
Self$Units<-ifelse(grepl("Colonoscopy  -  Therapeutic- Polypectomy",fixed=T,Self$ProcPerformed),3,
       ifelse(grepl("Colonoscopy  -  Therapeutic- Cauterization (APC etc) ",fixed=T,Self$ProcPerformed),3,
              ifelse(grepl("Colonoscopy  -  Therapeutic- EMR/ESD ",fixed=T,Self$ProcPerformed),4,
                     ifelse(grepl("Colonoscopy  -Therapeutic- EMR/ESD ",fixed=T,Self$ProcPerformed),4,
                            ifelse(grepl("Endoscopic Ultrasound EUS ",fixed=T,Self$ProcPerformed),2,
                                   ifelse(grepl("ERCP  -  Therapeutic- Metal stent insertion",fixed=T,Self$ProcPerformed),3,
                                          ifelse(grepl("ERCP  -  Therapeutic- Stent insertion",fixed=T,Self$ProcPerformed),3,
                                                 ifelse(grepl("Flexible Sigmoidoscopy",fixed=T,Self$ProcPerformed),1,
                                                        ifelse(grepl("Flexible Sigmoidoscopy  -  Therapeutic- Decompression ",fixed=T,Self$ProcPerformed),2,
                                                               ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- PEG Removal",fixed=T,Self$ProcPerformed),1,
                                                                      ifelse(grepl("Gastroscopy (OGD) - Therapeutic- EMR/ESD",fixed=T,Self$ProcPerformed),2,
                                                                             ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- Botox",fixed=T,Self$ProcPerformed),2,
                                                                                    ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- EMR/ESD ",fixed=T,Self$ProcPerformed),3,
                                                                                           ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- Gastric Balloon Removal",fixed=T,Self$ProcPerformed),3,
                                                                                                  ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- Injection therapy",fixed=T,Self$ProcPerformed),3,
                                                                                                         ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- Varices",fixed=T,Self$ProcPerformed),2,
                                                                                                                ifelse(grepl("Gastroscopy (OGD)  -Therapeutic- Gastric Balloon Insertion",fixed=T,Self$ProcPerformed),1,
                                                                                                                       ifelse(grepl("Colonoscopy",fixed=T,Self$ProcPerformed),2,
                                                                                                                              ifelse(grepl("Colonoscopy  -  Therapeutic- Dilatation",fixed=T,Self$ProcPerformed),3,
                                                                                                                                            ifelse(grepl("Endoscopic Ultrasound",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                   ifelse(grepl("  -  Therapeutic- Stone extraction",fixed=T,Self$ProcPerformed),3,
                                                                                                                                                          ifelse(grepl("ERCP  -  Therapeutic- Sphincterotomy",fixed=T,Self$ProcPerformed),3,
                                                                                                                                                                 ifelse(grepl("ERCP",fixed=T,Self$ProcPerformed),3,
                                                                                                                                                                        ifelse(grepl("Flexible Sigmoidoscopy  -  Therapeutic- Cauterization (APC etc) ",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                               ifelse(grepl("Flexible Sigmoidoscopy  -  Therapeutic- EMR/ESD   ",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                                      ifelse(grepl("Gastroscopy (OGD) -  Therapeutic- BRAVO",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                                                    ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- BRAVO",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                                                           ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- Dilatation",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                                                                  ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- HALO RFA",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                                                                         ifelse(grepl("Gastroscopy (OGD)  -  Therapeutic- Gastric Balloon Insertion",fixed=T,Self$ProcPerformed),2,
                                                                                                                                                                                                                                ifelse(grepl("Gastroscopy (OGD)",fixed=T,Self$ProcPerformed),1,
                                                                                                                                                                                                                                       ifelse(grepl("Gastroscopy(OGD)",fixed=T,Self$ProcPerformed),1,
                                                                                                                                                                                                                                              ifelse(grepl("PEG Insertion",Self$ProcPerformed),2,0)))))))))))))))))))))))))))))))))


#Now have to group and summarise by endoscopist:

AvUnitsByProcedure<-Self%>%group_by(Endo_Endoscopist)%>%summarise(meanUnits=mean(Units))
table(Self$Endo_Endoscopist,Self$Units)

#Now do work per day for both the first and second endoscopist
detach("package:ggplot2", unload=TRUE)
detach("package:reshape2", unload=TRUE)
detach("package:plyr", unload=TRUE)

WorkPerDay<-Self%>%group_by(Endo_Endoscopist,Endo_ResultEntered,Endo_EndoscopistType)%>%summarise(meanUnits=sum(Units))
WorkPerDaySecond<-Self%>%group_by(SecondEndo_Endoscopist,Endo_ResultEntered,Endo_EndoscopistType)%>%summarise(meanUnits=sum(Units))
WorkPerDaySecond<-WorkPerDaySecond[!is.na(WorkPerDaySecond$SecondEndo_Endoscopist),]
WorkPerDaySecond<-WorkPerDaySecond[WorkPerDaySecond$SecondEndo_Endoscopist!= "",]
WorkPerDaySecond<-rbind(WorkPerDay,WorkPerDaySecond)


WorkPerDay<-data.frame(WorkPerDay)
#Exclude lists where <2 units and >16 units (have to do this to exclude all day list - raw time doesnt say whether is am/pm as is  based ion a weird 12 hour clock)
WorkPerDay<-WorkPerDay[WorkPerDay$meanUnits>3,]
#Limited to 16 units so that all day lists dont get counted as 1 as no way of finding out if something is a morning or afternoon list
WorkPerDay<-WorkPerDay[WorkPerDay$meanUnits<16,]

library(ggplot2)
ggplot(WorkPerDay,aes(Endo_EndoscopistType,meanUnits))+
  geom_boxplot(fill="red")+
  labs(title="Number of units per list by endoscopist type", x="Endoscopist Type", y="Units per list") +
  geom_jitter()+
  theme(axis.text.x=element_text(size=16)) +
  theme(axis.text.y=element_text(size=16)) 


#Total number of units
WorkPerDay_tot<-WorkPerDay%>%group_by(Endo_Endoscopist)%>%summarise(meanUnits=mean(meanUnits))

WorkPerDay_totByType<-WorkPerDay%>%group_by(Endo_EndoscopistType)%>%summarise(meanUnits=mean(meanUnits))

WorkPerDay_tot$Endo_Endoscopist <- factor(WorkPerDay_tot$Endo_Endoscopist, levels = WorkPerDay_tot$Endo_Endoscopist[order(WorkPerDay_tot$meanUnits)])
ggplot(WorkPerDay_tot,aes(Endo_Endoscopist,meanUnits))+
  geom_point(size=3, colour="#CC0000")  +
  labs(title="Average number of units per list by endoscopist") +
  xlab("Endoscopist") + 
  ylab("Average number of units/list") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(legend.position="top")

WorkPerDay_totByType<-WorkPerDay%>%group_by(Endo_EndoscopistType)%>%summarise(meanUnits=mean(meanUnits))


#If we run 12 unit lists for everyone---percentage capacity increase:..

#Current DNA rate..

#Concentrate on increasing the capacity of those lists with fewer on them (ie <8 units per list)

#WorkPerDay_totLessThan8<-WorkPerDay_tot[WorkPerDay_tot$meanUnits<8,]
WorkPerDay_totLessThan8<-WorkPerDay_tot

#How commonly is the list done per person per week?
#Select the under-filled Endoscopists:
SelfLow<-data.frame(Self[which(Self$Endo_Endoscopist %in% WorkPerDay_totLessThan8$Endo_Endoscopist),])
SelfLow$WeekOfEndoscopy<-format(as.Date(SelfLow$Endo_ResultEntered), "%W")
SelfByPerson<-SelfLow%>%select(Endo_Endoscopist,WeekOfEndoscopy,Endo_ResultEntered)
SelfByPerson<-SelfByPerson%>%group_by(Endo_ResultEntered,Endo_Endoscopist)

#Get rid of duplicates
SelfByPerson<-unique(SelfByPerson)

#Average lists per week:
ListsPerWeek<-data.frame(table(SelfByPerson$Endo_Endoscopist,SelfByPerson$WeekOfEndoscopy))
AvListsPerWeek<-ListsPerWeek%>%group_by(Var1)%>%summarise(mean=mean(Freq))
AvListsPerWeek<-data.frame(AvListsPerWeek)
names(AvListsPerWeek)<-c("Endo_Endoscopist","meanlistsPerWeek")
AvListsPerWeek$Endo_Endoscopist<-as.character(AvListsPerWeek$Endo_Endoscopist)
AvListsPerWeek$Expected<-AvListsPerWeek$mean*10

AvListsPerWeek<-cbind(WorkPerDay_totLessThan8,AvListsPerWeek)
AvListsPerWeek$Current<-AvListsPerWeek$meanlistsPerWeek*AvListsPerWeek$meanUnits
AvListsPerWeek$ShortfallPerWeek<-AvListsPerWeek$Expected-AvListsPerWeek$Current
AvListsPerWeekAnon<-data.frame(AvListsPerWeek[2],AvListsPerWeek[4:7])
#Number of extra units per week we could be doing as a conservative estimate:
newobject<-xtable(AvListsPerWeekAnon)
print.xtable(newobject, type="html", file="S:\\Gastroenterology\\Seb\\R\\Data\\EndoscopyShortfallPerWeek.html")
sum(AvListsPerWeek$ShortfallPerWeek)*52                                  
