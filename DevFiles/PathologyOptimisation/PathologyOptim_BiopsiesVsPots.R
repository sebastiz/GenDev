library(stringr)
library(ggplot2)
library(lattice)
library(compare)
library(XLConnect)
library(grid)
library (reshape2)
library(dplyr)
library(gtools)
library(gplots)
library(gdata)
library(dplyr)
library(GGally)
library(knitr)
library(xtable)
library(pander)

EoEc = loadWorkbook("~\\All_GastroscopySince2009.xls")
EoEc = readWorksheet(EoEc, sheet="All_GastroscopySince2009",header=TRUE)


source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
EoEc<-EndoscChopper(EoEc)
EndoSubsetBx<-HistolChopper(EoEc)



EndoSubsetBx$Histo_ResultEntered<-as.Date(EndoSubsetBx$VisitDate,format="%Y-%m-%d",origin="30/12/1899")


strips_df<-data.frame(EndoSubsetBx$Histo_ResultEntered,EndoSubsetBx$Endoscopy_ID,EndoSubsetBx$NumberBx)
#Now do the total number of biopsies for that endoscopy
#Potential number of pots:




PotentialPotsPerEndoscopy<-strips_df%>%
  mutate(year=format(EndoSubsetBx.VisitDate, "%Y"))%>%
  mutate(month=format(EndoSubsetBx.VisitDate, "%m"))%>%
  group_by(EndoSubsetBx.Endoscopy_ID,year,month)%>%
  summarise(Frequency = sum(EndoSubsetBx.NumberBx))%>%
  mutate(PotentialPotsNeeded=ceiling(Frequency/8))
PotentialPotsPerEndoscopy<-data.frame(PotentialPotsPerEndoscopy)
PotentialPotsPerEndoscopy<-PotentialPotsPerEndoscopy%>%
  select(year,month,PotentialPotsNeeded)%>%
  group_by(year)%>%
  summarise(PotentialPotsPerEndoscopy = sum(PotentialPotsNeeded,na.rm=T))






ActualPotsPerEndoscopy<-strips_df%>%
  mutate(year=format(EndoSubsetBx.VisitDate, "%Y"))%>%
  mutate(month=format(EndoSubsetBx.VisitDate, "%m"))%>%
  group_by(EndoSubsetBx.Endoscopy_ID,year,month)%>%
  do(data.frame(ActualPotsUsed=nrow(.)))
ActualPotsPerEndoscopy<-data.frame(ActualPotsPerEndoscopy)
ActualPotsPerEndoscopy<-ActualPotsPerEndoscopy%>%
  select(year,month,ActualPotsUsed)%>%
  group_by(year)%>%
  summarise(ActualPotsPerEndoscopy = sum(ActualPotsUsed,na.rm=T))

#Did the number of endoscopies go down?
EndoscopiesDone<-strips_df%>%
  mutate(year=format(EndoSubsetBx.VisitDate, "%Y"))%>%
  mutate(month=format(EndoSubsetBx.VisitDate, "%m"))%>%
  group_by(EndoSubsetBx.Endoscopy_ID,year,month)%>%
  summarise(n=n())%>%
  group_by(year)%>%
  summarise(NumberOfEnsoscopiesDone=n())
EndoscopiesDone<-data.frame(EndoscopiesDone)



FinalPotsdf<-cbind(ActualPotsPerEndoscopy,PotentialPotsPerEndoscopy,EndoscopiesDone,by=c("year"))
FinalPotsdf<-FinalPotsdf[-1]
names(FinalPotsdf)<-c("ActualPots","year","PotentialPots","year.1", "NumberOfEndoscopiesDone")
FinalPotsdf<-FinalPotsdf%>%
  select(year,ActualPots,PotentialPots,NumberOfEnsoscopiesDone)%>%
  mutate(AbsoluteOverUsage=ActualPots-PotentialPots)%>%
  mutate(PercentageOverUsage=(AbsoluteOverUsage/ActualPots)*100)


sum(FinalPotsdf$OverUsage,na.rm=T)
