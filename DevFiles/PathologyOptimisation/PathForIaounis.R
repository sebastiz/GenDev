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

EoEc = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\Gastroscopy_HistoReport_run%2004-01-2017_Apr17_.xls")
EoEc = readWorksheet(EoEc, sheet="GSTT_TCR2232_GastroscopyProcedu",header=TRUE)
names(EoEc)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")
#Remove the first row if necessary
EoEc<-EoEc[2:nrow(EoEc),]

#Get rid of everything without an endoscopy report (because pre 2007 there won't be one, just histopath)
EoEc<-subset(EoEc,!(is.na(EoEc["Endo_ResultName"])))
#Split the spreadsheet up into new columns and then get rid of the original ones

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
EoEc<-EndoscChopper(EoEc)
EndoSubsetBx<-HistolChopper(EoEc)

writeWorksheetToFile("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\MyBariumDataWithHRMNoDupsLab.xlsx",data=EndoSubsetBx,sheet="blabla",startRow=1,startCol=1)

