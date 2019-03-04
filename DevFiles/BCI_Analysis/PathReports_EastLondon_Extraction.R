#Script for Richard Hackett Sebastian Zeki30/11/2018


#Import the data containing pathology data

library(readxl)
library(writexl)
library(EndoMineR)

mydata2<-read_excel("/home/rstudio/GenDev/DevFiles/BCI_Analysis/data/test_data_for_sz_barretts.xlsx")
mydata3<-read_excel("/home/rstudio/GenDev/DevFiles/BCI_Analysis/data/test_data_for_sz_barretts2.xlsx")
mydata4<-read_excel("/home/rstudio/GenDev/DevFiles/BCI_Analysis/data/test_data_for_sz_Barretts3.xlsx")
mydata2<-rbind(mydata2,mydata3,mydata4)


mydata2<-data.frame(read_excel("/home/rstudio/GenDev/DevFiles/BCI_Analysis/data/Barrett_ValidationData_100Set_23.01.19.xlsx"),stringsAsFactors = FALSE)



#This extracts all the pathology data (I've left the Endoscopy extraction for now)
EndoscTree<-c("Reporting laboratory reference number","Specimen","Clinical Details","Macroscopy","Microscopy","Conclusion","Reported by")
Mypath<-Extractor(mydata2,"Report",EndoscTree)


#Or can try this version of the extractor as the heading delimiters are sometimes in different positions

# for(i in 1:(length(EndoscTree)-1)) {
#   mydata2<-Extractor2(mydata2,'Report',as.character(EndoscTree[i]),
#                       as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
# }

#This extracts all the biopsy locations listed in the pathology report:
Mypath<-TermStandardLocation(Mypath,'Specimen')

#This cleans the Histology text to provide you with non-negative sentences in the column that you think 
#gives the Diagnosis or Conclusion. It will remove negative sentences to make
#Further term searches more accurate
Mypath<-HistolDx(Mypath,'Original')

#Try to get the number of biopsies 
#Get the worst path stage from the Barrett's biopsies

Barr<- Barretts_PathStage(Mypath,'Dx_Simplified')
Barr<- Barretts_PragueScore(Barr,'ClinicalDetails','Original')
Barr<-Barretts_FUType(Barr)

#Validation columns

Barr[,ncol(Barr)+1] = NA
Barr[,ncol(Barr)+1] = NA
Barr[,ncol(Barr)+1] = NA
Barr[,ncol(Barr)+1] = NA


names(Barr)[names(Barr) == 'V30'] <- 'IMorNoIM_Yes_No'
names(Barr)[names(Barr) == 'V31'] <- 'CStage_Yes_No'
names(Barr)[names(Barr) == 'V32'] <- 'MStage_Yes_No'
names(Barr)[names(Barr) == 'V33'] <- 'FU_Group_Yes_No'

write_xlsx(Barr, "/home/rstudio/Barrett_ValidationData.xlsx")


#Plan is to code each finding, then compare the rows then given summary table.

#Select the top 10 from the sheet
BarrTables<-Barr[1:100,]
#Do the comparison into a separate column
BarrTables$SameIM<-ifelse(trimws(BarrTables$IMorNoIM)==trimws(BarrTables$TRUE_IMorNoIM),1,0)
table(BarrTables$SameIM)




#Clean it up
BarrTables$TrueC<-as.character(BarrTables$TrueC)
#Get rid of confounding whitespace etc
BarrTables$TrueC<-gsub("(\\d+).*","\\1",BarrTables$TrueC)
#Do the comparison into a separate column
BarrTables$SameCStage<-ifelse(trimws(BarrTables$CStage)==trimws(BarrTables$TrueC),1,0)
table(BarrTables$SameCStage)




#Clean it up
BarrTables$TrueM<-as.character(BarrTables$TrueM)
#Get rid of confounding whitespace etc
BarrTables$TrueM<-gsub("(\\d+).*","\\1",BarrTables$TrueM)
#Do the comparison into a separate column
BarrTables$SameMStage<-ifelse(trimws(BarrTables$MStage)==trimws(BarrTables$TrueM),1,0)
table(BarrTables$SameMStage)


#See all of them 
View(BarrTables%>%select(ClinicalDetails,MStage,TrueM,SameMStage))

#See the troublemakers
View(BarrTables%>%filter(BarrTables$SameMStage==0)%>%select(Original,ClinicalDetails,MStage,TrueM))



#The output only needs to be evaluated as proportion that match (as there is only a binary output rather than a 2x2 contingency table)

