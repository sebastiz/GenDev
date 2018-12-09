#Script for Richard Hackett Sebastian Zeki30/11/2018


#Import the data containing pathology data

library(readxl)
library(writexl)
library(EndoMineR)

mydata2<-read_excel("/home/rstudio/GenDev/DevFiles/BCI_Analysis/data/test_data_for_sz_barretts.xlsx")
mydata3<-read_excel("/home/rstudio/GenDev/DevFiles/BCI_Analysis/data/test_data_for_sz_barretts2.xlsx")

mydata2<-rbind(mydata2,mydata3)


#This extracts all the pathology data (I've left the Endoscopy extraction for now)
EndoscTree<-c("Reporting laboratory reference number","Specimen","Clinical Details","Macroscopy","Microscopy","Conclusion","Reported by")
Mypath<-Extractor(mydata2,"Report",EndoscTree)


#Or can try this version of the extractor as the heading delimiters are sometimes in different positions

for(i in 1:(length(EndoscTree)-1)) {
  mydata2<-Extractor2(mydata2,'Report',as.character(EndoscTree[i]),
                      as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}

#This extracts all the biopsy locations listed in the pathology report:
mydata2<-TermStandardLocation(mydata2,'Specimen')

#This cleans the Histology text to provide you with non-negative sentences in the column that you think 
#gives the Diagnosis or Conclusion. It will remove negative sentences to make
#Further term searches more accurate
mydata2<-HistolDx(mydata2,'Microscopy')

#Try to get the number of biopsies 
#Get the worst path stage from the Barrett's biopsies

Barr<- Barretts_PathStage(mydata2,'Dx_Simplified')
Barr<- Barretts_PragueScore(Barr,'Report')
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
