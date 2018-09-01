library(EndoMineR)
library(readxl)



######################################### Data acquisiton######################################### 
#This is where the data is imported
Self<-read_excel("/home/rstudio/EndoMineR/data/GSTT_TCR2232_AllEndoscopyProcedure_since_2007.xlsx")

######################################### Data merging######################################### 
#This is where external datasets are merged with the current dataset. This can be done after cleaning as well especially if you need to clean dates etc.
######################################### Data cleaning######################################### 


names(Self)<-c("PatientID",  "NHSno", "PatientName", "birthdaynum","birthmonthnum" ,"birthyearnum" ,"Endo_ResultName", "Endo_ResultPerformed", "Endo_ResultEntered", "Endo_ResultText", "Histo_ResultName", "Histo_ResultPerformed", "Histo_ResultEntered", "Histo_ResultText")

#Remove the first row if necessary
Self<-subset(Self,!(is.na(Self["Endo_ResultName"])))



#Use Extractor2 as delimiters are organised in a non-consistent way:
Self$Endo_ResultText<-gsub('2nd Endoscopist:','Second endoscopist:',
                           Self$Endo_ResultText)
EndoscTree<-list("Patient Name:","Date of Birth:","Hospital Number:","Date of Procedure:","Endoscopist:","Second endoscopist:",
               "Referring Physician:","General Practicioner:","Nurses:",
               "Medications:","Instrument:","Extent of Exam:","Visualization:","Tolerance:","Complications:","Co-morbidity:",
               "INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED",
               "FINDINGS","ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","BIOPSIES","OPCS4 Code:","Signature:","")
for(i in 1:(length(EndoscTree)-1)) {
  Self<-Extractor2(Self,'Endo_ResultText',as.character(EndoscTree[i]),
                     as.character(EndoscTree[i+1]),as.character(EndoscTree[i]))
}


HistolTree<-c("NATURE OF SPECIMEN:","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY",
              "DIAGNOSIS","")
for(i in 1:(length(HistolTree)-1)) {
  Self<-Extractor2(Self,"Histo_ResultText",as.character(HistolTree[i]),
                    as.character(HistolTree[i+1]),as.character(HistolTree[i]))
}





######################################### Data forking (filtering and subsetting)######################################### 

Self<-EndoscEndoscopist(Self,'Endoscopist')
Self$Endoscopist<-gsub("Second","",Self$Endoscopist)
Self<-EndoscMeds(Self,'Medications')
Self<-EndoscInstrument(Self,'Instrument')
Self<-EndoscIndications(Self,'INDICATIONSFOREXAMINATION')
Self<-EndoscProcPerformed(Self,'PROCEDUREPERFORMED')


Self<-HistolHistol(Self,'HISTOLOGY')
Self<-HistolDx(Self,"DIAGNOSIS")
Self<-HistolExtrapolDx(Self,"DIAGNOSIS")

HowManyTests(Self,'DIAGNOSIS','DateofProcedure','')
