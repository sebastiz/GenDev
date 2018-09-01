library(readxl)
library(EndoMineR)
library(stringr)
library(dplyr)
library(lubridate)
library(psych)
library(zoo)
library(ggplot2)




#######################################????????????????Data acquisiton######################################### 
#This is where the data is imported
Mydata<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/AdHoc/GSTT_GastroscopyProcedure2007_2017.xlsx"))
names(Mydata)<-c("PatientID","NHSno","PatientName","birthdaynum","birthmonthnum","birthyearnum","Endo_ResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText")

#######################################????????????????Data merging######################################### 
#This is where external datasets are merged with the current dataset. This can be done after cleaning as well especially if you need to clean dates etc.
#######################################????????????????Data cleaning######################################### 


RenameCols<-function(x){
  names(x)<-c("PatientID","NHSno","PatientName","birthdaynum","birthmonthnum","birthyearnum","Endo_ResultName","Endo_ResultPerformed","Endo_ResultEntered","Endo_ResultText","Histo_ResultName","Histo_ResultPerformed","Histo_ResultEntered","Histo_ResultText")
  x<-x[-1,]
  return(x) 
}



Mydata2<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Apr17).xlsx"))
Mydata2<-RenameCols(Mydata2)
Mydata11<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Aug17).xlsx"))
Mydata11<-RenameCols(Mydata11)
Mydata3<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Dec17).xlsx"))
Mydata3<-RenameCols(Mydata3)
Mydata4<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Feb17).xlsx"))
Mydata4<-RenameCols(Mydata4)
Mydata5<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(July17).xlsx"))
Mydata5<-RenameCols(Mydata5)
Mydata6<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(June17).xlsx"))
Mydata6<-RenameCols(Mydata6)
Mydata7<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Mar17).xlsx"))
Mydata7<-RenameCols(Mydata7)
Mydata8<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(May17).xlsx"))
Mydata8<-RenameCols(Mydata8)
Mydata9<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Nov17).xlsx"))
Mydata9<-RenameCols(Mydata9)
Mydata10<-data.frame(read_excel("S:/Gastroenterology/Seb/R/Data/SelfAudit/Gastroscopy_TCR2232HistoReport_run%2004-01-2017(Oct17).xlsx"))
Mydata10<-RenameCols(Mydata10)


FinalData<-rbind(Mydata,Mydata2)
FinalData<-rbind(FinalData,Mydata3)
FinalData<-rbind(FinalData,Mydata4)
FinalData<-rbind(FinalData,Mydata5)
FinalData<-rbind(FinalData,Mydata6)
FinalData<-rbind(FinalData,Mydata7)
FinalData<-rbind(FinalData,Mydata8)
FinalData<-rbind(FinalData,Mydata9)
FinalData<-rbind(FinalData,Mydata10)
FinalData<-rbind(FinalData,Mydata11)



Mydata<-FinalData

Mydata$Endo_ResultEntered<-as.Date(Mydata$Endo_ResultEntered,origin="1899-12-30")
Mydata$Endo_ResultPerformed<-as.Date(Mydata$Endo_ResultPerformed,origin="1899-12-30")
Mydata$Histo_ResultPerformed<-as.Date(Mydata$Histo_ResultPerformed,origin="1899-12-30")
Mydata$Histo_ResultEntered<-as.Date(Mydata$Histo_ResultEntered,origin="1899-12-30")

HistolTree<-c("NATURE OF SPECIMEN:","CLINICAL DETAILS","MACROSCOPICAL DESCRIPTION","HISTOLOGY",
                 "DIAGNOSIS","")
for(i in 1:(length(HistolTree)-1)) {
  Mydata<-Extractor(Mydata,"Histo_ResultText",as.character(HistolTree[i]),
                    as.character(HistolTree[i+1]),as.character(HistolTree[i]))
}


Mydata$Endo_ResultText<-gsub("2nd Endoscopist","Second Endoscopist",Mydata$Endo_ResultText)
EndoTree<-list("Patient Name:","Date of Birth:","Hospital Number:","Date of Procedure:","Endoscopist:","Second Endoscopist:",
               "Referring Physician:","General Practicioner:","Nurses:",
               "Medications:","Instrument:","Extent of Exam:","Visualization:","Tolerance:","Complications:","Co-morbidity:",
               "INDICATIONS FOR EXAMINATION","PROCEDURE PERFORMED",
               "FINDINGS","ENDOSCOPIC DIAGNOSIS","RECOMMENDATIONS","COMMENTS","BIOPSIES","OPCS4 Code:","Signature:","")
for(i in 1:(length(EndoTree)-1)) {
  Mydata<-Extractor(Mydata,"Endo_ResultText",as.character(EndoTree[i]),
                    as.character(EndoTree[i+1]),as.character(EndoTree[i]))
}


Mydata<-EndoscEndoscopist(Mydata,'Endoscopist')
Mydata$Endoscopist<-gsub("Second","",Mydata$Endoscopist)
Mydata<-EndoscMeds(Mydata,'Medications')
Mydata<-EndoscInstrument(Mydata,'Instrument')
Mydata<-EndoscIndications(Mydata,'INDICATIONSFOREXAMINATION')
Mydata<-EndoscProcPerformed(Mydata,'PROCEDUREPERFORMED')


Mydata<-HistolHistol(Mydata,'HISTOLOGY')
Mydata<-HistolDx(Mydata,"DIAGNOSIS")
Mydata<-HistolExtrapolDx(Mydata,"DIAGNOSIS")



######################################################????????????????Data forking (filtering and subsetting)######################################### 
#To do: Decide on retrospective cohort question types that often get asked as generic questions then code that up

#Fork into procedure type first

#####****Upper GI diagnoses #####

############                Adenomas         ############
UGIdataframe <- Mydata[grepl("Gastroscopy", Mydata$PROCEDUREPERFORMED),]
MyUpperAdenomaDataFrame <- UGIdataframe[grepl(".*[Aa]denom.*", UGIdataframe$Dx_Simplified),]

############                Eosinophil         ############
EoEDx<-UGIdataframe[grepl("osinop",UGIdataframe$Dx_Simplified),]

EoEDx$HPF<-as.numeric(sapply(str_extract_all(EoEDx$Histo_ResultText, "[0-9]+(?=(\\s+[Ii]ntraepithelial)?(\\s+[Ee]osinophils)?\\s+(per|in one|in a|\\/)?.+([Hh][Pp][Ff]|[Hh]igh.+power.+field|[Hh]ighpower\\s+field))"), 
                             function(x) x[which.max(as.numeric(x))][1]))

#These are all the endoscopies with high hpf
PatientsWithEoE<-subset(EoEDx,EoEDx$HPF>=15)


############                Barretts         ############
#Will need to do negative removal here....
MyBarrettsData<-UGIdataframe[grepl("Barr",UGIdataframe$FINDINGS),]

############                small cell         ############
MySmallCell<-UGIdataframe[grepl("mall [Cc]ell",UGIdataframe$Dx_Simplified),]

#####***Lower GI diagnoses #####


###### Adenomas #####
LGIdataframe <- Mydata[grepl("Colonoscopy", Mydata$PROCEDUREPERFORMED),]

MyLowerAdenomaDataFrame <- dataframe[grep(".*[Aa]denom.*", LGIdataframe$Dx),]
#Remove the previous polyp rows
MyLowerAdenomaDataFrame<-MyLowerAdenomaDataFrame[!grepl("Surveillance- Previous Polyps",MyUpperAdenomaDataFrame$INDICATIONSFOREXAMINATION),]



#######################################????????????????Data accordionisation ############################################################

############                Barretts         ############
em<-SurveillanceCapacity(MyBarrettsData,"Endo_ResultPerformed")
how<-HowManyTests(MyBarrettsData,'INDICATIONSFOREXAMINATION','Endo_ResultPerformed','Surv')
b1<-BarrettsDataAccord_Prague(MyBarrettsData,'FINDINGS')
b2<-BarrettsDataAccord_PathStage(b1,'HISTOLOGY')
b3<-BarrettsDataAccord_Event(b2,'HISTOLOGY','PROCEDUREPERFORMED','Endo_ResultText','FINDINGS')
b4<-BarrettsDataAccord_FUGroup(b3,'FINDINGS')
Rule<-BarrettsPatientTracking_UniqueHospNum(b4,'Rule1','HospitalNumber')




#######################################????????????????Data analyses ############################################################


############                Barretts         ############
BarrettsQuality_AnalysisDocumentation(b4,"FINDINGS")
BarrettsSurveillance_PathDetection(b4,'Myplot')



#######################################????????????????Demographics for subset ################ 
#M:F ratio (extract from the title in the Patient name column
Mydata$Gender<-ifelse(grepl("MRS|MS|MISS", Mydata$PatientName), "Female","Male")

genderStats<-Mydata%>%group_by(Gender)%>%summarise(n=n())

MFRatio<-genderStats[2,2]/genderStats[1,2]
#Age and range of age (age at diagnosis)
Mydata$DiagnosisYear<-lubridate::year(Mydata$Endo_ResultEntered)
Mydata$AgeAtDiagnosis<-Mydata$DiagnosisYear-Mydata$birthyearnum
summ<-describe(Mydata$AgeAtDiagnosis) 

summ$median
summ$mean
summ$range
summ$max
summ$min
summ$sd
summ$n
BarrettsTherapeuticsRFA_ByCatheter(Therapeutics,"FINDINGS","ENDOSCOPICDIAGNOSIS")

#################### Measurement of an auditable parameter in a subset 
#"Time to endoscopy for acute upper gastrointestinal bleeding: results from a prospective pan-midlands trainee-LED audit"

#################### Description of outcomes - Descriptive only.
#"Outcomes of 360 HALO express radio-frequency ablation for barrett’s oesophagus related neoplasia ".
#"Medium-term outcome of endoscopic sphincterotomy in biliary manometry confirmed sphincter of oddi dysfuncton type 2".
#"Outcomes from mesenteric angiography and embolisation in non-variceal upper gi bleeding; a single centre experience".
#"International multicentre study of mallory weiss tear related gi bleeding: demographics, endoscopic therapy and outcome".
#"Endoscopic management of early neoplasia in barrett’s oesophagus – an outcome analysis from north-east england".
#"Endoscopic resection of early oesophageal adenocarcinoma with submucosal invasion: outcomes from a single centre".

#Steps: 
#a) Determine categories of outcome.
    #Categorize outcome as a separate column based on existing column.
#b) Do descriptive statistics .
#c) Develop theory of which exposure might be causing outcome- can then do multivariate analysis on it.
#d) Perform case-control study assessing the odds ratio of exposure causing outcome by using a contingency table (using function table()).
#e) Do RR or AR if cohort study / do OR if case-control study.

################Test accuracy measurements
#"The accuracy and tolerability of magnet assisted capsule endoscopy for the investigation of oesophageal pathology"
#"Histopathologist features predictive of diagnostic concordance amongst an international sample of pathologists diagnosing barrett’s dysplasia"

#Steps in order to 


################ Therapy comparison ################################################################



###################################Predictor studies 
#"Predictors of mortality and rebleeding outcomes after peptic ULCER bleeding "
#Is serology predictive of persisting villous atrophy in patients with established coeliac disease (CD)?"
#"giant gastric ulcers: malignancy yield and predictors from a 10-year retrospective single centre cohort"
#"the linx reflux management system: predictive value of pre-operative oesophageal physiology testing on post-operative outcomes"
#"Referral pathway and age influence the likelihood of biopsy to exclude eosinophilic oesophagitis in dysphagia"


##################Cost efficiency studies
#"Biopsy avoidance strategy in adult coeliac disease"

##################Variation in practice 
#"Variation in the investigation and diagnosis of eosinophilic oesophagitisin daily clinical practice"
#"INTERNATIONAL ASSESSMENT OF OUTCOME OF UPPER GI HAEMORRHAGE AT WEEKENDS"
#"NATIONAL SURVEY OF PRACTICE OF FAECAL MICROBIOTA TRANSPLANTATION FOR CLOSTRIDIUM DIFFICILE INFECTION IN THE UNITED KINGDOM"



# Clinical question                                                                                         Suggested best study design
# 
# Harm/Etiology                   'how to identify causes for disease (including iatrogenic forms)'       RCT > cohort > case control > case series Stats:
# 
# Diagnosis                       'how to select and interpret diagnostic tests'                           prospective, blind comparison to a gold standard
# 
# Therapy                         'how to select treatments that do more good than harm and that are worth the efforts and costs of using them:             RCT > cohort > case control > case series
# 
# Prevention                                                                                                RCT > cohort study > case control > case series
# 
# Prognosis                       'how to estimate the patient’s likely clinical course over time (based on factors other than the intervention) and anticipate likely complications of disease'         cohort study > case control > case series
# 
# Cost Benefit                                                                                               economic analysis
# Clinical Exam                                                                                           prospective, blind comparison to gold standard


#Case-control- better for rare disease and when exposure to disease is prolonged
#<-MALODD is retrospective to see what the risk was for getting the illness vs those who dont
#Outcome:
#Example question:

#SMORRT-> Cohort is propective and follows over time-Cohort Study, you are studying the risk factor and see if you can associate a disease to it. Therefore, you use Relative Risk and Attributable Risk.
#Outcome:
#Examplequestion: 'Determine the long-term effectiveness of influenza vaccines in elderly people'
#Am I using a retrospective cohort or a case control?Case-Control Study, you are studying the disease and see if you can associate risk factors to it. Therefore, you use Odds Ratio. Case-Control Studies are typically done for rare diseases; you compare the small number of rare disease with a control group and find out if there was a risk factor that might have caused their disease. Commonly case control studies are retrospective.
#######################################????????????????Data presentation ############################################################

###Function to see numbers per year
library(rlang)
HowMany <-
  function(dataframe,
           Endo_ResultPerformed) {
    
    Endo_ResultPerformeda <- sym(Endo_ResultPerformed)

    TestNumbers <-dataframe%>%arrange(as.Date(!!Endo_ResultPerformeda)) %>% group_by(
      year = year(as.Date(!!Endo_ResultPerformeda))
    ) %>% summarise(Number = n())
    TestNumbers2<-data.frame(TestNumbers)
    
    
    Myplot <-
      ggplot(data = TestNumbers2, aes(x = year, y = Number)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "loess") +
      theme_bw() +
      labs(title="Number of adenomas per year")
    functionResults <-
      list(Myplot = Myplot, TestNumbers = TestNumbers)
    return(functionResults)
    
  }

########## Upper GI
UGIAdenomaHowMany<-HowMany(MyUpperAdenomaDataFrame,"Endo_ResultPerformed")
EoEDxHowMany<-HowMany(EoEDx,"Endo_ResultPerformed")
MyBarrettsDataHowMany<-HowMany(MyBarrettsData,"Endo_ResultPerformed")
MySmallCellHowMany<-HowMany(MySmallCell,"Endo_ResultPerformed")

########## Lower GI


############                Adenomas         ############

# Get the number of adenomas diagnosed by year
TestNumbers <-MyAdenomaDataFrame%>%arrange(as.Date(Endo_ResultPerformed)) %>% group_by(
  year = year(as.Date(Endo_ResultPerformed))
) %>% summarise(Number = n())

TestNumbers2<-data.frame(TestNumbers)


Myplot <-
  ggplot(data = TestNumbers2, aes(x = year, y = Number)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  labs(title="Number diagnosed per year")


############                Eosinophil         ############



############                Barretts         ############

















