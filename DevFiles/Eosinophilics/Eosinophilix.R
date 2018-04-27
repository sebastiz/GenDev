library(stringr)
library(ggplot2)
library(lattice)
library(compare)
library(XLConnect)
library(grid)
library (reshape2)
library(plyr)
library(gtools)
library(gplots)
library(gdata)
library(dplyr)
library(GGally)
library(knitr)
library(xtable)
library(pander)
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


#some stuff tbbd


#Get all the EoE's from the raw dataset:

# EoEc = loadWorkbook("")
# EoEc = readWorksheet(EoEc, sheet="All_GastroscopySince2009",header=TRUE)
# 
# source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\CleanUp.R")
# EoEc<-EndoscChopper(EoEc)
# EoEc<-HistolChopper(EoEc)

library(readxl)
list.files(path="S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\EndoscopyFrom2007ToApr1_2017",pattern='*.xlsx')
file.list <- list.files(pattern='*.xlsx')
df.list <- lapply(file.list, read_excel)

############################################  DATA IMPORT #############################################  #############################################  
#Selected from patient who have oesinophilic mentioned in their biopsies
#EoEw = loadWorkbook("/Users/sebastianzeki/Dropbox/Work/Medical/Clinical/Gastro/Data_Infrastruc/Eosinophil/EOFinalDataSetFromHajirLookup.xls")
EoEw = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\EoE\\EOE_master.xls")
EoE = readWorksheet(EoEw, sheet="EoE",header=TRUE)


EoE$Endo_ResultEntered<-as.Date(EoE$Endo_ResultEntered,origin="1899-12-30")
EoE$Endo_ResultPerformed<-as.Date(EoE$Endo_ResultPerformed,origin="1899-12-30")
EoE$Histo_ResultPerformed<-as.Date(EoE$Histo_ResultPerformed,origin="1899-12-30")
EoE$Histo_ResultEntered<-as.Date(EoE$Histo_ResultEntered,origin="1899-12-30")

#############################################  ############### Kristina dataset ###########  #############################################  

#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
############################################# ////////////////////////////////////////////////////////////////#######################################  #############################################  
#############################################  DATA CLEANING- only if not using Hajir's dataset ###############################  #############################################  #############################################  
#############################################////////////////////////////////////////////////////////////////#############################  #############################################  
#############################################  #############################################  #############################################  #############################################
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  
#############################################  #############################################  #############################################  #############################################  



#Get rid of 'no evidence of'
EoEDx<-EoE[grep("osinop",EoE$Dx),]
EoEDx$Dx<-gsub("\n","-",EoEDx$Dx,fixed=TRUE)
EoEDx<-EoEDx[!grepl(".*[Nn]o\\s*evidence\\s*of\\s*[Ee]osinop.*",EoEDx$Dx),]
EoEDx<-EoEDx[!grepl(".*histological evidence\\s+of\\s+.*[Ee]osinop.*",EoEDx$Dx),]


#Now apply diagnostic criteria
#Try to extract values for number of eosinophils by selecting the line with high and the number
#If more than one value then take the highest value
library(zoo)
EoEDx$HPF<-as.numeric(sapply(str_extract_all(EoEDx$Histol, "[0-9]+(?=(\\s+[Ii]ntraepithelial)?(\\s+[Ee]osinophils)?\\s+(per|in one|in a|\\/)?.+([Hh][Pp][Ff]|[Hh]igh.+power.+field|[Hh]ighpower\\s+field))"), 
                             function(x) x[which.max(as.numeric(x))][1]))


#These are all the endoscopies with high hpf
PatientsWithEoE<-subset(EoEDx,EoEDx$HPF>=15)


##########################################################################################################################################################################
##########################################################################################################################################################################
##################################################################////////////////////////////////////////////////////////////////##########################################
####################################################################### Skip straight to here if using Hajir's premade dataset ################################################################################
##################################################################////////////////////////////////////////////////////////////////########################################
##########################################################################################################################################################################




PatientsWithEoE<-subset(EoE,EoE$HPF>=15)
#For diagrammR
PatientsWithEoEd<-PatientsWithEoE
#To get all the patients first endoscopy with high hpf (to avoid the SFED endoscopies)


PatientsWithEoE<-PatientsWithEoE %>%
  group_by(PatientID) %>%
  slice(which.min(Histo_ResultEntered))


#Further filter to only look at those with food bolus or dysphagia
PatientsWithEoE<-PatientsWithEoE[grep("[Dd]ysphagia|[Oo]dynophagia|[Ff]ood [Bb]olus",PatientsWithEoE$IndicationsFroExamination),]
#For diagrammR
PatientsWithEoEdy<-PatientsWithEoE




############################################################ Endoscopic ############################################################ 




#Trying to make a word cloud with the endoscopic text:




library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


jeopCorpus <- Corpus(VectorSource(PatientsWithEoEdy$Findings))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
jeopCorpus <- tm_map(jeopCorpus, removeWords, c("oesophagus", "diagnosis","taken","oesophag")) 
jeopCorpus <- tm_map(jeopCorpus, stemDocument)
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)


#Get the frequency table of terms being used
dtm <- TermDocumentMatrix(jeopCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


newdata3 <- d[order(d$word) , ]


#Number of unique patients
length(unique(PatientsWithEoEdy$PatientID))
#Number with more than one endoscopy- although given the selection criteria from the original dataset this is less meaningful
nrow(subset(PatientsWithEoEdy,duplicated(PatientsWithEoEdy$PatientID)==T))
#Frequency of endoscopies for those who have had more than one- need to get from the original dataaset
#Find number of patients with specific words mentioned (from the ACG grading classification)
PatientsWithEoEdy$Findings<-gsub("([a-z])([A-Z]*):","\\1\\.\\2",PatientsWithEoEdy$Findings)
PatientsWithEoEdy$Findings<-gsub("\\.","\\.\n",PatientsWithEoEdy$Findings)




#Difference between endoscopic findings and hpfs
Trachealization <- PatientsWithEoEdy[grepl(".*racheal.*|.*[Rr]idg.*", PatientsWithEoEdy$Findings), ]
TrachealizationHPF<-mean(Trachealization$HPF)
NoTrachealization <- PatientsWithEoEdy[!grepl(".*racheal.*", PatientsWithEoEdy$Findings), ]
NoTrachealizationHPF<-mean(NoTrachealization$HPF)


Furrows<- PatientsWithEoEdy[grepl(".*urrow.*", PatientsWithEoEdy$Findings), ]
FurrowsHPF<-mean(Furrows$HPF)
NoFurrows<- PatientsWithEoEdy[!grepl(".*urrow.*", PatientsWithEoEdy$Findings), ]
NoFurrowsHPF<-mean(NoFurrows$HPF)


# Rings<- PatientsWithEoE[grepl(".*[Rr]ing.*", PatientsWithEoE$Findings), ]
# RingsHPF<-mean(Rings$HPF)
# NoRings<- PatientsWithEoE[!grepl(".*[Rr]ing.*", PatientsWithEoE$Findings), ]
# NoRingsHPF<-mean(NoRings$HPF)


# Linear<- PatientsWithEoE[grepl(".*[Ll]inear.*", PatientsWithEoE$Findings), ]
# LinearHPF<-mean(Linear$HPF)
# NoLinear<- PatientsWithEoE[!grepl(".*[Ll]inear.*", PatientsWithEoE$Findings), ]
# NoLinearHPF<-mean(NoLinear$HPF)




# Ridged<- PatientsWithEoE[grepl(".*[Rr]idg.*", PatientsWithEoE$Findings), ]
# RidgedHPF<-mean(Ridged$HPF)
# NoRidged<- PatientsWithEoE[!grepl(".*[Rr]idg.*", PatientsWithEoE$Findings), ]
# NoRidgedHPF<-mean(NoRidged$HPF)


Narrow<- PatientsWithEoEdy[grepl(".*[Nn]arrow.*|[Pp]ipe", PatientsWithEoEdy$Findings), ]
NarrowHPF<-mean(Narrow$HPF)
NoNarrow<- PatientsWithEoEdy[!grepl(".*[Nn]arrow.*", PatientsWithEoEdy$Findings), ]
NoNarrowHPF<-mean(NoNarrow$HPF)


Plaques<- PatientsWithEoEdy[grepl(".*[Pp]laqu.*|.*[Ww]hit.*", PatientsWithEoEdy$Findings), ]
PlaquesHPF<-mean(Plaques$HPF)
NoPlaques<- PatientsWithEoEdy[!grepl(".*[Pp]laqu.*", PatientsWithEoEdy$Findings), ]
NoPlaquesHPF<-mean(NoPlaques$HPF)


Stricture<-PatientsWithEoEdy[grepl(".*[Ss]trict.*|.*[Rr]ing.*", PatientsWithEoEdy$Findings), ]
StrictureHPF<-mean(Stricture$HPF)
NoStricture<-PatientsWithEoEdy[!grepl(".*[Ss]trict.*", PatientsWithEoEdy$Findings), ]
NoStrictureHPF<-mean(NoStricture$HPF)


Exudate<-PatientsWithEoEdy[grepl(".*[Ee]xud.*", PatientsWithEoEdy$Findings), ]
ExudateHPF<-mean(Exudate$HPF)
NoExudate<-PatientsWithEoEdy[!grepl(".*[Ee]xud.*", PatientsWithEoEdy$Findings), ]
NoExudateHPF<-mean(NoExudate$HPF)


Oedema<-PatientsWithEoEdy[grepl(".*edema.*", PatientsWithEoEdy$Findings), ]
OedemaHPF<-mean(Oedema$HPF)
NoOedema<-PatientsWithEoEdy[!grepl(".*edema.*", PatientsWithEoEdy$Findings), ]
NoOedemaHPF<-mean(NoOedema$HPF)


#Get the normal from the diagnosis column but check this from the Findings column as sometimes what is called normal is not
Normal<-PatientsWithEoEdy[grepl("[Nn]ormal",PatientsWithEoE$EndoscopDiagn),]
Normal<-Normal[!grepl("racheal|edema|[Ee]xud|[Ss]trict|[Pp]laqu|[Ww]hit|[Nn]arrow|[Rr]idg|urrow|[Ll]inear|[Rr]ing", Normal$Findings), ]


Tots<-nrow(PatientsWithEoEdy)


n = c((nrow(Trachealization)/Tots)*100, (nrow(Furrows)/Tots)*100,
      (nrow(Narrow)/Tots)*100,
      (nrow(Plaques)/Tots)*100,(nrow(Stricture)/Tots)*100,(nrow(Exudate)/Tots)*100,
      (nrow(Oedema)/Tots)*100, (nrow(Normal)/Tots)*100)
s = c("Trachealization","Furrows","Narrow",
      "Plaques","Stricture","Exudate","Oedema","Normal")
Findings<-data.frame(s,n)


barplot(Findings$n,names.arg=c("Trachealization", "Furrows","Narrow",
                               "Plaques","Stricture","Exudate","Oedema","Normal"),
        las=3, ylab = "Percentage of endoscopies where feature described (%)",
        cex.lab = 2.0,cex.axis=1.5,cex.main = 1.5,cex.names=1.5,main = "EoE Findings Endoscopic")


############################ `other terms mis recognised ############################ `
Schatzki<-PatientsWithEoEdy[grepl(".*[Ss]cha.*|[Ss]hat", PatientsWithEoEdy$Findings), ]
Candida<-PatientsWithEoEdy[grepl(".*[Cc]andi", PatientsWithEoEdy$Findings), ]
Debris<-PatientsWithEoEdy[grepl(".*[Dd]ebris.*", PatientsWithEoEdy$Findings), ]
Oesophagitis<-PatientsWithEoEdy[grepl(".*[Oo]esophagit.*", PatientsWithEoEdy$Findings), ]
#Remove any oesophagitis that has eosinoph in it
Oesophagitis<-Oesophagitis[-grepl(".*[Ee]osinop.*", Oesophagitis$Findings), ]
Tots<-nrow(PatientsWithEoEdy)


n = c((nrow(Schatzki)/Tots)*100, (nrow(Candida)/Tots)*100,
      (nrow(Debris)/Tots)*100,(nrow(Oesophagitis)/Tots)*100)
s = c("Schatzki","Candida","Debris","Oesophagitis")
Findings2<-data.frame(s,n)


barplot(Findings2$n,names.arg=c("Schatzki","Candida","Debris","Oesophagitis"),
        las=3, ylab = "Other described features",main="Other described endoscopic features in EoE",
        cex.lab = 2.0,cex.axis=1.5,cex.main = 1.5,cex.names=1.5)


############################################################ ############################################################ ############################################################ 
############################################################ ############################################################ ############################################################ 
############################################################ ############################################################ ############################################################ 
############################################################ ############################################################ ############################################################ 
############################################################ Clinical ################################################### ############################################################ 
############################################################ ############################################################ ############################################################ 
############################################################ ############################################################ ############################################################ 
############################################################ ############################################################ ############################################################ 
############################################################ ############################################################ ############################################################ 


####################################### Age range of 'Normal endoscopy' patients ####################################### 


Normal$birthyearnum<-as.character(Normal$birthyearnum)
PatientsWithEoE$birthyearnum<-as.character(PatientsWithEoE$birthyearnum)
#PatientsWithEoE$Endo_ResultPerformed<-as.Date(PatientsWithEoE$Endo_ResultPerformed)
#PatientsWithEoE$Histo_ResultEntered<-as.Date(PatientsWithEoE$Histo_ResultEntered)
Normal$Ages<-ifelse(!is.na(Normal$Endo_ResultPerformed),round((Normal$Endo_ResultPerformed-as.Date(Normal$birthyearnum, "%Y"))/365,0),round((PatientsWithEoE$Histo_ResultEntered-as.Date(PatientsWithEoE$birthyearnum, "%Y"))/365,0))


Normal$Ages<-as.numeric(Normal$Ages)


barplot(table(Normal$Ages))
br = seq(0,100,by=5)
barplot(table(cut(Normal$Ages, br)))


####################################### Age range of all patients ####################################### 
PatientsWithEoE$birthyearnum<-as.character(PatientsWithEoE$birthyearnum)
#PatientsWithEoE$Endo_ResultPerformed<-as.Date(PatientsWithEoE$Endo_ResultPerformed)
#PatientsWithEoE$Histo_ResultEntered<-as.Date(PatientsWithEoE$Histo_ResultEntered)
PatientsWithEoE$Ages<-ifelse(!is.na(PatientsWithEoE$Endo_ResultPerformed),round((PatientsWithEoE$Endo_ResultPerformed-as.Date(PatientsWithEoE$birthyearnum, "%Y"))/365,0),round((PatientsWithEoE$Histo_ResultEntered-as.Date(PatientsWithEoE$birthyearnum, "%Y"))/365,0))


PatientsWithEoE$Ages<-as.numeric(PatientsWithEoE$Ages)
meanEoEAge<-mean(PatientsWithEoE$Ages)



barplot(table(PatientsWithEoE$Ages))
br = seq(0,100,by=5)
barplot(table(cut(PatientsWithEoE$Ages, br)))


####################################### Reasons For Endoscopy of patients ####################################### 
####################################### ####################################### 
####################################### 
PatientsWithEoE$IndicationsFroExamination<-gsub("PROCEDURE PERFORMED","",PatientsWithEoE$IndicationsFroExamination)
table(PatientsWithEoE$IndicationsFroExamination)


#Word frequency table
PatientsWithEoE$IndicationsFroExamination<-gsub("\\.","\n",PatientsWithEoE$IndicationsFroExamination)
PatientsWithEoE$IndicationsFroExamination<-gsub("/","\n",PatientsWithEoE$IndicationsFroExamination)
jeopCorpusIndic <- Corpus(VectorSource(PatientsWithEoE$IndicationsFroExamination))
jeopCorpusIndic <- tm_map(jeopCorpusIndic, PlainTextDocument)
jeopCorpusIndic <- tm_map(jeopCorpusIndic, removePunctuation)
jeopCorpusIndic <- tm_map(jeopCorpusIndic, stripWhitespace)
jeopCorpusIndic <- tm_map(jeopCorpusIndic, removeWords, stopwords('english'))
jeopCorpusIndic <- tm_map(jeopCorpusIndic, removeWords, c("oesophagus", "diagnosis","taken","oesophag")) 
jeopCorpusIndic <- tm_map(jeopCorpusIndic, stemDocument)
wordcloud(jeopCorpusIndic, max.words = 100, random.order = FALSE)


#Get the frequency table of terms being used
dtm <- TermDocumentMatrix(jeopCorpusIndic)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


newdata3 <- d[order(d$word) , ]


table(PatientsWithEoE$IndicationsFroExamination)




s<-strsplit(as.character(PatientsWithEoE$IndicationsFroExamination), '\n')
s1<-data.frame(director=unlist(s))
#Clean it up 
s1$director<-gsub(".*EOE.*","Eosinophilic oesophagitis",s1$director)
s1$director<-gsub("food","Food",s1$director)
s1$director<-gsub(".*[Bb]olus","Food bolus",s1$director)
s1$director<-gsub(".*[fF]ood [Bb]olus.*","Food bolus",s1$director)
s1$director<-gsub("FB.*","Food bolus",s1$director)
s1$director<-gsub(".*[Dd]ysphagia.*","Dysphagia",s1$director)
s1$director<-gsub(".*[Rr]eflux.*","Reflux",s1$director)
s1$director<-gsub(".*[Dd]yspepsia.*","Dyspepsia",s1$director)
s1$director<-gsub(".*[Bb]x.*|[Bb]iopsies","Dyspepsia",s1$director)
s1$director<-gsub(".*[Rr]esponse.*","Eosinophilic oesophagitis",s1$director)
s1$director<-gsub(".*sinoph.*","Eosinophilic oesophagitis",s1$director)


par(mar=c(20.1,10.1,14.1,5.1))


newdata3 <- d[order(d$word) , ]

barplot(head(sort(table(s1),decreasing=T),5),
        las=3,
        cex.lab = 0.5,cex.axis=0.5,cex.main = 1.5,cex.names=0.9,
        space=1,main = "Indications for Endoscopy of 
        \nEosinophilic Oesophagitis Patients",angle = 45)

#######################################  #######################################  ####################################### 
####################################### Histopathology  ####################################### ####################################### 
#######################################  #######################################  ####################################### 
#######################################  #######################################  ####################################### 
#######################################  #######################################  ####################################### 


#HPF vs Age


qplot(PatientsWithEoE$HPF,binwidth = 25)+
  labs(title="Eosinophil hpf spread in EoE", x="Max Eosinophils/hpf", y="Freq") +
  theme(axis.text=element_text(size=25)) +
  theme(axis.title=element_text(size=30))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=18,lineheight=.8, face="bold"))


ggplot(PatientsWithEoE) + 
  geom_point(aes(Ages, PatientsWithEoE$HPF, color = "red"),size=10)+
  labs(title="Age vs hpf", x="Age", y="Max Eosinophils/hpf") +
  theme(axis.text=element_text(size=25)) +
  theme(axis.title=element_text(size=30))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=18,lineheight=.8, face="bold"))








PatientsWithEoE$MacDescrip2<-gsub("[Oo]ne","1",PatientsWithEoE$MacDescrip)
PatientsWithEoE$MacDescrip2<-gsub("[Ss]ingle","1",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Tt]wo","2",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Tt]hree","3",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Ff]our","4",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Ff]ive","5",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Ss]ix","6",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Ss]even","7",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Ee]ight","8",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Nn]ine","9",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$MacDescrip2<-gsub("[Tt]en","10",PatientsWithEoE$MacDescrip2)
PatientsWithEoE$NumbOfBx<-str_extract_all(PatientsWithEoE$MacDescrip2, "([0-9]) piece")
PatientsWithEoE$NumbOfBx<-sapply(PatientsWithEoE$NumbOfBx, function(x) sum(as.numeric(unlist(str_extract_all(x, "^\\d+")))))

#Get the number of biopsies taken
summary(as.numeric(PatientsWithEoE$NumbOfBx))

qplot(PatientsWithEoE$NumbOfBx)+
  labs(title="Number of oesophageal biopsies for patients with EoE ", x="Number of biopsies", y="Freq") +
  theme(axis.text=element_text(size=25)) +
  theme(axis.title=element_text(size=30))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=18,lineheight=.8, face="bold"))


############################################################################################################################################
############################################################################################################################################
#########################################Description of the histology findings############################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################


jeopCorpusIndic <- Corpus(VectorSource(PatientsWithEoE$Histol))
jeopCorpusIndic <- tm_map(jeopCorpusIndic, PlainTextDocument)
jeopCorpusIndic <- tm_map(jeopCorpusIndic, removePunctuation)
jeopCorpusIndic <- tm_map(jeopCorpusIndic, stripWhitespace)
jeopCorpusIndic <- tm_map(jeopCorpusIndic, removeWords, stopwords('english'))
jeopCorpusIndic <- tm_map(jeopCorpusIndic, removeWords, c("eosinophil", "diagnosis","taken","oesophag")) 
jeopCorpusIndic <- tm_map(jeopCorpusIndic, stemDocument)
wordcloud(jeopCorpusIndic, max.words = 100, random.order = FALSE)
#Get the frequency table of terms being used
dtm <- TermDocumentMatrix(jeopCorpusIndic)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


newdata4 <- d[order(d$freq) , ]


#Perhaps look at the follosing as important for EoE:
#eosinophil abscesses, basal layer hyperplasia, and lamina propria fibrosis 


#See how the number of biopsies impacts on the number at hpf. More biopsies = a higher total?


#To do
#?Predictors of eosinophil count in those with known EoE
#Need diagrammR consort diagram
#Predictive analytics on predicting EoE with patient's Hajir has collected
#Decide on outstanding questions that need to be answered:
####### How to decide who to biopsy ################################################################################### # 
#Predictors of response




########################### Import the control dataset (dysphagics without EoE) ########################### ########################### ########################### ########################### 
########################### ########################### ########################### ########################### ########################### ########################### 

EoEc = loadWorkbook("~\\All_GastroscopySince2009.xls")
EoEc = readWorksheet(EoEc, sheet="All_GastroscopySince2009",header=TRUE)
#EoEc = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\EoE\\Pape_or_Abstracts\\EOEControls .xls")
#EoEc = readWorksheet(EoEc, sheet="blabla",header=TRUE)


EoEc$Endo_ResultEntered<-as.Date(EoEc$Endo_ResultEntered,origin="1899-12-30")
EoEc$Endo_ResultPerformed<-as.Date(EoEc$Endo_ResultPerformed,origin="1899-12-30")
EoEc$Histo_ResultPerformed<-as.Date(EoEc$Histo_ResultPerformed,origin="1899-12-30")
EoEc$Histo_ResultEntered<-as.Date(EoEc$Histo_ResultEntered,origin="1899-12-30")

library(lubridate)
EoEc1<-EndoscChopper(EoEc)
library(anytime)
EoEc2<-HistolChopper(EoEc1)


#Extract all the dysphagics:
EoEc<-EoEc2[grep("[Dd]ysphagia|[Oo]dynophagia|[Ff]ood [Bb]olus",EoEc2$IndicationsFroExamination),]
EoEc$birthyearnum<-as.character(EoEc$birthyearnum)
EoEc$Endo_ResultPerformed<-as.Date(EoEc$Endo_ResultPerformed)
EoEc$Ages<-ifelse(!is.na(EoEc$Endo_ResultPerformed),round((EoEc$Endo_ResultPerformed-as.Date(EoEc$birthyearnum, "%Y"))/365,0),round((EoEc$Histo_ResultEntered-as.Date(EoEc$birthyearnum, "%Y"))))


EoEc$type<-"Control"


EoEc<-EoEc[!is.na(EoEc$Histol),]
#Get rid of controls where the biopsies are probably duodenal
EoEc<-EoEc[!grepl(".*[Dd]uoden|.*[Ss]tomac",EoEc$Dx),]
writeWorksheetToFile("~\\MYTBB.xls",data=EoEc,sheet="blabla",startRow=1,startCol=1)


####################Start from here if you have preloaded the control dataset############################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################


EoEControl = loadWorkbook("S:\\Gastroenterology\\Seb\\R\\Data\\EoE\\Pape_or_Abstracts\\EOEControls .xls")
EoEc = readWorksheet(EoEControl, sheet="blabla",header=TRUE)
#Get the number of biopsies taken:
EoEc$MacDescrip2<-gsub("[Oo]ne","1",EoEc$MacDescrip)
EoEc$MacDescrip2<-gsub("[Ss]ingle","1",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Tt]wo","2",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Tt]hree","3",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Ff]our","4",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Ff]ive","5",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Ss]ix","6",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Ss]even","7",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Ee]ight","8",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Nn]ine","9",EoEc$MacDescrip2)
EoEc$MacDescrip2<-gsub("[Tt]en","10",EoEc$MacDescrip2)
EoEc$NumbOfBx<-str_extract_all(EoEc$MacDescrip2, "([0-9]) piece")
EoEc$NumbOfBx<-sapply(EoEc$NumbOfBx, function(x) sum(as.numeric(unlist(str_extract_all(x, "^\\d+")))))

#Get the number of biopsies taken
summary(as.numeric(EoEc$NumbOfBx))


PatientsWithEoE$type<-"Eosinophilic"
PatientsWithEoE<-as.data.frame(PatientsWithEoE)


#Bind the two datasets together
cols <- intersect(colnames(EoEc), colnames(PatientsWithEoE))
numEo<-nrow(PatientsWithEoE)
EoEc<-head(EoEc,numEo)


#############Sandboxing for just the normal EoE Endoscopy patients #############




EoEMaster<-rbind(EoEc[,cols], PatientsWithEoE[,cols])
EoEMaster$Sex<-ifelse(EoEMaster$Sex==1, "F","M")

#Random Sex column until I get the data straightened out
#EoEMaster$Sex<-sample(0:1,nrow(EoEMaster),replace=T)

#Add in the extracted endoscopic fields
#ggpairs(EoEMaster,columns=c("Ages","type","Sex","Normal","Abnorm"),colour=c("type"),diag=list(continuous="density", discrete="bar"), mapping=ggplot2::aes(fill=type,colour=type))



EoEMaster$Atopy...<-ifelse(EoEMaster$Atopy...==1, "Y","N")
EoEMaster$Asthma<-ifelse(EoEMaster$Asthma==1, "Y","N")
EoEMaster$allergic.rhinitis<-ifelse(EoEMaster$allergic.rhinitis==1, "Y","N")
EoEMaster$eczema.<-ifelse(EoEMaster$eczema.==1, "Y","N")
EoEMaster$Food.allergy.sensitisation.on.tsting.<-ifelse(EoEMaster$Food.allergy.sensitisation.on.tsting.==1, "Y","N")


EoEMaster$Trachealization  <- ifelse(grepl(".*racheal.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Furrows<- ifelse(grepl(".*urrow.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Rings<- ifelse(grepl(".*[Rr]ing.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Linear<- ifelse(grepl(".*[Ll]inear.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Ridged<- ifelse(grepl(".*[Rr]idg.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Narrow<- ifelse(grepl(".*[Nn]arrow.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$WhitePlaques<- ifelse(grepl(".*[Ww]hit.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Plaques<- ifelse(grepl(".*[Pp]laqu.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Stricture<-ifelse(grepl(".*[Ss]trict.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Exudate<-ifelse(grepl(".*[Ee]xud.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Oedema<-ifelse(grepl(".*edema.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Normal<-ifelse(!grepl(".*edema.*|.*[Oo]esophagitis.*|.*[Ee]xud.*|.*[Ss]trict.*|.*[Pp]laqu.*|.*[Ww]hit.*|.*[Tt]rache.*|.*[Nn]arrow.*|.*[Rr]idg.*|.*urrow.*|.*[Ll]inear.*|.*[Rr]ing.*", EoEMaster$Findings), "TRUE","FALSE")
EoEMaster$Abnorm<-ifelse(grepl("FALSE", EoEMaster$Normal), "TRUE","FALSE")


#Comparing the histologies
EoEMaster$Oedema<-ifelse(grepl(".*edema.*", EoEMaster$Histol), "TRUE","FALSE")
EoEMaster$Microabscesses<-ifelse(grepl("[Mm]icroabscesses|[Mm]icro-abscesses", EoEMaster$Histol), "TRUE","FALSE")
EoEMaster$Spongiosis<-ifelse(grepl(".*pongiosi.*", EoEMaster$Histol), "TRUE","FALSE")
EoEMaster$basalLayerHyperplasia<-ifelse(grepl(".*[Bb]asal.*[Hh]yperpl.*", EoEMaster$Histol), "TRUE","FALSE")
EoEMaster$Fibrosis<-ifelse(grepl(".*[Ff]ibro.*", EoEMaster$Histol), "TRUE","FALSE")
EoEMaster$Neutrophil<-ifelse(grepl(".*[Nn]eutrophil.*", EoEMaster$Histol), "TRUE","FALSE")
EoEMaster$Lymphocyte<-ifelse(grepl(".*[Ll]ymphocyte.*", EoEMaster$Histol), "TRUE","FALSE")


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
######################################## Sandboxing #########################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#Get the impedance data from the database

source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\AcquisitionFromDB.R")
ImpedanceData<-Impedance()
names(ImpedanceData)[names(ImpedanceData) == 'HospNum_Id'] <-"PatientID"

EoEvsImpedance<-merge(ImpedanceData,EoEMaster,by=c("PatientID"))
#This gives you those patients who had impedance with EoE
EoEMasterWithImpedanceOnly<-EoEvsImpedance[!is.na(EoEvsImpedance$type),]

#From Bravo records
BRAVOData<-BRAVO()
names(BRAVOData)[names(BRAVOData) == 'HospNum_Id'] <-"PatientID"

EoEvsBRAVO<-merge(BRAVOData,EoEMaster,by=c("PatientID"))
#This gives you those patients who had impedance with EoE
EoEMasterWithBRAVOOnly<-EoEvsImpedance[!is.na(EoEvsImpedance$type),]


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


EoEOedema<-table(EoEMaster$Oedema,EoEMaster$type)
EoEMicroabscesses<-table(EoEMaster$Microabscesses,EoEMaster$type)
EoESpongiosis<-table(EoEMaster$Spongiosis,EoEMaster$type)
EoEbasalLayerHyperplasia<-table(EoEMaster$basalLayerHyperplasia,EoEMaster$type)
EoEFibrosis<-table(EoEMaster$Fibrosis,EoEMaster$type)
EoENeutrophil<-table(EoEMaster$Neutrophil,EoEMaster$type)
EoELymphocyte<-table(EoEMaster$Lymphocyte,EoEMaster$type)
HistolTable<-cbind(EoEMicroabscesses,EoESpongiosis,EoEbasalLayerHyperplasia,EoEFibrosis,EoENeutrophil,EoELymphocyte)
print.xtable(cbind(EoEMicroabscesses,EoESpongiosis,EoEbasalLayerHyperplasia,EoEFibrosis,EoENeutrophil,EoELymphocyte),type="html",file="S:\\Gastroenterology\\Seb\\R\\Data\\EoE\\MyEoE.html")

EoEOedema<-chisq.test(EoEMaster$Oedema,EoEMasster$type)
EoEMicroabscesses<-chisq.test(EoEMaster$Microabscesses,EoEMaster$type)
EoESpongiosis<-chisq.test(EoEMaster$Spongiosis,EoEMaster$type)
EoEbasalLayerHyperplasia<-chisq.test(EoEMaster$basalLayerHyperplasia,EoEMaster$type)
EoEFibrosis<-chisq.test(EoEMaster$Fibrosis,EoEMaster$type)
EoENeutrophil<-chisq.test(EoEMaster$Neutrophil,EoEMaster$type)
EoELymphocyte<-chisq.test(EoEMaster$Lymphocyte,EoEMaster$type)
EoEMasterEoEDysphagics<-EoEMaster[EoEMaster$type=="Eosinophilic",]
EoEHistol<-data.frame(EoEOedema$p.value,EoEMicroabscesses$p.value,EoESpongiosis$p.value,EoEbasalLayerHyperplasia$p.value,EoEFibrosis$p.value,EoENeutrophil$p.value,EoELymphocyte$p.value)

EoEMasterForRattle<-data.frame(EoEMaster[c(3,30:49)])
#NoEoEHistol<-data.frame(oed,mic,spo,bas,fib,neu,lym)
#HistolNums<-rbind(EoEHistol,NoEoEHistol)


#Calculate ROC for age cut offs (Youden index for optimal age given sens and spec)
library(OptimalCutpoints)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "Ages", status = "type", tag.healthy = "Eosinophilic", methods = "Youden", data = EoEMasterForRattle, pop.prev = NULL,control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
summary(optimal.cutpoint.Youden)
plot(optimal.cutpoint.Youden)


writeWorksheetToFile("~\\EoEMasterSheet.xlsx",data=EoEMaster,sheet="blabla",startRow=1,startCol=1)



rmarkdown::render("S:\\Gastroenterology\\Seb\\R\\Scripts\\Eosinophilics\\EoEData.Rmd",output_file=paste("EoEReport",Sys.Date(),'.docx'))

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on EoEMasterForRattle [validate].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]$type)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red",size=2)
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve for Logistic regression analysis")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey",size=2)

print(p)

#To get the sens and spec and NPV etc assuming a cut off of 46 for males:

EoEMasterForRattle$ModelPos<-ifelse(EoEMasterForRattle$Ages<46&EoEMasterForRattle$Sex=="M","Yes","NO")
names(EoEMasterForRattle)[names(EoEMasterForRattle) == 'type'] <- 'Result'
names(EoEMasterForRattle)[names(EoEMasterForRattle) == 'ModelPos'] <- 'Test'
#So I can use the sens and spec function I already have:
EoEMasterForRattle$Result<-gsub("Control","Normal",EoEMasterForRattle$Result)
EoEMasterForRattle$Result<-gsub("Eosinophilic","AbnormalResult",EoEMasterForRattle$Result)
source("S:\\Gastroenterology\\Seb\\R\\Scripts\\Generics\\Analytics.R")
p<-SensSpec2(EoEMasterForRattle)

##### Sandboxing to subset just the normal endoscopies from both controls and Eosinophilics ###################### ########################### ########################### ################################
EoEMasterN<-EoEMaster[(EoEMaster$type=="Eosinophilic"&EoEMaster$Normal=="TRUE")|EoEMaster$type=="Control"&EoEMaster$Normal=="TRUE",]

########################### ########################### ###########################

#Just looking at the normal-endoscopy EoE patients:
NormalEndoscopyEoE<-EoEMasterN[grepl("Eosinophilic",EoEMaster$type),]
NormalEndoscopyEoE<-NormalEndoscopyEoE[grepl("TRUE",NormalEndoscopyEoE$Normal),]
NormalEndoscopyEoE_FandMoreThan52<-NormalEndoscopyEoE[NormalEndoscopyEoE$Ages>52,]
####Number missed with the cutoff as per Youden index
nrow(NormalEndoscopyEoE_FandMoreThan52)




#Get overview of the script:

c = readScript("S:\\Gastroenterology\\Seb\\R\\Scripts\\Eosinophilics\\Eosinophilix.R")
g = makeVariableGraph( info =getInputs(sc))

if(require(Rgraphviz))
  plot(g)
