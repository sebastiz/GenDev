#Common analytics Tasks

#Prep for Time Line analysis
library(dplyr)
library(ggplot2)
library(directlabels)
library(gridExtra)
library(splitstackshape)
library(googleVis)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


#Need the data frame, the date column name and then the grouping name
TimeLine<- function(x,column,TheName){
  
  x<-x %>%
    mutate(year=format(x[[column]], "%Y"))%>%
    mutate(month=format(x[[column]], "%m"))
   x$DatesMerge<-paste(x$year,x$month,sep=" ")
#   
#   
#   
#   
      mydf<-x %>%
      group_by(year,x[[TheName]])%>%
      dplyr::summarize(n=n())

names(mydf)<-c("year","Name","n")
   
    ggplot(mydf,aes(year,n))+
       geom_line(aes(group=mydf$Name,colour=mydf$Name,size=12))+
      scale_colour_discrete(guide = 'none') +
     scale_x_discrete(expand=c(0, 1)) +
     geom_dl(aes(label = mydf$Name), method = list("smart.grid", cex = 1))+
     xlab("Year") + 
     ylab("Freq")+
     scale_colour_discrete(guide = 'none')  +    
     theme(plot.margin = unit(c(1,3,1,1), "lines")) +
     theme(axis.text.x=element_text(size=18)) +
     theme(axis.text.y=element_text(size=18)) +
     theme(axis.title=element_text(size=20))+
     theme(axis.title=element_text(size=20))+
  theme(legend.position="none")
  
}

#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#As used in BariumSwallowStudy.R


SensSpec<-function(x,y){
  
   x$SensAndSpec<-ifelse(y=="Yes"&x$dx!="Normal","TP",
                         ifelse(y=="Yes"&x$dx=="Normal","FP", 
                                ifelse(y=="NO"&x$dx=="Normal","TN",
                                       ifelse(y=="NO"&x$dx!="Normal","FN","Unclassified"))))
  
  
  
#   x$SensAndSpec<-ifelse(y=="Yes"&!grepl("Normal",x$dx,perl=TRUE),"TP",
#                         ifelse(y=="Yes"&grepl("Normal",x$dx,perl=TRUE),"TN",
#                                ifelse(y=="NO"&grepl("Normal",x$dx,perl=TRUE),"FN",
#                                       ifelse(y=="NO"&!grepl("Normal",x$dx,perl=TRUE),"FN","Unclassified"))))
  
  TP<-nrow(x[x$SensAndSpec=="TP",])
  FP<-nrow(x[x$SensAndSpec=="FP",])
  TN<-nrow(x[x$SensAndSpec=="TN",])
  FN<-nrow(x[x$SensAndSpec=="FN",])
  
  Sens<-(TP/(TP+FN))*100
  Spec<-(TN/(TN+FP))*100
  Accuracy<-((TP+TN)/(TP+FP+FN+TN))*100
  NPV<-(TN/(FN+TN))*100
  PPV<-(TP/(TP+FP))*100    
  TotalNumPosHRM<-nrow(x[y=="Yes",])
  TotalNumNegHRM<-nrow(x[y=="NO",])
  #Add the values to a vector
  t<-c(Sens,Spec,Accuracy,NPV,PPV,TotalNumPosHRM,TotalNumNegHRM,TP,FP,TN,FN)
  return(t)
}
#First input is a column with the test result, the second column is the gold standard results
SensSpec2<-function(x){
  
  x$SensAndSpec<-ifelse(x$Test=="Yes"&x$Result!="Normal","TP",
                        ifelse(x$Test=="Yes"&x$Result=="Normal","FP", 
                               ifelse(x$Test=="NO"&x$Result=="Normal","TN",
                                      ifelse(x$Test=="NO"&x$Result!="Normal","FN","Unclassified"))))

  TP<-nrow(x[x$SensAndSpec=="TP",])
  FP<-nrow(x[x$SensAndSpec=="FP",])
  TN<-nrow(x[x$SensAndSpec=="TN",])
  FN<-nrow(x[x$SensAndSpec=="FN",])
  
  Sens<-(TP/(TP+FN))*100
  Spec<-(TN/(TN+FP))*100
  Accuracy<-((TP+TN)/(TP+FP+FN+TN))*100
  NPV<-(TN/(FN+TN))*100
  PPV<-(TP/(TP+FP))*100    
  TotalNumPosHRM<-nrow(x[x$Result=="Yes",])
  TotalNumNegHRM<-nrow(x[x$Result=="NO",])
  #Add the values to a vector
  t<-c(Sens,Spec,Accuracy,NPV,PPV,TotalNumPosHRM,TotalNumNegHRM,TP,FP,TN,FN)
  return(t)
}





#####################################################################################################################################################################
###################################################### Surveillance functions ##########################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#Each row needs to have a rule associated with it prior to the processing
SurveillanceTimeByRow<-function(x){
x %>%
  #This arranges each patients timepoint by date
  arrange(HospNum_Id,Endo_ResultEntered) %>%
  group_by(HospNum_Id) %>% 
  mutate(diffDate=difftime(Endo_ResultEntered,lead(Endo_ResultEntered,1),units="days"))


}

SurveillanceLastToNow<-function(x){
  x %>%
    #This arranges each patients timepoint by date
    arrange(HospNum_Id,Endo_ResultEntered) %>%
    group_by(HospNum_Id) %>% 
    mutate(diffDate=difftime(Sys.Date(),last(Endo_ResultEntered),units="days"))
}



SurveySankey<-function(dfw,y){
#Create the Sankey diagrams
library(data.table)
Sankey<-dcast(setDT(dfw)[, .SD, HospNum_Id], HospNum_Id~rowid(HospNum_Id), value.var =y)

PtFlow<-Sankey
PtFlow<-data.frame(PtFlow)
PtFlow<-PtFlow[!is.na(names(PtFlow))]
names(PtFlow)<-c("ord1","ord2","ord3","ord4","ord5","ord6","ord7","ord8")


orders <- PtFlow %>%
  select(ord1, ord2, ord3, ord4, ord5,ord6,ord7,ord8)


orders.plot <- data.frame()



for (i in 3:ncol(orders)) {  
  ord.cache <- orders %>%
    group_by(orders[ , i-1], orders[ , i]) %>%
    summarise(n=n())
  
  colnames(ord.cache)[1:2] <- c('from', 'to')
  
  # adding tags to carts
  ord.cache$from <- paste(ord.cache$from, '(', i-1, ')', sep='')
  ord.cache$to <- paste(ord.cache$to, '(', i, ')', sep='')
  
  ord.cache<-data.frame(ord.cache)
  orders.plot <- rbind(orders.plot, ord.cache)
  
}


orders.plot<-data.frame(orders.plot)
orders.plot<-orders.plot[grepl("[A-Z]",orders.plot$from)&grepl("[A-Z]",orders.plot$to), ]
orders.plot<-orders.plot[!grepl("NA",orders.plot$from)&!grepl("NA",orders.plot$to), ]
plot(gvisSankey(orders.plot, from='from', to='to', weight='n',
                options=list(height=900, width=1800, 
                             sankey="{link:{color:{fill:'black',stroke: 'black', strokeWidth: 1 }},
                             node: { color: { fill: '#a61d4c' },
                             label: { color: '#871b47',fontName: 'Open Sans',fontSize: 35 } }}")))

}






#####################################################################################################################################################################
###################################################### Quality assessment functions ##########################################################################################
#####################################################################################################################################################################
#####################################################################################################################################################################
#As used in MarkSamaan script. Part of the Quality assessment for doucmentation
#Input is a dataframe with x=dataframe name, y=column name you are doing text mining from and PropThreshold= the number of rows that have the terms you are interested in

#theframe is the dataframe, y is the columnn of interest and PropThreshold is the Proportion of reports Threshold for the graph
Documentation<-function(theframe,y,PropThreshold){
  #theframe<-as.data.frame(dfw)
  #y<-"Histol"

jeopCorpus <- Corpus(VectorSource(theframe[,y]))
#jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, content_transformer(removeWords), stopwords("english"))
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
jeopCorpus <- tm_map(jeopCorpus, stemDocument)
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)

#Get the frequency table of terms being used over all the reports (ie counts x2 if mentioned twice in the report)
dtm <- TermDocumentMatrix(jeopCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
ReportLookup<-function(tofind){
  paste(nrow(theframe[grep(tofind,theframe[,y]),]),tofind,sep=",")
}

DocsWithWords<-data.frame(t(data.frame(lapply(row.names(m),ReportLookup))))
names(DocsWithWords)<-"V1"
foo <- data.frame(do.call('rbind', strsplit(as.character(DocsWithWords$V1),',',fixed=TRUE)))
foo$X2<-as.character(foo$X2)
foo$X1<-as.numeric(foo$X1)
foo<-data.frame(X2 = names(foo), Prop = as.vector(foo)/nrow(theframe)*100)
foo<-foo[foo$Prop>PropThreshold,]

library(ggplot2)
ggplot(foo,aes(y=Prop,x=X2))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=-90,hjust=0,vjust=0)) + 
  xlab("Term") + 
  ylab("Proportion of reports")+
  theme(axis.text=element_text(size=20)) +
  labs(title=y) +  
  theme(axis.title=element_text(size=18))+
  theme(legend.title=element_blank())+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=18,lineheight=.8, face="bold"))

}

#As above but no PropThreshold and you have to specify the vector list of interesting words to match: title is the name of the dataframe, theframe is the dataframe itself,y is ther column of interest and myNotableWords is the list of words you are looking for

DirectedDocumentationNoGraph<-function(title,theframe,y,myNotableWords){

  jeopCorpus <- Corpus(VectorSource(theframe[,y]))
  #jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
  jeopCorpus <- tm_map(jeopCorpus, content_transformer(removeWords), stopwords("english"))
  jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
  jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
  jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
  #jeopCorpus <- tm_map(jeopCorpus, stemDocument)
  #wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
  
  #Get the frequency table of terms being used over all the reports (ie counts x2 if mentioned twice in the report)
  dtm <- TermDocumentMatrix(jeopCorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  str(jeopCorpus)
  ReportLookup<-function(tofind){
    paste(nrow(theframe[grep(tofind,theframe[,y]),]),tofind,sep=",")
  }
  if(nrow(m)>0){
    DocsWithWords<-data.frame(t(data.frame(lapply(row.names(m),ReportLookup))))
    #return(DocsWithWords)
    names(DocsWithWords)<-"V1"
    foo <- data.frame(do.call('rbind', strsplit(as.character(DocsWithWords$V1),',',fixed=TRUE)))
    see<-foo
    
    foo$X2<-as.character(foo$X2)
    foo$X1<-as.numeric(foo$X1)
    foo$Prop<-(foo$X1/nrow(theframe))*100
    
    
    #group all the words containing stems as per myNotableWords
    foo<-sapply(myNotableWords, function(x) sum(foo$Prop[grep(x, foo$X2)]))
    
    foo<-data.frame(X2 = names(foo), Prop = as.vector(foo))
}
}
#y is the column of the frame
BDR_DirectedDocumentation<-function(theframe,y){
  
  myNotableWords<-c("[Bb]arrett","[Cc]andida","[Cc]oeliac","[Ee]osinophilic","[Pp]eptic")
  
  jeopCorpus <- Corpus(VectorSource(theframe[,y]))
  #jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
  jeopCorpus <- tm_map(jeopCorpus, content_transformer(removeWords), stopwords("english"))
  jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
  jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
  jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
  #jeopCorpus <- tm_map(jeopCorpus, stemDocument)
  #wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
  
  #Get the frequency table of terms being used over all the reports (ie counts x2 if mentioned twice in the report)
  dtm <- TermDocumentMatrix(jeopCorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  str(jeopCorpus)
  ReportLookup<-function(tofind){
    paste(nrow(theframe[grep(tofind,theframe[,y]),]),tofind,sep=",")
  }
  if(nrow(m)>0){
    DocsWithWords<-data.frame(t(data.frame(lapply(row.names(m),ReportLookup))))
    #return(DocsWithWords)
    names(DocsWithWords)<-"V1"
    foo <- data.frame(do.call('rbind', strsplit(as.character(DocsWithWords$V1),',',fixed=TRUE)))
    see<-foo
    mydf
    #Now just need to merge with foo based on the title in this and then include in the overall plot at the end
    #Something like if title greps with names(mydf)= Flexi then merge on the basis of the words into a new column
    
    #just select the list that corresponds to the title
    
    
    foo$X2<-as.character(foo$X2)
    foo$X1<-as.numeric(foo$X1)
    foo$Prop<-(foo$X1/nrow(theframe))*100
    
    
    ########### FOR THE BDR ######### ############### ############### ############### ############### #####################
    #BDR should be the proportion of the reports where there has been a positive diagnosis for gastroscopy only:
    
    #sum the number of positive diagnosis for the terms in the corpus and then divide by the number of reports
    food<-sapply(myNotableWords, function(x) sum(foo$X1[grep(x, foo$X2)]))
    food<-data.frame(X2 = as.character(names(food)), Total = as.vector(food))
    food$X2<-as.character(food$X2)
    #Total number of diagnoses:
    BDR<-sum(food$Total)/nrow(theframe)*100
    print(BDR)
    return(BDR)
  }
}

DirectedDocumentation<-function(title,theframe,y,myNotableWords,mydf){

  jeopCorpus <- Corpus(VectorSource(theframe[,y]))
  #jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
  jeopCorpus <- tm_map(jeopCorpus, content_transformer(removeWords), stopwords("english"))
  jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
  jeopCorpus <- tm_map(jeopCorpus, stripWhitespace)
  jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
  #jeopCorpus <- tm_map(jeopCorpus, stemDocument)
  #wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
  
  #Get the frequency table of terms being used over all the reports (ie counts x2 if mentioned twice in the report)
  dtm <- TermDocumentMatrix(jeopCorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  str(jeopCorpus)
  ReportLookup<-function(tofind){
    paste(nrow(theframe[grep(tofind,theframe[,y]),]),tofind,sep=",")
  }
  if(nrow(m)>0){
  DocsWithWords<-data.frame(t(data.frame(lapply(row.names(m),ReportLookup))))
  #return(DocsWithWords)
  names(DocsWithWords)<-"V1"
  foo <- data.frame(do.call('rbind', strsplit(as.character(DocsWithWords$V1),',',fixed=TRUE)))
  see<-foo
  mydf
  #Now just need to merge with foo based on the title in this and then include in the overall plot at the end
  #Something like if title greps with names(mydf)= Flexi then merge on the basis of the words into a new column
  
  #just select the list that corresponds to the title
  
  
  foo$X2<-as.character(foo$X2)
  foo$X1<-as.numeric(foo$X1)
  foo$Prop<-(foo$X1/nrow(theframe))*100

 
  
  #group all the words containing stems as per myNotableWords
  foo<-sapply(myNotableWords, function(x) sum(foo$Prop[grep(x, foo$X2)]))
  foo<-data.frame(X2 = as.character(names(foo)), Prop = as.vector(foo))
  foo$X2<-as.character(foo$X2)
  mydf<-mydf[names(mydf)==title]
    try(foo<-merge(foo,mydf[[1]],by="X2"))
  
  try(names(foo)<-c("Pathology","Endoscopist","Unit_Average"))
  #foo<-melt(foo, id.vars="Pathology", na.rm=T)
  #names(foo)<-c("Pathology","Endoscopist","Unit_Average")
  library(ggplot2)
  try(ggplot(foo)+
    geom_bar(aes(y=Endoscopist,x=Pathology),stat="identity")+
    geom_point(aes(x=Pathology,y=Unit_Average),size=3, colour="#CC0000")+
    theme(axis.text.x=element_text(angle=-90,hjust=0,vjust=0)) + 
    labs(title=title) + 
    xlab("Term") + 
    ylab("Proportion of reports")+
    theme(axis.text=element_text(size=10)) +
    theme(axis.title=element_text(size=12))+
    theme(legend.title=element_blank())+
    theme(legend.position = "none")+
    theme(plot.title = element_text(size=12,lineheight=.8, face="bold")) 
  )
  }
  }


#This should be for diagnostic colonoscopy GRS only:
GRS_Type_Assess_By_Unit<-function(MyColonData){
  
  ######################################################### by endoscopist for adenomas ####################  #############################################  #############################################  
  #Get the adenomas vs the number of colons done by endoscopist:
  
  MyColonData<-MyColonData[grep("Colonoscopy",MyColonData$ProcPerformed),] 
  MyColonDataAdenomaDetectionByEndoscopist<-MyColonData[grep(".*[Aa]denom.*",MyColonData$Dx),] 
  MyColonDataAdenomaDetectionByEndoscopist<-MyColonDataAdenomaDetectionByEndoscopist%>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumAdenomas=nrow(.)))
  ####################################################################################################################################################################################
  
  MyColonDataColonoscopiesByEndoscopist<-MyColonData %>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumColons=nrow(.)))
  
  #Merge the two above by column to get proportion:
  MyColonDataADR<-full_join(MyColonDataAdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
  MyColonDataADR$PropAdenomas<-(MyColonDataADR$NumAdenomas/ MyColonDataADR$NumColons)*100
  
  
  ######################################################### by endoscopist for adenocarcinomas (without adenomas) ####################  #############################################  #############################################  
  MyColonDataAdenoCarcinomaDetectionByEndoscopist<-MyColonData[grepl(".*denoca.*",MyColonData$Histol)&!grepl(".*denom.*",MyColonData$Histol),] 
  MyColonDataAdenoCarcinomaDetectionByEndoscopist<-MyColonDataAdenoCarcinomaDetectionByEndoscopist%>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumAdenocarcinomas=nrow(.)))
  
  MyColonDataAdenocarcinomas<-full_join(MyColonDataAdenoCarcinomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
  MyColonDataAdenocarcinomas$PropAdenocarcinomas<-(MyColonDataAdenocarcinomas$NumAdenocarcinomas/ MyColonDataAdenocarcinomas$NumColons)*100
  
  ####################################################################################################################################################################################
  ######################################################### by endoscopist for dysplastic grade of adenomas
  MyColonData_HG_AdenomaDetectionByEndoscopist<-MyColonData[grepl(".*denoma.*",MyColonData$Histol)&grepl(".*[Hh]igh [Gg]rade.*",MyColonData$Histol),] 
  MyColonData_HG_AdenomaDetectionByEndoscopist<-MyColonData_HG_AdenomaDetectionByEndoscopist%>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumHighGradeAdenomas=nrow(.)))
  
  MyColonData_LG_AdenomaDetectionByEndoscopist<-MyColonData[grepl(".*denoma.*",MyColonData$Histol)&grepl(".*[Ll]ow [Gg]rade.*",MyColonData$Histol),] 
  MyColonData_LG_AdenomaDetectionByEndoscopist<-MyColonData_LG_AdenomaDetectionByEndoscopist%>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumLowGradeAdenomas=nrow(.)))
  
  MyColonDataHGD_Adenomas<-full_join(MyColonData_HG_AdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
  MyColonDataHGD_Adenomas$PropHGAdenomas<-(MyColonDataHGD_Adenomas$NumHighGradeAdenomas/ MyColonDataHGD_Adenomas$NumColons)*100
  
  MyColonDataLGD_Adenomas<-full_join(MyColonData_LG_AdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
  MyColonDataLGD_Adenomas$PropLGAdenomas<-(MyColonDataLGD_Adenomas$NumLowGradeAdenomas/ MyColonDataLGD_Adenomas$NumColons)*100
  
  MyColonData_Serr_AdenomaDetectionByEndoscopist<-MyColonData[grepl(".*[Ss]errated.*",MyColonData$Histol),] 
  MyColonData_Serr_AdenomaDetectionByEndoscopist<-MyColonData_Serr_AdenomaDetectionByEndoscopist%>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumSerrAdenomas=nrow(.)))
  
  MyColonDataSerr_Adenomas<-full_join(MyColonData_Serr_AdenomaDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
  MyColonDataSerr_Adenomas$PropSerrAdenomas<-(MyColonDataSerr_Adenomas$NumSerrAdenomas/ MyColonDataSerr_Adenomas$NumColons)*100
  
  #############################################  ######## hyperplastic detection rate by endoscopist (from whole dataset)  ####################  #############################################  #############################################  
  
  MyColonDataHyperplasticDetectionByEndoscopist<-MyColonData[grep(".*yperplastic.*",MyColonData$Dx),] %>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumHyperplastics=nrow(.)))
  
  MyColonDataColonoscopiesByEndoscopist<-MyColonData %>% 
    group_by(Endo_Endoscopist) %>% 
    do(data.frame(NumColons=nrow(.)))
  
  #Merge the two above by column to get proportion:
  MyColonDataHDR<-full_join(MyColonDataHyperplasticDetectionByEndoscopist,MyColonDataColonoscopiesByEndoscopist,by=c("Endo_Endoscopist"))
  MyColonDataHDR$PropHyperplastic<-(MyColonDataHDR$NumHyperplastics/ MyColonDataHDR$NumColons)*100
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
  #View(MyColonWithdrawalTime)
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
  #FinalTable<-full_join(FinalTable,MyColonDataLGD_Adenomas,by=c("Endo_Endoscopist"))
  #FinalTable<-full_join(FinalTable,MyColonDataHGD_Adenomas,by=c("Endo_Endoscopist"))
  #FinalTable<-full_join(FinalTable,MyColonDataSerr_Adenomas,by=c("Endo_Endoscopist"))
  FinalTable$HyperplasticToAdenomaRatio<-FinalTable$PropAdenoma/FinalTable$PropHyperplastic
  
  #Rename one column
  #FinalTable<-dplyr::rename(FinalTable,NumColons.y=NumColons)
  
  names(FinalTable)[names(FinalTable)=="NumColons.y"]<-"Colons(Count)"
  
  #Then get rid of the rest
  FinalTable<-FinalTable[,!grepl("Num",colnames(FinalTable))]
  FinalTable$Endo_Endoscopist<-NULL
  FinalTable$HyperplasticToAdenomaRatio<-NULL
  FinalTable$PropHyperplastic<-NULL
  FinalTable$PropAdenocarcinomas <-NULL
  FinalTable$HyperplasticToAdenomaRatio<-NULL
  FinalTable$HyperplasticToAdenomaRatio<-NULL
  FinalTable$WithdrawalTime
  #Need to have a table witrh fewer columns here
  #Need to re-arrange the columns and also wrap the words
  #Remove some columns:
  names(FinalTable)<-c("Withdrawal Time","Num Colons","%Adenomas")
  return(FinalTable)
  
}


############## Number of biopsies ######################################################## 


NumBx<-function(x){

dfwBx<-x[!is.na(x$NumbOfBxs),] 

NumBxPlot<- dfwBx %>% 
  group_by(Endo_Endoscopist) %>%
  summarise(avg=mean(NumbOfBxs,na.rm=T)) 


ggplot(NumBxPlot) +
  geom_point(aes(x=Endo_Endoscopist, y=avg),colour="red",size=3)+
  labs(title="Average number of biopsies by endoscopist" ) +
  theme(plot.margin = unit(c(0,0,0,0), "lines"))+
  ylab("Number of biopsies") +
  theme(axis.text.x=element_text(angle=-90)) +
  theme(axis.text.y=element_text(angle=-90)) +
  theme(legend.position="top")
}

##############  Size of biopsies ########################################################  

SizeBx<-function(x){
dfwBxSize<-x[!is.na(x$BxSize),] 
BxSizePlot<- dfwBxSize %>% 
  group_by(Endo_Endoscopist) %>%
  summarise(Mean=mean(BxSize)) 


ggplot(BxSizePlot) +
  geom_point(aes(x=Endo_Endoscopist, y=Mean),colour="red",size=3)+
  labs(title="Average of largest biopsy size for list" ) +
  theme(plot.margin = unit(c(0,0,0,0), "lines"))+
  ylab("Average size (cubic mm)") +
  theme(axis.text.x=element_text(angle=-90)) +
  theme(axis.text.y=element_text(angle=-90)) +
  theme(legend.position="top")
}




########################################################  ########################################################  
########################################################  ########################################################  
########################### Diagnosis Extractor ###############  #################################################  
########################################################  ########################################################  
########################################################  ########################################################  
#This gets run with (in the main script you are calling this from)
#PClst<-split(dfw,dfw$Endo_ResultName)
#names(PClst)<-paste0('df',gsub("\\s+","",names(PClst)))
#lapply(names(PClst), ProcDiagnosis)

#This should be for the pathology vs endoscopy done
ProcDiagnosis<-function(n,PClst,mydf) {
  #Note you pass the names of the dataframes in the list to the function and then you extract the dataframe from the list. Each dataframe should be a procedure
  this_is_a_name <- n
  print(n)
 
  this_is_my_data <- PClst[[n]] 
  mydf
  #The corpus to cover both path and endoscopy

 ifelse(grepl("Flexible Sigmoidoscopy$",this_is_a_name,perl=TRUE),myNotableWords<-c("[Cc]olitis","[Cc]rohn","[Hh]yperplastic","[Ii]nflammation","[Ii]schaemic","[Mm]icroscopic"),
             ifelse(grepl("Gastroscopy \\(OGD\\)$",this_is_a_name,perl=TRUE),myNotableWords<-c("[Bb]arrett","[Cc]andida","[Cc]oeliac","[Ee]osinophilic","[Pp]eptic"),
                   ifelse(grepl("Colonoscopy$",this_is_a_name,perl=TRUE),myNotableWords<-c("[Cc]olitis","[Cc]rohn","[Hh]yperplastic","[Ii]nflammation","[Ii]schaemic","[Mm]icroscopic"),
                       #ifelse(grepl("[Ee]RCP",this_is_a_name,perl=TRUE),myNotableWords<-c("[Ss]tone","[Ss]tricture","[Cc]arcin","[Ii]nflam"),
                              #ifelse(grepl("EUS",this_is_a_name,perl=TRUE),myNotableWords<-c("[Ss]tone","[Ss]tricture","[Cc]arcin","[Ii]nflam"),
                                     #ifelse(grepl("PEG",this_is_a_name,perl=TRUE),myNotableWords<-c("[Ss]tone","[Ss]tricture","[Cc]arcin","[Ii]nflam"),
         myNotableWords<-c("Nothing"))))

if(myNotableWords!="Nothing"){
  try(DirectedDocumentation(this_is_a_name,this_is_my_data,"Dx",myNotableWords,mydf))
}
}

BDR_ProcDiagnosis<-function(n,PClst) {
  #Note you pass the names of the dataframes n in the list to the function and then you extract the dataframe from the list. Each dataframe should be a procedure
  this_is_a_name <- n 
  this_is_my_data <- PClst[[n]] 
  #The corpus to cover both path and endoscopy
       
                                    
  if(myNotableWords!="Nothing"){
    try(BDR_DirectedDocumentation(this_is_my_data,"Dx"))
  }
}


#This is for the heatmap in Self Indications vs pathology-currently not using it
ProcDiagnosis2<-function(n,PClst) {
  #Note you pass the names of the dataframes in the list to the function and then you extract the dataframe from the list. Each dataframe should be a procedure
  this_is_a_name <- n 
  this_is_my_data <- PClst[[n]] 
  
  myNotableWords<-c("[Aa]denoma","[Aa]phthae","[Aa]phthous","[Cc]ancer","[Cc]arcinoma","[Cc]olitis","[Cc]rohn","[Gg]ranuloma","[Hh]yperplastic","[Ii]nflammation","[Ii]schaemic","[Mm]alignancy","[Pp]ostinflammatory","[Uu]lcer","[Bb]arrett","[Cc]andida","[Cc]oeliac","[Dd]ysplasia","[Ee]osinophilic","[Ee]rosion","[Gg]astritis","[Gg]astropathy","[Pp]apilloma","[Pp]eptic","[Rr]adiation")
  if(myNotableWords!="Nothing"){
    DirectedDocumentationNoGraph(this_is_a_name,this_is_my_data,"Dx_Simplified",myNotableWords)
  }
}

#################Functions to look at the locations of the biopsies####################################################################

PrelimCleanLocation<-function(x){
  
  
  x$SampleLocation<-as.character(str_extract_all(x$MacDescrip, pattern = "Nature of specimen.*?\\."))
  x$SampleLocation<-gsub("Nature\\s*of\\s*specimen\\s*(as\\s*stated\\s*)?on\\s*request\\s*form(\\s*=)?|Nature\\s*of\\s*specimen\\s*(as\\s*stated\\s*)?on\\s*request\\s*form\\s*and\\s*pot(\\s*=)?|Nature\\s*of\\s*specimen\\s*(as\\s*stated\\s*)?on\\s*form\\s*and\\s*pot(\\s*=)?","",x$SampleLocation)
  x$SampleLocation<-gsub("Nature\\s*of\\s*specimen.*?pot\\s*(=|:).*?\\.|Nature of specimen\\s*(=|:)? [Nn]ot stated.*?\\.|Nature\\s*of\\s*specimen.*?pot\\s*(=|:).*?\\.|Nature\\s*of\\s*specimen.*?pot\\s*.*?(\\.)?","",x$SampleLocation)
  
  
  #x$SampleLocation<-gsub("Nature\\s*of\\s*specimen.*?pot\\s*(=|:).*?|Nature\\s*of\\s*specimen(\\s*as\\s*stated)?\\s*on\\s*pot\\s*(=|:).*?\\\\n|Nature\\s*of\\s*specimen\\s*on\\s*pot\\s*[Nn]ot\\s*stated.*?(\\\\n)?|Nature\\s*of\\s*specimen\\s*not\\s*stated\\s*on\\s*pot\\s*=.*?\\\\n|Nature\\s*of\\s*specimen\\s*[Nn]ot\\s*stated\\s*on\\s*pot.*?\\\\n|Nature\\s*of\\s*specimen\\s*on\\s*pot\\s*not\\s*stated(=)?.*?\\\\n","",x$SampleLocation)
  x$SampleLocation<-gsub("Nature\\s*of\\s*specimen\\s*on\\s*pot","",x$SampleLocation)
  x$SampleLocation<-gsub("c\\(","",x$SampleLocation,perl=T)
  x$SampleLocation<-gsub("\n","\\.",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("\\","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("n\\","",x$SampleLocation,fixed=T)
  return(x)
}

#Extract all the terms used in the locataions:


#Do the term conversion- Function 2
TermStandardLocation<-function(x){
  x$SampleLocation<-gsub("[Rr][Ii][Gg][Hh][Tt]|($| )[Rr] |[Aa]sce |[Aa]scending|[Aa]scend[^a-z]|[Cc]olon [Rr]|[Rr] [Cc]olon|[Aa]sc ","Ascending ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ll][Ee][Ff][Tt]|lt |[Dd]escending|[Dd]escen[^a-z]|[Dd]esc[^a-z]|[Dd]es[^a-z]|[Cc]olon [Ll]|[Ll] [Cc]olon","Descending ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ss]igmoid|[Ss]igm[^a-z]|[Ss]igmo ","Sigmoid ",x$SampleLocation)
  x$SampleLocation<-gsub("[Rr]ectal|[Rr]ectum|[Rr]ectum[a-z]|[Rr]ect ","Rectum ",x$SampleLocation)
  x$SampleLocation<-gsub("[Tt]ransverse|[Tt]ransv[^a-z]|[Tt]ranv |[Tt]rans ","Transverse ",x$SampleLocation)
  x$SampleLocation<-gsub("[Cc]aecum|[Cc]aecal","Caecum ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ss]plenic","Splenic ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ii]leum|[Ii]leal","Ileum ",x$SampleLocation)
  x$SampleLocation<-gsub("[Rr]ectosigmoid","Rectosigmoid ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ii]leocaecal\\s*|[Ii][Cc][Vv]|[Ii]leo-[Cc]aecum","Ileocaecal ",x$SampleLocation)
  x$SampleLocation<-gsub("[Hh]ep[^a-z]|[Hh]epatic","Hepatic ",x$SampleLocation)
  x$SampleLocation<-gsub("[Cc]olonic|[Cc]olon |[Cc]ol[^a-z]","Colon ",x$SampleLocation)
  x$SampleLocation<-gsub("[Tt]erm |[Tt]erminal","Terminal ",x$SampleLocation)
  x$SampleLocation<-gsub("TI","Terminal Ileum ",x$SampleLocation)
  x$SampleLocation<-gsub("[Cc]aec ","Caecum ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ss]ig ","Sigmoid ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ii]leo\\s*-\\s*[Aa]nal ","Ileoanal ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ii]leo\\s*[Aa]nal ","Ileoanal ",x$SampleLocation)
  x$SampleLocation<-gsub("[Pp]re\\s*pouch","PrePouch ",x$SampleLocation)
  x$SampleLocation<-gsub("[Pp]re-[Pp]ouch","PrePouch ",x$SampleLocation)
  x$SampleLocation<-gsub("pouch","Pouch ",x$SampleLocation)
  x$SampleLocation<-gsub("IleoAnal([a-zA-Z]+)","IleoAnal \\1 ",x$SampleLocation)
  x$SampleLocation<-gsub("[Aa]nastomosis","Anastomosis",x$SampleLocation)
  x$SampleLocation<-gsub("[Xx]\\s*[1-9]|","",x$SampleLocation)
  x$SampleLocation<-gsub("[1-9]\\s*[Xx]|","",x$SampleLocation)
  
  
  x$SampleLocation<-gsub("[Hh]yperplastic","",x$SampleLocation)
  
  
  x$SampleLocation<-gsub("[Dd]istal|[Pp]roximal|[Pp]rox ","",x$SampleLocation)
  x$SampleLocation<-gsub("[Ss]essile","",x$SampleLocation)
  x$SampleLocation<-gsub("\\d+[Mm]{2}|\\d+[Cc][Mm]","",x$SampleLocation)
  x$SampleLocation<-gsub("[Pp]edunculated|[Pp]seudo","",x$SampleLocation)
  x$SampleLocation<-gsub("\\d","",x$SampleLocation)
  x$SampleLocation<-gsub("  "," ",x$SampleLocation)
  x$SampleLocation<-gsub(":","",x$SampleLocation)
  
  
  #For upper GI- Function 3
  x$SampleLocation<-gsub("[Gg]astric|[Ss]tomach","Stomach",x$SampleLocation)
  x$SampleLocation<-gsub("[Aa]ntrum|[Aa]ntral","Antrum",x$SampleLocation)
  x$SampleLocation<-gsub("[Dd]uodenum|[Dd]2|[Dd]uodenal","Duodenum",x$SampleLocation)
  x$SampleLocation<-gsub("[Oo]esophageal|[Oo]esophagus|esophag[^a-z]","Oesophagus",x$SampleLocation)
  x$SampleLocation<-gsub("[Gg][Oo][Jj]|[Gg]astro[Oo]esophageal","GOJ",x$SampleLocation)
  x$SampleLocation<-gsub("[Ff]undal|[Ff]undic|[Ff]undus","GOJ",x$SampleLocation)
  return(x)
}

#General cleanup - Function 4
FinalCleanUpLocator<-function(x){
  x$SampleLocation<-gsub("  "," ",x$SampleLocation)
  x$SampleLocation<-gsub("\\","",x$SampleLocation,fixed=TRUE)
  x$SampleLocation<-gsub("'","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("n\"","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("\"","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub(")","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("(","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub(", ,",",",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("?","",x$SampleLocation,fixed=T)
  x$SampleLocation<-gsub("\\.","",x$SampleLocation)
  dfMyColonData<-cSplit(x, 'SampleLocation', ',', drop=FALSE,direction = "long")
}

#Split by sample type:
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
PolypTidyUpLocator<-function(x){
  #Get all the polyps and tidy up the polyp data- Function 5
  x$Polyp<-str_extract_all(x$SampleLocation, ".*[Pp]olyp.*")
  x$Polyp<-gsub("[Pp]olyp.*","Polyp",x$Polyp)
  x$Polyp<-gsub("[Ss]mall|[Tt]iny|[Dd]iminutive","",x$Polyp)
  x$Polyp<-gsub("[Ll]arge","",x$Polyp)
  x$Polyp<-gsub("[Ff]lexure|[Ff]lex ","",x$Polyp)
  x$Polyp<-gsub("Ascending [Cc]olon","Ascending",x$Polyp)
  x$Polyp<-gsub("[Rr]ecto[?:(-|\\s*)]Sigmoid","RectoSigmoid",x$Polyp)
  x$Polyp<-gsub("[Bb]iops.*?[A-Z]","",x$Polyp)
  x$Polyp<-gsub("[Rr]ecto\\s*[Ss]igmoid","RectoSigmoid",x$Polyp)
  x$Polyp<-gsub("[Pp]olyp|olyp","",x$Polyp)
  x$Polyp<-gsub("[Ss]ided","",x$Polyp)
  x$Polyp<-gsub("[Mm]{2}","",x$Polyp)
  x$Polyp<-gsub("IleoCaecum","Ileocaecal",x$Polyp)
  x$Polyp<-gsub("Descending Colon","Descending",x$Polyp)
  x$Polyp<-gsub("Sigmoid Colon","Sigmoid",x$Polyp)
  x$Polyp<-gsub("Transverse Colon","Transverse",x$Polyp)
  x$Polyp<-trimws(x$Polyp, which = c("both"))
  x<-x[!grep("character()",x$Polyp),]
  xForPolyps<-x[!is.na(x$Polyp),]
}


########## IMAGES WITH RESULTS FOR BIOPSY AND OTHER LOCATOR ####################### 
########## For polyps  ############################################################      
# dfMyColonDataForPolyps<-PolypTidyUpLocator(df)
# PolypTable<-data.frame(table(dfMyColonDataForPolyps$Polyp))
# PolypTable<-PolypTable[PolypTable$Var1!="",]
# PolypTable<-subset(PolypTable,PolypTable$Freq>3)
# 
# 
# ggplot(PolypTable,aes(x=Var1,y=Freq))+
#   geom_bar(stat="identity", fill="lightblue", colour="black")+
#   theme(legend.position="bottom") +
#   xlab("Location") + 
#   ylab("Number of polyps") +
#   theme(axis.text.x=element_text(angle=-90,size=18))




plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[1:2] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}

# plot_jpeg('/Users/sebastianzeki/Dropbox/Work/Medical/Clinical/Gastro/Research/MarkPouchitis/colon-and-rectum.jpg')
# text(29.348,333.7705,labels=PolypTable$Freq[PolypTable$Var1=="Ascending"],cex=2.6)
# text(105.0414,156.4875,labels=PolypTable$Freq[PolypTable$Var1=="Caecum"],cex=2.6)
# text(729.2044,427.5464,labels=PolypTable$Freq[PolypTable$Var1=="Descending"],cex=2.6)
# text(109.5016,620.6219,labels=PolypTable$Freq[PolypTable$Var1=="Hepatic"],cex=2.6)
# text(69.5016,226.5161,labels=PolypTable$Freq[PolypTable$Var1=="Ileocaecal"],cex=2.6)
# text(411.2489,109.1275,labels=PolypTable$Freq[PolypTable$Var1=="RectoSigmoid"],cex=2.6)
# text(416.7742,53.3347,labels=PolypTable$Freq[PolypTable$Var1=="Rectum"],cex=2.6)
# text(332.4031,231.9347,labels=PolypTable$Freq[PolypTable$Var1=="Sigmoid"],cex=2.6)
# text(695.2321,691.9637,labels=PolypTable$Freq[PolypTable$Var1=="Splenic"],cex=2.6)
# text(441.4947,596.2528,labels=PolypTable$Freq[PolypTable$Var1=="Transverse"],cex=2.6)



PouchTidyUpLocator<-function(x){
  #Get all the polyps and tidy up the polyp data- Function 5
  x$Pouch<-gsub("[Bb]iops.*?[A-Z]","",x$SampleLocation)
  x$Pouch<-gsub("[Bb]iops.*(\\s+)?","",x$SampleLocation)
  x$Pouch<-gsub("=","",x$Pouch)
  x$Pouch<-gsub("\\.","",x$Pouch)
  x$Pouch<-gsub("[Cc][Mm]","",x$Pouch)
  x$Pouch<-gsub("[Mm]{2}","",x$Pouch)
  x$Pouch<-gsub("[Bb]x","",x$Pouch)
  x$Pouch<-gsub("[Bb]iopsies|[Bb]iopsy|[Bb]io.*?(y|ies)","",x$Pouch)
  x$Pouch<-gsub("[Aa]ll |[Aa]bove","",x$Pouch)
  x$Pouch<-gsub("[Aa]nal","Anal",x$Pouch)
  x$Pouch<-gsub("[Ss]ite","",x$Pouch)
  x$Pouch<-gsub("[Rr]andom","",x$Pouch)
  x$Pouch<-gsub("[Tt]erminal [Ii]leum","",x$Pouch)
  x$Pouch<-gsub("(b$|b\\s)","",x$Pouch)
  x$Pouch<-gsub("-","",x$Pouch,fixed=T)
  x$Pouch<-gsub("+","",x$Pouch,fixed=T)
  x$Pouch<-gsub("[Pp].*?ch","Pouch",x$Pouch)
  x$Pouch<-gsub("([a-z])[Pp]ouch","\\1 Pouch",x$Pouch)
  x$Pouch<-gsub(".*[Aa]nastomosis.*","Anastomosis",x$Pouch)
  x$Pouch<-gsub(".*[Aa]fferent.*","Afferent",x$Pouch)
  x$Pouch<-gsub("  "," ",x$Pouch)
  x$Pouch<-gsub("Ileum Anal","IleoAnal",x$Pouch,fixed=T)
  x$Pouch<-gsub("Pouch Ileum","Ileum Pouch",x$Pouch,fixed=T)
  x$Pouch<-gsub("IleoAnal Pouch","IleoAnal",x$Pouch,fixed=T)
  x$Pouch<-gsub("Ileo ","IleoAnal",x$Pouch,fixed=T)
  x$Pouch<-gsub("Ileum Pouch","Ileum",x$Pouch,fixed=T)
  x$Pouch<-gsub("smbowel","Ileum",x$Pouch,fixed=T)  
  x$Pouch<-gsub("Pouch Colon","Pouch",x$Pouch,fixed=T)
  #THis mops up whatever is left
  x$Pouch<-gsub("Pouch .*","Pouch",x$Pouch)
  x$Pouch<-gsub("[Pp][Rr][Ee]\\s*[Pp][Oo][Uu][Cc][Hh] .*","Pre-pouch",x$Pouch)
  x$Pouch<-gsub("Ileum.*","Ileum",x$Pouch)
  x$Pouch<-gsub(".*[Ii]leo[Aa]nal.*","IleoAnal",x$Pouch)
  x$Pouch<-gsub(".*[Rr]ectum.*","Rectum",x$Pouch)
  x$Pouch<-trimws(x$Pouch, which = c("both"))
  x<-x[!grep("character()",x$Pouch),]
  xForPouch<-x[!is.na(x$Pouch),]
}

