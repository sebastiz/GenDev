#Clinical Generics


#Symptom extraction

SymptomsExtractions<-function(x,y){
data$Dysphagia<-ifelse(grepl(".*[Dd]ysph.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Oo]dyn.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Ss]tuck.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Ss]tick.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Heartburn<-ifelse(grepl(".*[Hh]eart.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Rr]eflu.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Rr]etro.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Bb]urn.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Throat<-ifelse(grepl(".*[Tt]hroat.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Nn]eck.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Cough<-ifelse(grepl(".*[Cc]ough.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Cc]hok.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$ChestPain<-ifelse(grepl(".*[Ch]est.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$AbdoPain<-ifelse(grepl(".*[Ss]tomach.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Ee]piga.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Aa]bdom.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Hoarseness<-ifelse(grepl(".*[Hh]oarse.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Regurgitation<-ifelse(grepl(".*[Rr]egur.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Tt]aste.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Vomiting<-ifelse(grepl(".*[Vv]omit.*",data$IndicANDHx,perl=TRUE),"Yes","No")
data$Belch<-ifelse(grepl(".*[Bb]elch.*",data$IndicANDHx,perl=TRUE)|grepl(".*[Bb]urp.*",data$IndicANDHx,perl=TRUE),"Yes","No")
}