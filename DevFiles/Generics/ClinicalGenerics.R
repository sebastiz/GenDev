#Clinical Generics


#Symptom extraction

SymptomsExtractions<-function(x,y){
x$Dysphagia<-ifelse(grepl(".*[Dd]ysph.*",x$y,perl=TRUE)|grepl(".*[Oo]dyn.*",x$y,perl=TRUE)|grepl(".*[Ss]tuck.*",x$y,perl=TRUE)|grepl(".*[Ss]tick.*",x$y,perl=TRUE),"Yes","No")
x$Heartburn<-ifelse(grepl(".*[Hh]eart.*",x$y,perl=TRUE)|grepl(".*[Rr]eflu.*",x$y,perl=TRUE)|grepl(".*[Rr]etro.*",x$y,perl=TRUE)|grepl(".*[Bb]urn.*",x$y,perl=TRUE),"Yes","No")
x$Throat<-ifelse(grepl(".*[Tt]hroat.*",x$y,perl=TRUE)|grepl(".*[Nn]eck.*",x$y,perl=TRUE),"Yes","No")
x$Cough<-ifelse(grepl(".*[Cc]ough.*",x$y,perl=TRUE)|grepl(".*[Cc]hok.*",x$y,perl=TRUE),"Yes","No")
x$ChestPain<-ifelse(grepl(".*[Ch]est.*",x$y,perl=TRUE),"Yes","No")
x$AbdoPain<-ifelse(grepl(".*[Ss]tomach.*",x$y,perl=TRUE)|grepl(".*[Ee]piga.*",x$y,perl=TRUE)|grepl(".*[Aa]bdom.*",x$y,perl=TRUE),"Yes","No")
x$Hoarseness<-ifelse(grepl(".*[Hh]oarse.*",x$y,perl=TRUE),"Yes","No")
x$Regurgitation<-ifelse(grepl(".*[Rr]egur.*",x$y,perl=TRUE)|grepl(".*[Tt]aste.*",x$y,perl=TRUE),"Yes","No")
x$Vomiting<-ifelse(grepl(".*[Vv]omit.*",x$y,perl=TRUE),"Yes","No")
x$Belch<-ifelse(grepl(".*[Bb]elch.*",x$y,perl=TRUE)|grepl(".*[Bb]urp.*",x$y,perl=TRUE),"Yes","No")
}