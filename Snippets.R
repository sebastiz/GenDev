#A truck load of code snippets



################### System tasks #############................................... 1
#### Subset   ####.................................................................................................... 2
####reshape data and dplyr ####......................................................................... 3
####String searches ####....................................................................................... 5
#### Pasting strings  ####...................................................................................... 6
#### Sorting ####..................................................................................................... 7
####Descriptive stats ####..................................................................................... 7
#### merge cbind and rbind####......................................................................... 8
#### ggplot  ####...................................................................................................... 8
#### non-ggplot stuff ####................................................................................... 11
#### functions ####............................................................................................... 13
#### Conditionals ####......................................................................................... 14
#### Dates stuff ####............................................................................................. 14
#### dput ####........................................................................................................ 15
#### DNA snippets ####....................................................................................... 16

################### System tasks #############

####1. To call sed and awk for general cleaning up tasks####

system("sed -i '' 's/\"//g' /Users/sebastianzeki/Desktop/SequencingScripts/bedtools/bedtools2-master/CohortComparisons/AkagiT_ChromosomeAbnormEA.bed")

#####2. To call bedtools and other command line type things use, make sure the command is in the command path and then use (system("<command>"))####
system("bedtools etc etc blablabla")

#####3.  How to run perl and pass it a filename as a variable for it work on. Sed as well####
system(paste("perl -p -i -e 's/ /\t/g'",filename))

#####4. How to reference things in system(paste()) from the R script included####
system(paste("perl -p -i -e 's/ /\t/g'",filename1))

#####5. Write directly to change an external config file by inserting a filename####
system(paste("perl -i -p -e's{file1=\"\"}{\"/User/me/ct.txt\"}g' /Users/sebastianzeki/Desktop/tbb.conf"))

####********************************************************####
#### Subset   ####

#####1. Subsetting data: #Can subset by column by referencing the column name or number or whatever.####
newdata <- subset(total , total[,5] == 'g' & total[,8] == 'd')

#####2. Subset on basis of colmeans####
totalB_lowDiv2[,colMeans(totalB_lowDiv2)>30]

#####3. Get rid of rows with NA in a specific column####
rainbow2<-subset(rainbow,!(is.na(rainbow["CopyNumber"])))

#####4. Get rid of non adjacent columns####
resultOrderProportion[-c(2:6,8)]

#####5. Dont need to subset necessarily....####
resultReidBE[resultReidBE$ValuesReidBE<2,]

#####6. Subset by non adjacent columns####
WholeSVNoTrans2 <- data.frame(WholeSVNoTrans[2],WholeSVNoTrans[4])

#####7. delete a column from a dataframe####
data$col <- NULL

#####8. Only select certain columns from one dataframe to make another one####
df3 <- data.frame(df2[3:4],df2[9])
SelfComp <- data.frame(Self["PatientID"],Self["Findings"],Self["EndoscopDiagn"],Self["Dx"])

#####9. delete a row from r####
df = df[-1,]

#####10. move columns to certain positions without getting rid of them####
df <- df[ ,c(1,4:6,2:3)]

#####11. Count the number of rows####
nrow(SVProp)

#####12. Show unique repeat entries####
unique(df[duplicated(df),])

#####13. Get rid of all duplicates entirely####
df[!(duplicated(df) | duplicated(df, fromLast = TRUE)), ]

#####14. rounding up in r####
df2$Means<-round(df2[3])

#####15. Switch row to column####
data.frame(t(df))

#########16. How to create a dataframe####
n=c(2,3,4)
s=c("aa","bb","dd")
b=c(TRUE,FALSE,FALSE)
df=data.frame(n,s,b)


#####17. Remove first row in r####
df = df[-1,]

#####18. Remove columns based on dataframe list of values####
df1[setdiff(names(df1),df2$ID)]

#####19. omit rows which are na####
df[!(is.na(df$start_pc)
     
     #####or####
final[complete.cases(final),]

#####20. Replace specific value####
df[df==""]<-NA

#####21. Replace na with zero in R####
d[is.na(d)] <- 0

#####22. Create random numbers in a column in a dataframe####
CN_PerSampleOrder$number<-sample(100, size = nrow(CN_PerSampleOrder), replace = TRUE)

#####23. Remove NAs in a certain column in r####
OCForCorr[complete.cases(OCForCorr$UICC.stage)]

#####24. Subtract one dataframe from another in R####
library(dplyr)
setdiff(BigDF, SmallDF)
want<-anti_join(org_df,sub_df)

#####25. See if elements of one dataframe are in another dataframe (or not in another dataframe)####

Rule3NoRepeats<-data.frame(EndoSubsetOnSurveilPreDOIHospNum[which(!EndoSubsetOnSurveilPreDOIHospNum$x %in% EndoSubsetOnSurveilPostDOIHospNum$x),])


#####26. Proportion by subset in R####
DF<-ddply(DF,.(category1,category2),transform,prop=number/sum(number))


####********************************************************####


####reshape data and dplyr ####
library(reshape)
mdata <- melt(resultOrderProportion, id=c("Names","DivergenceOfRpts"))

#How to group rows according to whatever you want and then perform something on them
library(dplyr)
GroupedTableProportion <- RptCountsAll %>% group_by(Rptname = substr(Rptname, 1, 4)) %>% summarise(freq = sum(freq))
#How to group rows according to whatever you want and then perform something on them (group_by(dataframe,columnInDataFrame))
bySex <- group_by(OCCAMSdbTumCNOAC,Sex)
(sumSex <- summarize(bySex,count=n()))
#Combine columns of different length
library(plyr)
combined <- rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])

#Reshaping data with tidyr
library(dplyr)
library(tidyr)

long_data =
  res_matrix %>%
  as.data.frame %>%
  mutate(rowname = rownames(.) ) %>%
  gather(variable, value, AH_026C:WG_019C)

matches =
  long_data %>%
  filter(rowname == variable) %>%
  select(rowname, control_value = value)

long_result =
  long_data %>%
  left_join(matches) %>%
  mutate(ratio = value / control_value)

wide_result =
  long_result %>%
  select(-value, -control_value) %>%
  spread(variable, ratio)


#group results

library(dplyr)
sumdf <- yourdf %>%
  group_by(ID) %>%
  summarise_each(funs(sum))

#Merge the same rows using aggregate
total2<-aggregate(. ~ Rptname, data = total, FUN = sum)
#Average the duplicates
TTypedAllControlTBBX<-aggregate(TTypedAllControlTBB$CNI,by=list(name=TTypedAllControlTBB$Sample,etc1=TTypedAllControlTBB$TissueType,etc2=TTypedAllControlTBB$Extraction),data=TTypedAllControlTBB,FUN=mean)


#Convert table to matrix
tr<-as.data.frame.matrix(table(df3))

#Get minumum of a group
NAOGCName_Num_CStage_MStage<-NAOGCName_Num_CStage_MStage %>%
  slice(which.min(VisitDate))

####********************************************************####


####String searches ####

##Find nd replace text
counts2$chr <- gsub("chr", "",counts2$chr)

# How to use grep to subset data
irisSubset <- mydataWhole[grep("L1M.*", mydataWhole$Rptname), ]
#Find and replace with a regular expression in a column- this one inserts a 'per' at the beginning of the line.
newdataOrder[1] = sub("^","per",newdataOrder[1])


#Select rows where does not match the grep
RepeatAlusSequences3 <- as.data.frame(RepatAlusSequences[grep("^>chr(.*)", RepatAlusSequences$V1, invert=TRUE), ])
#Or as follows where he new column is called strip and the original is called chrom
#Split the string based on a pattern and put into another dataframe
library(stringr)
RepeatAlusSequencesdfMatch[c('Sequence', 'new_split_col')] <-  do.call(rbind,lapply(str_split(RepeatAlusSequencesdfMatch$Sequence,
    perl('(?=ggtcaggagttcgagaccag)'), 2), function(x) c(x[1],substr(x[2],1,146))))

require(stringr);counts$strip <- str_replace(counts$chrom, "chr", "")

#Trim whitespace from rows
library(stringr); df1[] <- lapply(df1, str_trim)

#Pick only certain columns based on grep
names(totsMerge) <- sub('.*?_(TC0\\d{2}[^_]*).*', '\\1', names(totsMerge))

#Quick find and replace
require(stringr);dfZ$chr <- str_replace(dfZ$chr, "chr", "hs")

#Rename columns based on a find replace names. You have to matchthe whole line.
names(cbind6Tum) <- gsub('.*(OCCAMS_[A-Z]{2}_[0-9]{3}_).*', '\\1', names(cbind6Tum))

#Crete a new column based on the values in another column
library(stringr)
MeDysplasia$Grade <- str_extract(MeDysplasia$SName, '[A-Z]GD')

#Match multiple samples
TTypedAllChol<-TTypedAll2[grepl("CholangioCarcinoma",TTypedAll2$TissueType)|grepl("Controls",TTypedAll2$TissueType),]

#Data cleaningas per here:  http://www.r-bloggers.com/three-quick-and-simple-data-cleaning-helper-functions-december-2013/
library(DataCombine)
ABNewDF <- FindReplace(data = ABData, Var = "a", replaceData = Replaces, from = "from", to = "to", exact = FALSE)

#Grep stringthat does not contain something
chromLength<- chromLength[!grepl("_", chromLength[,1]), ]

#Flagging when a string matches in a separate column with several conditions

Therap$EVENT <- ifelse(grepl("HALO|RFA|APC", Therap$ERPROCEDUREPERFORMED), "RFA",
   ifelse(grepl("EMR", Therap$Diagnosis), "EMR",
"nothing"))

####********************************************************####

#### Pasting strings  ####    

##Add string to dataframe column values
data$Chromosome <- paste("chr",as.character(data$Chromosome), sep="")

#Rename columns- in this case rename the first three. names is a function
names(newdata2) <- c("chrom", "chromStart", "chromEnd")
#Rename just one column specifically
names(df1)[1] <- c("chr")
#Export a file- tab delimited
write.table(mydata,"/Volumes/SeagateBackupPlusDrive/SequencingRawFiles/TumourOesophagealOCCAMS/SequencingScripts/countsAmendedInterestingColumnOnly.txt", sep="\t", row.names=FALSE, col.names=FALSE)


#To concatenate rows together use paste, same with rows
AllAluSx$MergeCol<-paste(AllAluSx$chr, AllAluSx$chrStart, AllAluSx$chrEnd, sep=":")
#Concatentate columns together and re-add to the dataframe
TIMEdbMerge<-paste(TIMEdb$Study, TIMEdb$Sample,TIMEdb$StudySample, sep="")
TIMEdb<-cbind(TIMEdbMerge,TIMEdb)

#### Sorting ####

#Sorting in R- dont forget the comma before the bracket
newdata3 <- newdata2[order(newdata2$chrom) , ]
#And sort by more than one column
newdata3 <- newdata2[order(newdata2$chrom ,newdata2$chromStart) , ]
#sort top to bottom
sortedDiscoveryTumours <- tup[order(-tup[,1]),]
#Order a column with mixed text and numbers by the numbers
library(gtools)
df2<-df[mixedorder(df$chr),]
#Reverse the order of something according to a column
TTypedAll$Sample<-rev(TTypedAll$Sample)

#Group and then slect the minimum of any column in that group using dplyr

NAOGCName_Num_CStage_MStage<-NAOGCAll %>%
  arrange(HospNum_Id, as.Date(NAOGCAll$VisitDate, '%d/%m/%y')) %>%
  group_by(HospNum_Id) %>%
  slice(which.min(VisitDate))


####********************************************************####


####Descriptive stats ####





#Count frequencies in a column (where the thing inside the "" is the column name)
library(dplyr)
count(newdata2, "Rptname")
#Group and count in one column
table(ZerofLINENameseq$V7)


#Group and get freq by group
DilatationsTable<-Dilatations %>%
  group_by(PATIENTRECORDNUMBER) %>%
  summarise (n=n())


####********************************************************####


#### merge cbind and rbind####



#merging datasets (this is by two columns)
total <- merge(data frameA,data frameB,by=c("chr","leftPos"))

#How to add a row to a dataframe
result = data.frame(Names = names(result), Values = result)
x <- data.frame(Names="FastSeq",Values=FastSeqSVs)
result <- rbind(x,result)

#Merge datasets and keep everything in both. Can also do inner and outer joins with this
library(dplyr)
rainbow<-merge(totsMerge,TIMEdb,all = TRUE)



####********************************************************####

#### ggplot  ####



#ggplot example
ggplot() +
  geom_point(aes(mergedGroup4$Rptname, mergedGroup4$PercentageChangeForSVStart, color = "red"), mergedGroup4) + 
  geom_point(shape=1) +
  geom_point(aes(mergedGroup4$Rptname, mergedGroup4$PercentageChangeForSVEnd, color = "green"), mergedGroup4) +
  geom_point(size=3, colour="#CC0000")  +
  labs(title="Number of repetitive elements (as a proportion of total genomic number) per \n family at the start and end of Oesophageal adenocarcinoma SV's\n", x="TY [°C]", y="Txxx") +
  scale_color_manual("",labels = c("SVStart", "SVEnd"), values = c("blue", "red")) +
  #geom_point(aes(mergedGroup4$Rptname, mergedGroup4$PercentageChangeForWholeSV), mergedGroup4) + theme(axis.text.x=element_text(angle=-90)) +
  xlab("Repetitive elements") +
  ylab("Percentage of all repeat elements") +
  theme(axis.text.x=element_text(angle=-90)) +
  theme(legend.position="top")

#How to order stuff with ggplot
ggplot(resultOrder) +
  geom_point(aes(reorder(resultOrder$Names, resultOrder$Proportion, mean), y=resultOrder$Proportion)) + 
  coord_flip() +
  #Order the x axis in ggplot
  DF1R$WorstH_path <- as.character(DF1R$WorstH_path)
#Then turn it back into an ordered factor
DF1R$WorstH_path <- factor(DF1R$WorstH_path,  levels=c("No_IM", "IM", "IGD","LGD","HGD","T1a","SM1","SM2","T1b_Unspec"))

#Get the data into long format in ggplot and in one line plot all the variable as a line.
library(reshape)
mdata <- melt(resultOrderProportion, id=c("Names","DivergenceOfRpts"))
ggplot(mdata) +
  geom_line(aes(mdata$Names, mdata$value, group=mdata$variable, colour=mdata$variable)) +
  theme(legend.position="bottom") +
  labs(title="Number of SVs each repeat element is found in (as a percentage, filtered for >30%)", x="TY [°C]", y="Txxx") +
  xlab("Repetitive elements") +
  ylab("Percentage of SVs") +
  theme(axis.text.x=element_text(angle=-90))

#ggplot geom_bar
ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")

#Put a plot within a plot and other neat bits and peices- data is from CopyNumberCountsWithLoops.R

aplot<-ggplot() +
  geom_point(aes(TTypedAll$Sample, TTypedAll$V1, group=TTypedAll$TissueType,colour=TTypedAll$TissueType),size=5) +
  labs(title="Copy number index for oesophageal adenocarcinoma, \nnon-dysplastic Barrett's and normal blood", x="Sample", y="Copy number index") +
  theme(axis.text=element_text(size=16)) +
  theme(axis.title=element_text(size=18))+
  scale_x_discrete(breaks=NULL) +
  theme(legend.title=element_blank())+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=18,lineheight=.8, face="bold"))


d=data.frame(x=c(1), y=c(38), vx=c(2), vy=c(0))
l=data.frame(x=c(1), y=c(33), vx=c(0), vy=c(5))
r=data.frame(x=c(3), y=c(33), vx=c(0), vy=c(5))

p1 <- ggplot(aes(y = TTypedAll$V1, x = factor(TTypedAll$TissueType)), data = TTypedAll)
p1<-p1 + geom_boxplot(aes(fill=TTypedAll$TissueType))+
  geom_segment(data=d, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=1, color="black") +
  geom_segment(data=l, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=1, color="black") +
  geom_segment(data=r, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=1, color="black") +
  geom_jitter(position=position_jitter(width=.2), size=1) +
  geom_text(aes(2, 40, label="*"),size=10)+
  xlab("Sample") +
  ylab("Copy number index")+
  theme(axis.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))+
  scale_x_discrete(breaks=NULL) +
  scale_fill_discrete(breaks=c("trt1","ctrl","trt2"))
theme(legend.title=element_blank())+
  theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.background = element_blank(), axis.line = element_line(colour = "black"))

vp <- viewport(width = 0.6, height = 0.5, x = 0.673, y = 0.655)
#Just draw the plot twice
print(aplot)
print(p1, vp = vp)

#Get a histogram plot of whatever you want in ggplot

df<-subset(df,df[8]==0)
table<-table(df[7])
table1<-as.data.frame(table)
table2 <- table1[order(table1$Freq) , ]
table3<- tail(table2,50)

library(ggplot2)
ggplot() +
  geom_point(aes(table3$Var1, table3$Freq, color = "red"), table3) +  
  geom_point(shape=1)

#Reorder a ggplot according to what you want. Have to use factors. This example reorders TTypedfTumvsB$Sample according to TTypedfTumvsB$TissueType
TTypedfTumvsB$Sample <- factor(TTypedfTumvsB$Sample, levels = TTypedfTumvsB$Sample[order(TTypedfTumvsB$TissueType)])
#To order by y axis as a continuous variable
resultOrder <- result[order(result$Freq), ]
resultOrder$Names <- factor(resultOrder$Names, levels = resultOrder$Names[order(resultOrder$Freq)])

#How to create a legend
legend(700,0.8,c("Low CN","Medium CN","High CN"), cex=0.8,lty=c(1,1), lwd=c(2.5,2.5),col=c("red","green","black"))# puts text in the legend



####********************************************************####


#### non-ggplot stuff ####




#Histogram plot of a column frequency with a subset for the most frequent
barplot(sort(table((ZerofLINENameseq$V7)[200:250])),cex.axis = 1, cex.names = 0.3, space= 12,main="Absolute Numbers of LINE elements found within SVs", las=2)

#Boxplot for grouped data
boxplot(df$sd ~df$chr)

# Example of a Bagplot -For continuous variables -like a boxplot for continuous variables
library(aplpack)
attach(mtcars)
bagplot(wt,mpg, xlab="Car Weight", ylab="Miles Per Gallon",
   main="Bagplot Example")

#Annotate a boxplot
fn<-boxplot2(rainbow$CopyNumber~rainbow$AFI,plot=FALSE)$stats
par(mar=c(4,6,4,2))
boxplot(rainbow$CopyNumber~rainbow$AFI,xlab="AFI status",ylab="Copy Number Change",outline=FALSE,col=c("red","green","black"))
text(1.15, fn[1], paste("Minimum Value =", fn[3]), adj=0, cex=.7)
text(1.15, fn[2], paste("Lower Quartile =", fn[2]), adj=0, cex=.7)
text(1.15, fn[3], paste("Median =", fn[3]), adj=0, cex=.7)
text(1.15, fn[4], paste("Upper Quartile =", fn[4]), adj=0, cex=.7)
text(1.15, fn[5], paste("Maximum Value =", fn[5]), adj=0, cex=.7)
arrows(1.14, fn[1], 1.02, fn[1])
arrows(1.14, fn[2], 1.02, fn[2])
arrows(1.14, fn[3], 1.02, fn[3])
arrows(1.14, fn[4], 1.02, fn[4])
arrows(1.14, fn[5], 1.02, fn[5])
title("TIME sample AFI \nvs copy number changes")


#To have a plot within a plot
library(lattice)
library(gridBase)
library(grid)

plot.new()
pushViewport(viewport())
xvars <- rnorm(25)
yvars <- rnorm(25)
plot(unlist(cbind6),type="p",cex=1.5,xlab="Sample", ylab="Copy Number Index", pch=21,bg="red", main= "Copy number Index for Combined HiSeq and MiSeq runs")
pushViewport(viewport(x=.6,y=.8,width=.25,height=.25,just=c("left","top")))
grid.rect()
par(plt = gridPLT(), new=TRUE)
pp<-boxplot(combined,main="Tumour,Diploid & Barrett's Normalised Copy Number Values using all \n LINE1 PCR loci", cex=0.5, xlab="Samples", ylab="Copy Number Value",pch=20, outline=FALSE,col=c("red","green","black"))
popViewport(2)

require(ggplot2)
require(grid)
plot(unlist(cbind6),type="p",cex=1.5,xlab="Sample", ylab="Copy Number Index", pch=21,bg="red", main= "Copy number Index for Combined HiSeq and MiSeq runs")
print(pp, vp=viewport(.8, .75, .2, .2))


#Import an image
img <- readPNG("sochi-logo.png")
g <- rasterGrob(img, interpolate=TRUE)
plot1 <- ggplot(mdat, aes(x = Count, y = Country, colour = Place))
+ geom_point()
+ facet_grid(.~Place) + theme_bw()
+ annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
+ scale_colour_manual(values=c("#CC6600", "#999999", "#FFCC33", "#000000"))

####********************************************************####

#### importing and exporting things ####

#To get a specific sheet from excel into R
library(XLConnect)
wk = loadWorkbook("/Users/sebastianzeki/Desktop/SequencingScripts/Plots/Results/Cellularities/cellularities_Sebastian.xlsx")
dfw = readWorksheet(wk, sheet="Sheet3",header=TRUE)
#To get the sheet names
library(gdata)
sheetNames("/Users/sebastianzeki/Dropbox/Work/Medical/Clinical/Gastro/Data_Infrastruc/Eosinophil/GSTT_HistoEndo_Eosinophil07toAug2016Report.xlsx")


#Subset by row
GroupTbbM[as.logical(rowSums(GroupTbbM > 0)), ]

#Add row.names as a proper column
myDF <- cbind(Row.Names = rownames(myDF), myDF)

#Subset groups
df[which(df$col2 > 25), ]


####********************************************************####


####  Statistics ####
#To get Kaplan-Meier curves
library(survival)
OCCAMSdbTumCN$D_Surv<-as.integer(OCCAMSdbTumCN$DOD-OCCAMSdbTumCN$DODg)
OCCAMSdbTumCN$Dead <- as.integer(OCCAMSdbTumCN$Dead)
mfit <- survfit(Surv(D_Surv, Dead )~1, data = OCCAMSdbTumCN)
par
plot(mfit)

####********************************************************####


#### functions ####



#Using functions for looping through data

library(irr)
lst <- mget(ls(pattern='total\\d+'))

classify_cnv = function (column)
  ifelse(column < 2, 1, ifelse(column > 2, 3, 2))

classify_all_cnvs = function (df) {
  df$CopyNumber.x = classify_cnv(df$CopyNumber.x)
  df$CopyNumber.y = classify_cnv(df$CopyNumber.y)
  df
}

result = lapply(lst, classify_all_cnvs)

lapply(result, function(df){
  kappa2(df[,c(5,8)], "squared")})

View(apply(AluJb,2,function(x){sum(x>0)}))

#### Convert files ####

#Convert a file to a csv
file <- '~/Downloads/PING CONCOURS DONNES.csv'
lines <- readLines(file)
columns <- strsplit(lines, ';')
headers <- sapply(columns, '[[', 1)
data <- lapply(columns, '[', -1)
df <- do.call(cbind, data)
colnames(df) <- headers
print(head(df))

#Convert data to JSON file format
library(rjson)
apply(X[,-1], 1, toJSON)

####********************************************************####


#### Conditionals ####

#ifelse with different conditions
OCCAMSdbTumCNOAC$CNSTATUS <- ifelse(OCCAMSdbTumCNOAC$CNI < 15, 1, ifelse(OCCAMSdbTumCNOAC$CNI >= 20 & OCCAMSdbTumCNOAC$CNI < 30, 2, 3))
#Several if statements put together
OCCAMSdbTumCN$CNSTATUS <- ifelse(OCCAMSdbTumCN$CN < 10, 1, ifelse(OCCAMSdbTumCN$CN >= 10 & OCCAMSdbTumCN$CN < 20, 2, 3))

#### Dates stuff ####


#Date tricks Get difference between current date and another column. Has a condition for na values
OCCAMSdbTumCNOAC$D_Surv = ifelse(!is.na(OCCAMSdbTumCNOAC$DOD),difftime(OCCAMSdbTumCNOAC$DOD,OCCAMSdbTumCNOAC$DODg,units="days"),difftime(Sys.time(),OCCAMSdbTumCNOAC$DODg,units="days"))
#Convert to date format from string (the format is the format the date is currently in as a string with hyphens but could be any punctuation)
OCCAMSdb$DOD<-as.Date(OCCAMSdb$DOD,format="%Y-%m-%d")

#Get difference between two dates in consecutive rows
DateSurveill<-EndoSubsetOnSurveil %>% arrange(HospNum_Id, VisitDate) %>% group_by(HospNum_Id) %>%
  mutate(diffDate = difftime(VisitDate, lag(VisitDate,1)))

#POSIX date things
date <- as.POSIXlt(data$VisitDate, format='%a %b %d %H:%M:%S %Y')

####********************************************************####


#### Lists stuff ####

#Put a load of dataframes into a list
df_list = lapply(names(dfMatched)[-(1:2)],
  FUN = function(x) dfMatched[, c(names(dfMatched)[1:2], x)])
names(df_list) = names(dfMatched)[-(1:2)]

#Renaming columns in a list
colnames <- c("chr","leftPos","Means")
for (i in seq_along(df_list)){
  colnames(df_list[[i]]) <- colnames
}

#Convert list to a datatable
library(data.table); DT <- rbindlist(df_list, idcol = TRUE)

#Extract data from a list
df <- data.frame(name=names(more),
  value=sapply(more, function(x) x$value))

#Convert nested list and get the unique values for each nested list
unique(tolower(unlist(EventList(),use.names = FALSE)))

<p class="MsoNormal" style="ma