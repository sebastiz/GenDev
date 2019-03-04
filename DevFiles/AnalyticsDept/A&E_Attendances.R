#Script for the analysis of A&E summarised data Sebastian Zeki 28/11/2018

######################################### Load libraries #########################################
#To install each package use install.packages("ggplot2") (replace ggplot with the name you are interested in)- this will only install from CRAN. Github downloads are blocked on Trust co
#computers


library(readxl)
library(rvest) # Web scraping library
library(zoo) # for date conversions
library(tidyverse) # general data wrangling
library(forecast)
library(ggplot2)
library(MAPA)

######################################### Data acquisiton#########################################
#Change path as necessary
mydata2<- read_excel("/home/rstudio/GenDev/DevFiles/AnalyticsDept/data/20181120_ED_attendances.xlsx")
#This allows you to view the data
View(mydata2)

#This shows you the structure of the data
str(mydata2)



######################################### Data cleaning#########################################
#Im going to convert it to a straightforward dataframe here
mydata2<-data.frame(mydata2,stringsAsFactors=FALSE)

mydata2$Date<-as.Date(mydata2$Date,format="YYYY-mm-dd",origin="1899-12-30")

#Convert to time series prepared data using forecast package with the correct minimum and maximum dates
#- can get this with min(mydata2$Date) and max(mydata2$Date)
myts_Breach<-ts(mydata2[,2], start=c(2015, 1,1), end=c(2018, 11,11), frequency=365) 
myts_Attend<-ts(mydata2[,3], start=c(2015, 1,1), end=c(2018, 11,11), frequency=365) 



######################################### Data accordionisation#########################################
#Create a decomposed data subset for later visualisation
f_Breach <- decompose(myts_Breach)
f_Attend <- decompose(myts_Attend)

######################################### Data Meta-cleaning (outliers and imputation of missing data) #########################################
#This is for later

x[!x %in% boxplot.stats(x)$out]

######################################### Data merging#########################################
#No merging needed

######################################### Data forking (filtering and subsetting)#######################
#No subsetting needed

######################################### Data analysis#########################################
#Start forecasting here

########### Model with HoltWinters ###########
#Just for the Breach data- model with HoltWinters
hw_fit_Breach <- HoltWinters(myts_Breach)

#Just for the Breach data- model with HoltWinters
hw_fit_Attend <- HoltWinters(myts_Attend)


########### Model with ets ###########
#ets fitting
ets_fit_Breach <- ets(myts_Breach)
ets_fit_Attend <- ets(myts_Attend)


########### Model with auto.arima ###########
#auto-arima model
arima_fit_Breach <- auto.arima(myts_Breach)
arima_fit_Attend <- auto.arima(arima_fit_Attend)
######################################### Data visualisation##########################################This just decomposes into time variants




##################Exploratory analysis#################


#Plot the decomposed data to start with, just to explore it
plot(f_Breach)
plot(f_Attend)

#Just a cool graph - have a play- shows the use of dynamic and interactive graphs in R
library(dygraphs)
dygraph(myts_Breach, main = "Breach Numbers A&E GSTT") %>% 
  dyRangeSelector(dateWindow = c("2015-01-01", "2019-01-01"))

dygraph(myts_Attend, main = "Attendance Numbers A&E GSTT") %>% 
  dyRangeSelector(dateWindow = c("2015-01-01", "2019-01-01"))




#### Plot the forecast Holt-Winters ############ 

# Plot a 12 month forecast from Holt-Winters
plot(forecast(hw_fit_Breach, h=50))
# Plot a 12 month forecast
plot(forecast(hw_fit_Attend, h=700))



#### ets plot ############ 
plot(forecast(ets_fit_Breach, h=12))
plot(forecast(ets_fit_Attend, h=12))


#### auto-arima plot ############ 
plot(forecast(arima_fit_Breach, h=12))
checkresiduals(arima_fit_Breach, plot=TRUE)

plot(forecast(arima_fit_Breach, h=12))
checkresiduals(arima_fit_Breach, plot=TRUE)

plot(forecast(arima_fit_Attend, h=12))
checkresiduals(arima_fit_Attend, plot=TRUE)
arima_fit_Attend


mapa(myts_Breach, model="ZZA", outplot=TRUE)
mapa(myts_Attend, model="ZZA", outplot=TRUE)




######################################### Code overview (with CodeDepends)#########################################


######################################### Dataset overview (with diagtrammeR)#########################################
