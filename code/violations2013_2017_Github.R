#Code by Laura Perovich January 2018
#RStudio 1.0.153 on Ubuntu
#Data pulled 1/25/18
#Pull five years of the NPDES violation data from the EPA website ("raw" data)

############################################################################
#First add a number of libraries to help us get and look at the data
library("rio")
library("plyr")
library("lattice")
library("data.table")
library(plotrix)
library(extrafont)

#Set a working directory where you would like the data files to be written to 
setwd("your directory here, using / slashes")

#Turn down the likelihood of seeing scientific notation in our tables
options("scipen"=100, "digits"=4)

########################################################### GET THE CHELSEA DATA
#sample link address for "all data" from effluent chart links
#https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=MA0004006&start_date=04/01/2014&end_date=07/14/2017

#This function turns a bunch of parameters into a URL to use to get some data
urlMakerRaw<-function(facilityPermitNum, startDate, endDate){
  #NOTE: dates must be in MM/DD/YYYY format!!
  url<-"https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id="
  url<-paste(url, facilityPermitNum, sep="")
  url<-paste(url, "&start_date=", sep="")
  url<-paste(url, startDate, sep="")
  url<-paste(url, "&end_date=", sep="")
  url<-paste(url, endDate, sep="")
  
  URLencode(url)
  return(url)
}

#Make a dataframe that will help us get the data that we need for Chelsea
#We need vectors of Chelsea permit numbers & location names that we can later turn into URLs
permitNum<-c("MA0003280", "MA0001091", "MA0003298", "MA0003425", "MA0000825", "MA0001929", "MA0004006")
locationName<-c("Chelsea Sandwich Terminal", "Gulf Oil Terminal", "Global REVCO Terminal", "Global Petroleum Terminal",
                 "Global South Terminal", "Irving Oil Terminal", "Sunoco Logistics Terminal")
locationCity<-c("Chelsea", "Chelsea", "Revere", "Revere", "Revere", "Revere", "East Boston")
chelseaSites<-data.frame(permitNum, locationName, locationCity)
chelseaSites$startDate<-"01/01/2013"
chelseaSites$endDate<-"01/01/2018" 


#Get all the Chelsea data, including all the available compounds/parameters
#Get the first dataset
newURL<-urlMakerRaw(chelseaSites$permitNum[[1]], chelseaSites$startDate[[1]], chelseaSites$endDate[[1]])
try(chelseaData<-import(newURL, format="csv"))

#Get the rest of the datasets and combine them with the initial data set
for (i in 2:nrow(chelseaSites)){
  newURL<-urlMakerRaw(chelseaSites$permitNum[[i]], chelseaSites$startDate[[i]], chelseaSites$endDate[[i]])
  try(aa<-import(newURL, format="csv"))
  chelseaData<-rbind(chelseaData, aa)
}
#How much data do we have?
#Note that this includes ALL the NPDES data from these facilities in this time frame, including non-violations. 
nrow(chelseaData)

#Save this file so you only have to get the data once
write.csv(chelseaData, "your file name here with / slashes and ending it .csv", row.names = FALSE)
#If you need to look at this data in R again, you can just run this part:
#chelseaData<-read.csv("data/rawDownload/chelseaData2013_2017.csv")

#data dictionary can be found here (scroll to the bottom): https://echo.epa.gov/tools/data-downloads/icis-npdes-download-summary
#some more info about how data is reported: http://water.ky.gov/permitting/Lists/NetDMR%20FAQ/AllItems.aspx

####################################################### CONSIDER DATA WITH NUMERIC VIOLATIONS ONLY
#Look at the NPDES numeric violations data, i.e. instances where the facilities have violated their permits because of effluent amounts
chelseaViolations<-subset(chelseaData, chelseaData$violation_severity>1)
nrow(chelseaViolations)
#note: you should see 76 numeric violations over these five years

#Restructure the column types so they are easier to work with
chelseaViolations$parameter_desc<-as.character(chelseaViolations$parameter_desc)
chelseaViolations$violation_severity<-as.character(chelseaViolations$violation_severity)
chelseaViolations$statistical_base_short_desc<-as.character(chelseaViolations$statistical_base_short_desc)
chelseaViolations$monitoring_period_end_date<-as.character(chelseaViolations$monitoring_period_end_date)

#Add a year column
for (i in 1:nrow(chelseaViolations)){
  chelseaViolations$year[[i]]<-strsplit(chelseaViolations$monitoring_period_end_date[[i]], "/")[[1]][3]
}
chelseaViolations$year<-as.numeric(as.character(chelseaViolations$year))

#Write out this file so you can use it later
write.csv(chelseaViolations, "your file name here with / and .csv at the end", row.names=FALSE)

####################################################### SUMMARIZE VIOLATIONS
#Here are a few different approaches you can use to summarize the data and look at it through various lenses

#violations for 8 different compounds
unique(chelseaViolations$parameter_desc)

#overall: how many numeric violations does each facility have over these five years?
byFacility<-data.frame(table(chelseaViolations$npdes_id))
colnames(byFacility)<-c("npdes_id", "totalNumViolations")

#overall: how many numeric violations are there by year/facility?
byYearFacility<-ddply(chelseaViolations, .(npdes_id, year), summarize,
                           numViolations = length(year)
)

#.....etc!

