#Project: Zillow Tracker
#Code: 8 Evaluate Out of Sample
#Author: Scott Onestak
#Last Executed: 2/20/2022

#Packages
library(tidyverse)
library(stringr)
library(h2o)
library(dplyr)

#Data
theData = read.csv('Data/cleanedFinalDataset.csv',header=T,stringsAsFactors=F)
redfin = read.csv('Data/redfin_data_for_asset_appreciation.csv',header=T,stringsAsFactors=F)


#Create field to determine which month to join to
theData$join_date = substr(theData$soldDate,1,7)
redfin$join_date = substr(redfin$Time,1,7)

#Modify fields for use
theData$soldDate = as.Date(theData$soldDate)
theData$listDate = as.Date(theData$listDate)

redfin$Time = as.Date(redfin$Time)

#Filter data to >= 2020
theData = theData %>% filter(substr(join_date,1,4) %in% c("2020","2021","2022"))

#Mutate the join for months too new for data
theMissing = setdiff(unique(theData$join_date),unique(redfin$join_date))
changeto = redfin[dim(redfin)[1],"join_date"]
theData$join_date = ifelse(theData$join_date %in% theMissing,changeto,theData$join_date)

#Calculate Asset Appreciation to Now
price_now = redfin[dim(redfin)[1],"Median_Price"]
redfin$appreciation = ((price_now - redfin$Median_Price) / redfin$Median_Price) + 1

#Join Appreciation and Adjust Sold Price
theData = theData %>% 
  left_join(.,redfin %>% select(join_date,appreciation),by="join_date") %>%
  mutate(soldPriceAdj = soldPrice * appreciation)

#Filter out suburbs with little volume
theSuburbs = theData %>% group_by(suburb) %>% summarise(count=n())
suburbsKeep = unlist(theSuburbs %>% filter(count > 20) %>% select(suburb))
theData = theData %>% filter(suburb %in% suburbsKeep)
theData$suburb = as.factor(theData$suburb)

#Get the test data points
theTestData = theData %>% filter(soldDate >= as.Date("2022-01-28") & !is.na(livingArea))

#read in gbm model and execute
h2o.init(max_mem_size = "16g")

H2O_theTestData = as.h2o(theTestData)

gbm = h2o.loadModel("Models/gbm")
theTestData$soldPriceAdjPred = as.vector(h2o.predict(gbm,H2O_theTestData))

h2o.shutdown(prompt=FALSE)


#Look at performance
theTestData$soldPriceAdjDiff = theTestData$soldPriceAdjPred - theTestData$soldPriceAdj
theTestData$soldPriceAdjDiffPrct = (theTestData$soldPriceAdjPred - theTestData$soldPriceAdj)/theTestData$soldPriceAdj

plot(theTestData$soldPriceAdj,theTestData$soldPriceAdjDiffPrct)
summary(theTestData$soldPriceAdjDiffPrct)

