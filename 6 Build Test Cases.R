#Project: Zillow Tracker
#Code: 6 Build Test Cases
#Author: Scott Onestak
#Last Executed: 2/14/2022

#library
library(tidyverse)

#read in data
theData = read.csv('Data/cleanedFinalDataset.csv',header=T,stringsAsFactors=F)

#Filter data down to suburbs wanting to look at
theData = theData %>% filter(suburb %in% c("Edgewood","Highland Park","Point Breeze","Point Breeze North",
                                           "Regent Square","Shadyside","Squirrel Hill North","Squirrel Hill South",
                                           "Swisshelm Park","Swissvale"))

#Get the average scores by suburb
avgSuburbScore = theData %>%
                    group_by(suburb) %>%
                    summarise(walk_score = median(walk_score,na.rm=T),
                              transit_score = median(transit_score,na.rm=T),
                              bike_score = median(bike_score,na.rm=T))

#Get the typical livingArea to lotArea ratio
area_ratios = theData$livingArea / theData$lotArea
median_area_ratio = round(median(area_ratios,na.rm=T),2)

#Get min and maxes for testing
min_baths = min(theData$baths,na.rm=T)
max_baths = max(theData$baths,na.rm=T)
med_baths = median(theData$baths,na.rm=T)
min_livingArea = min(theData$livingArea,na.rm=T)
max_livingArea = max(theData$livingArea,na.rm=T)
med_livingArea = median(theData$livingArea,na.rm=T)
min_bed = min(theData$bed,na.rm=T)
max_bed = max(theData$bed,na.rm=T)
med_bed = median(theData$bed,na.rm=T)

#Get medians for price range
theDataPriceFilter = theData %>% filter(soldPrice >= 150000 & soldPrice <= 250000)
med_baths_price = median(theDataPriceFilter$baths,na.rm=T)
med_livingArea_price = median(theDataPriceFilter$livingArea,na.rm=T)
med_bed_price = median(theDataPriceFilter$bed,na.rm=T)

#Do some reasonableness adjustments... some max and mins are extreme/questionable data values
min_baths = 1
max_baths = 4
min_livingArea = 1000
max_livingArea = 3000
min_bed = 1
max_bed = 4

#Get the median of unimportant (keep static) remaining variables
yearBuilt = median(theData$yearBuilt,na.rm=T)
sunScore = median(theData$sunScore,na.rm=T)
parkingCapacity = median(theData$parkingCapacity,na.rm=T)
hasImage = median(theData$hasImage,na.rm=T)
view = median(theData$view,na.rm=T)
stories = median(theData$stories,na.rm=T)
climateFactor = median(theData$climateFactor,na.rm=T)
phraseExclamation = median(theData$phraseExclamation,na.rm=T)
phraseLocation = median(theData$phraseLocation,na.rm=T)
phraseOld = median(theData$phraseOld,na.rm=T)
phraseOpen = median(theData$phraseOpen,na.rm=T)
phraseRenovated = median(theData$phraseRenovated,na.rm=T)
phraseCustom = median(theData$phraseCustom,na.rm=T)
phraseSmall = median(theData$phraseSmall,na.rm=T)
phraseStainless = median(theData$phraseStainless,na.rm=T)
buildFactor = median(theData$buildFactor,na.rm=T)
electricityFactor = median(theData$electricityFactor,na.rm=T)
solarFactor = median(theData$solarFactor,na.rm=T)

#Build Test Cases for heating type
heatingType = as.data.frame(
                rbind(c(1,0,0,0),
                      c(0,1,0,0),
                      c(0,0,1,0),
                      c(0,0,0,1)))
colnames(heatingType) = c("heatingGas","heatingRadiant","heatingElectric","heatingForcedAir")

#Build Test Cases for cooling type
coolingType = as.data.frame(
                rbind(c(0,0,0),
                      c(1,1,0),
                      c(1,0,1)))
colnames(coolingType) = c("hasCooling","CentralAir","WindowWallAir")

#Build Test Cases for flooring type
flooringType = as.data.frame(
                  rbind(c(1,0,0,0,0),
                        c(0,1,0,0,0),
                        c(0,0,1,0,0),
                        c(0,0,0,1,0),
                        c(0,0,0,0,1)))
colnames(flooringType) = c("flooringHardwood","flooringTile","flooringVinyl","flooringCarpet","flooringLaminate")

#Build Test Cases for parking
parkingType = as.data.frame(
                rbind(c(1,0,0,0,0,0),
                      c(0,1,1,0,0,0),
                      c(0,1,0,1,0,0),
                      c(0,1,0,0,1,0),
                      c(0,1,0,0,0,1)))
colnames(parkingType) = c("parkingStreet","parkingOffStreet",
                          "parkingGarage","parkingCovered","parkingAttached","parkingDetached")


#Create base table for the mins and maxes to join the rest of the test cases to
finalDataset = NA
for(i in seq(from=min_baths,to=max_baths,by=1)){
  for(j in seq(from=min_bed,to=max_bed,by=1)){
    for(k in seq(from=min_livingArea,to=max_livingArea,by=100)){
      temp = avgSuburbScore %>% mutate(baths = i,
                                       beds = j,
                                       livingArea = k,
                                       lotArea = k/median_area_ratio)
      if(i==min_baths & j==min_bed & k==min_livingArea){
        finalDataset = temp
      } else {
        finalDataset = rbind(finalDataset,temp)
      }
    }
  }
}

#Join additional test cases
testFile = crossing(finalDataset,coolingType,heatingType,flooringType,parkingType) %>%
              mutate(yearBuilt = yearBuilt,
                     sunScore = sunScore,
                     parkingCapacity = parkingCapacity,
                     hasImage = hasImage,
                     view = view,
                     stories = stories,
                     climateFactor = climateFactor,
                     phraseExclamation = phraseExclamation,
                     phraseLocation = phraseLocation,
                     phraseOld = phraseOld,
                     phraseOpen = phraseOpen,
                     phraseRenovated = phraseRenovated,
                     phraseCustom = phraseCustom,
                     phraseSmall = phraseSmall,
                     phraseStainless = phraseStainless,
                     buildFactor = buildFactor,
                     electricityFactor = electricityFactor,
                     solarFactor = solarFactor)

#Write out the test cases
write.csv(testFile,"Data/TestCases.csv",row.names=FALSE)
