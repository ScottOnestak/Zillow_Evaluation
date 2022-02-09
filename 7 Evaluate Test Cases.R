#Project: Zillow Tracker
#Code: 7 Evaluate Test Cases
#Author: Scott Onestak
#Last Executed: 2/8/2022

#library
library(h2o)

#read in test case dataset
testCases = read.csv("Data/TestCases.csv",header=T,stringsAsFactors=F)
testCases$suburb = as.factor(testCases$suburb)

#read in gbm model and execute
h2o.init(max_mem_size = "16g")

H2O_testCases = as.h2o(testCases)

gbm = h2o.loadModel("Models/gbm")
testCases$soldPricePred = as.vector(h2o.predict(gbm,H2O_testCases))

h2o.shutdown(prompt=FALSE)


#Calculate Differences by Parking
parkingSuburb = testCases %>% 
                  group_by(suburb,parkingStreet,parkingOffStreet,parkingGarage,parkingCovered,parkingAttached,parkingDetached) %>%
                  summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                  ungroup()
baseParkingSuburbs = parkingSuburb %>% filter(parkingStreet == 1) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
parkingSuburb = parkingSuburb %>% 
                  inner_join(.,baseParkingSuburbs,by="suburb") %>%
                  mutate(StreetParkingComp = (soldPricePred-base)/base) %>%
                  select(-base)
  
parking = testCases %>% 
            group_by(parkingStreet,parkingOffStreet,parkingGarage,parkingCovered,parkingAttached,parkingDetached) %>%
            summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
            ungroup()
baseparking = unlist(parking %>% filter(parkingStreet == 1) %>% select(soldPricePred))
parking = parking %>% mutate(StreetParkingComp = (soldPricePred-baseparking)/baseparking)

#Calculate Difference by Cooling
coolingSuburb = testCases %>% 
                  group_by(suburb,hasCooling,CentralAir,WindowWallAir) %>%
                  summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                  ungroup()
baseCoolingSuburbs = coolingSuburb %>% filter(hasCooling == 0) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
coolingSuburb = coolingSuburb %>% 
                  inner_join(.,baseCoolingSuburbs,by="suburb") %>%
                  mutate(NoCoolingComp = (soldPricePred-base)/base) %>%
                  select(-base)

cooling = testCases %>% 
            group_by(hasCooling,CentralAir,WindowWallAir) %>%
            summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
            ungroup()
basecooling = unlist(cooling %>% filter(hasCooling == 0) %>% select(soldPricePred))
cooling = cooling %>% mutate(NoCoolingComp = (soldPricePred-basecooling)/basecooling)

#Calculate Difference by Flooring


#Calculate Difference by Heating


#Calculate Difference by Suburb
suburb = testCases %>% group_by(suburb) %>% summarise(soldPricePred = mean(soldPricePred,na.rm=T))
basesuburb = unlist(suburb %>% filter(suburb == "Regent Square") %>% select(soldPricePred))
suburb = suburb %>% mutate(suburbComp = (soldPricePred-basesuburb)/basesuburb)

#Calculate Difference by Bed


#Calculate Difference by Bath


#Calculate Difference by Living Area

