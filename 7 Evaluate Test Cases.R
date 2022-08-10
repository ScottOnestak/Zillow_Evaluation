#Project: Zillow Tracker
#Code: 7 Evaluate Test Cases
#Author: Scott Onestak
#Last Executed: 7/10/2022

#library
library(h2o)
library(dplyr)

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
flooringSuburb = testCases %>% 
                  group_by(suburb,flooringHardwood,flooringTile,flooringVinyl,flooringCarpet,flooringLaminate) %>%
                  summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                  ungroup()
baseFlooringSuburbs = flooringSuburb %>% filter(flooringHardwood == 1) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
flooringSuburb = flooringSuburb %>% 
                  inner_join(.,baseFlooringSuburbs,by="suburb") %>%
                  mutate(HardwoodFlooringComp = (soldPricePred-base)/base) %>%
                  select(-base)

flooring = testCases %>% 
              group_by(flooringHardwood,flooringTile,flooringVinyl,flooringCarpet,flooringLaminate) %>%
              summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
              ungroup()
baseflooring = unlist(flooring %>% filter(flooringHardwood == 1) %>% select(soldPricePred))
flooring = flooring %>% mutate(HardwoodFlooringComp = (soldPricePred-baseflooring)/baseflooring)

#Calculate Difference by Heating
heatingSuburb = testCases %>% 
                    group_by(suburb,heatingGas,heatingRadiant,heatingElectric,heatingForcedAir) %>%
                    summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                    ungroup()
baseHeatingSuburbs = heatingSuburb %>% filter(heatingForcedAir == 1) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
heatingSuburb = heatingSuburb %>% 
                  inner_join(.,baseHeatingSuburbs,by="suburb") %>%
                  mutate(ForcedAirHeatingComp = (soldPricePred-base)/base) %>%
                  select(-base)

heating = testCases %>% 
            group_by(heatingGas,heatingRadiant,heatingElectric,heatingForcedAir) %>%
            summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
            ungroup()
baseheating = unlist(heating %>% filter(heatingForcedAir == 1) %>% select(soldPricePred))
heating = heating %>% mutate(ForcedAirHeatingComp = (soldPricePred-baseheating)/baseheating)

#Calculate Difference by Suburb
suburb = testCases %>% filter(beds == 3 & baths == 2 & livingArea == 2000) %>% group_by(suburb) %>% summarise(soldPricePred = mean(soldPricePred,na.rm=T))
basesuburb = unlist(suburb %>% filter(suburb == "Regent Square") %>% select(soldPricePred))
suburb = suburb %>% mutate(suburbComp = (soldPricePred-basesuburb)/basesuburb)

suburbPrice = testCases %>% filter(beds == 3 & baths == 2 & livingArea == 1500) %>% group_by(suburb) %>% summarise(soldPricePred = mean(soldPricePred,na.rm=T))
basesuburbprice = unlist(suburbPrice %>% filter(suburb == "Regent Square") %>% select(soldPricePred))
suburbPrice = suburbPrice %>% mutate(suburbComp = (soldPricePred-basesuburbprice)/basesuburbprice)

#Calculate Difference by Bed
bedSuburb = testCases %>% 
                group_by(suburb,beds) %>%
                summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                ungroup()
baseBedSuburbs = bedSuburb %>% filter(beds == 1) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
bedSuburb = bedSuburb %>% 
              inner_join(.,baseBedSuburbs,by="suburb") %>%
              mutate(OneBedComp = (soldPricePred-base)/base) %>%
              select(-base)

bed = testCases %>% 
        group_by(beds) %>%
        summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
        ungroup()
basebed = unlist(bed %>% filter(beds == 1) %>% select(soldPricePred))
bed = bed %>% mutate(OneBedComp = (soldPricePred-basebed)/basebed)

#Calculate Difference by Bath
bathSuburb = testCases %>% 
              group_by(suburb,baths) %>%
              summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
              ungroup()
baseBathSuburbs = bathSuburb %>% filter(baths == 1) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
bathSuburb = bathSuburb %>% 
              inner_join(.,baseBathSuburbs,by="suburb") %>%
              mutate(OneBathComp = (soldPricePred-base)/base) %>%
              select(-base)

bath = testCases %>% 
          group_by(baths) %>%
          summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
          ungroup()
basebath = unlist(bath %>% filter(baths == 1) %>% select(soldPricePred))
bath = bath %>% mutate(OneBathComp = (soldPricePred-basebath)/basebath)

#Calculate Difference by Living Area
areaSuburb = testCases %>% 
                group_by(suburb,livingArea) %>%
                summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                ungroup()
baseAreaSuburbs = areaSuburb %>% filter(livingArea == 1000) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
areaSuburb = areaSuburb %>% 
                inner_join(.,baseAreaSuburbs,by="suburb") %>%
                mutate(OneThousandComp = (soldPricePred-base)/base) %>%
                select(-base)

area = testCases %>% 
          group_by(livingArea) %>%
          summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
          ungroup()
basearea = unlist(area %>% filter(livingArea == 1000) %>% select(soldPricePred))
area = area %>% mutate(OneThousandComp = (soldPricePred-basearea)/basearea)

#Calculate Difference by Interest Rates
ratesSuburb = testCases %>%
                group_by(suburb,avg_rate) %>%
                summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
                ungroup()
baseRateSuburbs = ratesSuburb %>% filter(avg_rate == 3) %>% select(suburb,soldPricePred) %>% rename(base = soldPricePred)
ratesSuburb = ratesSuburb %>%
                inner_join(.,baseRateSuburbs,by="suburb") %>%
                mutate(ThreeComp = (soldPricePred - base)/base) %>%
                select(-base)

rates = testCases %>%
          group_by(avg_rate) %>%
          summarise(soldPricePred = mean(soldPricePred,na.rm=T)) %>%
          ungroup()
baseRate = unlist(rates %>% filter(avg_rate == 3) %>% select(soldPricePred))
rates = rates %>% mutate(ThreeComp = (soldPricePred - baseRate)/baseRate)

#write out files
write.csv(testCases,"Results/test_results/testCases.csv",row.names = F)
write.csv(area,"Results/test_results/livingArea.csv",row.names = F)
write.csv(areaSuburb,"Results/test_results/livingAreaBySuburb.csv",row.names = F)
write.csv(bath,"Results/test_results/baths.csv",row.names = F)
write.csv(bathSuburb,"Results/test_results/bathsBySuburb.csv",row.names = F)
write.csv(bed,"Results/test_results/beds.csv",row.names = F)
write.csv(bedSuburb,"Results/test_results/bedsBySuburb.csv",row.names = F)
write.csv(cooling,"Results/test_results/cooling.csv",row.names = F)
write.csv(coolingSuburb,"Results/test_results/coolingBySuburb.csv",row.names = F)
write.csv(flooring,"Results/test_results/flooring.csv",row.names = F)
write.csv(flooringSuburb,"Results/test_results/flooringBySuburb.csv",row.names = F)
write.csv(heating,"Results/test_results/heating.csv",row.names = F)
write.csv(heatingSuburb,"Results/test_results/heatingBySuburb.csv",row.names = F)
write.csv(parking,"Results/test_results/parking.csv",row.names = F)
write.csv(parkingSuburb,"Results/test_results/parkingBySuburb.csv",row.names = F)
write.csv(suburb,"Results/test_results/suburb.csv",row.names = F)
write.csv(suburbPrice,"Results/test_results/suburbPricepoint.csv",row.names = F)
write.csv(rates,"Results/test_results/rates.csv",row.names = F)
write.csv(ratesSuburb,"Results/test_results/suburbRates.csv",row.names = F)

