#Project: Zillow Tracker
#Code: 5 Evaluation
#Author: Scott Onestak
#Last Executed: 7/10/2022

#Packages
library(tidyverse)
library(stringr)
library(h2o)
library(fredr)

#Data
theData = read.csv('Data/cleanedFinalDataset.csv',header=T,stringsAsFactors=F)
redfin = read.csv('Data/redfin_data_for_asset_appreciation.csv',header=T,stringsAsFactors=F)

#Pull in Interest Rate Data from FRED
fredr_set_key("1e7ed343d3ccb82af108e43174bf8f1f")
fred_30yr = fredr(series_id = "MORTGAGE30US",
                  observation_start = as.Date("2019-01-01",format="%Y-%m-%d"),
                  frequency = "w") %>% select(date,value) %>% rename(rate=value)
fred_start = min(fred_30yr$date)
fred_end = Sys.Date()

rate = NA
for(i in seq(from=fred_start,to=fred_end,by=1)){
  if(i %in% fred_30yr$date){
    rate = unlist(fred_30yr %>% filter(date == i) %>% select(rate))
  }
  
  if(i == fred_start){
    mortgage_rates = as.data.frame(cbind(i,rate))
  } else {
    mortgage_rates = rbind(mortgage_rates,
                           as.data.frame(cbind(i,rate)))
  }
}
colnames(mortgage_rates) = c("date","rate")
mortgage_rates$date = as.Date(mortgage_rates$date,origin = "1970-01-01")

#Create lag... 30 - 45 days typically between accepting offer and closing
#A person would often lock in the interest rate at that time
mortgage_rates$avg_rate = NA
mortgage_cuttoff = fred_start + 45
for(i in seq(from=1,to=dim(mortgage_rates)[1],by=1)){
  if(mortgage_rates[i,"date"]>=mortgage_cuttoff){
    mortgage_rates[i,"avg_rate"] = round(mean(unlist(mortgage_rates %>% 
                                                      filter(date>=mortgage_rates[i,"date"]-45 & 
                                                             date<=mortgage_rates[i,"date"]-30) %>%
                                                      select(rate))),2)
  }
}
mortgage_rates = mortgage_rates %>% filter(!is.na(avg_rate))
colnames(mortgage_rates)[1] = "soldDate"

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
            left_join(.,mortgage_rates %>% select(soldDate,avg_rate),by="soldDate") %>%
            mutate(soldPriceAdj = soldPrice * appreciation)

#Filter out suburbs with little volume
theSuburbs = theData %>% group_by(suburb) %>% summarise(count=n())
suburbsKeep = unlist(theSuburbs %>% filter(count > 20) %>% select(suburb))
theData = theData %>% filter(suburb %in% suburbsKeep)
theData$suburb = as.factor(theData$suburb)

#tax assessed to list price ratio
taxAssessed = theData %>% 
                filter(!is.na(soldPrice) & !is.na(taxAssessedValue)) %>%
                mutate(taxAssessedDiff = (taxAssessedValue - soldPrice)/soldPrice + 1) %>%
                group_by(suburb) %>%
                summarise(medTaxAssessedDiff = median(taxAssessedDiff,na.rm=T),
                          meanTaxAssessedDiff = mean(taxAssessedDiff,na.rm=T))
write.csv(taxAssessed,"Data/taxAssessedToSoldPriceAdj.csv",row.names = FALSE)

#try to pull out most important variables in predicting price
h2o.init(max_mem_size = "16g")
y = "soldPriceAdj"
x = setdiff(colnames(theData),c(y,"Address","Street","City","State","ZipCode","zpid","url","soldPrice","area",
                                "latitude","longitude","soldDate","Type","description","cooling","heating",
                                "parkingLocation","flooring","daysToSale","sellDiff","sellDiffPrct","pricePerSquareFoot",
                                "join_date","appreciation","listPrice","listDate","taxAssessedValue","roof","style","avg_rate"))
H2O_theData = as.h2o(theData)

#Random Forest
#R^2 only 42%. Not good.  Try others to see if better fit for data.
rf = h2o.randomForest(y=y,
                      x=x,
                      training_frame=H2O_theData,
                      model_id = "rf",
                      nfolds=10,
                      seed=412)
rf_varimp_curr = as.data.frame(h2o.varimp(rf))
h2o.saveModel(rf,path="Models/",force=T)
rf_r2 = h2o.r2(rf)

#Gradient Boosting Machine
#R^2 90%.  Much better fit, and not worried about overfitting because the model is cross-validated.
#Best model.  Use gbm in understanding influences on pricing.
gbm = h2o.gbm(y=y,
              x=x,
              training_frame=H2O_theData,
              model_id = "gbm",
              nfolds=10,
              ntrees=250,
              seed=412)
gbm_varimp_curr = as.data.frame(h2o.varimp(gbm))
h2o.saveModel(gbm,path="Models/",force=T)
gbm_r2 = h2o.r2(gbm)

h2o.shutdown(prompt=F)

#Write out variable importance results
#The GBM variable importance list is very similar to the RF for the most important variables, but it seems to
#be more in line with my expectation of importance.  For example, hasCooling is much higher on the GBM varimp
#list than it is on the RF varimp list.
write.csv(rf_varimp_curr,"Results/randomForestVariableImportance.csv",row.names=F)
write.csv(gbm_varimp_curr,"Results/GBMVariableImportance.csv",row.names=F)

#Build linear model - Top Variables Only Account for 54% of variability
#Suburb is probably too split in the linear model but is an important variable overall
#Some signs don't make sense logically.  Bed should logically be positive, not negative.
#Conclusion: Not really a linear relationship. Nonparametric model needed.  GBM best option.
lm = lm(soldPriceAdj ~ suburb + avg_rate + baths + area + walk_score + beds,
         data=theData)
summary(lm)





