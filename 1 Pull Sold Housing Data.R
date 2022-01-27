#Project: Zillow Tracker
#Code: 1 Pull Sold Housing Data
#Author: Scott Onestak
#Last Executed: 1/25/2022

#Packages
library(rvest)
library(dplyr)
library(tidyr)
library(httr)
library(stringr)
library(jsonlite)

#Set the zip codes to pull
zipcode_search = c(15206,15217,15218,15221,15232)
numberoftiles = 40 #This is the number of tiles of listings that show up on Zillow

#Run through results to get webpages for the sold houses
theList = NA
listStarted = FALSE
for(i in seq(from=1,to=length(zipcode_search),by=1)){
  #Create building blocks of the url build to search
  cat(zipcode_search[i],"\n",sep="")
  str1 = paste("https://www.zillow.com/pittsburgh-pa-",zipcode_search[i],"/sold/",sep="")
  
  #check site status
  check = GET(str1)
  theCheck = unlist(http_status(check)$reason)
  
  #if the website exists, good to continue
  if(theCheck == "OK"){
    #Check url exists and how many pages will need to be built to find housing data
    holder = read_html(str1)
    count = holder %>% html_nodes('.result-count') %>% html_text() %>% str_remove(' results') %>% str_remove(",") %>% as.numeric()
    page_count = ceiling(min(count,1000)/numberoftiles)
    cat("\nPage Count: ",page_count,"\n",sep="")
    
    
    #Check to make sure strings are not empty... if not, build string to pull housing data and get home urls
    if(!is.na(str1)){
      theURL = NA
      for(j in seq(from=1,to=page_count,by=1)){
        cat("Page: ",j,"\n",sep="")
        theURL = paste(str1,j,"_p/",sep="")
        gotoStr = NA
        
        #check site status
        check2 = GET(theURL)
        theCheck2 = unlist(http_status(check2)$reason)
        
        #if the website exists, good to continue
        if(theCheck2 == "OK"){
          soldURLs = read_html(theURL) %>% 
                      html_nodes("script") %>% 
                      html_text()
          Sys.sleep(5)
          
          #Find the right script to use
          for(k in seq(from=1,to=length(soldURLs),by=1)){
            if(str_detect(soldURLs[k],"zpid") & str_detect(soldURLs[k],"<!--")){
              gotoStr = k
            }
          }
          
          if(!is.na(gotoStr)){
            temp = fromJSON(str_replace_all(str_replace_all(soldURLs[gotoStr],"<!--",""),"-->",""))$cat1$searchResults$listResults
            zpids = temp$zpid
            hasImages = temp$hasImage
            urls = temp$detailUrl
            statuses = temp$statusType
            soldPrices = temp$unformattedPrice
            addresses = temp$address
            streets = temp$addressStreet
            cities = temp$addressCity
            states = temp$addressState
            zipcodes = temp$addressZipcode
            beds = temp$beds
            baths = temp$baths
            areas = temp$area
            latitudes = temp$latLong$latitude
            longitudes = temp$latLong$longitude
            isZillowOwned = temp$isZillowOwned
            soldDates = trimws(str_replace_all(toupper(temp$variableData$text),"SOLD",""))
            homeTypes = temp$hdpData$homeInfo$homeType
            lotAreas = temp$hdpData$homeInfo$lotAreaValue
            lotAreaUnits = temp$hdpData$homeInfo$lotAreaUnit
            taxAssessedValues = temp$hdpData$homeInfo$taxAssessedValue
            
            temp_results = as.data.frame(cbind(addresses,streets,cities,states,zipcodes,zpids,urls,hasImages,
                                               statuses, soldPrices,beds,baths,areas,latitudes,longitudes,
                                               isZillowOwned,soldDates, homeTypes,lotAreas,lotAreaUnits,
                                               taxAssessedValues))
          }
          
          #Stack Results
          if(listStarted == FALSE){
            theList = temp_results
            listStarted = TRUE
          } else {
            theList = rbind(theList,temp_results)
          }
        } else {
          paste("Error: ",theURL,"\n",sep="")
        }
      }
    }
  } else {
    paste("Error: ",url_build[i],"\n",sep="")
  }
}

#Rename columns
colnames(theList) = c("Address","Street","City","State","ZipCode","zpid","url","hasImage","status","soldPrice",
                      "beds","baths","area","latitude","longitude","isZillowOwned","soldDate","Type","lotArea",
                      "lotAreaUnits","taxAssessedValue")
theList$soldDate = as.Date(theList$soldDate,format="%m/%d/%Y")

#Dedup the list... in case of multiple sells, only the most recent one will show but will show as multiple in list
theListDedup = unique(theList) %>% filter(!Type %in% c("APARTMENT","MANUFACTURED","LOT","CONDO"))
write.csv(theListDedup,"Data/theListDedup.csv",row.names = FALSE)





