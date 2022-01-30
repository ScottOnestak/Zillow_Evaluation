#Project: Zillow Tracker
#Code: 3 Pull House Specific Data
#Author: Scott Onestak
#Last Executed: 1/28/2022


#----------------------------------   Cannot run with chrome open   ----------------------------------
#Restart R session: .rs.restartR()


#Packages
library(rvest)
library(dplyr)
library(tidyr)
library(httr)
library(stringr)
library(jsonlite)
library(RSelenium)

#Selenium
##binman::list_versions("chromedriver") <- command to get the chrome version number to use
port = 2000L

#Read in prior
prior = read.csv("Data/finalDataset.csv",header=T,stringsAsFactors=F)
LFRprior = read.csv("Data/listedForRent.csv",header=T,stringsAsFactors=F)

#Create list of zpids to skip because they've already had their data pulled
zpid_skip = c(prior$zpid,LFRprior$zpid)

#Loop through list of sold and collect more detailed information... change batch number with run
theListDedup = read.csv("Data/theListDedup.csv",stringsAsFactors = F, header = T)
cat("Homes to Scrape: ",dim(theListDedup)[1],"\n",sep="")
finalDatasetMade = FALSE
finalDataset = NA
forRentMade = FALSE
listedForRent = NA
checkAgain = FALSE
current = theListDedup
i = 1
j = 0

#Loop through scraper
while(i <= dim(theListDedup)[1] & j <= 50){
  cat(i,"\n",sep="")
  
  if(!current[i,"zpid"] %in% zpid_skip){
    theLink = paste(current[i,"url"],sep="")
    
    #First try downloading the file
    download.file(theLink,destfile = "test.html",quiet=T)
    test = read_html("test.html") %>% html_text()
    if(!str_detect(toupper(test),"PLEASE VERIFY")){
      theProperty = read_html("test.html") %>% 
        html_nodes("div") %>% 
        html_nodes("script") %>%
        html_text()
    } else {
      #if that fails...try Selenium method
      j = j + 1
      port = as.integer(port+1)
      rD = rsDriver(browser="chrome", port=port, chromever = "97.0.4692.36", geckover = NULL, verbose = F)
      remDr = rD[["client"]]
      remDr$navigate(theLink)
      
      #pull html
      html = remDr$getPageSource()[[1]]
      
      #check if they want verification
      test = paste(html)
      if(str_detect(toupper(test),"PLEASE VERIFY")){
        element = remDr$findElements(value="//div/div")[[1]]
        remDr$mouseMoveToLocation(webElement = element)
        remDr$buttondown(buttonId = 'LEFT')
        Sys.sleep(10)
        remDr$buttonup(buttonId = 'LEFT')
        Sys.sleep(10)
      }
      
      theProperty = read_html(html) %>% 
        html_nodes("div") %>% 
        html_nodes("script") %>%
        html_text()
      
      remDr$quit()
      rD$server$stop()
      gc(remDr,verbose = F)
      gc(rD,verbose = F)
    }
    
    
    #Find the right script to use
    if(length(theProperty)>0){
      gotoStr = NA
      temp = NA
      for(j in seq(from=1,to=length(theProperty),by=1)){
        if(str_detect(toupper(theProperty[j]),"YEARBUILT")){
          gotoStr = j
        }
      }
      
      if(!is.na(gotoStr)){
        Sys.sleep(5)
        temp = fromJSON(fromJSON(str_replace_all(str_replace_all(theProperty[gotoStr],"<!--",""),"-->",""))$apiCache)
        
        status = tryCatch(temp[[2]]$property$homeStatus,error=function(e){NA})
        yearBuilt = tryCatch(temp[[2]]$property$yearBuilt,error=function(e){NA})
        livingArea = tryCatch(temp[[2]]$property$livingArea,error=function(e){NA})
        hoaFee = tryCatch(temp[[2]]$property$monthlyHoaFee,error=function(e){NA})
        sunScore = tryCatch(temp[[2]]$property$solarPotential$sunScore,error=function(e){NA})
        buildFactor = tryCatch(temp[[2]]$property$solarPotential$buildFactor,error=function(e){NA})
        climateFactor = tryCatch(temp[[2]]$property$solarPotential$climateFactor,error=function(e){NA})
        electricityFactor = tryCatch(temp[[2]]$property$solarPotential$electricityFactor,error=function(e){NA})
        solarFactor = tryCatch(temp[[2]]$property$solarPotential$solarFactor,error=function(e){NA})
        description = tryCatch(temp[[2]]$property$description,error=function(e){NA})
        suburb = tryCatch(paste(temp[[2]]$property$parentRegion$name,sep="",collapse = ","),error=function(e){NA})
        hasCooling = tryCatch(paste(temp[[2]]$property$resoFacts$hasCooling,sep="",collapse = ","),error=function(e){NA})
        cooling = tryCatch(paste(temp[[2]]$property$resoFacts$cooling,sep="",collapse = ","),error=function(e){NA})
        heating = tryCatch(paste(temp[[2]]$property$resoFacts$heating,sep="",collapse = ","),error=function(e){NA})
        parkingCapacity = tryCatch(paste(temp[[2]]$property$resoFacts$parkingCapacity,sep="",collapse = ","),error=function(e){NA})
        parkingLocation = tryCatch(paste(temp[[2]]$property$resoFacts$parkingFeatures,sep="",collapse = ","),error=function(e){NA})
        garageCapacity = tryCatch(paste(temp[[2]]$property$resoFacts$garageParkingCapacity,sep="",collapse = ","),error=function(e){NA})
        basement = tryCatch(paste(temp[[2]]$property$resoFacts$basementYN,sep="",collapse = ","),error=function(e){NA})
        flooring = tryCatch(paste(temp[[2]]$property$resoFacts$flooring,sep="",collapse = ","),error=function(e){NA})
        view = tryCatch(temp[[2]]$property$resoFacts$hasView,error=function(e){NA})
        stories = tryCatch(paste(temp[[2]]$property$resoFacts$stories,sep="",collapse = ","),error=function(e){NA})
        porch = tryCatch(paste(temp[[2]]$property$resoFacts$patioAndPorchFeatures,sep="",collapse = ","),error=function(e){NA})
        pricePerSquareFoot = tryCatch(paste(temp[[2]]$property$resoFacts$pricePerSquareFoot,sep="",collapse = ","),error=function(e){NA})
        roof = tryCatch(paste(temp[[2]]$property$resoFacts$roofType,sep="",collapse = ","),error=function(e){NA})
        style = tryCatch(paste(temp[[2]]$property$resoFacts$architecturalStyle,sep="",collapse = ","),error=function(e){NA})
        
        #Do checks for nulls
        if(is.null(yearBuilt)){yearBuilt = NA}
        if(is.null(livingArea)){livingArea = NA}
        if(is.null(hoaFee)){hoaFee = 0}
        if(is.null(sunScore)){sunScore = NA}
        if(is.null(buildFactor)){buildFactor = NA}
        if(is.null(climateFactor)){climateFactor = NA}
        if(is.null(electricityFactor)){electricityFactor = NA}
        if(is.null(solarFactor)){solarFactor = NA}
        if(is.null(description)){description = ""}
        if(is.null(suburb)){suburb = NA}
        if(is.null(hasCooling)){hasCooling = NA}
        if(is.null(cooling)){cooling = ""}
        if(is.null(heating)){heating = ""}
        if(is.null(parkingCapacity)){parkingCapacity = NA}
        if(is.null(parkingLocation)){parkingLocation = ""}
        if(is.null(garageCapacity)){garageCapacity = NA}
        if(is.null(basement)){basement = NA}
        if(is.null(flooring)){flooring = ""}
        if(is.null(view)){view = NA}
        if(is.null(stories)){stories = NA}
        if(is.null(porch)){porch = NA}
        if(is.null(pricePerSquareFoot)){pricePerSquareFoot = NA}
        if(is.null(style)){style = ""}
        
        #Get listing price and date to determine how long it was on the market and asking vs sale price
        price_history_holder = as.data.frame(temp[[2]]$property$priceHistory)
        price_history_holder$event = toupper(price_history_holder$event)
        listDate = NA
        listPrice = NA
        if(dim(price_history_holder)[1]>0){
          for(k in seq(from=dim(price_history_holder)[1],to=1,by=-1)){
            if(price_history_holder[k,"event"] == "LISTED FOR SALE"){
              listDate = price_history_holder[k,"date"]
              listPrice = price_history_holder[k,"price"]
            }
          }
        }
      } else {
        yearBuilt = NA
        livingArea = NA
        hoaFee = NA
        sunScore = NA
        buildFactor = NA
        climateFactor = NA
        electricityFactor = NA
        solarFactor = NA
        description = NA
        suburb = NA
        hasCooling = NA
        cooling = NA
        heating = NA
        parkingCapacity = NA
        parkingLocation = NA
        garageCapacity = NA
        basement = NA
        flooring = NA
        view = NA
        stories = NA
        porch = NA
        pricePerSquareFoot = NA
        roof = NA
        style = NA
        listDate = NA
        listPrice = NA
      }
      
      #create link for scores
      scores = paste("https://www.walkscore.com/score/",
                     str_replace_all(str_replace_all(tolower(current[i,"Address"]),",","")," ","-"),
                     sep="")
      #check site status
      check4 = GET(scores)
      theCheck4 = unlist(http_status(check4)$reason)
      walk_score = NA
      transit_score = NA
      bike_score = NA
      
      if(theCheck4 == "OK"){
        returned = tryCatch(paste(read_html(scores) %>% 
                                    html_nodes(xpath='//*[@class="block-header-badge score-info-link"]')),
                            error=function(e){NA})
        
        if(length(returned)>1){
          for(j in seq(from=1,to=length(returned),by=1)){
            
            #Walk Score
            if(str_detect(toupper(returned[j]),"WALK SCORE")){
              temp_score = unlist(str_split(unlist(str_split(returned[j],"<")),">"))
              for(k in seq(from=1,to=length(temp_score),by=1)){
                if(str_detect(toupper(temp_score[k]),"ALT") & str_detect(toupper(temp_score[k]),"WALK SCORE")){
                  walk_score = trimws(
                    str_replace_all(
                      str_replace_all(
                        unlist(
                          str_split(
                            unlist(
                              str_split(
                                str_remove_all(
                                  toupper(temp_score[k])
                                  ,'\"')
                                ,"ALT=")
                            )[1]
                            ,"SCORE/")
                        )[2]
                        ,".PNG","")
                      ,".SVG","")
                  )
                }
              }
            }
            
            #Transit Score
            if(str_detect(toupper(returned[j]),"TRANSIT SCORE")){
              temp_score = unlist(str_split(unlist(str_split(returned[j],"<")),">"))
              for(k in seq(from=1,to=length(temp_score),by=1)){
                if(str_detect(toupper(temp_score[k]),"ALT") & str_detect(toupper(temp_score[k]),"TRANSIT SCORE")){
                  transit_score = trimws(
                    str_replace_all(
                      str_replace_all(
                        unlist(
                          str_split(
                            unlist(
                              str_split(
                                str_remove_all(
                                  toupper(temp_score[k])
                                  ,'\"')
                                ,"ALT=")
                            )[1]
                            ,"SCORE/")
                        )[2]
                        ,".PNG","")
                      ,".SVG","")
                  )
                }
              }
            }
            
            #Bike Score
            if(str_detect(toupper(returned[j]),"BIKE SCORE")){
              temp_score = unlist(str_split(unlist(str_split(returned[j],"<")),">"))
              for(k in seq(from=1,to=length(temp_score),by=1)){
                if(str_detect(toupper(temp_score[k]),"ALT") & str_detect(toupper(temp_score[k]),"BIKE SCORE")){
                  bike_score = trimws(
                    str_replace_all(
                      str_replace_all(
                        unlist(
                          str_split(
                            unlist(
                              str_split(
                                str_remove_all(
                                  toupper(temp_score[k])
                                  ,'\"')
                                ,"ALT=")
                            )[1]
                            ,"SCORE/")
                        )[2]
                        ,".PNG","")
                      ,".SVG","")
                  )
                }
              }
            }
          }
        } else {
          walk_score = NA
          transit_score = NA
          bike_score = NA
        }
      } else {
        walk_score = NA
        transit_score = NA
        bike_score = NA
      }
      
      #stack results in final scraping dataset
      if(status != "FOR_RENT"){
        if(finalDatasetMade==FALSE){
          finalDataset = as.data.frame(cbind(t(unlist(as.list(current[i,]))),yearBuilt,livingArea,hoaFee,sunScore,
                                             buildFactor,climateFactor,electricityFactor,solarFactor,description,suburb,
                                             hasCooling,cooling,heating,parkingCapacity,parkingLocation,garageCapacity,
                                             basement,flooring,view,stories,porch,pricePerSquareFoot,roof,style,
                                             listDate,listPrice,walk_score,transit_score,bike_score))
          finalDatasetMade=TRUE
        } else {
          finalDataset = rbind(finalDataset,
                               as.data.frame(cbind(t(unlist(as.list(current[i,]))),yearBuilt,livingArea,hoaFee,sunScore,
                                                   buildFactor,climateFactor,electricityFactor,solarFactor,description,suburb,
                                                   hasCooling,cooling,heating,parkingCapacity,parkingLocation,garageCapacity,
                                                   basement,flooring,view,stories,porch,pricePerSquareFoot,roof,style,
                                                   listDate,listPrice,walk_score,transit_score,bike_score)))
        }
      } else {
        if(forRentMade==FALSE){
          listedForRent = current[i,]
          forRentMade=TRUE
        } else 
          listedForRent = rbind(listedForRent,current[i,])
      }
    } else {
      if(checkAgain==FALSE){
        toCheckAgain = current[i,]
        checkAgain=TRUE
      } else {
        toCheckAgain = rbind(toCheckAgain,current[i,])
      }
      cat("CHECK AGAIN\n")
      Sys.sleep(60)
    }
  }
  
  #Update counter
  i = i + 1
}


#if new, append and write out
if(is.null(dim(finalDataset))==FALSE){
  outFinalDataset = rbind(prior,finalDataset)
  write.csv(outFinalDataset,"Data/finalDataset.csv",row.names=F)
}

if(is.null(dim(listedForRent))==FALSE){
  outListedForRent = rbind(LFRprior,listedForRent)
  write.csv(outListedForRent,"Data/listedForRent.csv",row.names=F)
}


