#Project: Zillow Tracker
#Code: 4 Clean and Mine Data
#Author: Scott Onestak
#Last Executed: 2/14/2022

#Packages
library(tidyverse)
library(ggplot2)
library(ggmap)
library(osmdata)
library(rvest)

options(scipen=999)

#Read in Data
theDataset = read.csv("Data/finalDataset.csv",header=T,stringsAsFactors=F)


#Clean hasImage
theDataset$hasImage = ifelse(theDataset$hasImage==TRUE,1,0)
theDataset$hasImage[is.na(theDataset$hasImage)] = 0

#Clean suburb
theDataset$suburb = ifelse(theDataset$suburb %in% c("15206","15218","15221",""),NA,theDataset$suburb)

forGGPLOT = theDataset %>% filter(!is.na(latitude) & !is.na(longitude) & !is.na(suburb))
forGGPLOT2 = theDataset %>% filter(!is.na(latitude) & !is.na(longitude))
lat_min = min(forGGPLOT$latitude)
lat_max = max(forGGPLOT$latitude)
lon_min = min(forGGPLOT$longitude)
lon_max = max(forGGPLOT$longitude)
theMatrix = rbind(c(lon_min-.005,lon_max+.005),c(lat_min-.005,lat_max+.005))
colnames(theMatrix) = c("min","max")
rownames(theMatrix) = c("x","y")

big_streets = theMatrix %>%
                opq() %>%
                add_osm_feature(key = "highway", 
                                value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
                osmdata_sf()
med_streets = theMatrix %>%
                opq()%>%
                add_osm_feature(key = "highway", 
                                value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
                osmdata_sf()

small_streets = theMatrix %>%
                  opq()%>%
                  add_osm_feature(key = "highway", 
                                  value = c("residential", "living_street","unclassified","service", "footway")) %>%
                  osmdata_sf()

river = theMatrix %>%
            opq()%>%
            add_osm_feature(key = "waterway", value = "river") %>%
            osmdata_sf()

railway = theMatrix %>%
            opq()%>%
            add_osm_feature(key = "railway", value="rail") %>%
            osmdata_sf()

theMap = ggplot() +
          geom_sf(data = river$osm_lines,
                  inherit.aes = FALSE,
                  color = "steelblue",
                  size = .8,
                  alpha = .3) +
          geom_sf(data = railway$osm_lines,
                  inherit.aes = FALSE,
                  color = "black",
                  size = .2,
                  linetype="dotdash",
                  alpha = .5) +
          geom_sf(data = med_streets$osm_lines,
                  inherit.aes = FALSE,
                  color = "black",
                  size = .3,
                  alpha = .5) +
          geom_sf(data = small_streets$osm_lines,
                  inherit.aes = FALSE,
                  color = "#666666",
                  size = .2,
                  alpha = .3) +
          geom_sf(data = big_streets$osm_lines,
                  inherit.aes = FALSE,
                  color = "black",
                  size = .5,
                  alpha = .6) +
          coord_sf(xlim = c(lon_min-.005, lon_max+.005), 
                   ylim = c(lat_min-.005, lat_max+.005),
                   expand = FALSE) +
          geom_point(data=forGGPLOT2, aes(x=longitude, y=latitude, colour=suburb), 
                     size = 1, alpha=.6, inherit.aes = F) +
          scale_color_manual(values=c("aquamarine","chartreuse","blue","brown1",
                                      "cyan","darkgreen","firebrick","darkolivegreen1",
                                      "darkorchid","darksalmon","gold","darkseagreen",
                                      "deeppink","green","navy","orange",
                                      "tomato","tan1","brown","springgreen",
                                      "yellow","skyblue","seagreen1","orangered",
                                      "purple","sienna1","dodgerblue")) +
          labs(title = "Pittsburgh", subtitle = "Sold Houses by Suburb") 
ggsave(file="Data/suburb_map.pdf",plot=theMap, units="in", width = 18, height=25)

##The entirety of the NAs are Wilkinsburg... they can all be removed along with the other suburbs I'm not looking in
theDataset$suburb = ifelse(is.na(theDataset$suburb) & theDataset$longitude >= -79.9 & theDataset$latitude < 40.46,"Wilkinsburg",theDataset$suburb)

theDataset2 = theDataset %>% 
                filter(suburb %in% c("Swisshelm Park","Swissvale","Regent Square","Edgewood",
                                      "Squirrel Hill North","Squirrel Hill South","Point Breeze",
                                      "Highland Park") &
                       !is.na(longitude)) %>%
                filter(longitude <= -79.885)
              
theMap2 = ggplot() +
            geom_sf(data = river$osm_lines,
                    inherit.aes = FALSE,
                    color = "steelblue",
                    size = .8,
                    alpha = .3) +
            geom_sf(data = railway$osm_lines,
                    inherit.aes = FALSE,
                    color = "black",
                    size = .2,
                    linetype="dotdash",
                    alpha = .5) +
            geom_sf(data = med_streets$osm_lines,
                    inherit.aes = FALSE,
                    color = "black",
                    size = .3,
                    alpha = .5) +
            geom_sf(data = small_streets$osm_lines,
                    inherit.aes = FALSE,
                    color = "#666666",
                    size = .2,
                    alpha = .3) +
            geom_sf(data = big_streets$osm_lines,
                    inherit.aes = FALSE,
                    color = "black",
                    size = .5,
                    alpha = .6) +
            coord_sf(xlim = c(lon_min-.005, lon_max+.005), 
                     ylim = c(lat_min-.005, lat_max+.005),
                     expand = FALSE) +
            geom_point(data=theDataset2, aes(x=longitude, y=latitude, colour=suburb), 
                       size = 1, alpha=.6, inherit.aes = F) +
            scale_color_manual(values=c("blue","brown1","cyan","darkgreen","firebrick",
                                        "navy","purple","sienna1")) +
            labs(title = "Pittsburgh", subtitle = "Sold Houses by Suburb") 
ggsave(file="Data/suburb_map_filtered.pdf",plot=theMap2, units="in", width = 18, height=25)

#clean lotArea - convert all to sqft
theDataset$lotArea = ifelse((is.na(theDataset$lotArea) | is.na(theDataset$lotAreaUnits)),NA,
                                ifelse(theDataset$lotAreaUnits=="acres",theDataset$lotArea*43560,theDataset$lotArea))

#Clean hasCooling/cooling
theDataset$hasCooling = ifelse(theDataset$hasCooling==TRUE,1,0)
theDataset$hasCooling[is.na(theDataset$hasCooling)] = 0
theDataset$CentralAir = ifelse(str_detect(theDataset$cooling,"Central"),1,0)
theDataset$WindowWallAir = ifelse((str_detect(theDataset$cooling,"Window") | str_detect(theDataset$cooling,"Wall")),1,0)

#Clean heating
theDataset$heatingGas = ifelse(theDataset$heating=="",NA,ifelse(str_detect(theDataset$heating,"Gas"),1,0))
theDataset$heatingElectric = ifelse(theDataset$heating=="",NA,ifelse(str_detect(theDataset$heating,"Electric"),1,0))
theDataset$heatingForcedAir = ifelse(theDataset$heating=="",NA,ifelse(str_detect(theDataset$heating,"Forced"),1,0))
theDataset$heatingRadiant = ifelse(theDataset$heating=="",NA,ifelse(str_detect(theDataset$heating,"Radiant"),1,0))

#Clean parkingLocation
theDataset$parkingStreet = ifelse((str_detect(toupper(theDataset$parkingLocation),"ON STREET") |
                                   str_detect(toupper(theDataset$parkingLocation),"ON-STREET")),1,0)
theDataset$parkingOffStreet = ifelse((str_detect(toupper(theDataset$parkingLocation),"OFF STREET") |
                                      str_detect(toupper(theDataset$parkingLocation),"OFF-STREET") |
                                      str_detect(toupper(theDataset$parkingLocation),"GARAGE") |
                                      str_detect(toupper(theDataset$parkingLocation),"ATTACHED") |
                                      str_detect(toupper(theDataset$parkingLocation),"DETACHED") |
                                      str_detect(toupper(theDataset$parkingLocation),"COVERED")),1,0)                                  
theDataset$parkingGarage = ifelse(str_detect(toupper(theDataset$parkingLocation),"GARAGE"),1,0)
theDataset$parkingCovered = ifelse(str_detect(toupper(theDataset$parkingLocation),"COVERED"),1,0)
theDataset$parkingAttached = ifelse(str_detect(toupper(theDataset$parkingLocation),"ATTACHED"),1,0)
theDataset$parkingDetached = ifelse(str_detect(toupper(theDataset$parkingLocation),"DETACHED"),1,0)   

#Clean flooring
theDataset$flooringHardwood = ifelse(str_detect(toupper(theDataset$flooring),"WOOD"),1,0)
theDataset$flooringTile = ifelse(str_detect(toupper(theDataset$flooring),"TILE"),1,0)
theDataset$flooringCarpet = ifelse(str_detect(toupper(theDataset$flooring),"CARPET"),1,0)
theDataset$flooringVinyl = ifelse(str_detect(toupper(theDataset$flooring),"VINYL"),1,0)
theDataset$flooringLaminate = ifelse(str_detect(toupper(theDataset$flooring),"LAMINATE"),1,0)

#Clean view
theDataset$view = ifelse(theDataset$view==TRUE,1,0)
theDataset$view[is.na(theDataset$view)] = 0

#Clean roof
theDataset$roof[theDataset$roof == ""] = NA

#Calculate days to sale
theDataset$soldDate = as.Date(theDataset$soldDate)
theDataset$listDate = as.Date(theDataset$listDate)
theDataset$daysToSale = ifelse((!is.na(theDataset$soldDate) & !is.na(theDataset$listDate)),theDataset$soldDate - theDataset$listDate,NA)

#Calculate sale differential
theDataset$sellDiff = ifelse((!is.na(theDataset$soldPrice) & !is.na(theDataset$listPrice)),theDataset$soldPrice - theDataset$listPrice,NA)
theDataset$sellDiffPrct = ifelse((!is.na(theDataset$sellDiff) & !is.na(theDataset$listPrice)),theDataset$sellDiff / theDataset$listPrice,NA)

#Pull out key phrases to watch for in description
theDataset$phraseOld = ifelse((str_detect(toupper(theDataset$description),"ORIGINAL") | 
                               str_detect(toupper(theDataset$description),"VINTAGE") |
                               str_detect(toupper(theDataset$description),"TLC") |
                               str_detect(toupper(theDataset$description),"STARTER") |
                               str_detect(toupper(theDataset$description),"MAINTAINED") |
                               str_detect(toupper(theDataset$description),"POTENTIAL")),1,0)
theDataset$phraseSmall = ifelse((str_detect(toupper(theDataset$description),"COZY") | 
                                 str_detect(toupper(theDataset$description),"DOLLHOUSE") |
                                 str_detect(toupper(theDataset$description),"COTTAGE") |
                                 str_detect(toupper(theDataset$description),"CHARMING") |
                                 str_detect(toupper(theDataset$description),"QUAINT")),1,0)
theDataset$phraseCustom = ifelse((str_detect(toupper(theDataset$description),"CUSTOM") | 
                                  str_detect(toupper(theDataset$description),"UNIQUE")),1,0)
theDataset$phraseStainless = ifelse(str_detect(toupper(theDataset$description),"STAINLESS STEEL"),1,0)
theDataset$phraseRenovated = ifelse(str_detect(toupper(theDataset$description),"RENOVATED"),1,0)
theDataset$phraseOpen = ifelse(str_detect(toupper(theDataset$description),"OPEN"),1,0)
theDataset$phraseLocation = ifelse(str_detect(toupper(theDataset$description),"LOCATION"),1,0)
theDataset$phraseExclamation = ifelse(str_detect(toupper(theDataset$description),"!"),1,0)

#Remove unneccessary variables and write out file
cleanedDataset = theDataset %>% select(-c("status","isZillowOwned","lotAreaUnits","hoaFee","garageCapacity",
                                          "basement","porch"))
write.csv(cleanedDataset,"Data/cleanedFinalDataset.csv",row.names = F)

