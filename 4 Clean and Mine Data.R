#Project: Zillow Tracker
#Code: 4 Clean and Mine Data
#Author: Scott Onestak
#Last Executed: 1/30/2022

#Packages
library(tidyverse)
library(ggplot2)
library(ggmap)
library(osmdata)
library(rvest)

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
