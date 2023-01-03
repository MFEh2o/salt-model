#Calculate road density
#CTS 6 Dec 2022

library(tigris)
library(sf)
library(dplyr)
library(ggplot2)

#---- Initial messing around ----

#Load 

dRoads <- roads(state="NY",county="Dutchess",year=2022)

attributes(dRoads)

plot(dRoads)

unique(dRoads$RTTYP)

dCounties <- counties(state="NY",year=2022)

attributes(dCounties)

head(dCounties)

filter(dCounties,NAME=='Dutchess')

#land area of the county is in the ALAND column.
#water area is in AWATER column.
#These are both in m2

##
#Subset dRoads to include only the following types:
# - S1100 Primary roads
# - S1200 Secondary roads
# - S1400 Local neighborhood roads, rural roads, city streets
# - S1500 Vehicular trail (4WD) <actually, drop this one too - presumably little salt application>
# - S1630 Ramp
# - S1640 Service drive
# - S1780 Parking lot road

#Roads to keep, including assumed number of lanes per road
roadsToKeep <- data.frame(MTFCC=c('S1100','S1200','S1400','S1630','S1640','S1780'),nLanes=c(4,2,2,1,2,2))

#Merge dRoads with roadsToKeep to get dRoadsSub, including only roads that we want to keep amd assumed number of lanes
dRoadsSub <- inner_join(dRoads,roadsToKeep,by='MTFCC')



##
#An example calculation, for Dutchess County, NY
#Get the area
countyArea <- st_area(filter(dCounties,NAME=='Dutchess'))

#Get the length of each road segment (ignoring number of lanes)
dRoadsSub$roadLength <- st_length(dRoadsSub)

#Multiply by number of lanes to get lane-meters for each road segment
dRoadsSub$laneMeters <- dRoadsSub$nLanes*dRoadsSub$roadLength

#Sum across all roads in county
totalLaneMeters <- sum(dRoadsSub$laneMeters)

#Divide total lane meters by county area to get road density (lane-meters per m2)
roadDensity <- totalLaneMeters/countyArea
roadDensity

##
#Similar, for Manhattan ("New York" county)
dRoads <- roads(state="NY",county="New York",year=2022)
dRoadsSub <- inner_join(dRoads,roadsToKeep,by='MTFCC')
countyArea <- st_area(filter(dCounties,NAME=='New York'))
dRoadsSub$roadLength <- st_length(dRoadsSub)
dRoadsSub$laneMeters <- dRoadsSub$nLanes*dRoadsSub$roadLength
totalLaneMeters <- sum(dRoadsSub$laneMeters)
roadDensity <- totalLaneMeters/countyArea
roadDensity

##
#Similar, for Essex County
dRoads <- roads(state="NY",county="Essex",year=2022)
dRoadsSub <- inner_join(dRoads,roadsToKeep,by='MTFCC')
countyArea <- st_area(filter(dCounties,NAME=='Essex'))
dRoadsSub$roadLength <- st_length(dRoadsSub)
dRoadsSub$laneMeters <- dRoadsSub$nLanes*dRoadsSub$roadLength
totalLaneMeters <- sum(dRoadsSub$laneMeters)
roadDensity <- totalLaneMeters/countyArea
roadDensity


#---- Calculate across all counties in a state ----

##
#For New York

#Identify state
state <- 'NY'

#Get list of counties and associated data
dCounties <- counties(state=state,year=2022)

#Roads to keep, including assumed number of lanes per road
roadsToKeep <- data.frame(MTFCC=c('S1100','S1200','S1400','S1630','S1640','S1780'),nLanes=c(4,2,2,1,2,2))

#Set up output
dOut <- data.frame(state=rep(state,dim(dCounties)[1]),county=dCounties$NAME,roadDensity=numeric(dim(dCounties)[1]))

#Loop over counties, pulling out data and doing calculation
for (i in 1:dim(dCounties)[1]) {
  
  #Identify county
  county <- dCounties$NAME[i]
  
  #Get roads data for that county
  dRoads <- roads(state=state,county=county,year=2022)
  
  #Merge dRoads with roadsToKeep to get dRoadsSub, including only roads that we want to keep and assumed number of lanes
  dRoadsSub <- inner_join(dRoads,roadsToKeep,by='MTFCC')
  
  #Calculate total lane-meters of road in county
  dRoadsSub$roadLength <- st_length(dRoadsSub)
  dRoadsSub$laneMeters <- dRoadsSub$nLanes*dRoadsSub$roadLength
  totalLaneMeters <- sum(dRoadsSub$laneMeters)
  
  #Calculate county area
  countyArea <- st_area(filter(dCounties,NAME==county))
  
  #Calculate road density, m m-2
  roadDensity <- totalLaneMeters/countyArea
  
  #Save result
  dOut[i,'roadDensity'] <- roadDensity
}

#Look at result, sorted in order of increasing density
arrange(dOut,roadDensity)

#Re-name result so as not to write over it below
dOutNY <- dOut

#Histogram of road density in NY counties
hist(dOutNY$roadDensity,main="",xlab=expression(Road~density~'('*lane*'-'*m~m^-2*')'))


##
#Same, for Massachusetts

#Identify state
state <- 'MA'

#Get list of counties and associated data
dCounties <- counties(state=state,year=2022)

#Roads to keep, including assumed number of lanes per road
roadsToKeep <- data.frame(MTFCC=c('S1100','S1200','S1400','S1630','S1640','S1780'),nLanes=c(4,2,2,1,2,2))

#Set up output
dOut <- data.frame(state=rep(state,dim(dCounties)[1]),county=dCounties$NAME,roadDensity=numeric(dim(dCounties)[1]))

#Loop over counties, pulling out data and doing calculation
for (i in 1:dim(dCounties)[1]) {
  
  #Identify county
  county <- dCounties$NAME[i]
  
  #Get roads data for that county
  dRoads <- roads(state=state,county=county,year=2022)
  
  #Merge dRoads with roadsToKeep to get dRoadsSub, including only roads that we want to keep and assumed number of lanes
  dRoadsSub <- inner_join(dRoads,roadsToKeep,by='MTFCC')
  
  #Calculate total lane-meters of road in county
  dRoadsSub$roadLength <- st_length(dRoadsSub)
  dRoadsSub$laneMeters <- dRoadsSub$nLanes*dRoadsSub$roadLength
  totalLaneMeters <- sum(dRoadsSub$laneMeters)
  
  #Calculate county area
  countyArea <- st_area(filter(dCounties,NAME==county))
  
  #Calculate road density, m m-2
  roadDensity <- totalLaneMeters/countyArea
  
  #Save result
  dOut[i,'roadDensity'] <- roadDensity
}

#Look at result, sorted in order of increasing density
arrange(dOut,roadDensity)