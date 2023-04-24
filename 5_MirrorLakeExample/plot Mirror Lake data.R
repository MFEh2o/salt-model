#Plot Mirror Lake outlet chloride concentrations
#CTS 6 Dec 2022

library(dplyr)
library(lubridate)
library(ggplot2)

#---- Load 'historical' Mirror Lake data ----

#These data are posted on EDI, and the code below (to next section header) is the code
#posted on EDI for bringing the data into R.
#Date range here is 1967-06-28 through 2021-12-28

# Package ID: knb-lter-hbr.86.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Mirror Lake outlet streamwater 1967 â 2021.
# Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/86/7/9cc548168a3dfec8c2bbae7855300f96" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "ws",     
                 "yr",     
                 "mo",     
                 "dy",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "Na",     
                 "NH4",     
                 "pH",     
                 "SO4",     
                 "NO3",     
                 "Cl",     
                 "PO4",     
                 "SiO2"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$ws)!="factor") dt1$ws<- as.factor(dt1$ws)
if (class(dt1$yr)!="factor") dt1$yr<- as.factor(dt1$yr)
if (class(dt1$mo)!="factor") dt1$mo<- as.factor(dt1$mo)
if (class(dt1$dy)!="factor") dt1$dy<- as.factor(dt1$dy)
if (class(dt1$Ca)=="factor") dt1$Ca <-as.numeric(levels(dt1$Ca))[as.integer(dt1$Ca) ]               
if (class(dt1$Ca)=="character") dt1$Ca <-as.numeric(dt1$Ca)
if (class(dt1$Mg)=="factor") dt1$Mg <-as.numeric(levels(dt1$Mg))[as.integer(dt1$Mg) ]               
if (class(dt1$Mg)=="character") dt1$Mg <-as.numeric(dt1$Mg)
if (class(dt1$K)=="factor") dt1$K <-as.numeric(levels(dt1$K))[as.integer(dt1$K) ]               
if (class(dt1$K)=="character") dt1$K <-as.numeric(dt1$K)
if (class(dt1$Na)=="factor") dt1$Na <-as.numeric(levels(dt1$Na))[as.integer(dt1$Na) ]               
if (class(dt1$Na)=="character") dt1$Na <-as.numeric(dt1$Na)
if (class(dt1$NH4)=="factor") dt1$NH4 <-as.numeric(levels(dt1$NH4))[as.integer(dt1$NH4) ]               
if (class(dt1$NH4)=="character") dt1$NH4 <-as.numeric(dt1$NH4)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$SO4)=="factor") dt1$SO4 <-as.numeric(levels(dt1$SO4))[as.integer(dt1$SO4) ]               
if (class(dt1$SO4)=="character") dt1$SO4 <-as.numeric(dt1$SO4)
if (class(dt1$NO3)=="factor") dt1$NO3 <-as.numeric(levels(dt1$NO3))[as.integer(dt1$NO3) ]               
if (class(dt1$NO3)=="character") dt1$NO3 <-as.numeric(dt1$NO3)
if (class(dt1$Cl)=="factor") dt1$Cl <-as.numeric(levels(dt1$Cl))[as.integer(dt1$Cl) ]               
if (class(dt1$Cl)=="character") dt1$Cl <-as.numeric(dt1$Cl)
if (class(dt1$PO4)=="factor") dt1$PO4 <-as.numeric(levels(dt1$PO4))[as.integer(dt1$PO4) ]               
if (class(dt1$PO4)=="character") dt1$PO4 <-as.numeric(dt1$PO4)
if (class(dt1$SiO2)=="factor") dt1$SiO2 <-as.numeric(levels(dt1$SiO2))[as.integer(dt1$SiO2) ]               
if (class(dt1$SiO2)=="character") dt1$SiO2 <-as.numeric(dt1$SiO2)

# Convert Missing Values to NA for non-dates

dt1$Ca <- ifelse((trimws(as.character(dt1$Ca))==trimws("-3")),NA,dt1$Ca)               
suppressWarnings(dt1$Ca <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$Ca))==as.character(as.numeric("-3"))),NA,dt1$Ca))
dt1$Mg <- ifelse((trimws(as.character(dt1$Mg))==trimws("-3")),NA,dt1$Mg)               
suppressWarnings(dt1$Mg <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$Mg))==as.character(as.numeric("-3"))),NA,dt1$Mg))
dt1$K <- ifelse((trimws(as.character(dt1$K))==trimws("-3")),NA,dt1$K)               
suppressWarnings(dt1$K <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$K))==as.character(as.numeric("-3"))),NA,dt1$K))
dt1$Na <- ifelse((trimws(as.character(dt1$Na))==trimws("-3")),NA,dt1$Na)               
suppressWarnings(dt1$Na <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$Na))==as.character(as.numeric("-3"))),NA,dt1$Na))
dt1$NH4 <- ifelse((trimws(as.character(dt1$NH4))==trimws("-3")),NA,dt1$NH4)               
suppressWarnings(dt1$NH4 <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$NH4))==as.character(as.numeric("-3"))),NA,dt1$NH4))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("-3")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("-3"))),NA,dt1$pH))
dt1$SO4 <- ifelse((trimws(as.character(dt1$SO4))==trimws("-3")),NA,dt1$SO4)               
suppressWarnings(dt1$SO4 <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$SO4))==as.character(as.numeric("-3"))),NA,dt1$SO4))
dt1$NO3 <- ifelse((trimws(as.character(dt1$NO3))==trimws("-3")),NA,dt1$NO3)               
suppressWarnings(dt1$NO3 <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$NO3))==as.character(as.numeric("-3"))),NA,dt1$NO3))
dt1$Cl <- ifelse((trimws(as.character(dt1$Cl))==trimws("-3")),NA,dt1$Cl)               
suppressWarnings(dt1$Cl <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$Cl))==as.character(as.numeric("-3"))),NA,dt1$Cl))
dt1$PO4 <- ifelse((trimws(as.character(dt1$PO4))==trimws("-3")),NA,dt1$PO4)               
suppressWarnings(dt1$PO4 <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$PO4))==as.character(as.numeric("-3"))),NA,dt1$PO4))
dt1$SiO2 <- ifelse((trimws(as.character(dt1$SiO2))==trimws("-3")),NA,dt1$SiO2)               
suppressWarnings(dt1$SiO2 <- ifelse(!is.na(as.numeric("-3")) & (trimws(as.character(dt1$SiO2))==as.character(as.numeric("-3"))),NA,dt1$SiO2))



#---Clean historical data ----

#Check structure
str(dt1)

#Create a date column
dt1$date <- paste(dt1$yr,dt1$mo,dt1$dy,sep='-')
#And convert to date
dt1$date <- ymd(dt1$date)


#---- Plot data ----

#Plot
ggplot(dt1) +
  geom_point(mapping=aes(x=date,y=Cl)) +
  theme_bw()

#Plot just the date range used in Likens and Buso 2009
#Set x axis limits to include full range of data through 2021
ggplot(filter(dt1,year(date)<2008)) +
  geom_point(mapping=aes(x=date,y=Cl)) +
  labs(x=NULL,y=expression(Cl^'-'~'('~mg~L^-1~')')) +
  scale_x_date(limits=as.Date(c(NA,'2021-12-31'))) +
  theme_bw()

#Plot for manuscript

#First open jpg graphics device
jpeg(filename='chloride Mirror Lake outlet 1967-2021.jpeg',width=3.5,height=3.5,units='in',res=500)

#Plot
ggplot() +
  geom_point(data=filter(dt1,year(date)<2008),mapping=aes(x=date,y=Cl),col=1) +
  geom_point(data=filter(dt1,year(date)>=2008),mapping=aes(x=date,y=Cl),col=2) +
  labs(x=NULL,y=expression(Cl^'-'~'('~mg~L^-1~')')) +
  theme_bw()

#Close device
dev.off()
