#Plot Mirror Lake outlet chloride concentrations
#CTS 6 Dec 2022

library(dplyr)
library(lubridate)
library(ggplot2)

#---- Load 'historical' Mirror Lake data ----

#These data are posted on EDI, and the code below (to next section header) is the code
#posted on EDI for bringing the data into R.
#Date range here is 1967-06-28 through 2010-10-29

# Package ID: knb-lter-hbr.86.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hubbard Brook Experimental Forest: Chemistry of Mirror     Lake outlet streamwater 1967 - 2010.
# Data set creator:  Gene E. Likens - Cary Institute of Ecosystem Studies 
# Metadata Provider:    - Hubbard Brook Experimental Forest LTER 
# Contact:    - Information Manager, Hubbard Brook LTER   - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/86/6/9cc548168a3dfec8c2bbae7855300f96" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep="," 
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

dt1$Ca <- ifelse((trimws(as.character(dt1$Ca))==trimws("-3.000")),NA,dt1$Ca)               
suppressWarnings(dt1$Ca <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$Ca))==as.character(as.numeric("-3.000"))),NA,dt1$Ca))
dt1$Mg <- ifelse((trimws(as.character(dt1$Mg))==trimws("-3.000")),NA,dt1$Mg)               
suppressWarnings(dt1$Mg <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$Mg))==as.character(as.numeric("-3.000"))),NA,dt1$Mg))
dt1$K <- ifelse((trimws(as.character(dt1$K))==trimws("-3.000")),NA,dt1$K)               
suppressWarnings(dt1$K <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$K))==as.character(as.numeric("-3.000"))),NA,dt1$K))
dt1$Na <- ifelse((trimws(as.character(dt1$Na))==trimws("-3.000")),NA,dt1$Na)               
suppressWarnings(dt1$Na <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$Na))==as.character(as.numeric("-3.000"))),NA,dt1$Na))
dt1$NH4 <- ifelse((trimws(as.character(dt1$NH4))==trimws("-3.000")),NA,dt1$NH4)               
suppressWarnings(dt1$NH4 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$NH4))==as.character(as.numeric("-3.000"))),NA,dt1$NH4))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("-3.00")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("-3.00")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("-3.00"))),NA,dt1$pH))
dt1$SO4 <- ifelse((trimws(as.character(dt1$SO4))==trimws("-3.000")),NA,dt1$SO4)               
suppressWarnings(dt1$SO4 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$SO4))==as.character(as.numeric("-3.000"))),NA,dt1$SO4))
dt1$NO3 <- ifelse((trimws(as.character(dt1$NO3))==trimws("-3.000")),NA,dt1$NO3)               
suppressWarnings(dt1$NO3 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$NO3))==as.character(as.numeric("-3.000"))),NA,dt1$NO3))
dt1$Cl <- ifelse((trimws(as.character(dt1$Cl))==trimws("-3.000")),NA,dt1$Cl)               
suppressWarnings(dt1$Cl <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$Cl))==as.character(as.numeric("-3.000"))),NA,dt1$Cl))
dt1$PO4 <- ifelse((trimws(as.character(dt1$PO4))==trimws("-3.000")),NA,dt1$PO4)               
suppressWarnings(dt1$PO4 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$PO4))==as.character(as.numeric("-3.000"))),NA,dt1$PO4))
dt1$SiO2 <- ifelse((trimws(as.character(dt1$SiO2))==trimws("-3.000")),NA,dt1$SiO2)               
suppressWarnings(dt1$SiO2 <- ifelse(!is.na(as.numeric("-3.000")) & (trimws(as.character(dt1$SiO2))==as.character(as.numeric("-3.000"))),NA,dt1$SiO2))


#---Clean historical data ----

#Check structure
str(dt1)

#Problem with 'ws' column? Metadata says this indicates watershed and is designated as '70',
#but there are also values of 20 and 72 in here
table(dt1$ws)
#Only 1 of each of those values. Presumably just typos - leave in.

#Create a date column
dt1$date <- paste(dt1$yr,dt1$mo,dt1$dy,sep='-')
#And convert to date
dt1$date <- ymd(dt1$date)


#---- Plot historical data ----

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


#---- Load data since mid-2013 ----

#These data are from the HBWatER internal data portal. They are not yet publicly available.

#Read 'current' HBWatER data, June 2013 through end of 2021
dCurrent <- read.csv('HBEFdata_Current_2022-10-28.csv')

#From dCurrent, keep Mirror Lake outlet observations (site=ML70) and only those columns needed
dCurrent <- dCurrent %>%
  filter(site=='ML70') %>%
  select(date,Cl)

#Convert date column to date class
dCurrent$date <- ymd(dCurrent$date)


#---- Load and clean 2010-2013 data ----

#This file is from Brenda Minicucci and includes 2010-2014 data. Apparently this got lost in the shuffle when
#Don Buso retired; it is not yet available even on the HBWatER internal portal.

#Read file and look at structure
dGap <- read.csv('ml70-xls  6-2010 to 6-2014.csv')
str(dGap)

#Create date column by pasting YEAR, MONTH, DAY
dGap$date <- paste(dGap$YEAR,dGap$MONTH,dGap$DAY,sep='-')
#And convert to date
dGap$date <- ymd(dGap$date)

#Keep only those columns needed
dGap <- select(dGap,date,Cl=CL_CONC)

#Quick check: where dates overlap between dCurrent and dGap, are Cl concentrations the same?
plot(Cl~date,data=dCurrent,xlim=as.Date(c('2013-01-01','2015-01-01')))
points(Cl~date,data=dGap,col='red')
#Looks like yes, and like there are some dates in dGap that aren't in dCurrent even during
#the period of overlap.

#So - to put together complete data set, use historical data until that ends, then dGap
#until that ends, then dCurrent.


#---- Combine data.frames to get all data together ----

#Get rid of extra columns from dt1
dt1 <- select(dt1,date,Cl)

#Date ranges
range(dt1$date)
range(dGap$date)
range(dCurrent$date)

#Check the 2010-06-07 date in dt1 and dGap
filter(dt1,date==as.Date('2010-06-07'))
filter(dGap,date==as.Date('2010-06-07'))
#Same. Drop from dGap
dGap <- filter(dGap,date!=as.Date('2010-06-07'))

#Combine dt1 and dGap
dAll <- rbind(dt1,dGap)

#Drop rows from dCurrent that are before the end of dGap (and dAll)
dCurrent <- filter(dCurrent,date>range(dAll$date)[2])

#Combine dCurrent into dAll
dAll <- rbind(dAll,dCurrent)


#---- Plot all data ----

#Use hollow points for data after 2007

#First open jpg graphics device
jpeg(filename='chloride Mirror Lake outlet 1967-2021.jpeg',width=3.5,height=3.5,units='in',res=300)

#Plot
ggplot() +
  geom_point(data=filter(dAll,year(date)<2008),mapping=aes(x=date,y=Cl),col=1) +
  geom_point(data=filter(dAll,year(date)>=2008),mapping=aes(x=date,y=Cl),col=2) +
  labs(x=NULL,y=expression(Cl^'-'~'('~mg~L^-1~')')) +
  theme_bw()

#Close device
dev.off()


