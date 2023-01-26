#Simple salt model results
#CTS 21 Oct 2022

library(tidyverse)
library(gridExtra)
library(deSolve)
library(tigris)
library(sf)
library(ggspatial)
library(MetBrewer)
library(raster)
library(patchwork)
library(USAboundaries)
library(feather)

source('dSalt.r')
source('dSaltForce.r')


#---- Road density ----

#Overview: loop across states in CONUS, and then across ~county-level areas within each state.
#Use the county-level FIPS code to identify areas. This is actually county in some states, but
#some other administrative unit in other states (e.g. CT; see help for tigris::counties for
#details).
#For each ~county, calculate road density.
#Use the 2020 data. Originally used 2022, but ran into a problem with CT (and perhaps other
#states?) - COUNTYFP had been updated to reflect non-county units if you pulled data using 
#counties(), but these COUNTYFP FIPS codes were not recognized as valid by roads(), which I
#think expected the original county codes.

#Roads to keep, including assumed number of lanes per road
roadsToKeep <- data.frame(MTFCC=c('S1100','S1200','S1400','S1630','S1640','S1780'),nLanes=c(4,2,2,1,2,2))
#Use only the following types of road:
# - S1100 Primary roads
# - S1200 Secondary roads
# - S1400 Local neighborhood roads, rural roads, city streets
# - S1500 Vehicular trail (4WD) <actually, drop this one too - presumably little salt application>
# - S1630 Ramp
# - S1640 Service drive
# - S1780 Parking lot road
#And assume that average number of lanes for each of these road types is 4,2,2,1,2,2.

#List of all states in CONUS
conusStates <- state.abb[-which(state.abb%in%c('AK','HI'))]


##
#Code commented out over next ~45 lines does the calculation of road density for each
#county in CONUS. Last line commented out save the resulting "dDens" data.frame. If running
#this code for first time (or after a change), uncomment and re-save dDens. Otherwise, can
#just load the saved dDens object.

# #Set up output
# dDens <- data.frame(state=character(0),county=character(0),roadDensity=numeric(0))
# 
# #Loop over states
# for (s in 1:length(conusStates)) {
#   
#   #Identify which state to use
#   state <- conusStates[s]
#   
#   #Get set of ~counties for this state
#   dCounties <- counties(state=state,year=2020)
#   
#   #Loop over counties, pulling out data and doing calculation
#   for (i in 1:dim(dCounties)[1]) {
#     
#     #Identify county
#     countyFP <- dCounties$COUNTYFP[i]
#     countyName <- dCounties$NAME[i]
#     
#     #Get roads data for that county
#     dRoads <- roads(state=state,county=countyFP,year=2020)
#     
#     #Merge dRoads with roadsToKeep to get dRoadsSub, including only roads that we want to keep and assumed number of lanes
#     dRoadsSub <- inner_join(dRoads,roadsToKeep,by='MTFCC')
#     
#     #Calculate total lane-meters of road in county
#     dRoadsSub$roadLength <- st_length(dRoadsSub)
#     dRoadsSub$laneMeters <- dRoadsSub$nLanes*dRoadsSub$roadLength
#     totalLaneMeters <- sum(dRoadsSub$laneMeters)
#     
#     #Calculate county area
#     countyArea <- st_area(filter(dCounties,NAME==countyName))
#     
#     #Calculate road density, m m-2
#     roadDensity <- totalLaneMeters/countyArea
#     
#     #Save result
#     dDensTemp <- data.frame(state=state,county=countyName,roadDensity=roadDensity)
#     dDens <- rbind(dDens,dDensTemp)
#     
#   }
# }
# 
# #Save the dDens object to permit skipping those (time-consuming) calculations in a subsequent session
# save(dDens,file='data outputs/dDens.RData')

#If proceeding from previously saved file, skip the for-loop above and use:
load('data outputs/dDens.RData')

#Histogram of road density
hist(dDens$roadDensity,main="",xlab=expression(Road~density~'('*lane*'-'*m~m^-2*')'))

#Looks like a couple very high values. Check these out
range(dDens$roadDensity)
dDens[which(as.numeric(dDens$roadDensity)>0.5),]
#I bet these are "independent cities" (see help for tigris::counties).
#One check: do these county names show up more than once?
filter(dDens,state=='VA',county=='Franklin')
filter(dDens,state=='VA',county=='Fairfax')
#Yes. Drop these two rows for simplicity.
dDens <- filter(dDens,as.numeric(roadDensity)<0.5)
#Histogram again
hist(dDens$roadDensity,main="",xlab=expression(Road~density~'('*lane*'-'*m~m^-2*')'))
#A few of remaining values still look unreasonably high, given that Manhattan (New York
#County, NY) is around 0.03.
dDens[which(as.numeric(dDens$roadDensity)>0.03),]
#I bet all of these places with values in excess of New York County are also "independent
#cities". Drop.
dDens <- filter(dDens,as.numeric(roadDensity)<0.031)
#Probably still some independent cities and other geographic oddities in the data set,
#but at least now the top end of the distribution is probably a fairly realistic top end
#for road density at county level. Proceed.

#Histogram again
hist(dDens$roadDensity,main="",xlab=expression(Road~density~'('*lane*'-'*m~m^-2*')'))

#Tidier version of figure - used later in creating Fig. 2
g1 <- ggplot(dDens) +
  geom_histogram(mapping=aes(x=as.numeric(roadDensity))) +
  scale_x_log10(breaks=c(0.00010,0.001,0.01),
                minor_breaks=c(seq(0.0002,0.0009,0.0001),seq(0.002,0.009,0.001),0.02,0.03),
                labels=scales::number_format(accuracy = 0.0001)) +
  labs(x=expression(Road~density~'('*'lane-m'~m^-2*')'),y='Number of counties') +
  theme_bw()
g1


#---- Model Application 1: Solve through time for a few scenarios ----

#Initial conditions for all simulations
inits <- c(SW=0,SL=0)

#Time steps to run simulations for
times <- seq(0,100,1)


##
#Scenario 1 - high relative yield from watershed

#Salt yield of watershed per unit of precipitation, m-1
phi <- 0.1

#Application rate of road salt, kg (lane-m)-1 y-1
alpha <- 5
#Road density, lane-m m-2
delta <- 0.010
#Watershed area, m2 (note 1e6 m2 is 1 km2)
A <- 1e6
#Precipitation (net of evapotranspiration), m y-1
p <- 0.25
#Lake volume, m3
#Assume for now a 1 ha lake with mean depth 5 m
V <- 50000

#Gather parameters
pars <- c(phi=phi,alpha=alpha,delta=delta,A=A,p=p,V=V)

#Solve through time
s1 <- ode(inits,times,dSalt,pars)


##
#Scenario 2 - low relative yield from watershed

#Salt yield of watershed per unit of precipitation, m-1
phi <- 0.025

#Application rate of road salt, kg (lane-m)-1 y-1
alpha <- 5
#Road density, lane-m m-2
delta <- 0.010
#Watershed area, m2 (note 1e6 m2 is 1 km2)
A <- 1e6
#Precipitation (net of evapotranspiration), m y-1
p <- 0.25
#Lake volume, m3
#Assume for now a 1 ha lake with mean depth 5 m
V <- 50000

#Gather parameters
pars <- c(phi=phi,alpha=alpha,delta=delta,A=A,p=p,V=V)

#Solve through time
s2 <- ode(inits,times,dSalt,pars)


##
#Scenario 3 - application rate increasing

#For this one have to use dSaltForce, which allows for forcing function on alpha

#Salt yield of watershed per unit of precipitation, m-1
phi <- 0.1

#Approximation function for application rate of road salt, kg (lane-m)-1 y-1
#Assume that application rate starts at 5 kg (lane-m)-1 y-1 and increases at 1% y-1
x <- seq(1,100,1)
alpha <- approxfun(x=x,y=5*1.01^(x-1),method='linear',rule=2)

#Road density, lane-m m-2
delta <- 0.010
#Watershed area, m2 (note 1e6 m2 is 1 km2)
A <- 1e6
#Precipitation (net of evapotranspiration), m y-1
p <- 0.25
#Lake volume, m3
#Assume for now a 1 ha lake with mean depth 5 m
V <- 50000

#Gather parameters
pars <- c(phi=phi,delta=delta,A=A,p=p,V=V)

#Solve through time
s3 <- ode(inits,times,dSaltForce,pars)


##
#Gather and plot results from scenarios

#Get salt mass in lake from each scenario and gather in one data.frame
sOut <- data.frame(year=s1[,'time'],SL1=s1[,'SL'],SL2=s2[,'SL'],SL3=s3[,'SL'])

#Calculate concentation of salt in each year for each scenario, mg L-1
sOut <- sOut %>%
  mutate(CL1=SL1/V*1000,
         CL2=SL2/V*1000,
         CL3=SL3/V*1000)

#Open graphics device
jpeg('figures/Figure 1.jpg',width=4,height=4,units='in',res=300)

#Plot
plot(CL3~year,data=sOut,type='l',lty=3,xlab='year',ylab=expression(Salt~concentration~'('*mg~Cl-~L^-1*')'))
points(CL1~year,data=sOut,type='l',lty=1)
points(CL2~year,data=sOut,type='l',lty=2)
legend('topleft',legend=c('Fast flowpaths, increasing application','Fast flowpaths, constant application','Slow flowpaths, constant application'),
       lty=c(3,1,2),bty='n',cex=0.75)

dev.off()


#---- Model Application 2: Calculate equilibrium salt concentration across parameter ranges ----

#This uses the analytical solution to the model given in dSalt.r

##
#Define parameter ranges

#Application rate of road salt, kg (lane-m)-1 y-1
#Dugan et al. 2017 PNAS Fig. 1 gives upper end of range as 35 US tons per lane mile.
#I think this is per year.
#Convert that to metric units: 35*907.185/(1.60934*1000)=19.7295.
#But this weight is measured on NaCl, not Cl-. Molar mass is 22.99 for Na, 35.453 for Cl,
#so 61% of salt mass is Cl-; that gets you to 19.7295*0.61= 12.035 kg (lane-m)-1 y-1. 
#Round to 12 kg (lane-m)-1 y-1.
#Another estimate is available from Hintz et al. 2022 Frontiers in Ecology and the Environment.
#This gives 12-75 metric tons (km two-lane highway)-1 y-1 as range for "many US states,
#Canada, and Sweden". Converting to units that we use here (including multiplying by 0.5
#to get from mass per two-lane-km to mass per lane-km, and by 0.61 to get from mass of salt
#to mass of Cl-) gives range of 3.660 to 22.875 kg
#(lane-m)-1 y-1.
#So - use range 2.5 to 25 kg (lane-m)-1 y-1
alpha <- c(2.5,5,10,15,20,25)#c(0:12)

#Road density, lane-m m-2
#See section on road density above. County-level road density in CONUS ranges from a little
#over 0.0001 to 0.0310 lane-m m-2, with most values in the range of ~0.002 to 0.008.
#Set up a series of values covering this range.
#For comparison, Dugan et al. 2017 PNAS (Fig. 3) suggests that range of road density in
#500 m buffer around the lakes they considered is from near-0 to maybe something like 20
#km km-2. This decreases at larger buffer sizes. Convert to m m-2: 0 to 20*(1000/1e6) gives
#0 to 0.020 m m-2. That does not (I think) account for multiple lanes.
delta <- c(0.0001,0.0003,0.0010,0.0030,0.0100,0.0300)

#Precipitation (net of evapotranspiration), m y-1
#From LakeATLAS data (see Model Application 3, below), take precipitation minus actual
#evapotranspiration. Values range from <0.02 m to >2 m, but all the really high ones are 
#in the Pacific Northwest. Use 0.02, 0.25, 0.50 m y-1 to approximate typical dry, mesic,
#wet conditions - roughly Montana,  Michigan, Connecticut.
p <- c(0.05,0.25,0.50)


##
#Calculate equilibrium at each parameter set

#First create structure to hold the parameter sets and results
dOut <- expand.grid(alpha=alpha,delta=delta,p=p)

#Add a column naming the levels of p as dry, mesic, wet
dOut$precipRegime <- character(length=dim(dOut)[1])
dOut$precipRegime[dOut$p==0.05] <- 'dry'
dOut$precipRegime[dOut$p==0.25] <- 'mesic'
dOut$precipRegime[dOut$p==0.50] <- 'wet'

#Calculate equilibrium concentration of salt in lake (g m-3 = mg L-1)
dOut <- dOut %>%
  mutate(CL=alpha*delta/p*1000)

##
#Plot results - Figure 2

#Create a factor version of alpha, with lowest level of factor the highest value of alpha,
#for convenience in plotting
dOut$alphaFac <- factor(dOut$alpha,levels=sort(unique(dOut$alpha),decreasing=TRUE))

#Create plot
g2 <- ggplot(filter(dOut,delta>0)) +
  geom_line(mapping=aes(x=delta,y=CL,group=alphaFac,color=alphaFac)) +
  facet_grid(cols=vars(precipRegime)) +
  labs(x=expression(Road~density~'('*'lane-m'~m^-2*')'),
       y=expression(Equilibrium~salt~concentration~'('*mg~Cl-~L^-1*')'),
       color=expression(atop(Road~salt~application~rate,'('*kg~Cl-~'('*'lane-m'*')'^-1~y^-1*')'))) +
  theme_bw() +
  theme(legend.position=c(0.85,0.75),
        legend.background=element_rect(fill='transparent'),
        legend.key.size=unit(0.75,'lines'),
        legend.text=element_text(size=9),
        legend.title = element_text(size=10)) +
  scale_y_log10(minor_breaks=c(seq(0.3,0.9,0.1),seq(2,9,1),seq(20,90,10),seq(200,900,100),seq(2000,9000,1000))) +
  scale_x_log10(labels=scales::number_format(accuracy = 0.0001)) +
  scale_color_brewer(type='seq',palette='Reds')

#Check plot
g2

#Open graphics device
jpeg('figures/Figure 2.jpg',width=8.88,height=4,units='in',res=300)

#Put g2 and g1 on same figure
grid.arrange(g2,g1,nrow=1,widths=c(3,1))

#Close device
dev.off()


#---- Model Application 3: Spatially-explicit predictions of equilibrium road salt concentration ----

# Just a helpful stackoverflow function to make case_when into factors 
# https://stackoverflow.com/questions/49572416/r-convert-to-factor-with-order-of-levels-same-with-case-when
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# Load salt data that was extract from the FALCONE dataset using LAGOS polygon overlays
# Networked watersheds
out.nws = read_csv('data/saltExtractMean_LAGOSnws.csv') |> 
  mutate(cl_kg_m2 = salt * 0.6067 * 0.453592 / 1e6 ) |> 
  mutate(spatial_division = 'nws')# convert from pounds salt per km2 to kg Cl- per m2

# Watersheds
out.ws = read_csv('data/saltExtractMean_LAGOSws.csv') |> 
  mutate(cl_kg_m2 = salt * 0.6067 * 0.453592 / 1e6 )  |> # convert from pounds salt per km2 to kg Cl- per m2
  mutate(spatial_division = 'ws') |> 
  filter(!lagoslakeid %in% out.nws$lagoslakeid) |> 
  bind_rows(out.nws) |> 
  dplyr::select(!nws_zoneid) 

# Load LAGOS lake information csv to bit for GitHub. Pared down and saved as feather. 
# lagos.info = read_csv('data/LAGOS/lake_information.csv') |> 
#   dplyr::select(lagoslakeid, lake_namegnis, lake_namelagos, lake_lat_decdeg, 
#                 lake_lon_decdeg, lake_states, ws_zoneid, nws_zoneid)
# write_feather(lagos.info, 'data/LAGOS/lake_info.feather')

lagos.info = read_feather('data/LAGOS/lake_info.feather')

# Load AET data, previously extracted from HydroBASIN matchup
aet.hb = read_csv('data/aetExtract_HydroBASIN.csv') %>% 
  dplyr::select(lagoslakeid, pre_mm_uyr:rdd_mk_uav)
#rdd_mk_uav in meters per km², road density in total watershed upstream of lake pour point
#pre_mm_uyr precipitation in total watershed upstream of lake pour point annual average
#aet_mm_uyr actual evapotranspiration in total watershed upstream of lake pour point annual average

####### Join data ########
join.df = lagos.info |> 
  left_join(out.ws) |> 
  left_join(aet.hb)


####### Calculate equilibrium concentration of salt in lake (g m-3 = mg L-1) ########
# Group by CL concentration
b <- join.df %>%
  mutate(p = (pre_mm_uyr - aet_mm_uyr)/1000) |> # precipitation in meters
  mutate(CL = cl_kg_m2/p*1000) |> # convert fom kg/m3 (aka g/L) to mg/L by dividing by 1000
  mutate(CL.group = fct_case_when(CL < 10 ~ '0-10',
                                  CL < 50 ~ '10-50',
                                  CL < 120 ~ '50-120',
                                  CL < 230 ~ '120-230',
                                  CL < 500 ~ '230-500',
                                  CL >= 500 ~ '500+')) |> 
  filter(!is.na(CL))

############# Create spatial data #############
# Convert data frame to sf object
join.df.sf <- st_as_sf(x = b, 
                       coords = c("lake_lon_decdeg", "lake_lat_decdeg"),
                       crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


# What state are all the points in? 
states_sf <- st_transform(us_states(map_date = NULL, resolution = c("low", "high"), states = NULL), crs(join.df.sf)) |> 
  dplyr::select(name, state_name, state_abbr)

b.state <- as.data.frame(st_join(join.df.sf, states_sf, join = st_intersects))
b.state.sf <- st_join(join.df.sf, states_sf, join = st_intersects)

# Write model output for faster loading
# st_write(b.state.sf, dsn = 'data/saltModeloutput_LAGOS.GeoJSON', delete_dsn = TRUE)

############# ############## MAPPING ############## ##############
## Esri basemap URLs ####
world_gray <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

m1 = ggplot() +
  annotation_map_tile(type = world_gray, zoom = 5) + # Esri Basemap
  geom_sf(data = join.df.sf |> filter(!is.na(CL)), aes(fill = CL.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 7)[-4]), 
                    name = expression(atop("Road Salt",~(mg~Cl^"-"~L^"-")))) +
  guides(fill=guide_legend(override.aes=list(size=3))) +
  theme_bw(base_size = 9) +
  theme(axis.title = element_blank(),
        legend.position = c(0.9,0.2),
        legend.background = element_blank(),
        legend.title = element_text(size = 7),
        legend.key = element_blank(),
        legend.key.height = unit(0.1,'cm'))

m2 = ggplot() +
  annotation_map_tile(type = world_gray, zoom = 8) + # Esri Basemap
  geom_sf(data = join.df.sf |> filter(!is.na(CL)), aes(fill = CL.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 7)[-4]), 
                    name = expression(atop("Road Salt",~(mg~Cl^"-"~L^"-")))) +
  theme_bw(base_size = 9) +
  coord_sf(crs = 4326, ylim = c(41.5,46), xlim = c(-89, -82)) +
  scale_x_continuous(breaks = seq(-88, -82, by = 2)) +
  theme(legend.position = 'none',
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.1,'cm'))

############# ############## State # ############## ##############

b.state.sum = b.state |> 
  filter(!is.na(CL)) |> 
  mutate(cl230 = if_else(CL >= 230, TRUE, FALSE)) |> 
  mutate(cl120 = if_else(CL >= 120, TRUE, FALSE)) |> 
  group_by(state_name, state_abbr) |> 
  mutate(n = n()) |> 
  summarise(cl.230 = sum(cl230), cl.120 = sum(cl120), n = first(n)) |> 
  mutate(prop.120 = round(cl.120/n,2), prop.230 = round(cl.230/n,2)) |> 
  filter(!is.na(state_name)) |> 
  filter(cl.120 >= 40) |> # filter to states with more than 5 lakes
  pivot_longer(cols = cl.230:cl.120) |> 
  arrange(desc(prop.230), name) 

# Order factors in descending order
b.state.sum = b.state.sum |> 
  mutate(state_name = factor(state_name, levels = unique(b.state.sum$state_name)))


p1 = ggplot(b.state.sum) +
  geom_col(aes(x= state_name, y = value, fill = name), width = 0.8, position = 'identity') +
  geom_text(data = b.state.sum |> filter(name == 'cl.120'), 
            aes(x= state_name, y = value, label=prop.120), vjust= -0.5, size = 2) +
  geom_text(data = b.state.sum |> filter(name == 'cl.230'), 
            aes(x= state_name, y = value, label=prop.230), vjust= -0.5, size = 2) +
  ylab("No. lakes > chloride threshold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_fill_manual(values = c( '#e35e28', '#b5361c'), 
                    labels = c('120 mg/L','230 mg/L'), name = 'Threshold') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.key.size = unit(0.3,'cm'),
        legend.position = c(0.85,0.8))

############# ############## Join Figures ############## ##############
layout <- '
AA
AA
BB
'
plotjoin = m1 + p1 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size  = 8))

ggsave(plot = plotjoin, filename = 'figures/Figure3.png', 
       width = 6, height = 6, dpi = 500, bg = 'white')



#### Pull out Fig. 3B data a little differently

#Total number of lakes in each state
nLakes <- b.state %>%
  group_by(state_name) %>%
  summarize(nLakes=n())
nLakes

#Number of lakes that exceed 230 mg/L threshold
n230 <- b.state %>%
  filter(CL>= 230) %>%
  group_by(state_name) %>%
  summarize(n230=n())

#Number of lakes that exceed 120 mg/L threshold
n120 <- b.state %>%
  filter(CL>= 120) %>%
  group_by(state_name) %>%
  summarize(n120=n())

#Join
nLakes <- left_join(nLakes,n230,by='state_name')
nLakes <- left_join(nLakes,n120,by='state_name')

#Calculate proportions and sort by proportion > 230 mg/L threshold
nLakes <- nLakes %>%
  mutate(p230=n230/nLakes,p120=n120/nLakes) %>%
  arrange(desc(p230))

print(nLakes,n=Inf)

#How many lakes are predicted to exceed the 120 mg/L threshold?
sum(nLakes$n120,na.rm=T)
