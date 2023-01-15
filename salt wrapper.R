#Simple salt model results
#CTS 21 Oct 2022

library(tidyverse)
library(gridExtra)
library(deSolve)
library(tigris)
library(sf)
library(ggspatial)
library(MetBrewer)
library(stars)

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


####################################################################################
  ########## Scenario of decreasing alpha from 8 to 0,2,4 after 60 years #########
####################################################################################
times = 1:200
inits = c(SW=0,SL=0)
pars <- c(phi = 0.1, alpha = 8, delta = 0.010, A = 1e6, p = 0.25, V = 50000)
#Solve through time
ss.8 <- ode(inits,times,dSalt,pars) |> 
  as.tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 8')

#Solve through time
times = 60:200
inits = c(SW = ss.8[60,]$SW, SL = ss.8[60,]$SL)

#Decrease alpha to 4
pars <- c(phi = 0.1, alpha = 4, delta = 0.010, A = 1e6, p = 0.25, V = 50000)
ss.4 = ode(inits,times,dSalt,pars) |> 
  as.tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 4')
#Decrease alpha to 2
pars <- c(phi = 0.1, alpha = 2, delta = 0.010, A = 1e6, p = 0.25, V = 50000)
ss.2 = ode(inits,times,dSalt,pars) |> 
  as.tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 2')
#Decrease alpha to 0
pars <- c(phi = 0.1, alpha = 0, delta = 0.010, A = 1e6, p = 0.25, V = 50000)
ss.0 = ode(inits,times,dSalt,pars) |> 
  as.tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 0')

# Join scenarios
ss.out = ss.8 |> bind_rows(ss.4) |> bind_rows(ss.2) |> bind_rows(ss.0) |> 
  mutate(CL = SL/V*1000)

# Plot and save scenarios
ggplot(ss.out) +
  geom_path(aes(x = time, y = CL, group = scenario, color = scenario), linetype = 1, linewidth = 1) +
  scale_color_manual(values = (met.brewer("Tam", 4))) +
  ylab("Road Salt Concentration"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') +
  theme_bw(base_size = 9) +
  theme(legend.position = c(0.15,0.87),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )

ggsave('figures/Figure1b.png', width = 3, height = 3, dpi = 500)


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
       color=expression(atop(Road~salt~application~rate,'('*kg~'('*'lane-m'*')'^-1~y^-1*')'))) +
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
# The following code was used to subset LakeATLAS to CONUS
# hydrolakes.pt = st_read('data/LakeATLAS_all/LakeATLAS_Data_v10_shp/LakeATLAS_v10_shp/LakeATLAS_v10_pnt_west.shp')
# 
# hydrolakes.us = hydrolakes.pt %>%
#   filter(Country == 'United States of America') %>%
#   dplyr::mutate(lon = sf::st_coordinates(.)[,1],
#                 lat = sf::st_coordinates(.)[,2]) |>
#   filter(lat < 50, lon > -150, lat > 22) |>
#   dplyr::select(Hylak_id:Wshd_area,starts_with("rdd"), starts_with("pre_mm_u"), starts_with("aet_mm_u"),
#          starts_with("pet_mm_u"), starts_with("urb"), 'HYBAS_L12')
# 
# st_write(hydrolakes.us, 'data/LakeATLAS_all/LakeATLAS_CONUS.shp', delete_dsn = TRUE)

# Load geospatial data already filtered to parameters of interest (needed to be small enough for GitHub)
hydrolakes.us = st_read('data/LakeATLAS_all/LakeATLAS_CONUS.shp')

#rdd_mk_uav in meters per km², road density in total watershed upstream of lake pour point
#pre_mm_uyr precipitation in total watershed upstream of lake pour point annual average
#aet_mm_uyr actual evapotranspiration in total watershed upstream of lake pour point annual average


library(raster)
# r0 = raster('data/USGSsalt/1992_2015/2010.tif') 
# r1 = raster('data/USGSsalt/1992_2015/2011.tif') 
# r2 = raster('data/USGSsalt/1992_2015/2012.tif') 
# r3 = raster('data/USGSsalt/1992_2015/2013.tif') 
# r4 = raster('data/USGSsalt/1992_2015/2014.tif') 
# r5 = raster('data/USGSsalt/1992_2015/2015.tif') 
# stacked <- stack(r0, r1, r2, r3, r4, r5) # make a raster stack
# # calculate a mean raster
# meanR <- calc(stacked, fun = mean)
# writeRaster(meanR, 'data/USGSsalt/mean2010_2015.tif')

# Get USGS road salt data mean from 2010-2015
r = raster('data/USGSsalt/mean2010_2015.tif')


# Load HYDROsheds level 12 shapefiles
level12 = st_read('~/Documents/HydroAtlas/hybas_na_lev01-12_v1c/hybas_na_lev12_v1c.shp') |> 
  filter(HYBAS_ID %in% hydrolakes.us$HYBAS_L12)

level12.valid = level12 %>%
  st_make_valid(.) |> 
  st_transform(st_crs(r))

# Extract raster data and take mean (this step takes a while, load csv to save time)
# saltExtract = raster::extract(r, level12.valid)
# saltExtract.mean = lapply(saltExtract,mean) %>%
#   do.call(rbind.data.frame, .) |> 
#   rename(mean.salt = 1) |> 
#   mutate(HYBAS_ID = level12.valid$HYBAS_ID)
# write_csv(saltExtract.mean, file = 'data/saltExtractMean.csv')
saltExtract.mean = read_csv('data/saltExtractMean.csv')

# Convert units and add salt 
b = hydrolakes.us |> 
  left_join(saltExtract.mean,  by = c('HYBAS_L12' = 'HYBAS_ID')) |> 
  mutate(cl_kg_m2 = mean.salt * 0.6067 * 0.453592 / 1e6 ) |>  # convert from pounds salt per km2 to kg Cl- per m2
  mutate(delta = rdd_mk_uav/1e6) |> # convert to meters per m2
  mutate(p = (pre_mm_uyr - aet_mm_uyr)/1000) #Precipitation (net of evapotranspiration), m y-1

# Check ranges
range(b$cl_kg_m2, na.rm = TRUE)
range(b$delta)
range(b$p)

#Application rate of road salt, kg (lane-m)-1 y-1
#Use range 2.5 to 25 kg (lane-m)-1 y-1
alpha <- c(2.5,5,10,15,20,25)#c(0:12)

# Just a helpful stackoverflow function to make case_when into factors 
# https://stackoverflow.com/questions/49572416/r-convert-to-factor-with-order-of-levels-same-with-case-when
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

# Calculate equilibrium concentration of salt in lake (g m-3 = mg L-1)
# Group by CL concentration
b <- b %>%
  # mutate(CL = 10*delta/p*1000) |> 
  mutate(CL = cl_kg_m2/p*1000) |> 
  mutate(CL.group = fct_case_when(CL < 10 ~ '0-10',
                                  CL < 50 ~ '10-50',
                                  CL < 120 ~ '50-120',
                                  CL < 230 ~ '120-230',
                                  CL < 500 ~ '230-500',
                                  CL >= 500 ~ '500+')) |> 
  filter(!is.na(CL))

# Plots histogram 
ggplot(b) +
  geom_histogram(aes(x = CL), fill = 'red4', color = 'black') +
  scale_y_log10() +
  xlab("Equlibrium salt concentration"~(mg~Cl^"-"~L^-1)) +
  theme_bw(base_size = 9) 

# What state are all the points in? 
states_sf <- st_transform(us_states(map_date = NULL, resolution = c("low", "high"), states = NULL), 4326) |> 
  dplyr::select(name, state_name, state_abbr)
b.state <- as.data.frame(st_join(b, states_sf, join = st_intersects))
b.state.sf <- st_join(b, states_sf, join = st_intersects)
# st_write(b.state.sf, dsn = 'data/saltModeloutput.GeoJSON', delete_dsn = TRUE)

# Check highest concentrations in lakes/states
b.state.sf |> filter(CL == max(CL))
b.state.sf |> filter(state_abbr == 'IL') |> dplyr::select(Hylak_id, CL, CL.group) |> 
  arrange(desc(CL))

############# ############## MAPPING ############## ##############
## Esri basemap URLs ####
world_gray <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

m1 = ggplot() +
  annotation_map_tile(type = world_gray, zoom = 5) + # Esri Basemap
  geom_sf(data = b, aes(fill = CL.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 6)[-4]), 
                    name = expression(atop("Road Salt",~(mg~Cl^"-"~L^"-")))) +
  theme_bw(base_size = 9) +
  theme(axis.title = element_blank(),
        legend.position = c(0.9,0.2),
        legend.background = element_blank(),
        legend.title = element_text(size = 7),
        legend.key = element_blank(),
        legend.key.height = unit(0.1,'cm'))

m2 = ggplot() +
  annotation_map_tile(type = world_gray, zoom = 8) + # Esri Basemap
  geom_sf(data = b, aes(fill = CL.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 6)[-4]), 
                    name = expression(atop("Road Salt",~(mg~Cl^"-"~L^"-")))) +
  theme_bw(base_size = 9) +
  coord_sf(crs = 4326, ylim = c(41.5,46), xlim = c(-89, -82)) +
  scale_x_continuous(breaks = seq(-88, -82, by = 2)) +
  theme(legend.position = 'none',
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.1,'cm'))

# ggsave('figures/saltLakes.png', height = 4, width = 6, dpi = 500)  

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
  filter(cl.120 >= 5) |> # filter to states with more than 5 lakes
  pivot_longer(cols = cl.230:cl.120) |> 
  arrange(desc(prop.230), name) |> 
  mutate(state_name = factor(state_name, levels = unique(state_name)))


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
BC
'
plotjoin = m1 + p1 + m2 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size  = 8))
ggsave(plot = plotjoin, filename = 'figures/Figure3.png', 
       width = 6, height = 6, dpi = 500, bg = 'white')

