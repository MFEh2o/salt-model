#Simple salt model results
#CTS 21 Oct 2022

library(dplyr)
library(ggplot2)
library(deSolve)

source('dSalt.r')
source('dSaltForce.r')


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
#See 'road density.r'. In that script I took US Census Bureau "TIGER/Line" data, plus
#some assumptions about average number of lanes for different road classes, and calculated
#road density at the county level for a bunch of counties. Highest value in NY State is for
#New York County (i.e. Manhattan), where density is 0.0310 lane-m m-2. That's presumably
#about as high as road density gets anywhere. Other boroughs of NYC are 0.014 to 0.027.
#Densest counties that are not boroughs of the city are near suburbs, and are 0.014 to 0.015.
#Lowest value in NY State is 0.0007 lane-m m-2 in Hamilton County (Adirondacks), which
#apparently is the least densely populated county east of the Mississippi.
#For comparison, Dugan et al. 2017 PNAS (Fig. 3) suggests that range of road density in
#500 m buffer around the lakes they considered is from near-0 to maybe something like 20
#km km-2. This decreases at larger buffer sizes. Convert to m m-2: 0 to 20*(1000/1e6) gives
#0 to 0.020 m m-2. That does not (I think) account for multiple lanes, but seems like it is
#in similar ballpark as the numbers I got from TIGER/Line.
delta <- seq(0,30,2)*(1000/1e6)

#Precipitation (net of evapotranspiration), m y-1
#From LakeATLAS data (see 'hydrolakeSalt.r'), take precipitation minus actual evapotranspiration.
#Values range from <0.02 m to >2 m, but all the really high ones are in the Pacific Northwest.
#Uaw 0.02, 0.25, 0.50 m y-1 to approximate typical dry, mesic, wet conditions - roughly
#Montana,  Michigan, Connecticut.
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

#Open graphics device
jpeg('figures/Figure 2.jpg',width=6.66,height=4,units='in',res=300)

#Plot
ggplot(dOut) +
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
  scale_y_log10(minor_breaks=c(seq(2,9,1),seq(20,90,10),seq(200,900,100),seq(2000,9000,1000))) +
  scale_x_continuous(labels=scales::number_format(accuracy = 0.001)) +
  scale_color_brewer(type='seq',palette='Reds')

#Close device
dev.off()
