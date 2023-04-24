#Rate function for salt model

#State variables are:
#SW - Mass of salt in the watershed, kg Cl-
#SL - Mass of salt in the lake, kg Cl-

#Parameters are:
#alpha - Application rate of road salt, kg (m road)-1 y-1
#delta - Road density, m m-2
#A - Watershed area, m2 (note 1e6 m2 is 1 km2)
#p - Precipitation (net of evapotranspiration), m y-1
#phi - Salt yield of watershed per unit of precipitation, m-1
#V - Lake volume, m3

dSalt <- function(time, state, pars) {
  with(as.list(c(state,pars)), {
    #Rate of change in Cl- in watershed, kg y-1
    dSW <- alpha*delta*A - p*phi*SW
    #Rate of change in Cl- in lake, kg y-1
    dSL <- p*phi*SW - p*A*(1/V)*SL
    #Return the rates of change
    return(list(c(dSW,dSL)))
  })
}
