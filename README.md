# salt-model
simple salt model

This repo contains code and data related to a simple differential equation model of salt concentrations in lakes and their watersheds.

The main script is 'salt wrapper.r'. Mostly this wrapper script works with the anlyitcal equilibrium solutions of the model, but it also calls 'dSalt.r', which defines the differential equations, to do some dynamic simulations.

The 'plot Mirror Lake data.r' script plots empirical data on salt concentrations in Mirror Lake, NH, but loading data files stored on EDI and here in this repo.

The 'road density.r' script uses US Census Bureau TIGER/Line data to calculate road densities at the county level.


## Application 3
Predict the equilibrium road salt concentration for all lakes larger than 1 ha in the contiguous United States.

Watershed (n = 477,965), networked watersheds (n = 25,531), and lake data (in data/LAGOS) were obtained from LAGOS locus (Cheruvelil et al. 2021). 
See https://lagoslakes.org/lagos-us-overview/

Annual gridded 1-km2 data on road salt application rates were obtained from Falcone et al. (2018).
See https://www.usgs.gov/data/estimates-road-salt-application-across-conterminous-united-states-1992-2015

Precipitation and actual evapotranspiration were obtained from HydroSHEDS and HydroATLAS, a global dataset of hydro-environmental characteristics (Lehner et al. 2022)
See https://www.hydrosheds.org/hydroatlas

