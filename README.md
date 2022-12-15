# salt-model
simple salt model

This repo contains code and data related to a simple differential equation model of salt concentrations in lakes and their watersheds.

The main script is 'salt wrapper.r'. Mostly this wrapper script works with the anlyitcal equilibrium solutions of the model, but it also calls 'dSalt.r', which defines the differential equations, to do some dynamic simulations.

The 'plot Mirror Lake data.r' script plots empirical data on salt concentrations in Mirror Lake, NH, but loading data files stored on EDI and here in this repo.

The 'road density.r' script uses US Census Bureau TIGER/Line data to calculate road densities at the county level.
