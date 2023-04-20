library(sf)
library(tidyverse)
library(ggspatial)
library(MetBrewer)
library(stars)
library(patchwork)
library(USAboundaries)
library(raster)
library(rgdal)

######### Extract road salt use in watersheds #########

# Get USGS road salt data mean from 2010-2015
r = raster('3_DerivedData/mean2010_2015.tif')
sum(values(r), na.rm = T)* 0.0005

# The following geodatabase must be downloaded from EDI. 
# gis_locus_v1.0.gdb

# LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical 
# characteristics of lakes and their watersheds in the conterminous U.S.

# Smith, N.J., K.E. Webster, L.K. Rodriguez, K.S. Cheruvelil, and P.A. Soranno.
# 2021. LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical
# characteristics of lakes and their watersheds in the conterminous U.S. ver 1.
# Environmental Data Initiative.
# https://doi.org/10.6073/pasta/e5c2fb8d77467d3f03de4667ac2173ca (Accessed
# 2023-04-20).

################# Load LAGOS networked watersheds #################
# set your own working directory
data <- st_read('~/Documents/LAGOS_US/gis_locus_v1.0.gdb/', layer='nws')

# Extract over raster
saltExtract.mean = rep(NA, nrow(data))

for (i in 1:nrow(data)) {
  print(i)
  saltExtract = raster::extract(r, data[i,])
  saltExtract.mean[i] = sapply(saltExtract,mean, na.rm = T)
  
  if (i %in% seq(0,25000,by = 500)){
    write_csv(data.frame(lagoslakeid = data$lagoslakeid,
                         nws_zoneid = data$nws_zoneid,
                         salt = saltExtract.mean), '3_DerivedData/saltExtractMean_LAGOSnws.csv')
  }
} 

################# Load LAGOS watersheds #################
# data <- st_read('~/Documents/LAGOS_US/gis_locus_v1.0.gdb/', layer='ws')

# Extract over raster
saltExtract.mean = rep(NA, nrow(data))

for (i in 1:nrow(data)) {
  print(i)
  saltExtract = raster::extract(r, data[i,])
  saltExtract.mean[i] = sapply(saltExtract,mean, na.rm = T)
  
  if (i %in% seq(0,nrow(data),by = 500)){
    write_csv(data.frame(lagoslakeid = data$lagoslakeid,
                         nws_zoneid = data$ws_zoneid, 
                         salt = saltExtract.mean), '3_DerivedData/saltExtractMean_LAGOSws.csv')
  }
} 

