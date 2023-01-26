library(sf)
library(tidyverse)
library(ggspatial)
library(MetBrewer)
library(stars)
library(patchwork)
library(USAboundaries)
library(raster)

# Get USGS road salt data mean from 2010-2015
r = raster('data/USGSsalt/mean2010_2015.tif')
sum(values(r), na.rm = T)* 0.0005


################# Load LAGOS networked watersheds #################
library(rgdal)

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
                         salt = saltExtract.mean), 'data/saltExtractMean_LAGOSnws.csv')
  }
} 

################# Load LAGOS watersheds #################
data <- st_read('~/Documents/LAGOS_US/gis_locus_v1.0.gdb/', layer='ws')


# Extract over raster
saltExtract.mean = rep(NA, nrow(data))

for (i in 1:nrow(data)) {
  print(i)
  saltExtract = raster::extract(r, data[i,])
  saltExtract.mean[i] = sapply(saltExtract,mean, na.rm = T)
  
  if (i %in% seq(0,nrow(data),by = 500)){
    write_csv(data.frame(lagoslakeid = data$lagoslakeid,
                         nws_zoneid = data$ws_zoneid, 
                         salt = saltExtract.mean), 'data/saltExtractMean_LAGOSws.csv')
  }
} 

################# Test for Mendota/Monona #################
out.nws = read_csv('data/saltExtractMean_LAGOSnws.csv') |> 
  mutate(cl_kg_m2 = salt * 0.6067 * 0.453592 / 1e6 )  # convert from pounds salt per km2 to kg Cl- per m2

out.nws |> filter(lagoslakeid %in% c(5371, 4559, 827))

out.ws = read_csv('data/saltExtractMean_LAGOSws.csv') |> 
  mutate(cl_kg_m2 = salt * 0.6067 * 0.453592 / 1e6 )  # convert from pounds salt per km2 to kg Cl- per m2

out.ws |> filter(lagoslakeid %in% c(5371, 4559, 827))

quantile(out.ws$cl_kg_m2, na.rm = T, probs = seq(0,1,0.1))

ggplot(out.ws) +
  geom_density(aes(x = cl_kg_m2)) +
  geom_density(data = out.nws, aes(x = cl_kg_m2), col = 'navy') +
  geom_density(data = b, aes(x = cl_kg_m2), col = 'red4') +
  scale_x_log10()
