library(tidyverse)
library(sf)
library(raster)
################# Load HydroBasins watersheds #################

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
# set your own working directory
lagoslakes <- st_read('~/Documents/LAGOS_US/gis_locus_v1.0.gdb/', layer = 'lake_as_point')

hb_08 <- st_read('1_RawData/BasinATLAS_all/BasinATLAS_v10_lev08_USA.shp') |> 
  st_transform(crs(lagoslakes)) |> 
  dplyr::select(OBJECTID, HYBAS_ID, pre_mm_uyr, aet_mm_uyr, pet_mm_uyr, rdd_mk_uav)

#rdd_mk_uav in meters per km², road density in total watershed upstream of lake pour point
#pre_mm_uyr precipitation in total watershed upstream of lake pour point annual average
#aet_mm_uyr actual evapotranspiration in total watershed upstream of lake pour point annual average

# Extract over raster
aet.extract = tibble(
  lagoslakeid = rep(NA, nrow(lagoslakes)),
  nws_zoneid = rep(NA, nrow(lagoslakes)),
  ws_zoneid = rep(NA, nrow(lagoslakes)),
  OBJECTID = rep(NA, nrow(lagoslakes)),
  HYBAS_ID = rep(NA, nrow(lagoslakes)),
  pre_mm_uyr = rep(NA, nrow(lagoslakes)),
  aet_mm_uyr = rep(NA, nrow(lagoslakes)),
  pet_mm_uyr = rep(NA, nrow(lagoslakes)),
  rdd_mk_uav = rep(NA, nrow(lagoslakes))
)

extract = list()
# extract by 10 watersheds at once
usei = seq(0,nrow(hb_08), by = 10)
for (i in usei) {
  print(i)
  
  extract[[((i+10)/10)]] = st_intersection(hb_08[i:(i+9),], lagoslakes) |> 
    as_tibble() |> 
    dplyr::select(!geometry)
} 

extract.df = do.call(rbind.data.frame, extract)

write_csv(extract.df, '3_DerivedData/aetExtract_HydroBASIN.csv')


