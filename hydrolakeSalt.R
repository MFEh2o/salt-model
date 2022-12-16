library(sf)
library(tidyverse)
library(ggspatial)
library(MetBrewer)
library(stars)


# The following code was used to subset LakeATLAS to CONUS
# hydrolakes.pt = st_read('~/Downloads/LakeATLAS_Data_v10_shp/LakeATLAS_v10_shp/LakeATLAS_v10_pnt_west.shp')
# 
# hydrolakes.us = hydrolakes.pt |>
#   filter(Country == 'United States of America') %>%
#   dplyr::mutate(lon = sf::st_coordinates(.)[,1],
#                 lat = sf::st_coordinates(.)[,2]) |>
#   filter(lat < 50, lon > -150, lat > 22) |> 
#   dplyr::select(Hylak_id:Wshd_area,starts_with("rdd"), starts_with("pre_mm_u"), starts_with("aet_mm_u"), 
#          starts_with("pet_mm_u"), starts_with("urb"))
# 
# st_write(hydrolakes.us, 'data/LakeATLAS_CONUS.shp', delete_dsn = TRUE)

# Load geospatial data already filtered to parameters of interest (needed to be small enough for GitHub)
hydrolakes.us = st_read('data/LakeATLAS_CONUS.shp')

#rdd_mk_uav in meters per km², road density in total watershed upstream of lake pour point
#pre_mm_uyr precipitation in total watershed upstream of lake pour point annual average
#aet_mm_uyr actual evapotranspiration in total watershed upstream of lake pour point annual average


# Get USGS road salt data from 2015
r = read_stars('data/USGSsalt/2015.tif') 
# Transform points to have compatible projection
a.project = hydrolakes.us |>
  st_transform(st_crs(r))
# Extract raster data
usgsExtract = st_extract(r, a.project) |> 
  mutate(salt_kg_m2 = `2015.tif` * 0.453592 / 1e6)

# Convert units and add salt 
b = hydrolakes.us |>
  mutate(salt_kg_m2 = usgsExtract$salt_kg_m2) |> 
  mutate(delta = rdd_mk_uav/1e6) |> # convert to meters per m2
  mutate(p = (pre_mm_uyr - aet_mm_uyr)/1000) #Precipitation (net of evapotranspiration), m y-1

# Check ranges
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
  mutate(CL = salt_kg_m2/p*1000) |> 
  mutate(CL.group = fct_case_when(CL < 10 ~ '0-10',
                              CL < 50 ~ '10-50',
                              CL < 100 ~ '50-100',
                              CL < 300 ~ '100-300',
                              CL < 500 ~ '300-500',
                              CL >= 500 ~ '500+')) 

# Plots histogram 
ggplot(b) +
  geom_histogram(aes(x = CL), fill = 'red4', color = 'black') +
  scale_y_log10() +
  xlab("Equlibrium salt concentration"~(mg~Cl^"-"~L^-1)) +
  theme_bw(base_size = 9) 

############# ############## MAPPING ############## ##############
## Esri basemap URLs ####
world_gray <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() +
  annotation_map_tile(type = world_gray, zoom = 5) + # Esri Basemap
  geom_sf(data = b, aes(fill = CL.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 6)), name = 'Cl (mg/L)') +
  # scale_fill_gradientn(colors = rev(met.brewer("Peru1"))) +
  # scale_color_gradientn(colors = rev(met.brewer("Peru1"))) +
  theme_bw(base_size = 9)

ggsave('figures/saltLakes.png', height = 4, width = 6, dpi = 500)  


#Plot salt use, does it makes sense?
test <- b %>%
  mutate(salt_kg_m2.group = case_when(salt_kg_m2 < 0.005 ~ '0-0.005',
                                      salt_kg_m2 < 0.01 ~ '0.005-0.01',
                                      salt_kg_m2 < 0.02 ~ '0.01-0.02',
                                      salt_kg_m2 < 0.03 ~ '0.02-0.03',
                                      salt_kg_m2 < 0.05 ~ '0.03-0.05',
                                      salt_kg_m2 >= 0.05 ~ '0.05+'))

ggplot() +
  annotation_map_tile(type = world_gray, zoom = 5) + # Esri Basemap
  geom_sf(data = test, aes(fill = salt_kg_m2.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 6)), name = 'Salt (kg/m2)') +
  theme_bw(base_size = 9)
ggsave('figures/USGSsalt2015.png', width = 6, height = 4, dpi = 500)
