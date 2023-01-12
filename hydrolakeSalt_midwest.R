library(sf)
library(tidyverse)
library(ggspatial)
library(MetBrewer)
library(stars)
library(patchwork)
library(USAboundaries)

# The following code was used to subset LakeATLAS to midwest
hydrolakes.midwest = hydrolakes.pt |>
  filter(Country == 'Canada' | Country == 'United States of America') %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) |>
  filter(lat < 48, lon > -96, lat > 38, lon < -67) |>
  dplyr::select(Hylak_id:Wshd_area,starts_with("rdd"), starts_with("pre_mm_"), starts_with("aet_mm_"),
                starts_with("pet_mm_u"), starts_with("urb"), starts_with("cls_cl_"), starts_with("tmp_dc_"),
                starts_with("ari_ix_u"), starts_with("cmi_ix_"), starts_with("snw_pc_"))

st_write(hydrolakes.midwest, 'data/LakeATLAS_Midwest.shp', delete_dsn = TRUE)

# Load geospatial data already filtered to parameters of interest (needed to be small enough for GitHub)
hydrolakes.midwest = st_read('data/LakeATLAS_Midwest.shp')

#rdd_mk_uav in meters per km², road density in total watershed upstream of lake pour point
#pre_mm_uyr precipitation in total watershed upstream of lake pour point annual average
#aet_mm_uyr actual evapotranspiration in total watershed upstream of lake pour point annual average

# Convert units and add salt 
b = hydrolakes.midwest |>
  mutate(delta = rdd_mk_uav/1e6) |> # convert to meters per m2
  mutate(p = (pre_mm_uyr - aet_mm_uyr)/1000) #Precipitation (net of evapotranspiration), m y-1

# Check ranges
range(b$delta)
range(b$p)

#Application rate of road salt, kg (lane-m)-1 y-1
#Use range 2.5 to 25 kg (lane-m)-1 y-1
alpha <- c(2.5,5,10,15,20,25) #c(0:12)

# convert kg per meter to tons per mile
10 * 1.774 /0.6

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
  mutate(CL = 10*delta/p*1000) |> # set salt to 10 kg lane meter
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

############# ############## MAPPING ############## ##############
## Esri basemap URLs ####
world_gray <-  paste0('https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

m1.midwest = ggplot() +
  annotation_map_tile(type = world_gray, zoom = 5) + # Esri Basemap
  geom_sf(data = b , aes(fill = CL.group), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_manual(values = rev(met.brewer("Peru1", 6)), name = 'Cl (mg/L)') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw(base_size = 9) +
  theme(legend.position = 'none',
        legend.background = element_blank(),
        # legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.height = unit(0.1,'cm')); m1.midwest

# ggsave('figures/saltLakes.png', height = 4, width = 6, dpi = 500)  
