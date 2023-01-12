library(sf)
library(tidyverse)
library(ggspatial)
library(MetBrewer)
library(stars)

hydrolakes.pt = st_read('data/LakeATLAS_all/LakeATLAS_Data_v10_shp/LakeATLAS_v10_shp/LakeATLAS_v10_pnt_west.shp')

hydrolakes.us = hydrolakes.pt |>
  filter(Country == 'United States of America') %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) |>
  filter(lat < 50, lon > -150, lat > 40) |>
  dplyr::select(Hylak_id:Wshd_area,starts_with("rdd"), starts_with("pre_mm_"), starts_with("aet_mm_"),
         starts_with("pet_mm_u"), starts_with("urb"), starts_with("cls_cl_"), starts_with("tmp_dc_"), 
         starts_with("ari_ix_u"), starts_with("cmi_ix_"), starts_with("snw_pc_"))

# Plot road density
ggplot() +
  annotation_map_tile(type = world_gray, zoom = 4) + # Esri Basemap
  geom_sf(data = test, aes(fill = rdd_mk_uav), size = 1, stroke = 0.2, shape = 21) +
  scale_fill_gradientn(colors = rev(met.brewer("Peru1"))) +
  theme_bw(base_size = 9)

# st_write(hydrolakes.us, 'data/LakeATLAS_CONUS.shp', delete_dsn = TRUE)

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
  filter(!is.na(salt_kg_m2))

library(ranger)
library(pdp)

## Random Forest covariance matrix, with only predictors selected ####
rf_cov <- b %>% 
  as.data.frame() |> 
  # dplyr::select(rdd_mk_vav: snw_pc_v12)
  select(tmp_dc_l12, tmp_dc_l01:tmp_dc_l04, aet_mm_uyr, cmi_ix_l12, cmi_ix_l01:cmi_ix_l04, snw_pc_uyr, pre_mm_uyr, 
         rdd_mk_vav, rdd_mk_uav, urb_pc_use) |> 
  mutate(rdd_mk_uav = log10(rdd_mk_uav + 0.001))

sapply(rf_cov, function(x) sum(is.na(x))) # Check if there are NA values

# Sampling routine to use 95% of lakes as in-bag samples  ####
ntree = 500

# Custom inbag sampling routine 
random_lake_samps <- lapply(1:ntree, function(i){
  unique_lakes <- unique(b$Hylak_id)
  lake_samp <- sample(unique_lakes, size =0.95*length(unique_lakes), replace=F) # In-bag uses 95% of lakes
  samp = as.integer(b$Hylak_id %in% lake_samp)
  return(samp)
}
)

## Run RF model ####
rf_model <- ranger(dependent.variable.name = 'salt',
                   data = data.frame(salt = b$salt_kg_m2, rf_cov),
                   # inbag = random_lake_samps,
                   mtry = 4,
                   num.trees = ntree, quantreg = T,
                   importance = 'permutation',
                   keep.inbag = TRUE)
rf_model
# varImp(rf_model)
# importance(rf_model)


imDF = tibble::enframe(rf_model$variable.importance)

ggplot(imDF, aes(x=reorder(name,value), y=value,fill=value))+ 
  geom_bar(stat="identity", position="dodge") + coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill = 'none')+
  scale_fill_gradient(low="red", high="blue")


rf_model %>%
  partial(pred.var = "rdd_mk_uav") %>%
  plotPartial(rug = TRUE, train = rf_cov)

quantile(rf_cov$rdd_mk_uav, probs = seq(0,1,by = 0.1))


