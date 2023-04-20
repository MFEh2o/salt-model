library(raster)

### Downloaded tifs from: https://www.sciencebase.gov/catalog/item/5b15a50ce4b092d9651e22b9 ###
# Read in rasters of salt use from 2010-2015
r = raster('1_RawData/USGSsalt_Falcone/2010.tif')
r1 = raster('1_RawData/USGSsalt_Falcone/2011.tif')
r2 = raster('1_RawData/USGSsalt_Falcone/2012.tif')
r3 = raster('1_RawData/USGSsalt_Falcone/2013.tif')
r4 = raster('1_RawData/USGSsalt_Falcone/2014.tif')
r5 = raster('1_RawData/USGSsalt_Falcone/2015.tif')
stacked <- stack(r, r1, r2, r3, r4, r5) # make a raster stack

# Calculate a mean raster
meanR <- calc(stacked, fun = mean)
# Save raster
writeRaster(meanR, '3_DerivedData/mean2010_2015.tif')
