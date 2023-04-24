# Package ID: edi.854.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical characteristics of lakes and their watersheds in the conterminous U.S..
# Data set creator:  Nicole Smith - Michigan State University 
# Data set creator:  Katherine Webster - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Data set creator:  Patricia Soranno - Michigan State University 
# Contact:  Kendra Cheruvelil -  Michigan State University  - ksc@msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

library(tidyverse)
library(feather)
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/854/1/007ca4f5ec02bb5809fc661dcfa7a903" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

dt1 <- read_csv(infile1)
dt1

# Load LAGOS lake information csv to bit for GitHub. Pared down and saved as feather. 
lagos.info = dt1 |>
  dplyr::select(lagoslakeid, lake_namegnis, lake_namelagos, lake_lat_decdeg,
                lake_lon_decdeg, lake_states, ws_zoneid, nws_zoneid)

write_feather(lagos.info, '3_DerivedData/LAGOS_lake_information.feather')
