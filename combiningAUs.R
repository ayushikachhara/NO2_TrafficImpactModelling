library(readxl)
library(dplyr)
library(sp)
library(sf)
library(rgdal)
library(raster)

latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"
path <- "S:/kachharaa/NO2 spatial modelling/AUOutputs_2/"
setwd(path)
all.Au <- list.files("S:/kachharaa/NO2 spatial modelling/AUOutputs_2/",
                     pattern = ".xls$")

allAUdf <- rbind_list(lapply(all.Au, function(x) read_excel(paste0(path,x))))
allAUdf2 <- allAUdf %>% group_by(AU2013_V1_) %>%
  summarise_all(mean,na.rm = T)

au2013 <- readOGR("S:/kachharaa/NO2 spatial modelling/statsnzarea-unit-2013-SHP/area-unit-2013.shp",
                  stringsAsFactors = F)

au2013df <- merge(au2013, allAUdf2, by = "AU2013_V1_", all.x = T)
au2013df <- spTransform(au2013df, CRS(NZTM_CRS))

# coastline <- readOGR("S:/kachharaa/NO2 spatial modelling/observed_modelled_UB/outputshapefiles/nz-coastlines-and-islands-polygons-topo-150k.shp")
# coastline <- spTransform(coastline, CRS(NZTM_CRS))

# au2013df2 <- au2013df[coastline,]
# plot(au2013df2)

writeOGR(au2013df, ".","AU2013_BGadded_3", 
         "ESRI Shapefile", overwrite_layer = TRUE)
