#### creating buffers for all traffic data ####

## import libraries ##
library(librarian)

shelf(data.table,
      ggplot2,
      zoo,
      dplyr,
      gridExtra,
      grid,
      plotly,
      RColorBrewer,
      statsr,
      dplyr,
      pracma,
      openair,
      mapplots,
      mapview,
      leaflet,
      rgdal,
      sp,
      raster,
      rgeos,
      lubridate,
      tidyr,
      sf,
      lib = tempdir())

## import traffic files## 
path = "S:/kachharaa/NO2 spatial modelling/traffic_data/NZTA_traffic/"
listoffiles <- list.files(path = path, pattern = "*.shp")

## calculate buffer columns ###
listNZTA <- list()


k = 1
for(i in 1:length(listoffiles)) {
  tryCatch({
  f1 <- readOGR(paste0(path,listoffiles[[(i)]]))
  
  ## creating buffer that reduces NO2 to 2
  f1$AADT_cal2 <- ifelse(f1$trafficVol<8000, 8000, f1$trafficVol)
  f1$buffer2 <- 66.7*log(f1$AADT_cal2) - 596
  print(i)
  listNZTA[[k]] <- f1
  k = k+1
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 
}

## apply buffer and create an output of buffer shapes ####
setwd("S:/kachharaa/NO2 spatial modelling/traffic_data/trafficBuffers/")
bufferlist <- list()
i = 1
for(i in 1:length(listNZTA)) {
  f1 <- listNZTA[[i]]
  buff1 <- gUnaryUnion(gBuffer(f1,width = f1$buffer2, byid = TRUE))
  buff1 <- SpatialPolygonsDataFrame(buff1, data.frame(FID=as.numeric(1))) 
  plot(buff1)
  bufferlist[[i]] <- buff1
  nameoffile <- unlist(strsplit(listoffiles[i], ".shp"))
  nameoffile <-paste0(nameoffile, "_buffer")
  writeOGR(buff1, ".",nameoffile, "ESRI Shapefile",
           overwrite_layer = T)
  print(i)
}

