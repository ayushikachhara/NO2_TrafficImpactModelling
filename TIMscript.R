##### running TIM simulations on a real-world road network #####
library(mapplots)
library(mapview)
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(data.table)
library(ggplot2)
library(zoo)
library(dplyr)
library(gridExtra)
library(grid)
library(maptools)
library(rgeos)
library(plotly)
library(RColorBrewer)
library(moments)
library(semTools)
library(statsr)
library(RCurl)
# library(parallel)
# library(doParallel)
library(sf)

### constants and paths ####
latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"
path <- "S:/kachharaa/NO2 spatial modelling/powerlaw_grids/"

setwd(path)

## function to make point grid ###
makeGrid = function(region) {
  b = bbox(region)
  xs = seq(b["x", "min"], b["x", "max"], by = 10) ## change the "by" value to alter grid spacing ##
  ys = seq(b["y", "min"], b["y", "max"], by = 10)
  pts = expand.grid(xs, ys)
  
  coordinates(pts) = pts
  proj4string(pts) = proj4string(region)
  return(as(pts, "SpatialPoints"))
}

## import traffic file ####
traffic <- readOGR("S:/kachharaa/NO2 spatial modelling/traffic_data/NZTA_traffic/Queenstown_traffic_NZTA.shp")
traffic <- spTransform(traffic, CRS(NZTM_CRS))
traffic$OBJECTID <- 1:nrow(traffic)
plot(traffic)

## import polygon grids file ####
grids <-  readOGR("S:/kachharaa/NO2 spatial modelling/traffic_data/Grids/Queenstown_1km_grid.shp") # traffic
grids <- spTransform(grids, CRS(NZTM_CRS))
plot(grids)
allids <- grids$id

### create a points grid ####
allpoints <- makeGrid(grids)

## add an ID column ##
allpoints$SiteID <- 1:length(allpoints)


## creating list of points within each grid ####
ptsR.list <- list()
for(i in 1:length(allids)) {
  cur.grid = grids[grids$id == allids[i],]
  # plot(cur.grid)
  ptsR <- allpoints[cur.grid,]
  ptsR.list[[i]] <- ptsR
  print(i)
}

## initialise empty list
no2.master.dist <- list()
#i = 1
### calculating TIF ####
start_time <- Sys.time()
for(i in 1:length(ptsR.list)) {
  # if(is.null(overlay)) {
    #   print(paste(i, "has no roads within 1 km"))
    #   next
    # }
  ## subset for one point list at a time
  ptsR <- ptsR.list[[i]]
  
  ## select current grid ####
  cur.grid = grids[grids$id == allids[i],]
  buffer.grid <- gBuffer(cur.grid, width = 2000) ## create a 2km buffer around current list of points
  plot(buffer.grid)
  lines(cur.grid)
  
  overlay <-intersect(traffic, buffer.grid) ## select traffic within buffer
  lines(overlay)
  overlay.vol <- as.data.frame(overlay)
  
  ## calculate distance from each point to each line in traffic file ###
  no2.dist1 <- gDistance(ptsR,overlay,byid = T)
  no2.dist1 <- melt(no2.dist1)
  colnames(no2.dist1) <-c("roadno","SiteID","distance")
  
  no2.dist1$OBJECTID <- overlay$OBJECTID[no2.dist1$roadno]
  
  ## join with traffic data to bring in AADT to the table
  no2.dist1 <- inner_join(no2.dist1, overlay.vol, by = "OBJECTID")
  
  
  ## tif distance weighted calculations ###
  multiplier = 1
  no2.dist1$no2.0.010<- (no2.dist1$trafficVol*multiplier)*exp(-0.010*no2.dist1$distance)
  no2.dist1$no2.0.015<- (no2.dist1$trafficVol*multiplier)*exp(-0.015*no2.dist1$distance)
  no2.dist1$no2.0.02 <-  (no2.dist1$trafficVol*multiplier)*exp(-0.02*no2.dist1$distance)
  no2.dist1$no2.0.025<- (no2.dist1$trafficVol*multiplier)*exp(-0.025*no2.dist1$distance)
  no2.dist1$no2.0.03 <-  (no2.dist1$trafficVol*multiplier)*exp(-0.03*no2.dist1$distance)
  
  no2.impact <- no2.dist1 %>% group_by(SiteID) %>%
    summarise(sumno2.0.01 = sum(no2.0.010, na.rm = T),
              sumno2.0.015 = sum(no2.0.015, na.rm = T),
              sumno2.0.02 = sum(no2.0.02, na.rm = T),
              sumno2.0.025 = sum(no2.0.025, na.rm = T),
              sumno2.0.03 = sum(no2.0.03, na.rm = T))
  
  ## add to list and move on ###
  no2.master.dist[[i]] <-  no2.impact
  print(i)
      
}

end_time <- Sys.time()
print(end_time - start_time)

## combine all elements of the list ###
no2.master <- rbindlist(no2.master.dist)

## calculate NO2
no2.master$no2 <- (0.226*no2.master$sumno2.0.01^0.381)

## get coordinates from an initially created grid
df <- as.data.frame(allpoints@coords)
df$SiteID <- allpoints@data$SiteID

## add TIM data to the points ##
no2impact <- merge(df, no2.master, by = "SiteID", all.y = T)
write.csv(no2impact, "Queenstown10m_power.csv")

coordinates(no2impact) <- ~Var1+Var2
proj4string(no2impact) <-  CRS(NZTM_CRS)
writeOGR(no2impact, ".","Queenstown10m_power", 
         "ESRI Shapefile", overwrite_layer = TRUE)


## rasterise ####
##########################
## import urban areas file ###
urbanareas <- readOGR("Q:/AirQual/shape_files/LCDB4/LCDB_urban_area_200m_buffer2.shp")
urbanareas <- spTransform(urbanareas, CRS(NZTM_CRS))

## subset for each town ###
urban.sub <- urbanareas[which(urbanareas$name == "Queenstown"),]
plot(urban.sub)

pointinres <- no2impact[urban.sub,]

rast <- raster() ## empty raster
extent(rast) <- extent(pointinres) ### define extent based on current points
res(rast) <- 10 ## define cell size 
crs(rast) <- proj4string(pointinres) ## define projection ##
rast2 <- rasterize(pointinres, rast, pointinres$no2, fun=mean) ## add values to empty raster 
plot(rast2)
lines(urban.sub)

## output raster
tmp <- "S:/kachharaa/NO2 spatial modelling/powerlaw_grids/grid_rasters_UAclipped/"
writeRaster(rast2,filename=file.path(tmp, "Queenstown_extended_powerlaw.tif"), format="GTiff", overwrite=TRUE )


## create a freq.dist table for 0.1 bands ######
## convert to a datatable
addresses.df <- as.data.table(pointinres)

## create 0.1 bands
new.df <- cbind.data.frame(threshold = seq(min(addresses.df$no2, na.rm = T),
                                           max(addresses.df$no2, na.rm = T), 0.1),
                           no.pixels = NA) 
new.df$threshold <- round(new.df$threshold,2)

### count pixels less than or equal to current threshold
for(i in 1:nrow(new.df)) {
  new.df$no.pixels[i] <- sum(addresses.df$no2 <= new.df$threshold[i])
  
}

new.df$perc.pixels <- (100*new.df$no.pixels)/max(new.df$no.pixels)
plot(new.df$perc.pixels, col = "blue")
# new.df$start.thresh <- c(0, new.df$threshold[1:(nrow(new.df)-1)]) ###Reverses order for inverse plot
write.csv(new.df, 
          "S:/kachharaa/NO2 spatial modelling/FrequencyDistributions/freqdist_UA_Queenstown.csv", 
          row.names = F)


