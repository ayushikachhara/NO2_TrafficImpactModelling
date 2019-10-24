library(librarian)

shelf(mapplots,
      mapview,
      leaflet,
      rgdal,
      sp,
      raster,
      data.table,
      ggplot2,
      zoo,
      dplyr,
      gridExtra,
      grid,
      maptools,
      rgeos,
      plotly,
      RColorBrewer,
      moments,
      semTools,
      statsr, lib = tempdir())

### constants and paths ####
latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"
path <- "S:/kachharaa/NO2 spatial modelling/traffic_data/"
setwd(path)

### import observed data ####
all.sites <- read.csv("S:/kachharaa/NO2 spatial modelling/modelforeachsite/observationaldata/NZTA_2018.csv", 
                      stringsAsFactors = F)

regions <- unique(all.sites$region)

### import traffic data ####
ALLtraffic <- readOGR("S:/kachharaa/NO2 spatial modelling/traffic_data/CoreLogicALL_merged.shp", stringsAsFactors = F)
ALLtraffic <- spTransform(ALLtraffic, CRS(NZTM_CRS))


nzta.list <- list() ## initiate empty list
## model for each region ###
for(i in 1:length(regions)) {
  cur.region = regions[i]
  ### import observed data ####
  all_observed <- all.sites %>% 
    filter(region == cur.region & !is.na(NZTM_E))
  
  ### summarise all observed data annually per site ####
  all.ann <- all_observed %>% group_by(site_ID, NZTM_E,NZTM_N) %>%
    summarise(location = unique(location)[1],
              type = unique(type)[1],
              region = unique(region)[1],
              sa_ann = mean(sa_ann, na.rm = T))
  
  ## table to spatial
  coordinates(all.ann) <- ~NZTM_E + NZTM_N
  proj4string(all.ann) <- CRS(NZTM_CRS) 
  
  
  ## create buffer and subset traffic ####
  regionalbuffer <- gBuffer(all.ann, width = 10000)
  trafficdata <- ALLtraffic[regionalbuffer,]
  
  ## checks ###3
  plot(regionalbuffer)
  points(all.ann)
  lines(trafficdata)
  
  ### calculate distances for each of these points####
  trafficdata$OBJECTID <- rownames(trafficdata@data)
  all_traffic.df <- as.data.frame(trafficdata) 
  
  ### calculte distance to each site
  dist.all <- gDistance(all.ann, trafficdata, byid = T)
  dist.all <- melt(dist.all) ## MATRIX TO LONG
  colnames(dist.all) <-c("OBJECTID","SiteID_rowno","distance")
  
  dist.all$SiteID <- all.ann$site_ID[dist.all$SiteID_rowno]
  all.vol <- trafficdata[, c("OBJECTID","trafficVol")]
  
  dist.all <- merge(dist.all, all.vol, by = "OBJECTID", all = T)
  
  ### based on distances and traffic vol calculate TIF values ####
  
  multiplier <- 1 ##trafficdata$Shape_Leng
  dist.all$no2.0.001<- (dist.all$trafficVol*multiplier)*exp(-0.001*dist.all$distance)
  dist.all$no2.0.002<- (dist.all$trafficVol*multiplier)*exp(-0.002*dist.all$distance)
  dist.all$no2.0.003<- (dist.all$trafficVol*multiplier)*exp(-0.003*dist.all$distance)
  dist.all$no2.0.004<- (dist.all$trafficVol*multiplier)*exp(-0.004*dist.all$distance)
  dist.all$no2.0.005<- (dist.all$trafficVol*multiplier)*exp(-0.005*dist.all$distance)
  dist.all$no2.0.006<- (dist.all$trafficVol*multiplier)*exp(-0.006*dist.all$distance)
  dist.all$no2.0.007<- (dist.all$trafficVol*multiplier)*exp(-0.007*dist.all$distance)
  dist.all$no2.0.008<- (dist.all$trafficVol*multiplier)*exp(-0.008*dist.all$distance)
  dist.all$no2.0.009<- (dist.all$trafficVol*multiplier)*exp(-0.009*dist.all$distance)
  dist.all$no2.0.010<- (dist.all$trafficVol*multiplier)*exp(-0.010*dist.all$distance)
  dist.all$no2.0.015<- (dist.all$trafficVol*multiplier)*exp(-0.015*dist.all$distance)
  dist.all$no2.0.02 <-  (dist.all$trafficVol*multiplier)*exp(-0.02*dist.all$distance)
  dist.all$no2.0.025<- (dist.all$trafficVol*multiplier)*exp(-0.025*dist.all$distance)
  dist.all$no2.0.03 <-  (dist.all$trafficVol*multiplier)*exp(-0.03*dist.all$distance)
  dist.all$no2.0.035 <-  (dist.all$trafficVol*multiplier)*exp(-0.035*dist.all$distance)
  dist.all$no2.0.04 <-  (dist.all$trafficVol*multiplier)*exp(-0.04*dist.all$distance)
  dist.all$no2.0.045 <-  (dist.all$trafficVol*multiplier)*exp(-0.045*dist.all$distance)
  dist.all$no2.0.05 <-  (dist.all$trafficVol*multiplier)*exp(-0.05*dist.all$distance)
  
  ## accummulated TIF ###
  impact.all <- dist.all %>% group_by(SiteID) %>%
    summarise(sumno2.0.001 = sum(no2.0.001, na.rm = T),
              sumno2.0.002 = sum(no2.0.002, na.rm = T),
              sumno2.0.003 = sum(no2.0.003, na.rm = T),
              sumno2.0.004 = sum(no2.0.004, na.rm = T),
              sumno2.0.005 = sum(no2.0.005, na.rm = T),
              sumno2.0.006 = sum(no2.0.006, na.rm = T),
              sumno2.0.007 = sum(no2.0.007, na.rm = T),
              sumno2.0.008 = sum(no2.0.008, na.rm = T),
              sumno2.0.009 = sum(no2.0.009, na.rm = T),
              sumno2.0.01 = sum(no2.0.010, na.rm = T),
              sumno2.0.015 = sum(no2.0.015, na.rm = T),
              sumno2.0.02 = sum(no2.0.02, na.rm = T),
              sumno2.0.025 = sum(no2.0.025, na.rm = T),
              sumno2.0.03 = sum(no2.0.03, na.rm = T),
              sumno2.0.035 = sum(no2.0.035, na.rm = T),
              sumno2.0.04 = sum(no2.0.04, na.rm = T),
              sumno2.0.045 = sum(no2.0.045, na.rm = T),
              sumno2.0.05 = sum(no2.0.05, na.rm = T))
  
  impact.all <- impact.all[complete.cases(impact.all$SiteID),]
  
  ## check the validity of gDistance
  max.impact.all <- dist.all %>% group_by(SiteID) %>%
    summarise(OBJECTID = OBJECTID[which.max(no2.0.015)],
              distance = distance[which.max(no2.0.015)],
              trafficVol = trafficVol[which.max(no2.0.015)],
              maxno2 = no2.0.015[which.max(no2.0.015)])
  
  impact.all <- merge(impact.all, max.impact.all,
                      by = "SiteID", all = T)
  
  all.calc <- merge(all.ann, impact.all, 
                    by.x = "site_ID",by.y = "SiteID", all = T)
  
  all.calc <- as.data.table(all.calc)
  
  nzta.list[[i]] <- all.calc
  
  print(paste(cur.region, "done"))
  
}

## merge data together
allnzta_modelled <- rbind_list(nzta.list)
write.csv(allnzta_modelled,"Q:/AirQual/Ayushi_WD/NO2 spatial modelling/modelforeachsite/TIFoutputs/NZTA2018_TIF.csv",
          row.names = F)
