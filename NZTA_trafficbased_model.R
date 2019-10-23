regionwise.model <- function(trafficdata, obs.data) {
  
  ### summarise all observed data annually per site ####
  all.ann <- obs.data %>% group_by(site_ID, NZTM_E,NZTM_N) %>%
    summarise(location = unique(location)[1],
              type = unique(type)[1],
              region = unique(region)[1],
              sa_ann = mean(sa_ann, na.rm = T))
  
  ## table to spatial
  coordinates(all.ann) <- ~NZTM_E + NZTM_N
  proj4string(all.ann) <- CRS(NZTM_CRS)
  
  ### calculate distances for each of these points####
  trafficdata$OBJECTID <- 1:nrow(trafficdata)
  all_traffic.df <- as.data.frame(trafficdata) 
  
  dist.all <- gDistance(all.ann, trafficdata, byid = T)
  dist.all <- melt(dist.all) ## MATRIX TO LONG
  colnames(dist.all) <-c("roadno","SiteID_rowno","distance")
  dist.all$OBJECTID <- trafficdata$OBJECTID[dist.all$roadno+1] 
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
  return(all.calc)
}
