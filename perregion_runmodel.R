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
all.sites <- read.csv("S:/kachharaa/NO2 spatial modelling/modelforeachsite/observationaldata/ALLSITES.csv", 
                      stringsAsFactors = F)


# w.sites <- c("LowerHutt","UpperHutt","Porirua","Wellington")
# all_traffic <-  readOGR("./NZTA_traffic/ALLAuckland_traffic_NZTA.shp")

all.sites <- all.sites# %>% filter(origin == "NZTA2016")
sites <- "Queenstown" # unique(all.sites$region)
# sites <- sites[-c(1,2,3,6,7,25)] ## Auckland, Wellington and Taranaki - removed because traffic name doesn't match

### function of modelrun
source("S:/kachharaa/NO2 spatial modelling/modelforeachsite/NZTA_trafficbased_model.R")
nzta.list <- list()
i = 1
### Run the model for one region at a time ####
for(i in 1:length(sites)) {
  # cur.region = w.sites[i]
  # a = 
  # b = 
  cur.region =  sites[i]
  all_traffic <-  readOGR(paste0("./NZTA_traffic/",cur.region,"_traffic_NZTA.shp"))
  all_traffic <- spTransform(all_traffic, CRS(NZTM_CRS))
  proj4string(all_traffic) <- CRS(NZTM_CRS)
 
   ### import observed data ####
  all_observed <- all.sites %>% 
    filter(region == cur.region & !is.na(NZTM_E))
  
  # obs.data = all_observed
  # trafficdata = all_traffic

  ## run model for each site
  all.calc <- regionwise.model(all_traffic,all_observed)
  all.calc <- as.data.table(all.calc)
  # all.calc$mod.no2 <- a*all.calc$sumno2.0.01^b ## once you have the model you can re-run by added A and B
  
  # plot(all.calc$sa_ann,all.calc$mod.no2,
  #      type = "p", main = cur.region)
  nzta.list[[i]] <- all.calc
  # write.csv(all.calc, 
  #           paste0("S:/kachharaa/NO2 spatial modelling/modelforeachsite/", 
  #                  cur.region, "NZTASites_model.csv"),
  #           row.names = F)
  print(paste(cur.region, "done"))
}


## combine all results and export ###
all.nzta <- rbind_list(nzta.list)
write.csv(all.nzta, 
          "S:/kachharaa/NO2 spatial modelling/modelforeachsite/Queenstown.csv", 
          row.names = F)
