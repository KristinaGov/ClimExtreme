# Visualize the weather stations' locations and locations of the power plants

# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "ggExtra",
           "rgdal", "sp", "EnvStats", "classInt", "tmap",
           "tmaptools","magick", "ggnewscale",  "viridis")
for (i in 1:length(packs)) {
  pack <- packs[i]
  if(!is.element(pack, installed.packages()[,1]))
  {install.packages(pack)
  }else {print(paste(packs[i]," already installed"))}
  require(pack,character.only=TRUE)
}
###################################################################

#########             set working directory             ###########
mydir<-getwd()
setwd(mydir)
###################################################################


###################################################################
###################################################################
#                         Read data                               #
###################################################################
###################################################################
plz.de<-readOGR("./data_plz_loc/DE_map.shp",layer ="DE_map")
# Create a dtataframe with the map with location data from the shp file
plz.df<-as.data.frame(plz.de@data)
# Write plz data to file to organize data about weather stations' locations
write.csv(as.data.frame(plz.de@data),file="./data_plz_loc/plz.de.csv")
# Read HOURLY temperatures
# Collect file names in the folder with data on temperatuires from the weather stations
files.hr.ls<-as.list(list.files(path = "./data_temp/stations_hr/tempfiles/", 
                                all.files = T, 
                                full.names = T,
                                pattern = "produkt_tu_stunde"
) )
#length(files.hr.ls) # Check the length of the list of names - 20
# Check that all files exist:
exist.list<-list()
for (i in 1:length(files.hr.ls)) {
  exist.list[[i]]<-file.exists(files.hr.ls[[i]])
}
all(exist.list)
# read data about weather stations (names, ID, location lat / lon)
stations.det.df<-as.data.frame(readxl::read_excel("./data_temp/list_stations.xlsx", 
                                                  sheet = "stat", col_names = T))
# Available column names in each file of the temperature data: SSTATIONS_ID;MESS_DATUM;QN_9;TT_TU;RF_TU;eor
# TT_TU - air temperature at 2 meter high. STATIONS_ID - unique station ID. MESS_DATUM - datetime object.
# Take only necessary data columns:
take<-c("STATIONS_ID")
# Read the data on temperatures to the list of lists:
stations.hr.data.ls<-list()
for (i in 1:length(files.hr.ls)) {
  temp.hr.stations.tb<-read.table(files.hr.ls[[i]], header = TRUE, sep = ";")
  stations.hr.data.ls[[i]]<-unique(temp.hr.stations.tb[take])
}
# Transform lists of lists to the data frame:
stations.hr.id<-as.data.frame(dplyr::bind_rows(stations.hr.data.ls[1:length(stations.hr.data.ls)]))

# The data about plz (post codes) to lat/lon can be found here: http://download.geonames.org/export/zip/
plztolat.df<-as.data.frame(readxl::read_excel("./data_plz_util/plztolat.xlsx", col_names = TRUE,range = "B1:F16700",))

# Collect the data about the selected 20 stations from the data frame with all stations' details
about.hr.stations.df<-dplyr::left_join(stations.hr.id, stations.det.df, by=c("STATIONS_ID"="id") )

# Read the data about units position:
units<-as.data.frame(readxl::read_excel("./data_plz_util/KWK2019.xlsx", sheet= "units", col_names = TRUE))
###################################################################



###################################################################
###################################################################
#                    Plot weather stations                        #
###################################################################
###################################################################
# To plot weather stations on the map we need to know their plz (post codes)
# according to the SHP file we have!
# To find the plz by the given lon/lat position - we find the closest plz centered location 
# given in the plztolat data:

tmp.loc.df<-plztolat.df
lonlat.ls<-list()

for (i in 1:dim(about.hr.stations.df)[1]) {
  loc.df<-about.hr.stations.df[i,]
  loc.mat<-as.matrix(loc.df[,c("lat", "lon")])
  tmp.loc.df$loc.lat<-loc.df$lat
  tmp.loc.df$loc.lon<-loc.df$lon
  tmp.loc.df$euc<-sqrt(apply( (( as.matrix(tmp.loc.df[,c("lat", "lon")]) - as.matrix(tmp.loc.df[,c("loc.lat", "loc.lon")]) ) ^ 2),1, sum) )
  lonlat.df<-data.frame(id=loc.df$STATIONS_ID,
                        city=loc.df$city,
                        region=loc.df$region,
                        lat=loc.df$lat,
                        lon=loc.df$lon,
                        from=loc.df$from,
                        to=loc.df$to,
                        plz=tmp.loc.df[which.min(tmp.loc.df$euc),"plz"])
  lonlat.ls[[i]]<-lonlat.df
}
loc.stat.df<- dplyr::bind_rows(lonlat.ls[1:length(lonlat.ls)])
sum(is.na(loc.stat.df$lon)) # Check: has to be 0
sum(is.na(loc.stat.df$lat)) # Check: has to be 0
summary(loc.stat.df)

# Add color index to the regions where weather stations are located (so we mark them on the map):
loc.stat.df$bubble<-1
# Create a label name to show on the map:
loc.stat.df$full_name<-paste(loc.stat.df$city, ", St. No: ", loc.stat.df$id, sep="")
plz.df$plz<-as.numeric(plz.df$plz)
# Add this data to the data frame sourced from shp
data:
all.plz.stat.df<-left_join(plz.df, loc.stat.df[,c("plz","full_name","color", "bubble")], by=c("plz"="plz"))
sum(!is.na(all.plz.stat.df$full_name)) # Check - have to be the same number as stations in the set 20

# Write data about weather to the shp file @data:
plz.de@data<-all.plz.stat.df


# Finally plot:
graphname<-paste("./Rplots/stations_locations.png", sep="")
graph<- tm_shape(plz.de) + 
  tm_fill(col="grey", border.col ="white") +
  tm_shape(plz.de) +
  tm_symbols(shape="bubble", size=0.3, 
             col = "full_name",
             shapeNA = NA, showNA = NULL, colorNA = NULL, textNA = NULL,
             title.col=paste("Weather stations", sep = " " ),
             legend.shape.show = F,) +
  tm_layout(inner.margins=c(0.02,0.02,0.02,0.02),  # bottom, left, top, and right 
            outer.margins=c(0.02,0.02,0.02,0.02),
            legend.outside=T,
            legend.outside.position =c("right"),
            legend.just="center",legend.frame=F,
            legend.width=-0.6,legend.text.size=0.6) 
tmap_save(graph, filename = graphname)
dev.off()
###################################################################



###################################################################
## Not run:
# Plotting via maptools for separate country is not very accurate.
library(maptools)   # for data(wrld_simpl)
data(wrld_simpl)

country.names<-c("DE")
wrld <- subset(wrld_simpl, wrld_simpl@data$ISO2 %in% country.names)
wrld.f<-ggplot2::fortify(wrld, region="ISO2")

ggplot()+
  geom_map(data=wrld.f, map=wrld.f, aes(map_id=id, x=long, y=lat),fill="gray",color="#7f7f7f",size=0.25) +
  geom_point(data=about.hr.stations.df, 
             aes(x=lon, y=lat,  color = factor(city)),
             size=4) 
## End(**Not run**)
###################################################################



###################################################################
###################################################################
#                    Plot generation units                        #
###################################################################
###################################################################
plz.de@data<-units
bubble<-c("coal", "gas", "nuc")
title<-c("coal", "gas", "nuclear")
pplant<-cbind(bubble,title)
col<-c("sienna4", "purple4", "paleturquoise3")
ycol<-cbind(bubble,col)

graphname<-paste("./Rplots/powerplants_locations.png",sep="")
graph<- tm_shape(plz.de)+
  tm_fill(col="grey",border.col ="white")+
  tm_bubbles(size= pplant[1,1],col=col[1], alpha=0.7,
             border.col=col[1],scale=1, 
             size.lim = c(0, 6000),  
             shapes.legend.fill = "gray",
             title.size=paste("Installed Capacities of",pplant[1,1], sep = " " )) +
  tm_shape(plz.de)+
  tm_bubbles(size= pplant[2,1],col=col[2], alpha=0.7,
             border.col=col[2],scale=1, 
             size.lim = c(0, 6000), 
             title.size=paste("Installed Capacities of",pplant[2,1], sep = " " )) +
  tm_shape(plz.de)+
  tm_bubbles(size= pplant[3,1],col=col[3], alpha=0.7,
             border.col=col[3],scale=1, 
             size.lim = c(0, 6000), 
             title.size=paste("Installed Capacities of",pplant[3,1], sep = " " )) +
  tm_layout(inner.margins=c(0.02,0.02,0.02,0.02),
            outer.margins=c(0.02,0.02,0.02,0.04),
            legend.outside=T,
            legend.outside.position =c("right"),
            legend.just="center",legend.frame=F,
            legend.width=-0.6,legend.text.size=0.6)
tmap_save(graph, filename = graphname )
dev.off()
##################################################################