# This script reads the data on temperatures in Germany from the source files
# Source: Deutscher Wetterdienst (DWD)
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/soil_temperature/historical/
# In the "./data_temp/stations/tempfiles/" and ."/data_temp/stations_hr/tempfiles/" is taken for the selected weather stations
# on the basis of the earliest records available and geographical locations.
# Here you find the figure of temperatures time series since 1893 and smoothed time series showing the trend (simple moving average)

# Make sure the work space is in pristine condition:
rm(list=ls(all=TRUE))

#########                call libraries                 ###########
packs <- c("dplyr", "plyr","tidyr","raster","ncdf4","ggplot2", "ggnewscale",
           "RColorBrewer","ggpubr","readxl", "pastecs","xts", "TTR","ggfortify",
           "CommEcol", "fBasics", "EnvStats", "geosphere", "ggpmisc", "scales",
           "lubridate","tidyquant", "gridExtra", "tidyverse", "viridis",
           "ts", "strucchange", "fxregime", "zoo", "fitdistrplus")
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


##################################################################
##################################################################
#                   Read files and glimpse                       #
##################################################################
##################################################################

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
# Read data about weather stations (names, ID, location lat / lon)
stations.det.df<-as.data.frame(readxl::read_excel("./data_temp/list_stations.xlsx", 
                                                  sheet = "stat", col_names = T))
# Available column names in each file of the temperature data: SSTATIONS_ID;MESS_DATUM;QN_9;TT_TU;RF_TU;eor
# TT_TU - air temperature at 2 meter high. STATIONS_ID - unique station ID. MESS_DATUM - datetime object.
# Take only necessary data columns:
take<-c("STATIONS_ID","MESS_DATUM","TT_TU")
# Read the data on temperatures to the list of lists:
stations.hr.data.ls<-list()
for (i in 1:length(files.hr.ls)) {
  temp.hr.stations.tb<-read.table(files.hr.ls[[i]], header = TRUE, sep = ";")
  stations.hr.data.ls[[i]]<-temp.hr.stations.tb[take]
}
# Transform lists of lists to the dataframe:
stations.hr.data.df<-as.data.frame(dplyr::bind_rows(stations.hr.data.ls[1:length(stations.hr.data.ls)]))
# "-999" is a default NA value in the DWD data. Remove it for default R designation "NA":
stations.hr.data.df$TT_TU[stations.hr.data.df$TT_TU == "-999"]<-NA
# Transform time-stamp to date format (have a look at the start datetime value, UTC tz is defined in metadata):
stations.hr.data.df$datetime<-as.POSIXct(as.character(stations.hr.data.df$MESS_DATUM), 
                                     origin ="1893010101",
                                     format="%Y%m%d%H", tz="UTC")

#########        First glance: temperature data        ###########
# Check: the data has hourly resolution, but maybe there are missing hours!
datatime<-length(unique(stations.hr.data.df$MESS_DATUM)) # Check: 1104467 unique hours
# Take min and max date in the data and check how many hours should be in the sequence:
seqntime<-length(seq(min(stations.hr.data.df$date), max(stations.hr.data.df$date), by="hours" ) ) # Check: 1104479 hours!
# There are 12 hours missing! 
datatime-seqntime
# Take time sequence to the new data frame:
stations.hr.df<-data.frame(datetime=seq(min(stations.hr.data.df$date), max(stations.hr.data.df$date), by="hours"))
# Create temporary data frame to make time series from the original data and merge it with the full sequence of dates:
temporary.df<-data.frame(date=unique(stations.hr.data.df$MESS_DATUM))
temporary.df$datetime<-as.POSIXct(as.character(temporary.df$date), 
                                         origin ="1893010101",
                                         format="%Y%m%d%H", tz="UTC")
stations.hr.df<-left_join(stations.hr.df,temporary.df, by=c("datetime"="datetime"))
# Have a look what dates are missing: They are from 1934-08-18 13 to 1934-08-19 00:00:00:
stations.hr.df[which(is.na(stations.hr.df$date)),]
# Take unique stations' id, and check how many stations are in the sample:
length(unique(stations.hr.data.df$STATIONS_ID)) # Check: 20
# Create a data frame to collect data specifically on the meteo-stations in the sample:
stations.hr.id<-as.data.frame(unique(stations.hr.data.df$STATIONS_ID))
colnames(stations.hr.id)<-"id"
# Collect the data about the selected 20 stations from the dataframe with all stations' details
about.hr.stations.df<-dplyr::left_join(stations.hr.id, stations.det.df, by=c("id"="id") )
# Collect the measurements and data about stations to one data frame
# And reshape long data frame to columns by station id.
for (i in 1:dim(stations.hr.id)[1]) {
  tomerge.df<-dplyr::select(dplyr::filter(stations.hr.data.df, STATIONS_ID==about.hr.stations.df[i,1]),
                            c("MESS_DATUM", "TT_TU"))
  cname<-paste(about.hr.stations.df[i,1], sep="")
  colnames(tomerge.df)<-c("date", cname)
  stations.hr.df<-left_join(stations.hr.df, tomerge.df, by=c("date"="date"))
}
# Split the time-stamp to "year", "mon", "day", "hr"
stations.hr.df<-tidyr::extract(stations.hr.df, date, into = c("year", "mon", "day","hr"), "(.{4})(.{2})(.{2})(.{2})", remove=F)
##################################################################

##################################################################
#file.name.3<-paste("./data_temp/stations/about_hr_stations.csv" )
#write.csv(about.hr.stations.df, file=file.name.3)
##################################################################

##################################################################
##################################################################
#                  Brief regional glimpse                        #
##################################################################
##################################################################

#########          Gather t° data by regions           ###########
# Probably can be done in a more efficient manner!
# Gather regions by relative positions:
s.reg<-c("Bayern","Baden-Württemberg")
mid.reg<-c("Hessen", "Thüringen", "Sachsen-Anhalt")
n.reg<-c("Schleswig-Holstein","Mecklenburg-Vorpommern", "Niedersachsen")
e.reg<-c("Brandenburg", "Sachsen", "Berlin")
w.reg<-c("Nordrhein-Westfalen","Rheinland-Pfalz", "Saarland")

south<-dplyr::select(dplyr::filter(about.hr.stations.df, region %in% s.reg ), c("id"))
south<-as.character(south$id)
mid<-dplyr::select(dplyr::filter(about.hr.stations.df, region %in% mid.reg ), c("id"))
mid<-as.character(mid$id)
north<-dplyr::select(dplyr::filter(about.hr.stations.df, region %in% n.reg ), c("id"))
north<-as.character(north$id)
east<-dplyr::select(dplyr::filter(about.hr.stations.df, region %in% e.reg ), c("id"))
east<-as.character(east$id)
west<-dplyr::select(dplyr::filter(about.hr.stations.df, region %in% w.reg ), c("id"))
west<-as.character(west$id)
##################################################################

#########       Take row means and max by regions      ###########
stations.hr.df$DEavr<-rowMeans(stations.hr.df[,as.character(about.hr.stations.df$id)], na.rm = T)
stations.hr.df$s.avr<-rowMeans(subset(stations.hr.df, select=south), na.rm = T)
stations.hr.df$m.avr<-rowMeans(subset(stations.hr.df, select=mid),   na.rm = T)
stations.hr.df$n.avr<-rowMeans(subset(stations.hr.df, select=north), na.rm = T)
stations.hr.df$e.avr<-rowMeans(subset(stations.hr.df, select=east),  na.rm = T)
stations.hr.df$w.avr<-rowMeans(subset(stations.hr.df, select=west),  na.rm = T)
stations.hr.df$DEmax<-apply(dplyr::select(stations.hr.df, as.character(about.hr.stations.df$id)), 1, function(x) max(na.omit(x)))
# replace NaN and inf entries occurred due to mena and max for absent measurments in the dataset
stations.hr.df[mapply(is.infinite, stations.hr.df)] <- NA
stations.hr.df[mapply(is.nan, stations.hr.df)] <- NA
##################################################################
#file.name.4<-paste("./data_temp/stations/stations_hr_temp.csv")
#write.csv(stations.hr.df, file=file.name.4)
##################################################################


##################################################################
##################################################################
#       Find long-term trends with smoothing filter (SMA)        #
#                    And plot the graphs                         #
##################################################################
##################################################################
begin.1<- (min(stations.hr.df$datetime))  # "1893-01-01 01:00:00 UTC"
end.1  <- (max(stations.hr.df$datetime))  # "2018-12-31 23:00:00 UTC"
NO.1   <- as.numeric(lubridate::ymd_hms(end.1)-lubridate::ymd_hms(begin.1))*24 #Number of hours: 1104478
# Check the end date - it has to be the same as in the data (see end.1):
max(lubridate::ymd_hms(begin.1) + lubridate::hours(0:NO.1)) 
# Set the time frequencies of measurements in the sample per year and day. 
# Aware of leap years!
n.years<-2
n.days<-365
n.hours<-24
##################################################################

# For each time series add approx values for NA entries (for 12 missing hours in 1934)
# Missing values (NAs) are replaced by linear interpolation.
# Why doing this? - NA are not recognized by SMA.

#########      Do SMA for average of 20 meteostations  ###########
#########                       Plot SMA               ###########
DE.avr<-na.approx(stations.hr.df$DEavr, na.rm = FALSE)
DE.max<-na.approx(stations.hr.df$DEmax, na.rm = FALSE)
# Make time series from the data (frequency - number of observations per year): 
DE.avr.ts<-ts(DE.avr, start=c(1893,01,01,NO.1), frequency=n.hours*n.days)
glimpse(DE.avr.ts)
# SMA function from the TTR package, n - number of periods to estimate average [years*days*years]
DE.avr.ts.sma<-SMA(DE.avr.ts, n=n.years*n.hours*n.days)
# Transform SMA to ts format
DE.avr.ts.sma.ts<-ts(DE.avr.ts.sma,  start=c(1893,01,01,NO.1), frequency=n.hours*n.days)
glimpse(DE.avr.ts.sma.ts)
#########      Do SMA for maximum of 20 meteostations  ###########
DE.max.ts<-ts(DE.max, start=c(1893,01,01,NO.1), frequency=n.hours*n.days)
glimpse(DE.max.ts)
DE.max.ts.sma<-SMA(DE.max.ts, n=n.years*n.hours*n.days)
DE.max.ts.sma.ts<-ts(DE.max.ts.sma,  start=c(1893,01,01,NO.1), frequency=n.hours*n.days)
#########                  aggregate                   ###########
ggdf.1 <- data.frame(date=seq(begin.1,end.1,by="hours"), tmp=as.matrix(DE.avr.ts))
ggdf.2 <- data.frame(date=seq(begin.1,end.1,by="hours"), tmp=as.matrix(DE.avr.ts.sma.ts))
ggdf.3 <- data.frame(date=seq(begin.1,end.1,by="hours"), tmp=as.matrix(DE.max.ts.sma.ts))
#########                   Plot SMA                   ###########
ggplot()+
  #geom_line(data=ggdf.1,aes(x=date, y=tmp)) +
  #geom_line(data=ggdf.2,aes(x=date, y=tmp), col="wheat", size=1) +
  #geom_line(data=ggdf.3,aes(x=date, y=tmp), col="coral3", size=1) + brewer.pal(6, "Set3")[3]
  geom_line(data=ggdf.2,aes(x=date, y=tmp), col=brewer.pal(4, "Set3")[4], size=1) +
  geom_line(data=ggdf.3,aes(x=date, y=tmp), col=brewer.pal(4, "Set3")[3], size=1) + 
  labs(title = "Time seies: hourly temperatures [t°C]",
       subtitle = paste(n.years, "-years simple moving average for DE average (beige)","\n" ,
                        n.years, "-years simple moving average for DE maximum (red)","\n" ,
                        "Air temperature at 2 meter high", sep=""),
       y="Temperature [C°]") +
  scale_x_datetime(name="", labels = date_format("%Y"), 
                   expand=c(0,0),date_breaks="10 years",
                   date_minor_breaks = "years",
                   limits = (c(begin.1,end.1))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=19),
        axis.text.y = element_text(angle = 90, hjust = 1, size=19),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.justification="center", 
        legend.position=c(0.95,0.89),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=24),
        legend.key.size = unit(1,"line"),
        legend.title.align=0.5)
#ggsave("./Rplots/temperatures.png", dpi = 600, width = 540, height = 310, units = "mm")
ggsave("./Rplots/temperature_trends.png", dpi = 300, width = 210, height = 210, units = "mm")
##################################################################
# Files are saved in ./Rplot/temperatures.png and ./Rplot/temperatures_trends.png